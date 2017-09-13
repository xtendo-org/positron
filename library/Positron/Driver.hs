module Positron.Driver
    ( ConnConf
    , defaultConnConf
    , setDBHost
    , setDBPort
    , setDBName
    , setDBUser
    , setDBPassword
    , connect
    , close
    , withDatabase
    , unsafeExecPrepared
    , unsafeExec
    ) where

import Positron.Import

-- data types

import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- extra modules

import qualified Database.PostgreSQL.LibPQ as PQ

-- local modules

import Positron.Types
import Positron.Parser

data ConnConf = ConnConf
    { dbHost :: Maybe Text
    , dbPort :: Maybe Word16
    , dbName :: Maybe Text
    , dbUser :: Maybe Text
    , dbPassword :: Maybe Text
    }

defaultConnConf :: ConnConf
defaultConnConf = ConnConf Nothing Nothing Nothing Nothing Nothing

setDBHost :: ConnConf -> Maybe Text -> ConnConf
setDBHost c x = c { dbHost = x }
infixl 4 `setDBHost`

setDBPort :: ConnConf -> Maybe Word16 -> ConnConf
setDBPort c x = c { dbPort = x }
infixl 4 `setDBPort`

setDBName :: ConnConf -> Maybe Text -> ConnConf
setDBName c x = c { dbName = x }
infixl 4 `setDBName`

setDBUser :: ConnConf -> Maybe Text -> ConnConf
setDBUser c x = c { dbUser = x }
infixl 4 `setDBUser`

setDBPassword :: ConnConf -> Maybe Text -> ConnConf
setDBPassword c x = c { dbPassword = x }
infixl 4 `setDBPassword`

connect
    :: Positron positron
    => ConnConf
    -> IO positron
connect ConnConf{..} = do
    conn <- PQ.connectdb conninfo
    _ <- PQ.exec conn "SET client_min_messages TO WARNING;"
    positron <- pMake conn
    _ <- unsafeExec positron (pCreateQueries positron) >>= \case
        Right _ -> return conn
        Left err -> fail ("While executing CREATE queries: " <> show err)
    forM_ (pPrepareds positron) $ \ (stmtName, stmtQuery) -> let
        onError = do
            -- B.putStrLn stmtName
            -- PQ.errorMessage conn >>= maybe (return ()) B.putStrLn
            B.putStrLn $ fold [stmtName, " failed, deallocate and retry"]
            _ <- PQ.exec conn ("DEALLOCATE \"" <> stmtName <> "\";")
            PQ.errorMessage conn >>= maybe (return ()) B.putStrLn
            prepare
        prepare = PQ.prepare conn stmtName stmtQuery Nothing >>= \ case
            Nothing -> onError
            Just result -> PQ.resultStatus result >>= \ case
                PQ.CommandOk -> return ()
                PQ.TuplesOk -> return ()
                _ -> onError
        in prepare
    return positron
  where
    conninfo = B.intercalate " " $ mapMaybe (fmap T.encodeUtf8) sources
    sources =
        [ fmap ("host=" <>) dbHost
        , fmap (("port=" <>) . T.pack . show) dbPort
        , fmap ("dbname=" <>) dbName
        , fmap ("user=" <>) dbUser
        , fmap ("password=" <>) dbPassword
        ]

close :: Positron p => p -> IO ()
close p = PQ.finish (pConn p)

withDatabase :: Positron positron
    => ConnConf -> (positron -> IO a) -> IO a
withDatabase connConf = bracket (connect connConf) close

execBase
    :: Positron p => p
    -> ByteString
    -> (Connection -> IO (Maybe PQ.Result))
    -> IO (Either PositronError PQ.Result)
execBase positron errorInfo action = withLock lock $ action conn >>= \case
    Nothing -> unknownError
    Just result -> PQ.resultStatus result >>= \case
        PQ.CommandOk -> returnAfterError result
        PQ.TuplesOk -> returnAfterError result
        _ -> unknownError
  where
    conn = pConn positron
    lock = pLock positron
    unknownError = PQ.errorMessage conn >>= \case
        Nothing -> hopeLost "unknown PostgreSQL error"
        Just bErr -> let err = T.decodeUtf8 bErr in case parsePQError err of
            Left _ -> hopeLost err
            Right pErr -> return $ Left pErr
    hopeLost msg = return $ Left $ UnknownPositronError msg
    -- FIXME: print something better than "preparedName" for easier debugging
    printStmt = B.putStr (errorInfo <> "\n")
    printIf s = when (s /= "") $ printStmt >> print s

    returnAfterError result = do
        PQ.errorMessage conn >>= maybe (return ()) printIf
        return (Right result)

unsafeExecPrepared
    :: Positron p => p
    -> ByteString -> [Maybe ByteString] -> IO (Either PositronError PQ.Result)
unsafeExecPrepared positron preparedName args = execBase positron preparedName
    (\ conn -> PQ.execPrepared conn preparedName fields PQ.Binary)
  where
    -- FIXME: print something better than "preparedName" for easier debugging
    fields :: [Maybe (ByteString, PQ.Format)]
    fields = map withFormatting args
    withFormatting :: Maybe ByteString -> Maybe (ByteString, PQ.Format)
    withFormatting = fmap $ \ x -> (x, PQ.Binary)

unsafeExec
    :: Positron p => p -> ByteString -> IO (Either PositronError PQ.Result)
unsafeExec positron stmt = execBase positron stmt (`PQ.exec` stmt)

withLock :: MVar () -> IO a -> IO a
withLock lock action = bracket (takeMVar lock) (putMVar lock) (const action)
