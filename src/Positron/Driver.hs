{-# language LambdaCase #-}

module Positron.Driver
    ( connect
    , unsafeExecPrepared
    , unsafePlainExec
    , unsafeRawExec
    ) where

import Positron.Import

-- data types

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- extra modules

import qualified Database.PostgreSQL.LibPQ as PQ

-- local modules

import Positron.Types
import Positron.Parser

connect
    :: Positron positron
    => Maybe Text
    -> Maybe Word16
    -> Maybe Text
    -> Maybe Text
    -> Maybe Text
    -> IO positron
connect dbHost dbPort dbName dbUser dbPassword = do
    conn <- PQ.connectdb conninfo
    _ <- PQ.exec conn "SET client_min_messages TO WARNING;"
    pMake conn
  where
    conninfo = B.intercalate " " $ mapMaybe (fmap T.encodeUtf8) sources
    sources =
        [ fmap ("host=" <>) dbHost
        , fmap (("port=" <>) . T.pack . show) dbPort
        , fmap ("dbname=" <>) dbName
        , fmap ("user=" <>) dbUser
        , fmap ("password=" <>) dbPassword
        ]

execBase
    :: Positron p => p
    -> ByteString
    -> (PQ.Connection -> IO (Maybe PQ.Result))
    -> IO (Either PositronError ())
execBase positron errorInfo action = withLock lock $ action conn >>= \case
    Nothing -> unknownError
    Just result -> PQ.resultStatus result >>= \case
        PQ.CommandOk -> do
            PQ.errorMessage conn >>= maybe (return ()) printIf
            return (Right ())
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

unsafeExecPrepared
    :: Positron p => p
    -> ByteString -> [Maybe ByteString] -> IO (Either PositronError ())
unsafeExecPrepared positron preparedName args = execBase positron preparedName
    (\ conn -> PQ.execPrepared conn preparedName fields PQ.Binary)
  where
    -- FIXME: print something better than "preparedName" for easier debugging
    fields :: [Maybe (ByteString, PQ.Format)]
    fields = map withFormatting args
    withFormatting :: Maybe ByteString -> Maybe (ByteString, PQ.Format)
    withFormatting = fmap $ \ x -> (x, PQ.Binary)

unsafePlainExec
    :: Positron p => p -> ByteString -> IO (Either PositronError ())
unsafePlainExec positron stmt = execBase positron stmt
    (`PQ.exec` stmt)

unsafeRawExec
    :: Positron p
    => p
    -> ByteString
    -> IO (Either ByteString PQ.Result)
unsafeRawExec positron stmt = withLock lock $
    PQ.exec conn stmt >>= \case
        Nothing -> unknownError
        Just result -> PQ.resultStatus result >>= \case
            PQ.TuplesOk -> do
                PQ.errorMessage conn >>= maybe (return ()) printIf
                return (Right result)
            _ -> unknownError
  where
    conn = pConn positron
    lock = pLock positron
    unknownError = Left . fromMaybe "unknown PostgreSQL error" <$>
        PQ.errorMessage conn
    printIf s = when (s /= "") $ print s

withLock :: MVar () -> IO a -> IO a
withLock lock action = do
    takeMVar lock
    value <- action
    putMVar lock ()
    return value
