{-# language LambdaCase #-}

module Positron.Driver
    ( Positron(..)
    , connect
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

data Positron = Conn PQ.Connection (MVar ())

instance Show Positron where
    show _ = "<Positron object>"

connect
    :: Maybe Text
    -> Maybe Word16
    -> Maybe Text
    -> Maybe Text
    -> Maybe Text
    -> IO Positron
connect dbHost dbPort dbName dbUser dbPassword = do
    conn <- PQ.connectdb conninfo
    box <- newMVar ()
    _ <- PQ.exec conn "SET client_min_messages TO WARNING;"
    return (Conn conn box)
  where
    conninfo = B.intercalate " " $ mapMaybe (fmap T.encodeUtf8) sources
    sources =
        [ fmap ("host=" <>) dbHost
        , fmap (("port=" <>) . T.pack . show) dbPort
        , fmap ("dbname=" <>) dbName
        , fmap ("user=" <>) dbUser
        , fmap ("password=" <>) dbPassword
        ]

unsafePlainExec :: Positron -> ByteString -> IO (Either PositronError ())
unsafePlainExec (Conn conn lock) stmt = withLock lock $
    PQ.exec conn stmt >>= \case
        Nothing -> unknownError
        Just result -> PQ.resultStatus result >>= \case
            PQ.CommandOk -> do
                PQ.errorMessage conn >>= maybe (return ()) printIf
                return (Right ())
            _ -> unknownError
  where
    unknownError = PQ.errorMessage conn >>= \case
        Nothing -> hopeLost
        Just err -> case parsePQError $ T.decodeUtf8 err of
            Left _ -> hopeLost
            Right pErr -> return $ Left pErr
    hopeLost = return $ Left $ UnknownPositronError "unknown PostgreSQL error"
    printStmt = B.putStr (stmt <> "\n")
    printIf s = when (s /= "") $ printStmt >> print s


unsafeRawExec
    :: Positron
    -> ByteString
    -> IO (Either ByteString PQ.Result)
unsafeRawExec (Conn conn lock) stmt = withLock lock $
    PQ.exec conn stmt >>= \case
        Nothing -> unknownError
        Just result -> PQ.resultStatus result >>= \case
            PQ.TuplesOk -> do
                PQ.errorMessage conn >>= maybe (return ()) printIf
                return (Right result)
            _ -> unknownError
  where
    unknownError = Left . fromMaybe "unknown PostgreSQL error" <$>
        PQ.errorMessage conn
    printIf s = when (s /= "") $ print s

withLock :: MVar () -> IO a -> IO a
withLock lock action = do
    takeMVar lock
    value <- action
    putMVar lock ()
    return value
