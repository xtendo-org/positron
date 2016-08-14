{-# language LambdaCase #-}

module Positron.Driver
    ( Connection(..)
    , connect
    , unsafePlainExec
    , unsafeRawExec
    ) where

import Import

-- data types

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- extra modules

import qualified Database.PostgreSQL.LibPQ as PQ

newtype Connection = Conn PQ.Connection

instance Show Connection where
    show _ = "<PostgreSQL connection>"

connect
    :: Maybe Text
    -> Maybe Word16
    -> Maybe Text
    -> Maybe Text
    -> Maybe Text
    -> IO Connection
connect dbHost dbPort dbName dbUser dbPassword = do
    conn <- PQ.connectdb conninfo
    return (Conn conn)
  where
    conninfo = B.intercalate " " $ mapMaybe (fmap T.encodeUtf8) sources
    sources =
        [ fmap ("host=" <>) dbHost
        , fmap (("port=" <>) . T.pack . show) dbPort
        , fmap ("dbname=" <>) dbName
        , fmap ("user=" <>) dbUser
        , fmap ("password=" <>) dbPassword
        ]

unsafePlainExec :: Connection -> ByteString -> IO (Either ByteString ())
unsafePlainExec (Conn conn) stmt = PQ.exec conn stmt >>= \case
    Nothing -> unknownError
    Just result -> PQ.resultStatus result >>= \case
        PQ.CommandOk -> do
            PQ.errorMessage conn >>= maybe (return ()) printIf
            return (Right ())
        _ -> unknownError
  where
    unknownError = Left . fromMaybe "unknown PostgreSQL error" <$>
        PQ.errorMessage conn
    printStmt = B.putStr (stmt <> "\n")
    printIf s = when (s /= "") $ printStmt >> print s


unsafeRawExec
    :: Connection
    -> ByteString
    -> IO (Either ByteString PQ.Result)
unsafeRawExec (Conn conn) stmt = PQ.exec conn stmt >>= \case
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
