{-# language LambdaCase #-}

module Positron.Driver
    ( Connection(..)
    , connect
    , plainExec
    ) where

-- data types

import Control.Monad
import Data.Maybe
import Data.Monoid
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word (Word16)

-- extra modules

import qualified Database.PostgreSQL.LibPQ as PQ

newtype Connection = Conn PQ.Connection

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

plainExec :: Connection -> ByteString -> IO (Either ByteString ())
plainExec (Conn conn) stmt = printStmt >> PQ.exec conn stmt >>= \case
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
    printIf s = when (s /= "") $ print s
