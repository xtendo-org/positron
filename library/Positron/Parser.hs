module Positron.Parser
    ( parsePQError
    ) where

import Positron.Import

-- extra modules

import qualified Data.Attoparsec.Text as A

-- local modules

import Positron.Types

parsePQError :: Text -> Either String PositronError
parsePQError = A.parseOnly pqError

pqError :: A.Parser PositronError
pqError = do
    _ <- A.string "ERROR: "
    _ <- A.takeWhile isSpace
    A.choice [duplicateKeyParser]
  where
    duplicateKeyParser = do
        _ <- A.string "duplicate key"
        _ <- A.takeTill (== ':')
        _ <- A.takeTill (== '(')
        _ <- A.anyChar
        key <- A.takeTill (== ')')
        _ <- A.takeTill (== '(')
        _ <- A.anyChar
        value <- A.takeTill (== ')')
        return $ DuplicateKey key value
