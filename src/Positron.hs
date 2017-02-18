{-# language TemplateHaskell #-}
{-# language RecordWildCards #-}
{-# language LambdaCase #-}

module Positron
    ( table
    , Column
    , ColumnType
    , ColumnProp(..)
    , (//)
    , Query(..)
    , module Positron.Alias
    , module Positron.Query
    , mkPositron
    , mkCreateAll

    , Positron
    , connect
    , unsafePlainExec
    , unsafeRawExec

    , PositronError(..)

    -- re-export data types
    , Int16
    , Int32
    , Int64
    , Scientific
    , Float
    , Double
    , Word16
    , Word32
    , Word64
    , ByteString
    , Text

    ) where

import Positron.Import

-- extra modules

import qualified Data.ByteString.Char8 as B (pack)
import qualified Database.PostgreSQL.LibPQ as PQ

-- local modules

import Positron.Alias
import Positron.Driver
import Positron.Query
import Positron.Types
import Positron.Unsafe
import Positron.Util

mkPositron :: String -> Q [Dec]
mkPositron namespace = do
    createStmts <- mkCreateAll

    -- record field names that cannot be captured
    connField <- newName "conn"
    connType <- [t| Connection |]
    lockField <- newName "lock"
    lockType <- [t| MVar () |]

    let
        rec fieldName fieldType =
            (fieldName, Bang NoSourceUnpackedness SourceStrict, fieldType)
        recs =
            [ rec connField connType
            , rec lockField lockType
            ]
        dataDec = DataD [] (mkName $ "Positron" <> namespace) [] Nothing
            [RecC dataName recs] []

    pairs <- runIO readPrepared
    instanceDec <- [d|
        instance Positron $(return $ ConT dataName) where
            pConn = $(return $ VarE connField)
            pLock = $(return $ VarE lockField)
            pMake conn = do
                -- TODO: handle prepare errors
                forM_ pairs $ \ (stmtName, stmtQuery) ->
                    PQ.prepare conn stmtName stmtQuery Nothing
                lock <- newMVar ()
                return $ $(return $ ConE dataName) conn lock
        |]

    return $ dataDec : instanceDec <> createStmts
  where
    dataName = mkName $ "Positron" <> namespace

mkCreateAll :: Q [Dec]
mkCreateAll = getCurrentTableMap >>= tree
  where
    tree tableMap = [d|
        createAll :: ByteString
        createAll = fold $ reverse
            $(return $ ListE
                (map (VarE . mkName . ("create" ++) . cap . fst) tableMap)
            )
        |]

table :: String -> [Column] -> Q [Dec]
table tabName pcols = do
    columns <- mapM analyze pcols
    thisModuleStr <- show <$> thisModule
    addTable thisModuleStr (tabName, [(acn, a) | a@AC{..} <- columns])
    let
        cqName = mkName $ "create" ++ capTabName
        cqSigDec = SigD cqName (ConT ''ByteString)
        recs = for columns $ \ac@AC{..} ->
            ( mkName $ decap tabName ++ cap acn
            , Bang
                (if acnl then NoSourceUnpackedness else SourceUnpack)
                SourceStrict
            , columnTypeCon ac
            )
        primaryKeys = map (snake . acn) $ filter acp columns
        foreignKeys = gatherFKs columns
        indexedKeys = map (snake . acn) $ filter aci columns
        createQuery = fold
            [ "CREATE TABLE IF NOT EXISTS "
            , snakeTabName
            , " (\n    "
            , fold $ for columns $ \AC{..} -> fold
                [ snake acn, " ", show act
                , if acUnique then " UNIQUE" else ""
                , if acnl then " NULL" else " NOT NULL"
                , ",\n    "
                ]
            , "PRIMARY KEY ("
            , intercalate ", " primaryKeys
            , ")"
            , if foreignKeys /= []
                then concatMap (",\n    " ++) foreignKeys
                else ""
            , "\n);\n"
            , if indexedKeys /= []
                then fold $ for indexedKeys $ \colName -> fold
                    [ "CREATE INDEX IF NOT EXISTS ix_"
                    , snakeTabName, "_", colName
                    , " ON ", snakeTabName, " (", colName, ");\n"
                    ]
                else ""
            ]
    cqExp <- [| B.pack createQuery |]
    let cqValDec = ValD (VarP cqName) (NormalB cqExp) []
    return
        [ DataD [] dataName [] Nothing [RecC dataName recs]
            [ConT ''Eq, ConT ''Show]
        , cqSigDec
        , cqValDec
        ]
  where
    snakeTabName = snake tabName
    capTabName = cap tabName
    dataName = mkName capTabName
    gatherFKs [] = []
    gatherFKs (AC{..} : cs) = case acf of
        Just (tn, cn) -> fmtFK acn tn cn : gatherFKs cs
        Nothing -> gatherFKs cs
    fmtFK n t c = fold
        [ "FOREIGN KEY(", snake n, ") REFERENCES "
        , snake t, " (", snake c, ")"
        ]

analyze :: Column -> Q AnalyzedColumn
analyze (Column n t pk idx nl unique) = case t of
    Psmallint -> ret DBsmallint
    Pinteger -> ret DBinteger
    Pbigint -> ret DBbigint
    Pdecimal -> ret DBdecimal
    Pnumeric -> ret DBnumeric
    Preal -> ret DBreal
    Pdouble -> ret DBdouble
    Psmallserial -> ret DBsmallserial
    Pserial -> ret DBserial
    Pbigserial -> ret DBbigserial
    Pvarchar len -> ret $ DBvarchar len
    Ptext -> ret DBtext
    Pforeignkey s -> do
        let
            (tn, dottedColName) = break (== '.') s
            cn = tail dottedColName
        thisModuleStr <- show <$> thisModule
        lookupColumn thisModuleStr tn cn >>= \case
            Nothing -> fail $ fold
                ["Column \"", cn, "\" of Table \"", tn, "\" not found"]
            Just AC{..} -> return $ acBase (plain act) (Just (tn, cn))
  where
    ret dt = return (acBase dt Nothing)
    acBase = AC n pk idx nl unique
    plain = \case
        DBsmallserial -> DBsmallint
        DBserial -> DBinteger
        DBbigserial -> DBbigint
        x -> x
