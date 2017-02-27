{-# language TemplateHaskell #-}
{-# language RecordWildCards #-}
{-# language LambdaCase #-}

module Positron
    ( model
    , Column
    , Property(..)
    , (//)
    , Query(..)
    , Condition
    , whose
    , Parametric(..)
    , module Positron.Alias
    , module Positron.Query
    , mkPositron

    , Positron
    , connect

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

-- local modules

import Positron.Alias
import Positron.Driver
import Positron.Query
import Positron.Types
import Positron.Unsafe
import Positron.Util

mkPositron :: String -> Q [Dec]
mkPositron namespace = do
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

    createQueries <- fold . reverse . map buildCreateQuery <$>
        getCurrentTableMap
    instanceDec <- [d|
        instance Positron $(return $ ConT dataName) where
            pConn = $(return $ VarE connField)
            pLock = $(return $ VarE lockField)
            pPrepareds = const pairs
            pCreateQueries = const createQueries
            pMake conn = do
                lock <- newMVar ()
                return $ $(return $ ConE dataName) conn lock
        |]

    return $ dataDec : instanceDec
  where
    dataName = mkName $ "Positron" <> namespace
    buildCreateQuery (tableName, columnPairs) = let
        snakeTableName = snake (decap tableName)
        columns = map snd columnPairs
        primaryKeys = map (snake . acn) $ filter acp columns
        indexedKeys = map (snake . acn) $ filter aci columns
        foreignKeys = map formatForeignKey $ mapMaybe
            (\ c -> fmap (\ x -> (acn c, x)) (acf c)) columns
        in fold
            [ "CREATE TABLE IF NOT EXISTS "
            , snakeTableName
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
                then foldMap (",\n    " ++) foreignKeys
                else mempty
            , "\n);\n"
            , if indexedKeys /= []
                then fold $ for indexedKeys $ \columnName -> fold
                    [ "CREATE INDEX IF NOT EXISTS ix_"
                    , snakeTableName, "_", columnName
                    , " ON ", snakeTableName, " (", columnName, ");\n"
                    ]
                else mempty
            ]
    formatForeignKey (columnName, (targetTableName, targetColumnName)) = fold
        [ "FOREIGN KEY(", snake columnName, ") REFERENCES "
        , snake targetTableName, " (", snake targetColumnName, ")"
        ]

model :: String -> [Column] -> Q [Dec]
model tableName plainColumns = do
    columns <- mapM (analyze tableName) plainColumns
    thisModuleStr <- show <$> thisModule
    addTable thisModuleStr (tableName, [(acn, a) | a@AC{..} <- columns])
    let
        recs = for columns $ \ac@AC{..} ->
            ( mkName acFullName
            , Bang
                (if acnl then NoSourceUnpackedness else SourceUnpack)
                SourceStrict
            , columnTypeCon ac
            )

    condDecs <- fmap fold <$> forM columns $ \ AC{..} -> let
        condName = mkName $ acFullName ++ "EqParam"
      in do
        defAST <- [| ParamEqual acn |]
        return
            [ SigD condName $ ConT ''Condition
            , ValD (VarP condName) (NormalB defAST) []
            ]

    return $
        DataD [] dataName [] Nothing [RecC dataName recs]
            [ConT ''Eq, ConT ''Show]
        : condDecs
  where
    dataName = mkName tableName

analyze :: String -> Column -> Q AnalyzedColumn
analyze tableName (Column n t pk idx nl unique) = case t of
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
    acBase = AC n fullName pk idx nl unique
    plain = \case
        DBsmallserial -> DBsmallint
        DBserial -> DBinteger
        DBbigserial -> DBbigint
        x -> x
    fullName = decap tableName ++ cap n
