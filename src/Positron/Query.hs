{-# language TemplateHaskell #-}
{-# language RecordWildCards #-}
{-# language LambdaCase #-}

module Positron.Query
    ( query
    , queryInsert
    , queryUpsert
    , queryGet
    ) where

import Positron.Import

-- data types

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- extra modules

import qualified Database.PostgreSQL.LibPQ as PQ

-- local modules

import Positron.Driver
import Positron.Types
import Positron.Unsafe
import Positron.Util

queryInsert :: String -> String -> Q [Dec]
queryInsert = queryUpsertBase False

queryUpsert :: String -> String -> Q [Dec]
queryUpsert = queryUpsertBase True

query :: String -> Query -> Q [Dec]
query funcStr = \case
    Insert tableName -> prepareXxsert False funcStr tableName
    Select targets conds -> prepareSelect funcStr targets conds

prepareSelect :: String -> [String] -> [Condition] -> Q [Dec]
prepareSelect funcStr targets conds = do
    -- TODO: support column selection
    table <- getTable tableName
    let
        columns = map snd table
        -- params: the parameters of this function. They are the parametric
        -- ("$1", "$2", etc.) part of the "where" clause.
        params = catMaybes $ flip map conds $ \case
            ParamEqual s -> let
                msg = "parameter column not found: " <> s
                in Just $ fromMaybe (error msg) $ lookup s table
            _ -> Nothing
        condBuilder _ [] = []
        condBuilder ctr (x : xs) = case x of
            ParamEqual fieldName ->
                fold [B.string7 fieldName, " = $", B.word16Dec ctr] :
                    condBuilder (ctr + 1) xs
            FixedEqual _ _ ->
                -- TODO: type checking
                error "FixedEqual is not implemented"
        queryStr = toByteString $ fold
            [ "select "
            , fold $ intersperse ", " $ map (B.string7 . acn) columns
            , " from "
            , B.string7 $ snake tableName
            , if not (null conds) then
                " where " <> fold (intersperse " and " $ condBuilder 1 conds)
              else mempty
            , ";"
            ]

    -- register to the list of prepareds
    preparedName <- do
        moduleName <- B.string7 . (\ (Module _ (ModName s)) -> s) <$>
            thisModule
        return $ toByteString $ fold [moduleName, ".", B.string7 funcStr]
    addPrepared (preparedName, queryStr)

    -- construct AST for this function
    -- positronArg: The argument of the type "Positron"
    positronArg <- newName "_positron"
    -- keyTHArgs: The arguments that are used in the "where" clause
    keyTHArgs <- forM params $ \ ac -> do
        thName <- newName $ "_" <> acn ac
        return (thName, ac)
    -- encodedArgs: All function arguments that are converted into
    -- "Maybe ByteString" so that they can be passed to unsafeExecPrepared
    -- immediately.
    let encodedArgs = ListE $ map argumentAST keyTHArgs

    -- execResult: The name to bind the execPrepared result
    execResultName <- newName "execResult"
    let execResult = return $ VarE execResultName

    -- typeSignature: The type signature of the resulting function.
    -- For example,
    -- "Positron p => p -> Int64 -> IO (Either PositronError [MyModel])"
    typeSignature <- let
        argTypes = map columnTypeCon params
        applyArgs t = foldr (\ x y -> AppT (AppT ArrowT x) y) t argTypes
        returnValueType = [t| IO [$(r (ConT capTabName))] |]
        in positronContext . applyArgs <$> returnValueType

    rowIndexName <- newName "rowIndex"
    let rowIndex = return $ VarE rowIndexName

    -- bindPairs: pairs of binding for each field in one row.
    -- For example, "c1 <- dbUnstore <$> PQ.getvalue' result 0 1"
    bindPairs <- forM (zip [0 :: Int16 ..] columns) $ \ (i, AC{..}) -> do
        fname <- newName acn
        unstoreExp <- if acnl
            then [| fmap dbUnstore |]
            else [| dbUnstore . fromMaybe (error "NOT NULL field is NULL") |]
        getvalueExp <- [| fmap $(r unstoreExp)
            (PQ.getvalue' $(execResult) $(rowIndex) i) |]
        return (fname, getvalueExp)
    -- oneRowResultExp: The final line of oneRowGetter's do notation.
    oneRowResultExp <- do
        prefix <- [| return . Just |]
        return $ NoBindS $ AppE prefix $
            foldl' (\ x y -> AppE x (VarE y))
                (ConE capTabName) (map fst bindPairs)

    let
        oneRowGetterAST = LamE [VarP rowIndexName] $ DoE $
            [BindS (VarP x) y | (x, y) <- bindPairs] ++ [oneRowResultExp]

    execPreparedAST <- [| do
        $(r (VarP execResultName)) <- unsafeExecPrepared
            $(r $ VarE positronArg) preparedName $(r encodedArgs)
                >>= either (fail . show) return
        ntuples <- PQ.ntuples $(execResult)
        if ntuples > 0
            then mapM [0 .. ntuples - 1] $(r oneRowGetterAST)
            else return []
        |]


    return
        [ SigD funcName typeSignature
        , FunD funcName
            [ Clause
                (VarP positronArg : map (VarP . fst) keyTHArgs)
                (NormalB execPreparedAST)
                []
            ]
        ]
  where
    funcName = mkName funcStr
    -- TODO: targets is not in fact always a table name
    tableName = head targets
    capTabName = mkName $ cap tableName
    r = return

prepareXxsert :: Bool -> String -> String -> Q [Dec]
prepareXxsert isUpsert funcStr tableName = withTable $ \ rawTable -> do
    let
        table = filter (not . isSerial . snd) rawTable
        columns = map snd table
        allPKNames = fold $ intersperse ", " $
            map (B.string7 . snake . acn) $ filter acp columns
        columnNames = map (snake . fst) table
        queryStr = toByteString $ fold
            [ "insert into ", B.string7 $ snake tableName, " ("
            , fold $ B.string7 <$> intersperse ", " columnNames
            , ") values ("
            , fold $ intersperse ", " $ map ("$" <>)
                [B.intDec x | x <- [1 .. length columnNames]]
            , ")"
            , if isUpsert then fold
                [ " ON CONFLICT ("
                , allPKNames
                , ") DO UPDATE SET "
                , fold $ intersperse ", " $ map
                    ((\ name -> fold [name, " = EXCLUDED.", name])
                        . B.string7)
                    columnNames
                ]
              else mempty
            , ";"
            ]
    -- Register the prepared name and statement to the global store
    preparedName <- do
        moduleName <- B.string7 . (\ (Module _ (ModName s)) -> s) <$>
            thisModule
        return $ toByteString $ fold [moduleName, ".", B.string7 funcStr]
    addPrepared (preparedName, queryStr)

    -- Build AST for the query function
    -- columnTHArgs: The arguments that will appear on the left side and the
    -- right side of the function definition. "Points" as in point-free.
    columnTHArgs <- forM columns $ \ ac -> do
        thName <- newName $ "_" <> acn ac
        return (thName, ac)
    -- encodedArgs: The list of arguments for the prepared statement.
    -- Its type is [Maybe ByteString].
    let encodedArgs = ListE $ map argumentAST columnTHArgs
    -- positronArg: The argument of the type "Positron"
    positronArg <- newName "_positron"
    resultTypeSignature <- [t| IO (Either PositronError ()) |]

    mainContentExp <- [| either Left (const ()) <$> unsafeExecPrepared
        $(return $ VarE positronArg)
        $(return $ LitE $ StringL $ g preparedName)
        $(return encodedArgs)
        |]
    return
        [ SigD funcName $ positronContext $
            foldr ((\ x y -> AppT (AppT ArrowT x) y) . columnTypeCon)
                resultTypeSignature columns
        , FunD funcName
            [ Clause
                (VarP positronArg : map (VarP . fst) columnTHArgs)
                (NormalB mainContentExp)
                []
            ]
        ]
  where
    funcName = mkName funcStr
    withTable f = getTable tableName >>= f
    isSerial AC{..} = case act of
        DBsmallserial -> True
        DBserial -> True
        DBbigserial -> True
        _ -> False

queryUpsertBase :: Bool -> String -> String -> Q [Dec]
queryUpsertBase upsert queryStr tableName = getTable tableName >>=
    \ rawTable -> let
        table = filter (not . isSerial . snd) rawTable
        acols = map snd table
        allPKNames :: String
        allPKNames = mconcat $ intersperse ", " $
            map (snake . acn) $ filter acp acols
        columnTypes = map columnTypeCon acols
        columnArgs = map (VarP . mkName . ("_" ++) . fst) table
        columnNames = map (snake . fst) table
        insertHeadPart = LitE $ StringL $ mconcat
            [ "insert into ", snake tableName, " ("
            , mconcat $ intersperse ", " columnNames, ") values ("
            ]
        upsertClause = LitE $ StringL $ if upsert then mconcat
            [ ") ON CONFLICT ("
            , allPKNames
            , ") DO UPDATE SET "
            , mconcat $ intersperse ", " $ flip map columnNames $
                \colName -> mconcat [colName, " = EXCLUDED.", colName]
            , ";"
            ]
            else ");"
      in do
        mainContentExp <- [| mconcat
            [ $(return insertHeadPart)
            , toByteString $ mconcat $ intersperse ", "
                $(return $ ListE $ map expMake acols)
            , ")"
            , $(return upsertClause)
            ]
            |]
        resultTypeSignature <- [t| IO (Either PositronError ()) |]
        return
            [ SigD queryName $ positronContext $
                foldr (\x y -> AppT (AppT ArrowT x) y)
                    resultTypeSignature
                    columnTypes
            , FunD queryName
                [ Clause
                    (VarP (mkName "_conn") : columnArgs)
                    (NormalB $ AppE
                        (AppE (VarE 'unsafePlainExec) (VarE $ mkName "_conn"))
                        mainContentExp
                    )
                    []
                ]
            ]
  where
    queryName = mkName queryStr
    expMake AC{..} = case act of
        DBsmallint -> defaultWrapVarE 'B.int16Dec
        DBinteger -> defaultWrapVarE 'B.int32Dec
        DBbigint -> defaultWrapVarE 'B.int64Dec
        DBsmallserial -> defaultWrapVarE 'B.int16Dec
        DBserial -> defaultWrapVarE 'B.int32Dec
        DBbigserial -> defaultWrapVarE 'B.int64Dec
        DBvarchar _ -> textMake
        DBtext -> textMake
        _ -> defaultWrap bShow
      where
        textMake = wrap
            (VarE 'T.encodeUtf8Builder)
            (AppE
                (if acnl then AppE (VarE 'fmap) sanitize else sanitize)
                (VarE $ mkName ("_" ++ acn))
            )
        bShow = InfixE
            (Just (VarE 'B.byteString))
            (VarE '(.))
            (Just (InfixE
                (Just (VarE 'B.pack))
                (VarE '(.))
                (Just (VarE 'show))
            ))
        sanitize = InfixE (Just quote) (VarE '(.)) (Just escape)
        quote = InfixE
            (Just (InfixE (Just (LitE (StringL "'"))) (VarE '(<>)) Nothing))
            (VarE '(.))
            (Just (InfixE Nothing (VarE '(<>)) (Just (LitE (StringL "'")))))
        escape = AppE
            (AppE (VarE 'T.replace) (AppE (VarE 'T.pack) (LitE (StringL "'"))))
            (AppE (VarE 'T.pack) (LitE (StringL "''")))

        defaultWrapVarE = defaultWrap . VarE
        defaultWrap f = wrap f (VarE $ mkName $ "_" ++ acn)
        wrap converter value = if acnl
            then AppE
                (AppE (AppE (VarE 'maybe) (LitE (StringL "null"))) converter)
                value
            else AppE converter value
    isSerial AC{..} = case act of
        DBsmallserial -> True
        DBserial -> True
        DBbigserial -> True
        _ -> False

queryGet :: String -> String -> Q [Dec]
queryGet funcStr tableName = getTable tableName >>= \ columnMap -> do
    let
        columnNames = map (snake . fst) columnMap
        acols = map snd columnMap
        -- FIXME: support composite key (multiple primary keys)
        pk = head $ filter acp acols
        pkType = return $ columnTypeCon pk
    resultTypeSignature <- positronContext <$> [t|
        $(pkType) -> IO (Maybe $(return $ ConT capTabName))
        |]
    connArg <- newName "connArg"
    keyArg <- newName "keyArg"
    resultName <- newName "resultName"
    queryAST <- [| toByteString $ mconcat
        [ $( return $ LitE $ StringL $ mconcat
            [ "select ", mconcat $ intersperse ", " columnNames
            , " from ", snake tableName, " where ", acn pk, " = "
            ]
          )
        , dbStore $( return $ VarE keyArg )
        , ";"
        ]
        |]
    bindPairs <- forM (zip [0 :: Integer ..] acols) $ \ (i, AC{..}) -> do
        fname <- newName acn
        unstoreExp <- if acnl
            then [| fmap dbUnstore |]
            else [| dbUnstore . fromMaybe (error "NOT NULL field is NULL") |]
        getvalueExp <- [| fmap $(return unstoreExp)
            (PQ.getvalue' $(return $ VarE resultName) 0 i) |]
        return (fname, getvalueExp)
    resultExp <- do
        prefix <- [| return . Just |]
        return $ NoBindS $ AppE prefix $
            foldl' (\ x y -> AppE x (VarE y))
                (ConE capTabName) (map fst bindPairs)
    doExp <- [| do
        $(return (VarP resultName)) <- unsafeRawExec
            $(return $ VarE connArg) $(return queryAST) >>=
                either (fail . show) return
        ntuples <- fmap (> 0) (PQ.ntuples $(return $ VarE resultName))
        if ntuples
            then $(return $ DoE $
                [BindS (VarP x) y | (x, y) <- bindPairs] ++ [resultExp])
            else return Nothing
        |]
    return
        [ SigD funcName resultTypeSignature
        , FunD funcName
            [ Clause [VarP connArg, VarP keyArg] (NormalB doExp) []
            ]
        ]
  where
    funcName = mkName funcStr
    capTabName = mkName $ cap tableName

getTable :: String -> Q Table
getTable tableName = do
    currentTableMap <- getCurrentTableMap
    return $ fromMaybe (error noTable) $ lookup tableName currentTableMap
  where
    noTable = "Can't find the table: " ++ tableName


-- positronContext: The "Positron p => p ->" part of type signature.
positronContext :: Type -> Type
positronContext = let p = mkName "p" in
    ForallT [PlainTV p] [AppT (ConT ''Positron) (VarT p)] .
    AppT (AppT ArrowT (VarT p))


-- argumentAST: Build AST of a function argument
argumentAST :: (Name, AnalyzedColumn) -> Exp
argumentAST (thName, AC{..}) = wrapper (VarE thName)
  where
    wrapper = if acnl
        then AppE (AppE (VarE 'fmap) binaryEncodeAST)
        else AppE (ConE 'Just) . AppE binaryEncodeAST
    binaryEncodeAST = VarE 'binaryStore
