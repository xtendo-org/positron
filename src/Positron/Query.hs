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
    Select (SelectModel tableName) conds ->
        prepareSelectModel funcStr tableName conds
    GetModel tableName -> getTable tableName >>= prepareGet funcStr tableName
    Select (SelectFields _) _ -> error
        "selecting fields is not implemented yet"

prepareGet :: String -> String -> Table -> Q [Dec]
prepareGet funcStr tableStr table = do
    preparedName <- do
        moduleName <- B.string7 . (\ (Module _ (ModName s)) -> s) <$>
            thisModule
        return $ toByteString $ fold [moduleName, ".", B.string7 funcStr]
    addPrepared (preparedName, queryStr)

    -- construct AST for this function
    -- positronArg: The argument of the type "Positron"
    positronArg <- newName "_positron"
    -- keyTHArgs: The arguments that are used in the "where" clause
    keyTHArgs <- forM primaryKeys $ \ ac -> do
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
        applyArgs t = foldr (\ x y -> AppT (AppT ArrowT x) y) t primaryKeyCons
        returnValueType = [t| IO (Maybe $(r (ConT tableName))) |]
        in positronContext . applyArgs <$> returnValueType

    bindPairs <- forM (zip [0 :: Int16 ..] columns) $ \ (i, AC{..}) -> do
        fieldName <- newName acn
        unstoreExp <- let msg = "NOT NULL field is NULL: " <> acn in if acnl
            then [| fmap binaryUnstore |]
            else [| binaryUnstore . fromMaybe (error msg) |]
        getvalueExp <- [| fmap $(r unstoreExp)
            (PQ.getvalue $(execResult) 0 i) |]
        return (fieldName, getvalueExp)

    resultExp <- do
        prefix <- [| return . Just |]
        return $ NoBindS $ AppE prefix $ foldl' (\ x y -> AppE x (VarE y))
            (ConE tableName) (map fst bindPairs)

    doExp <- [| do
        $(r (VarP execResultName)) <- unsafeExecPrepared
            $(r $ VarE positronArg) preparedName $(r encodedArgs)
                >>= either (fail . show) return
        ntuples <- PQ.ntuples $(execResult)
        if ntuples > 0
            then $(return $ DoE $
                [BindS (VarP x) y | (x, y) <- bindPairs] ++ [resultExp])
            else return Nothing
        |]

    return
        [ SigD funcName typeSignature
        , FunD funcName
            [ Clause (VarP positronArg : map (VarP . fst) keyTHArgs)
                (NormalB doExp) []
            ]
        ]

  where
    funcName = mkName funcStr
    tableName = mkName tableStr
    columns = map snd table
    primaryKeys = filter acp columns
    primaryKeyCons = map columnTypeCon primaryKeys
    queryStr = toByteString $ fold
        [ "select "
        , fold $ intersperse ", " $ map (B.string7 . snake . acn) columns
        , " from "
        , B.string7 $ snake $ decap tableStr
        , " where "
        , fold $ intersperse "," $ for (zip [1 ..] primaryKeys) $ \ (i, pk) ->
            B.string7 (acn pk) <> " = $" <> B.word16Dec i
        , ";"
        ]
    r = return

prepareSelectModel :: String -> String -> [Condition] -> Q [Dec]
prepareSelectModel funcStr tableStr conds = do
    -- TODO: support column selection
    table <- getTable tableStr
    let
        columns = map snd table
        -- params: the parameters of this function. They are the parametric
        -- ("$1", "$2", etc.) part of the "where" clause.
        params = catMaybes $ for conds $ \case
            ParamEqual s -> let
                msg = "parameter column not found: " <> s
                in Just $ fromMaybe (error msg) $ lookup s table
            _ -> Nothing
        condBuilder _ [] = []
        condBuilder ctr (x : xs) = case x of
            ParamEqual fieldName ->
                fold [B.string7 (snake fieldName), " = $", B.word16Dec ctr] :
                    condBuilder (ctr + 1) xs
            FixedEqual _ _ ->
                -- TODO: type checking
                error "FixedEqual is not implemented"
        queryStr = toByteString $ fold
            [ "select "
            , fold $ intersperse ", " $ map (B.string7 . snake . acn) columns
            , " from "
            , B.string7 $ snake $ decap tableStr
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
        returnValueType = [t| IO [$(r (ConT tableName))] |]
        in positronContext . applyArgs <$> returnValueType

    rowIndexName <- newName "rowIndex"
    let rowIndex = return $ VarE rowIndexName

    -- bindPairs: pairs of binding for each field in one row.
    -- For example, "c1 <- dbUnstore <$> PQ.getvalue' result 0 1"
    bindPairs <- forM (zip [0 :: Int16 ..] columns) $ \ (i, AC{..}) -> do
        fname <- newName acn
        unstoreExp <- if acnl
            then [| fmap binaryUnstore |]
            else [| binaryUnstore . fromMaybe (error "NOT NULL field is NULL") |]
        getvalueExp <- [| fmap $(r unstoreExp)
            (PQ.getvalue' $(execResult) $(rowIndex) i) |]
        return (fname, getvalueExp)
    -- oneRowResultExp: The final line of oneRowGetter's do notation.

    let
        oneRowResultExp = NoBindS $ AppE (VarE 'return) $
            foldl' (\ x y -> AppE x (VarE y)) (ConE tableName) $
            map fst bindPairs
        oneRowGetterAST = LamE [VarP rowIndexName] $ DoE $
            [BindS (VarP x) y | (x, y) <- bindPairs] ++ [oneRowResultExp]

    execPreparedAST <- [| do
        $(r (VarP execResultName)) <- unsafeExecPrepared
            $(r $ VarE positronArg) preparedName $(r encodedArgs)
                >>= either (fail . show) return
        ntuples <- PQ.ntuples $(execResult)
        if ntuples > 0
            then forM [0 .. ntuples - 1] $(r oneRowGetterAST)
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
    tableName = mkName tableStr
    r = return

prepareXxsert :: Bool -> String -> String -> Q [Dec]
prepareXxsert isUpsert funcStr tableStr = withTable $ \ rawTable -> do
    let
        table = filter (not . isSerial . snd) rawTable
        columns = map snd table
        allPKNames = fold $ intersperse ", " $
            map (B.string7 . snake . acn) $ filter acp columns
        columnNames = map (snake . fst) table
        queryStr = toByteString $ fold
            [ "insert into ", B.string7 $ snake $ decap tableStr, " ("
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

    mainContentExp <- [| fmap (const ()) <$> unsafeExecPrepared
        $(return $ VarE positronArg)
        $(return $ LitE $ StringL $ T.unpack $ T.decodeUtf8 preparedName)
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
    withTable f = getTable tableStr >>= f
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
            [ "insert into ", snake $ decap tableName, " ("
            , mconcat $ intersperse ", " columnNames, ") values ("
            ]
        upsertClause = LitE $ StringL $ if upsert then mconcat
            [ ") ON CONFLICT ("
            , allPKNames
            , ") DO UPDATE SET "
            , mconcat $ intersperse ", " $ for columnNames $
                \colName -> mconcat [colName, " = EXCLUDED.", colName]
            , ";"
            ]
            else ");"
      in do
        conn <- newName "conn"
        mainQueryExp <- [| mconcat
            [ $(return insertHeadPart)
            , toByteString $ mconcat $ intersperse ", "
                $(return $ ListE $ map expMake acols)
            , ")"
            , $(return upsertClause)
            ]
            |]
        mainContentExp <- [| fmap (const ()) <$>
            unsafeExec $(return $ VarE conn) $(return mainQueryExp) |]
        resultTypeSignature <- [t| IO (Either PositronError ()) |]
        return
            [ SigD queryName $ positronContext $
                foldr (\x y -> AppT (AppT ArrowT x) y)
                    resultTypeSignature
                    columnTypes
            , FunD queryName
                [Clause (VarP conn : columnArgs) (NormalB mainContentExp) []]
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
queryGet funcStr tableStr = getTable tableStr >>= \ columnMap -> do
    let
        columnNames = map (snake . fst) columnMap
        acols = map snd columnMap
        -- FIXME: support composite key (multiple primary keys)
        pk = head $ filter acp acols
        pkType = return $ columnTypeCon pk
    resultTypeSignature <- positronContext <$> [t|
        $(pkType) -> IO (Maybe $(return $ ConT tableName))
        |]
    connArg <- newName "connArg"
    keyArg <- newName "keyArg"
    resultName <- newName "resultName"
    queryAST <- [| toByteString $ mconcat
        [ $( return $ LitE $ StringL $ mconcat
            [ "select ", mconcat $ intersperse ", " columnNames
            , " from ", snake $ decap tableStr, " where ", acn pk, " = "
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
                (ConE tableName) (map fst bindPairs)
    doExp <- [| do
        $(return (VarP resultName)) <- unsafeExec
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
    tableName = mkName tableStr

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
