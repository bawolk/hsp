{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Functions where

import           Control.Monad                  ( filterM )
import           Data.Text                      ( Text )
import           Language.Haskell.Interpreter
import           Types

type ExtraS a = SLine -> SLine -> SLine -> SLine -> a
type Func4S a = [SLine] -> ExtraS a -> [a]
type ExtraL a = SLine -> SLine -> LLine -> LLine -> a
type Func4L a = [LLine] -> ExtraL a -> [a]

type ExtraSIO a = SLine -> SLine -> SLine -> SLine -> IO a
type Func4SIO a = [SLine] -> ExtraSIO a -> IO [a]
type ExtraLIO a = SLine -> SLine -> LLine -> LLine -> IO a
type Func4LIO a = [LLine] -> ExtraLIO a -> IO [a]

mkL :: [[Text]] -> [Text] -> Pipe
mkL ps os = L $ zipWith3 Line [0 ..] ps os

renumberPipe :: Pipe -> Pipe
renumberPipe (S slines sh) =
    S (zipWith (\n line -> line { n = n }) [0 ..] slines) sh
renumberPipe (L llines) = L $ zipWith (\n line -> line { n = n }) [0 ..] llines

-- E.g., 'p <> "xx"'
f1 :: String -> [SLine] -> Interpreter Pipe
f1 cmdexpr ss = do
    fun <- interpret cmdexpr (as :: SLine -> Text)
    let slines = fun <$> ss
    return $ mkS slines (o <$> ss) False

-- E.g., 'keep ["user"]'
f2 :: String -> [SLine] -> Bool -> Bool -> Interpreter Pipe
f2 cmdexpr ss keepfalse numbered = do
    fun <- interpret cmdexpr (as :: SLine -> Bool)
    let f sline = if fun sline then sline else blankSLine
        modFun = if keepfalse then map f else filter fun
        ns     = modFun ss
    return $ renumberPipe $ S ns numbered

-- E.g., 'sort pp', 'drop 2 pp', 'expand pp' (after a split)
f3 :: String -> PipeEnv -> Interpreter Pipe
f3 cmdexpr pipeEnv = do
    fun <- interpret cmdexpr (as :: PipeEnv -> [SLine])
    let slines = fun pipeEnv
    return $ renumberPipe $ S slines True

-- E.g., a split like 'dot', 'c', etc.
f4 :: String -> [SLine] -> Interpreter Pipe
f4 cmdexpr ss = do
    fun <- interpret cmdexpr (as :: SLine -> [Text])
    return $ mkL (fun <$> ss) (o <$> ss)

-- E.g., a join following a split like 'dot | u'
f5 :: String -> [LLine] -> Interpreter Pipe
f5 cmdexpr ll = do
    fun <- interpret cmdexpr (as :: LLine -> Text)
    return $ mkS (fun <$> ll) (o <$> ll) False

-- E.g., operations like 'divide 2 pp'
f6 :: String -> PipeEnv -> Interpreter Pipe
f6 cmdexpr pipeEnv = do
    fun <- interpret cmdexpr (as :: PipeEnv -> [[SLine]])
    let slines = fun pipeEnv
        mergeSLines sls = Line 0 (getText <$> sls) ""
    return $ renumberPipe (L $ mergeSLines <$> slines)

-- handles 'oneline pp'
f7 :: String -> PipeEnv -> Interpreter Pipe
f7 cmdexpr pipeEnv = do
    fun <- interpret cmdexpr (as :: PipeEnv -> Text)
    let str = fun pipeEnv
    return $ S [Line 0 str ""] False

-- No longer used
f8 :: String -> PipeEnv -> Interpreter Pipe
f8 cmdexpr pipeEnv = do
    fun <- interpret cmdexpr (as :: PipeEnv -> [Text])
    let llines = fun pipeEnv
    return $ mkS llines (repeat "") True

-- E.g., 'divide 2 pp | p'.  Returning to Text mode following a split 
f9 :: String -> [LLine] -> Interpreter Pipe
f9 cmdexpr ll = do
    fun <- interpret cmdexpr (as :: LLine -> [Text])
    return $ mkL (fun <$> ll) (o <$> ll)

-- Text manipulation when hp, sp, or fp present
f10 :: String -> [SLine] -> Func4S Text -> Interpreter Pipe
f10 cmdexpr ss func = do
    fun <- interpret cmdexpr (as :: ExtraS Text)
    return $ mkS (func ss fun) (o <$> ss) False

-- E.g., 'p == fp'.  Filtering when hp, sp, or fp present
f11
    :: String
    -> [SLine]
    -> Func4S (SLine, Bool)
    -> Bool
    -> Bool
    -> Interpreter Pipe
f11 cmdexpr ss func keepfalse numbered = do
    fun <- interpret cmdexpr (as :: ExtraS Bool)
    let f sp fp p hp = (p, fun sp fp p hp)
        nsTuple = func ss f
        ns      = if keepfalse
            then foldr
                (\(p, b) acc -> if b then p : acc else blankSLine : acc)
                []
                nsTuple
            else foldr (\(p, b) acc -> if b then p : acc else acc) [] nsTuple
    return $ renumberPipe $ S ns numbered

-- E.g., 'drop 2 $ hpp 1' when the first pipe was a split
f12 :: String -> PipeEnv -> Interpreter Pipe
f12 cmdexpr pipeEnv = do
    fun <- interpret cmdexpr (as :: PipeEnv -> [LLine])
    let llines = fun pipeEnv
    return $ renumberPipe $ L llines

-- E.g., 'c|p!!1 == "cat"'.  Filtering on a split line
f13 :: String -> [LLine] -> Bool -> Interpreter Pipe
f13 cmdexpr ll keepfalse = do
    fun <- interpret cmdexpr (as :: LLine -> Bool)
    let f lline = if fun lline then lline else blankLLine
        modFun = if keepfalse then map f else filter fun
        ns     = modFun ll
    return $ renumberPipe $ L ns

-- E.g., 'c|u|hp 1'.  Using hp when referred pipe is a split
f14 :: String -> [LLine] -> Func4L [Text] -> Interpreter Pipe
f14 cmdexpr ll func = do
    fun <- interpret cmdexpr (as :: ExtraL [Text])
    return $ mkL (func ll fun) (o <$> ll)

-- Using hp when referred pipe is a split and producing a Text
f15 :: String -> [LLine] -> Func4L Text -> Interpreter Pipe
f15 cmdexpr ll func = do
    fun <- interpret cmdexpr (as :: ExtraL Text)
    -- return $ mkL (func ll fun) (o <$> ll)
    return $ mkS (func ll fun) (o <$> ll) False

-- E.g., when 'pwd', 'envXXX', or 'date' is used
fio1 :: String -> [SLine] -> Interpreter Pipe
fio1 cmdexpr ss = do
    fun    <- interpret cmdexpr (as :: SLine -> IO Text)
    slines <- liftIO $ sequence (fun <$> ss)
    return $ mkS slines (o <$> ss) False

-- Filtering when 'pwd', 'envXXX', etc. are used
fio2 :: String -> [SLine] -> Bool -> Bool -> Interpreter Pipe
fio2 cmdexpr ss keepfalse numbered = do
    fun <- interpret cmdexpr (as :: SLine -> IO Bool)
    let f nstr = (\b -> if b then nstr else blankSLine) <$> fun nstr
        modFun = if keepfalse then mapM f else filterM fun
    ns <- liftIO $ modFun ss
    return $ renumberPipe $ S ns numbered

-- E.g., '[text pwd] ++ pp'.  Allows use of IO results with pp types.
fio3 :: String -> PipeEnv -> Bool -> Interpreter Pipe
fio3 cmdexpr pipeEnv numbered = do
    fun    <- interpret cmdexpr (as :: PipeEnv -> IO [SLine])
    slines <- liftIO $ fun pipeEnv
    return $ renumberPipe $ S slines (not numbered)

-- E.g., 'shell "ls"'.  Result is a split and IO used
fio4 :: String -> [SLine] -> Interpreter Pipe
fio4 cmdexpr ss = do
    fun    <- interpret cmdexpr (as :: SLine -> IO [Text])
    slines <- liftIO $ sequence (fun <$> ss)
    return $ mkL slines (o <$> ss)

-- E.g., like 'dot | envUSER <> p!!1'.  A join following a split, with IO 
fio5 :: String -> [LLine] -> Interpreter Pipe
fio5 cmdexpr ll = do
    fun    <- interpret cmdexpr (as :: LLine -> IO Text)
    llines <- liftIO $ sequence $ fun <$> ll
    return $ mkS llines (o <$> ll) False

-- handles things like 'pwd <> oneline pp' with IO
fio7 :: String -> PipeEnv -> Interpreter Pipe
fio7 cmdexpr pipeEnv = do
    fun <- interpret cmdexpr (as :: PipeEnv -> IO Text)
    str <- liftIO $ fun pipeEnv
    return $ S [Line 0 str ""] False

-- No longer used
fio8 :: String -> PipeEnv -> Interpreter Pipe
fio8 cmdexpr pipeEnv = do
    fun    <- interpret cmdexpr (as :: PipeEnv -> IO [Text])
    llines <- liftIO $ fun pipeEnv
    return $ mkS llines (repeat "") True

fio9 :: String -> [LLine] -> Interpreter Pipe
fio9 cmdexpr ll = do
    fun    <- interpret cmdexpr (as :: LLine -> IO [Text])
    llines <- liftIO . sequence $ fun <$> ll
    return $ mkL llines (o <$> ll)

-- Text manipulation when hp, sp, or fp present with IO
fio10 :: String -> [SLine] -> Func4SIO Text -> Interpreter Pipe
fio10 cmdexpr ss func = do
    fun    <- interpret cmdexpr (as :: ExtraSIO Text)
    slines <- liftIO $ func ss fun
    return $ mkS slines (o <$> ss) False

-- E.g., 'pwd == hp 1'.  Filtering when hp, sp, or fp present and IO
fio11
    :: String
    -> [SLine]
    -> Func4SIO (SLine, Bool)
    -> Bool
    -> Bool
    -> Interpreter Pipe
fio11 cmdexpr ss func keepfalse numbered = do
    fun <- interpret cmdexpr (as :: ExtraSIO Bool)
    let f sp fp p hp = (p, ) <$> fun sp fp p hp
    nsTuple <- liftIO $ func ss f
    let ns = if keepfalse
            then foldr
                (\(p, b) acc -> if b then p : acc else blankSLine : acc)
                []
                nsTuple
            else foldr (\(p, b) acc -> if b then p : acc else acc) [] nsTuple
    return $ renumberPipe $ S ns numbered

-- No longer used
fio12 :: String -> PipeEnv -> Interpreter Pipe
fio12 cmdexpr pipeEnv = do
    fun    <- interpret cmdexpr (as :: PipeEnv -> IO [LLine])
    llines <- liftIO $ fun pipeEnv
    return $ renumberPipe $ L llines

fio13 :: String -> [LLine] -> Bool -> Interpreter Pipe
fio13 cmdexpr ll keepfalse = do
    fun <- interpret cmdexpr (as :: LLine -> IO Bool)
    let f nstr = (\b -> if b then nstr else blankLLine) <$> fun nstr
        modFun = if keepfalse then mapM f else filterM fun
    ns <- liftIO $ modFun ll
    return $ renumberPipe $ L ns

fio14 :: String -> [LLine] -> Func4LIO [Text] -> Interpreter Pipe
fio14 cmdexpr ll func = do
    fun    <- interpret cmdexpr (as :: ExtraLIO [Text])
    llines <- liftIO $ func ll fun
    return $ mkL llines (o <$> ll)
