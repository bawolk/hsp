{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Process
    ( process
    , renumberPipe
    )
where

import           Language.Haskell.Interpreter
                                         hiding ( name )
import qualified Data.HashMap.Strict           as H
                                                ( empty )
import           Data.List
import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import           Data.Maybe                     ( fromMaybe )

import           Functions
import           Parser
import           PipeFolds                      ( PipeInfo(..)
                                                , getPipeInfo
                                                )
import           Types

type HasIO = Bool
data Split = SplitYes | SplitNo
data FuncArg = PP | Extra Split | Simple Split deriving Show
data FuncResult
    = RText
    | RTextList
    | RBool
    | RSLineList
    | RLLineList
    | RSLineListList
    | RUnknown
    deriving Show
type FuncData = (String, HasIO, FuncArg)

instance Show Split where
    show = \case
        SplitYes -> "LLine"
        SplitNo  -> "SLine"

toFuncResult :: String -> FuncResult
toFuncResult funcType = case noIOResult of 
    "Text"      -> RText
    "T.Text"    -> RText
    "[Text]"    -> RTextList
    "[T.Text]"  -> RTextList
    "Bool"      -> RBool
    "[SLine]"   -> RSLineList
    "[Line Text]"   -> RSLineList
    "[Line T.Text]" -> RSLineList
    "[LLine]"   -> RLLineList
    "[Line [Text]]" -> RLLineList
    "[Line [T.Text]]" -> RLLineList
    "[[SLine]]" -> RSLineListList
    x           -> case length x of
        1 -> RText -- something like Data.String.IsString p => SLine -> p
        3 -> RTextList -- something like Data.String.IsString a => SLine -> [a]
        _ -> RUnknown
  where
    result = last (splitOnStr " -> " funcType)
    noIOResult = fromMaybe result (stripPrefix "IO " result)

toSplit :: Bool -> Split
toSplit b = if b then SplitYes else SplitNo

isNumbered :: Pipe -> Bool
isNumbered (S _ False) = False
isNumbered _           = True

say :: String -> Interpreter ()
say = liftIO . putStrLn

equateLists :: [a] -> [a] -> a -> ([a], [a])
equateLists l1 l2 item = unzip $ go l1 l2
  where
    go []       []       = []
    go []       (y : ys) = (item, y) : go [] ys
    go (x : xs) []       = (x, item) : go xs []
    go (x : xs) (y : ys) = (x, y) : go xs ys

buildFunction :: PipeEnv -> [Token] -> FuncData
buildFunction PipeEnv{..} toks = (func, not (null ioLines), funcArg)
  where
    pipe     = head pipeline
    PipeInfo{..} = getPipeInfo toks
    hpipe    = case hpToken of
        Just (Hp n) -> Just $ history n pipeline
        _           -> Nothing
    ptype = case hpipe of
        Just x  -> if isS x then SplitNo else SplitYes
        Nothing -> if isS pipe then SplitNo else SplitYes
    pwd  = if haspwd then "pwd <- pwdFunc;" else ""
    date = if hasdate then "date <- getCurrentTime;" else ""
    envs = concatMap (\(Env s) -> s ++ " <- envFunc " ++ show s ++ ";")
                     envTokens
    shells =
        let f n (Shell s) =
                    "shell" ++ show n ++ " <- shellFunc " ++ show s ++ ";"
        in  concat $ zipWith f [0 ..] shellTokens
    glob = maybe ""
                 (\(Glob s) -> "glob <- globFunc " ++ show s ++ ";")
                 globToken
    ioLines = pwd ++ date ++ envs ++ shells ++ glob
    transToks tks =
        let f (ts, n) tok
                | Shell _ <- tok = (Shell ("shell" ++ show n) : ts, n + 1)
                | otherwise      = (tok : ts, n)
        in  reverse $ fst $ foldl' f ([], 0) tks
    cmd = tokensToString $ transToks $ transBuiltin toks PipeEnv{..}
    funcArg | haspp    = PP
            | hasextra = Extra ptype
            | otherwise         = Simple ptype
    funcPre = case funcArg of
        PP -> "(\\(env :: PipeEnv)"
        Extra p ->
            "(\\(sp::SLine) (fp::SLine) (pline::"
                ++ show p
                ++ ") (hist::"
                ++ show p
                ++ ")"
        Simple p -> "(\\(pline::" ++ show p ++ ")"
    funcPost = if null ioLines
        then "(" ++ cmd ++ "))"
        else
            -- "do{dummy <- (return ()) :: IO ();" --puts function in IO monad
             "do{" ++ ioLines ++ "return $ " ++ cmd ++ "})"
    func = funcPre ++ " -> " ++ funcPost

process :: PipeEnv -> [Token] -> Interpreter PipeEnv
process PipeEnv{..} toks = do
    -- say $ show toks
    let pipe            = head pipeline
        numbered        = isNumbered pipe
-- keep history only if history is used in the pipeline
        currentPipeline = if keepHistory then pipeline else []
        pipeInfo        = getPipeInfo toks
        hpipe           = case hpToken pipeInfo of
            Just (Hp n) -> Just $ history n currentPipeline
            _           -> Nothing
        (pipeSLines, histSLines) =
            equateLists (getSLines pipe) (maybe [] getSLines hpipe) blankSLine
        (pipeLLines, histLLines) =
            equateLists (getLLines pipe) (maybe [] getLLines hpipe) blankLLine
        fextra :: [a] -> [a] -> (SLine -> SLine -> a -> a -> b) -> [b]
        fextra hpps pps fun = zipWith4 fun
                                       (spp ++ repeat blankSLine)
                                       (fpp ++ repeat blankSLine)
                                       pps
                                       hpps
        fextraIO :: [a] -> [a] -> (SLine -> SLine -> a -> a -> IO b) -> IO [b]
        fextraIO hpps pps fun = liftIO $ sequence $ fextra hpps
                                                           pps
                                                           fun
        (cmdexpr, hasIO, funcArg) = buildFunction PipeEnv{..} toks
    -- say cmdexpr
    t <- typeOf cmdexpr
    -- say t
    let funcResult = toFuncResult t
    -- say $ show (hasIO, funcArg, funcResult)
    newPipe <- case (hasIO, funcArg, funcResult) of
        (False, Simple SplitNo, RText) -> f1 cmdexpr pipeSLines
        (False, Simple SplitNo, RBool) ->
            f2 cmdexpr pipeSLines keepFalse numbered
        (False, PP             , RSLineList    ) -> f3 cmdexpr PipeEnv{..}
        (False, Simple SplitNo , RTextList     ) -> f4 cmdexpr pipeSLines
        (False, Simple SplitYes, RText         ) -> f5 cmdexpr pipeLLines
        (False, PP             , RSLineListList) -> f6 cmdexpr PipeEnv{..}
        (False, PP             , RText         ) -> f7 cmdexpr PipeEnv{..}
        (False, PP             , RTextList     ) -> f8 cmdexpr PipeEnv{..}
        (False, Simple SplitYes, RTextList     ) -> f9 cmdexpr pipeLLines
        (False, Extra SplitNo, RText) ->
            f10 cmdexpr pipeSLines (fextra histSLines)
        (False, Extra SplitNo, RBool) ->
            f11 cmdexpr pipeSLines (fextra histSLines) keepFalse numbered
        (False, PP             , RLLineList) -> f12 cmdexpr PipeEnv{..}
        (False, Simple SplitYes, RBool     ) -> f13 cmdexpr pipeLLines keepFalse
        (False, Extra SplitYes, RTextList) ->
            f14 cmdexpr pipeLLines (fextra histLLines)
        (False, Extra SplitYes, RText) ->
            f15 cmdexpr pipeLLines (fextra histLLines)

        (True, Simple SplitNo, RText) -> fio1 cmdexpr pipeSLines
        (True, Simple SplitNo, RBool) ->
            fio2 cmdexpr pipeSLines keepFalse numbered
        (True, PP             , RSLineList) -> fio3 cmdexpr PipeEnv{..} numbered
        (True, Simple SplitNo , RTextList ) -> fio4 cmdexpr pipeSLines
        (True, Simple SplitYes, RText     ) -> fio5 cmdexpr pipeLLines
                                        -- fio6 not implemented
        (True, PP             , RText     ) -> fio7 cmdexpr PipeEnv{..}
        (True, PP             , RTextList ) -> fio8 cmdexpr PipeEnv{..}
        (True, Simple SplitYes, RTextList ) -> fio9 cmdexpr pipeLLines
        (True, Extra SplitNo, RText) ->
            fio10 cmdexpr pipeSLines (fextraIO histSLines)
        (True, Extra SplitNo, RBool) ->
            fio11 cmdexpr pipeSLines (fextraIO histSLines) keepFalse numbered
        (True, PP, RLLineList) -> fio12 cmdexpr PipeEnv{..}
        (True, Simple SplitYes, RBool) -> fio13 cmdexpr pipeLLines keepFalse
        (True, Extra SplitYes, RTextList) ->
            fio14 cmdexpr pipeLLines (fextraIO histLLines)
        _ -> errorWithoutStackTrace $ "Unimplemented pipe expression: " ++ show
            (hasIO, funcArg, funcResult)
    return $ PipeEnv { pipeline = newPipe : currentPipeline,.. }
