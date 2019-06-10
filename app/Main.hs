{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception              ( displayException )
import           Control.Monad                  ( when
                                                , unless
                                                , foldM
                                                , filterM
                                                )
import qualified Data.HashMap.Strict           as H
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Data.Text                      ( Text )
import           Data.List
import           Data.Maybe
import           Language.Haskell.Interpreter
                                         hiding ( name )
import           Options.Applicative            (execParser)
import           System.Directory               ( doesPathExist
                                                , getTemporaryDirectory
                                                )
import           System.Environment
import           System.Exit
import           System.FilePath

import           Colors
import           Display                        ( displayPipe
                                                , removeBlanks
                                                )
import           Macros
import           Options
import           Parser
import           PipeFolds                      ( PipeInfo(..)
                                                , getPipeInfo
                                                )
import           Process
import           Types

runCommands :: String -> PipeEnv -> Interpreter PipeEnv
runCommands cmds pipeEnv = do
    let tokenPipe = tokenPipeline cmds (macroMap pipeEnv)
        pipeEnv'  = pipeEnv { keepHistory = any (any isHistory) tokenPipe }
    customDirectory <- liftIO $ lookupEnv "HSP_CUSTOM"
    case customDirectory of
      Nothing -> return ()
      Just dir -> do
        b <- liftIO
             $  doesPathExist
             $  dir </> "HspCustom.hs"
        when
          b
          (do
              set [searchPath := [dir]]
              loadModules ["HspCustom"]
              setTopLevelModules ["HspCustom"]
          )
    setImportsQ
        [ ("Prelude"    , Nothing)
        , ("Data.Text"  , Just "T")
        , ("Data.Time"  , Nothing)
        , ("Data.List"  , Nothing)
        , ("Types"      , Nothing)
        , ("HspCommands", Nothing)
        ]
    set [languageExtensions := [OverloadedStrings, ScopedTypeVariables]]
    foldM process pipeEnv' tokenPipe

getTmpFileName :: IO String
getTmpFileName = do
    dir  <- getTemporaryDirectory
    name <- getEnv "USER"
    return (dir </> "hsp_rerun_" ++ name)

main :: IO ()
main = do
    options <- execParser opts
    let color          = if noColor options then colorNo else colorYes
    let mcolor         = T.unpack . color
    let (cmds, second) = fromMaybe ("", []) (uncons (args options))
    macroFile <- do
      mdir <- lookupEnv "HSP_MACRO_DIR"
      dir <- maybe (getEnv "HOME") return mdir
      return $ dir </> "hsp_user_macros.json"
    macroGroupFile <- do
      mdir <- lookupEnv "HSP_GROUP_MACRO_DIR"
      dir <- maybe (getEnv "HOME") return mdir
      return $ dir </> "hsp_group_macros.json"
    macroMap      <- mkMacroMapFromFile macroFile
    macroGroupMap <- mkMacroMapFromFile macroGroupFile
    let macroCombinedMap = if macroGroup options
            then H.union macroMap macroGroupMap
            else macroMap
        actionMap  = if macroGroup options then macroGroupMap else macroMap
        actionFile = if macroGroup options then macroGroupFile else macroFile
    unless
        (null $ macroSave options)
        (do
            let macro = mkMacro (macroSave options) cmds
            saveMacro mcolor actionFile macro actionMap
            exitSuccess
        )
    when
        (macroList options)
        (do
            sequence_
                $   displayMacro mcolor
                <$> (sort . H.elems) macroCombinedMap
            exitSuccess
        )
    unless
        (null $ macroFind options)
        (do
            sequence_ $ displayMacro mcolor <$> sort
                (findMacros (macroFind options) macroCombinedMap)
            exitSuccess
        )
    unless
        (null $ macroDelete options)
        (do
            deleteMacro mcolor (macroDelete options) actionFile actionMap
            exitSuccess
        )
    tmpFile <- getTmpFileName
    let stdin = if blankInputs options == 0
            then TIO.getContents
            else return (T.replicate (blankInputs options) "\n")
    rawcontents <- if not (rerun options) then stdin else TIO.readFile tmpFile
    unless (rerun options) $ TIO.writeFile tmpFile rawcontents
    when (null cmds) exitSuccess
    textFile <- if null (textFile options)
        then return ""
        else TIO.readFile (textFile options)

    let pp      = mkInitS (T.lines rawcontents)
        spp     = (\s -> Line 0 (T.pack s) (T.pack s)) <$> second
        fpp     = (\s -> Line 0 s s) <$> T.lines textFile
        pipeEnv = PipeEnv spp fpp [pp] macroCombinedMap False (Options.keepFalse options)
    result <- runInterpreter $ runCommands cmds pipeEnv
    case result of
        Left err -> printInterpreterError err
        Right val ->
            let output = head (pipeline val)
            in  TIO.putStr $ displayPipe color $ if ignoreBlanks options
                    then renumberPipe $ removeBlanks output
                    else output

printInterpreterError :: InterpreterError -> IO ()
printInterpreterError error = do
    let e = displayException error
    putStrLn
        $  T.unpack (colorYes Red)
        ++ "Syntax error"
        ++ errorType e
        ++ T.unpack (colorYes Off)
    putStrLn e
  where
    errorType e
        | "too few arguments" `isInfixOf` e 
        = ": type mismatch, probably too few arguments; perhaps you left out p"
        | "Couldn't match expected type ‘[Char]’" `isInfixOf` e
        = ": perhaps you need a function or operator appropriate for Text, not String"
        | "Couldn't match type" `isInfixOf` e || "expected type" `isInfixOf` e
        = ": type mismatch"
        | "use of ‘+’" `isInfixOf` e
        = ": perhaps you meant to use '++' instead of '+'"
        | "not in scope" `isInfixOf` e
        = let v = head $ words $ concat $ drop 1 $ splitOnStr "scope:" e
          in  ": variable not in scope: " ++ v
        | otherwise
        = ""
