module Macros where

import           Data.Aeson
import           System.Directory
import           System.Environment
import           Data.Maybe                     ( fromMaybe )
import qualified Data.HashMap.Strict           as H
import           Data.Time
import           Data.List

import           Types
import           Colors

type ColorM = Color -> String
getMacros :: FilePath -> IO [Macro]
getMacros f = do
    b <- doesPathExist f
    if b then fromMaybe [] <$> decodeFileStrict' f else return []

mkMacroMap :: [Macro] -> MacroMap
mkMacroMap = H.fromList . ((\m -> (name m, m)) <$>)

mkMacroMapFromFile :: FilePath -> IO MacroMap
mkMacroMapFromFile f = mkMacroMap <$> getMacros f

mkMacro :: String -> String -> IO Macro
mkMacro saveStr cmd = do
    user <- getEnv "USER"
    date <- getCurrentTime
    let split    = break ('#' ==) saveStr
        trim     = dropWhileEnd (' ' ==) . dropWhile (' ' ==)
        name     = trim $ fst split
        comments = '#' : trim (drop 1 $ snd split)
        fmtDate  = formatTime defaultTimeLocale "%F %T" date
    return $ Macro name $ MacroInfo fmtDate cmd user comments

saveMacro :: ColorM -> FilePath -> IO Macro -> MacroMap -> IO ()
saveMacro color f macro mmap = do
    m <- macro
    encodeFile f (H.elems (H.insert (name m) m mmap))
    (\m -> color Yellow ++ name m ++ " successfully saved")
        <$> macro
        >>= putStrLn


findMacros :: String -> MacroMap -> [Macro]
findMacros s mmap = H.elems $ H.filter f mmap
  where
    f (Macro name (MacroInfo _ _ user comments)) =
        any (isInfixOf s) [name, user, comments]

deleteMacro :: ColorM -> String -> FilePath -> MacroMap -> IO ()
deleteMacro color s f mmap = if H.member s mmap
    then do
        encodeFile f (H.elems (H.delete s mmap))
        putStrLn $ color Magenta ++ s ++ " macro has been successfully deleted"
    else putStrLn $ color Red ++ s ++ " does not exist"

displayMacro :: ColorM -> Macro -> IO ()
displayMacro color m = do
    let indent = replicate (11 + length ((user . macro) m)) ' '
    putStrLn $ color Magenta ++ name m
    putStrLn
        $  color Yellow
        ++ "         "
        ++ (user . macro) m
        ++ "  "
        ++ (date . macro) m
    putStrLn $ color Off ++ indent ++ (command . macro) m
    putStrLn $ color Green ++ indent ++ (comments . macro) m
    putStrLn ""
