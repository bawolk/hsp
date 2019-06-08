module Options where

import Options.Applicative

data Options = Options
    { rerun :: !Bool
    , noColor :: !Bool
    , keepFalse :: !Bool
    , ignoreBlanks :: !Bool
    , textFile :: !String
    , blankInputs :: !Int
    , macroDelete :: !String
    , macroFind :: !String
    , macroGroup :: !Bool
    , macroList :: !Bool
    , macroSave :: !String
    , args :: ![String]
    }

falseFlag :: String -> Char -> String -> Parser Bool
falseFlag longOpt shortOpt helpStr = switch
    ( long longOpt
    <> short shortOpt
    <> help helpStr )

blankOption :: String -> Char -> String -> Parser String
blankOption longOpt shortOpt helpStr = strOption
    ( long longOpt
    <> short shortOpt
    <> value ""
    <> help helpStr)

zeroOption :: String -> Char -> String -> Parser Int
zeroOption longOpt shortOpt helpStr = option auto
    ( long longOpt
    <> short shortOpt
    <> value 0 
    <> help helpStr)

versionOption :: Parser (a -> a)
versionOption = infoOption "1.0.0" (long "version" <> short 'v' <> help "Show version")

options :: Parser Options
options = Options
    <$> falseFlag "rerun" 'r' "Use the input to the previous hsp run"
    <*> falseFlag "no-color" 'c' "Print uncolored output"
    <*> falseFlag "keep-false" 'k' "Print blank lines for lines that test as false"
    <*> falseFlag "ignore-blanks" 'i' "Remove blank lines from output"
    <*> blankOption "text-file" 't' "Specify text file to load (for advanced use). Normally cat a file into hsp"
    <*> zeroOption "blank-inputs" 'b' "Generate this number of blank input lines"
    <*> blankOption "macro-delete" 'd' "Delete specified macro"
    <*> blankOption "macro-find" 'f' "List macros containing search term in name, user, or comments"
    <*> falseFlag "macro-group" 'g' "Specify group macros for save and delete; default is user"
    <*> falseFlag "macro-list" 'l' "List all available macros"
    <*> blankOption "macro-save" 's' ("Saves current command as macro. Use \"#\" for adding " ++
            "comments.  Example: hsp -s \"my_macro # number lines\" 'tshow (n+1) <> p'")
    <*> many (argument str (metavar "COMMAND [STRING ...]"))

opts :: ParserInfo Options
opts = info (helper <*> versionOption <*> options)
    ( fullDesc
    <> progDesc "A haskell-centric command line text manipulation tool."
    <> header "hsp - a haskell pipeline processor" )