{-# LANGUAGE MultiWayIf #-}

module Parser
    ( tokensToString
    , tokenPipeline
    , transBuiltin
    )
where

import           Control.Monad
import           Data.List
import           Data.Maybe
import qualified Data.HashMap.Strict           as H
import           Text.Parsec             hiding ( tokens )
import qualified Text.Parsec.Token             as P
import           Text.Parsec.Language           ( haskellDef )

import           Types


isIdentifier :: Token -> Bool
isIdentifier (Identifier _) = True
isIdentifier _              = False

lexer = P.makeTokenParser (haskellDef { P.reservedOpNames = ["||"] })

stringLiteral, charLiteral, ignore, bar :: Parsec String () Token
stringLiteral = Ignore . show <$> P.stringLiteral lexer
charLiteral = Ignore . show <$> P.charLiteral lexer
ignore = fmap (Ignore . (: [])) anyChar
bar = char '|' >> return Bar
doublebar = P.reservedOp lexer "||" >> return (Ignore "|| ")

splitJoin :: [String]
splitJoin =
    [ "s"
    , "slash"
    , "d"
    , "dot"
    , "u"
    , "underscore"
    , "c"
    , "colon"
    , "w"
    , "whitespace"
    , "mm"
    , "comma"
    , "m"
    , "minus"
    , "a"
    , "all"
    ]

implicitP = ["k", "keep", "l", "lose", "rekeep", "rek", "relose", "rel"]

directs = ["n", "o", "original"]

-- Handle identifiers
specials :: Parsec String () Token
specials = do
    i <- P.identifier lexer
    if
        | i == "p"
        -> return P
        | i == "pp"
        -> return Pp
        | i `elem` splitJoin
        -> return $ Split i
        | i `elem` directs
        -> return $ Identifier ("(" ++ i ++ " pline)")
        | i `elem` implicitP
        -> return $ Identifier ("(" ++ i ++ " (getText pline))")
        -- handles T. properly
        | i == "T"
        -> return $ Ignore "T"
        | "env" `isPrefixOf` i
        -> return $ Env i
        | i == "shell"
        -> Shell <$> P.stringLiteral lexer
        | i == "glob"
        -> Glob <$> P.stringLiteral lexer
        | i == "sp"
        -> return Sp
        | i == "fp"
        -> return Fp
        | i == "spp"
        -> return Spp
        | i == "fpp"
        -> return Fpp
        | i == "hp"
        -> Hp . fromInteger <$> P.integer lexer
        | i == "hpp"
        -> Hpp . fromInteger <$> P.integer lexer
        | otherwise
        -> return $ Identifier i

transBuiltin :: [Token] -> PipeEnv -> [Token]
transBuiltin toks pipeEnv = map tb toks  where
    pipe = head (pipeline pipeEnv)
    tb (Ignore s) = Ignore s
    tb (Split s) =
        Identifier ("(" ++ s ++ (if isS pipe then "" else "'") ++ " (getText pline))")
    tb P = Identifier "(getText pline)"
    tb Pp | isS pipe  = Identifier "(getSLines $ head $ pipeline env)"
          | otherwise = Identifier "(getLLines $ head $ pipeline env)"
    tb Sp  = Identifier "(getText sp)"
    tb Fp  = Identifier "(getText fp)"
    tb Spp = Identifier "(spp env)"
    tb Fpp = Identifier "(fpp env)"
    tb (Hpp n) =
        let getType = if isS (history n (pipeline pipeEnv))
                then "(getS"
                else "(getL"
        in  Identifier
                $  getType
                ++ "Lines (history ("
                ++ show n
                ++ ") (pipeline env)))"
    tb (Hp   _) = Identifier "(getText hist)"
    tb (Glob _) = Identifier "glob"
    tb t        = t

tokens :: Parsec String () [Token]
tokens =
    many $ choice [stringLiteral, charLiteral, specials, doublebar, bar, ignore]

-- Splits token lists at Bar tokens, dropping Bars
splitOnBar :: [Token] -> [[Token]]
splitOnBar toks = filter ((/= Bar) . head) $ groupBy f toks
  where
    f Bar Bar = True
    f Bar _   = False
    f _   Bar = False
    f _   _   = True

tokenizeMacro :: Macro -> [Token]
tokenizeMacro m =
    let cmd = (command . macro) m
    in
        case parse tokens "" cmd of
            Right x -> x
            Left e ->
                errorWithoutStackTrace
                    $  "Macro "
                    ++ name m
                    ++ "error "
                    ++ show e

expandMacros :: [Token] -> MacroMap -> [Token]
expandMacros toks mmap | H.null mmap = toks
                       | otherwise   = foldr f [] toks
  where
    f tok acc
        | Identifier s <- tok, H.member s mmap
        = tokenizeMacro (mmap H.! s) ++ acc
        | otherwise
        = tok : acc

tokenPipeline :: String -> MacroMap -> [[Token]]
tokenPipeline cmd mmap = case parse tokens "" cmd of
    Right x -> splitOnBar $ expandMacros x mmap
    Left  e -> errorWithoutStackTrace $ "Parse error " ++ show e

-- Combines a single token into its string expression
tokenToString :: Token -> String
tokenToString (Ignore     s) = s
tokenToString (Identifier s) = s ++ " "
tokenToString (Shell      s) = s ++ " "
tokenToString (Env        s) = s ++ " "
tokenToString _              = ""

tokensToString :: [Token] -> String
tokensToString = concatMap tokenToString
