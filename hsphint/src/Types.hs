{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where

import GHC.Exts( IsString(..) )
import Data.List
import           Data.Aeson
import           GHC.Generics
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import Data.Text (Text, pack, unpack, splitOn)

data Line a = Line { n :: Int, getText :: a, o :: Text } 
type SLine = Line Text
type LLine = Line [Text]
data Pipe = S {ssl :: [SLine], showNumber :: Bool}
          | L [LLine]
type Pipeline = [Pipe]

instance Eq a => Eq (Line a) where
  l1 == l2 = getText l1 == getText l2
instance Ord a => Ord (Line a) where
  compare l1 l2 = compare (getText l1) (getText l2)

data MacroInfo =
    MacroInfo { date :: !String
          , command :: !String
          , user :: !String
          , comments :: !String
          } deriving (Show, Ord, Eq, Generic)

data Macro =
    Macro { name :: !String
          , macro :: !MacroInfo
          } deriving (Show, Ord, Eq, Generic)

data Token = Identifier String
           | Shell String
           | Glob String
           | P
           | Pp
           | Fp
           | Sp
           | Fpp
           | Spp
           | Env String
           | Split String
           | Hp Int
           | Hpp Int
           | Ignore String
           | Bar
        deriving (Show, Eq)

instance FromJSON MacroInfo
instance ToJSON MacroInfo
instance FromJSON Macro
instance ToJSON Macro

type MacroMap = M.HashMap String Macro

data PipeEnv = PipeEnv { spp :: [SLine]
                       , fpp :: [SLine]
                       , pipeline :: Pipeline
                       , macroMap :: MacroMap
                       , keepHistory :: Bool
                       , keepFalse :: Bool
                       }

instance Functor Line where
  fmap f s = s { getText = f (getText s) }                       
instance IsString (Line Text) where
  fromString s = Line 0 (pack s) (pack s)

tshow :: Show a => a -> Text
tshow = pack . show

--Split on the given sublist - splitOnStr "a" "abcaabca" == ["","bc","","bc",""]
splitOnStr :: String -> String -> [String]
splitOnStr delim str = map unpack $ splitOn (pack delim) (pack str)

history :: Int -> Pipeline -> Pipe
history n pipeline
    | n < 0 = pipeline !! (- (n + 1))
    | otherwise = reverse pipeline !! n

mkS :: [Text] -> [Text] -> Bool -> Pipe
mkS ps os = S (zipWith3 Line [0..] ps os)

mkInitS :: [Text] -> Pipe
mkInitS ps = mkS ps ps False

isS :: Pipe -> Bool
isS (S _ _) = True
isS _      = False

getSLines :: Pipe -> [SLine]
getSLines (S  s _) = s
getSLines _      = []

getLLines :: Pipe -> [LLine]
getLLines (L s) = s
getLLines _     = []

isHistory :: Token -> Bool
isHistory (Hp _ ) = True
isHistory (Hpp _ ) = True
isHistory _ = False

blankSLine:: SLine
blankSLine = Line 0 "" ""

blankLLine :: LLine
blankLLine = Line 0 [""] ""
