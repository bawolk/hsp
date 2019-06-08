{-# LANGUAGE OverloadedStrings #-}
module HspCommands where

import           Data.Char
import           Text.Regex
import           Data.List
import           Data.Maybe
import qualified Data.Text                     as T
import           System.Environment             ( getEnv )
import           System.FilePath                ( pathSeparator
                                                , splitFileName
                                                , takeExtension
                                                )
import           System.FilePath.Glob           ( namesMatching )
import           System.Process
import           Types

infixl 9 !!!

-- Remove qualification where no conflict
-- Makes setImportsQ function in Main.hs simpler
type Text = T.Text
replace = T.replace
splitOn = T.splitOn
pack = T.pack
unpack = T.unpack

original = o

-- converts string to proper type for use with "pp"
text :: Text -> SLine
text s = Line 0 s s

-- split/join functions
s, slash, d, dot, u, underscore, c, colon, mm, comma, m, minus, w, whitespace, a, all
  :: Text -> [Text]
s = splitOn "/"
slash = s
s' = T.intercalate "/"
slash' = s'

d = splitOn "."
dot = d
d' = T.intercalate "."
dot' = d'

u = splitOn "_"
underscore = u
u' = T.intercalate "_"
underscore' = u'

c = splitOn ":"
colon = c
c' = T.intercalate ":"
colon' = c'

mm = splitOn ","
comma = mm
mm' = T.intercalate ","
comma' = mm'

m = splitOn "-"
minus = m
m' = T.intercalate "-"
minus' = m'

w = T.words
whitespace = w
w' = T.unwords
whitespace' = w'

a = T.words . T.map (\c -> if not (isAlphaNum c) then ' ' else c)
all = a
a' = T.unwords
all' = a'

-- Built-in functions

-- Returns a [Text] where each Text consists of consecutive elements
-- satisfying the predicate
groupOnly :: (Char -> Bool) -> Text -> [Text]
groupOnly pred s = filter (pred . T.head) $ T.groupBy f s
  where f a b = pred a && pred b

-- Text functions

cletters, cdigits, cpunctuation :: Text -> [Text]
-- Returns a list of contiguous letters - "ad76 bv!!" => ["ad", "bv"]
cletters = groupOnly isLetter

-- Returns a list of contiguous digits - "ad76 bv7!!" => ["76", "7"]
cdigits = groupOnly isDigit

-- Returns a list of contiguous punctuation - "ad76#bv7!!" => ["#", "!!"]
cpunctuation = groupOnly isPunctuation

lower, upper :: Text -> Text
lower = T.toLower
upper = T.toUpper

-- removes the last occurrence of string d and all that follows
trim :: Text -> Text -> Text
trim d = T.intercalate d . init . splitOn d

-- Remove a set of strings from within another
kill :: [Text] -> Text -> Text
kill a b = foldl' killstr b a where killstr a b = T.concat $ T.splitOn b a

-- 'clean delim string' replaces runs of non-alphanumeric characters 
-- (except for '/', '.', and delim) with delim.  If delim is longer than one
-- character, first character is used.
clean :: Text -> Text -> Text
clean delim s | T.null delim = s
              | otherwise = T.intercalate (T.singleton x) . T.words $ T.map f s
 where
  x = T.head delim
  f c = if not (isAlphaNum c) && c `notElem` ['/', '.', x] then ' ' else c

re :: String -> Text -> Text
re regex t = case matchRegex (mkRegex regex) (unpack t) of
  Just (x : xs) -> pack x
  _             -> ""
-- Replaces each occurrence of the text that matches the first group of the
-- regex regular expression with the text new.
sub :: String -> Text -> Text -> Text
sub regex new t = pack $ subRegex (mkRegex regex) (unpack t) (unpack new)

dir :: Text -> Text
dir = pack . fst . splitFileName . unpack

file :: Text -> Text
file = pack . snd . splitFileName . unpack

ext :: Text -> Text
ext s = case takeExtension (unpack s) of
  (x : xs) -> pack xs
  _        -> ""

keep :: Text -> [Text] -> Bool
keep a bs = or [ T.isInfixOf x a | x <- bs ]
k = keep

lose :: Text -> [Text] -> Bool
lose a bs = not $ keep a bs
l = lose

rekeep :: Text -> String -> Bool
rekeep t regex = isJust $ matchRegex (mkRegex regex) (unpack t)
rek = rekeep

relose :: Text -> String -> Bool
relose t regex = not $ rekeep t regex
rel = relose

integer :: Text -> Integer
integer s = read (unpack s) :: Integer

double :: Text -> Double
double s = read (unpack s) :: Double

--List selection function

(!!!) :: [a] -> [Int] -> [a]
p !!! sl = (map fst . sortOn snd) $ go [] ord (zip p [0 ..])
 where
  ord = sortOn fst (zip sl [0 ..])
  go result a@((i, j) : xs) b@((field, k) : ps)
    | i == k = go ((field, j) : result) xs b
    | i > k  = go result a ps
    | i < k  = errorWithoutStackTrace "(!!!): negative index"
  go result (x : xs) [] = errorWithoutStackTrace "(!!!): index too large"
  go result _        _  = result

-- Functions on pp, spp, fpp, and hpp 

delimit :: Text -> [SLine] -> [SLine]
delimit delim sls = (\s -> Line 0 s "") <$> splitOn delim (oneline sls)

divide :: Int -> [a] -> [[a]]
divide n = go []
 where
  go result rem | null rem  = result
                | otherwise = go (result ++ [take n rem]) (drop n rem)

before :: Text -> Int -> [SLine] -> [[SLine]]
before t n pp = go [] ss
 where
  ss = reverse $ p <$> pp
  go result rem
    | null rem
    = drop 1 $ ((\s -> Line 0 s "") <$>) <$> result
    | otherwise
    = let nextrem = dropWhile (not . T.isInfixOf t) rem
      in  go (reverse (take (n + 1) nextrem) : result) (drop 1 nextrem)

after :: Text -> Int -> [SLine] -> [[SLine]]
after t n pp = go [] ss
 where
  ss = p <$> pp
  go result rem
    | null rem
    = ((\s -> Line 0 s "") <$>) <$> reverse result
    | otherwise
    = let nextrem = dropWhile (not . T.isInfixOf t) rem
      in  go (take (n + 1) nextrem : result) (drop 1 nextrem)

matrix :: Text -> Int -> [SLine] -> [[SLine]]
matrix s n pp = zipWith (++) (before s n pp) (drop 1 <$> after s n pp)

oneline :: [SLine] -> Text
oneline sls = T.unwords $ fmap p sls

onelined :: Text -> [SLine] -> Text
onelined d sls = T.intercalate d $ fmap p sls

-- takes a split pp or split hpp and makes each field a separate line
expand :: [LLine] -> [SLine]
expand lls = zipWith3 Line [0 ..] (concatMap p lls) (repeat "")

uniq :: Eq a => [a] -> [a]
uniq = nub

blanklines :: Int -> [SLine]
blanklines n = replicate n blankSLine

digits, letters, punctuation, apost, quote :: Text
digits = "0123456789"
letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
punctuation = "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
apost = "'"
quote = "\""

-- IO commands

globFunc :: String -> IO [Text]
globFunc s = do
  pwd <- getEnv "HSP_PWD"
  let prefix  = pwd ++ [pathSeparator]
      -- if glob pattern relative, need to prefix it with correct directory
      globStr = if head s == pathSeparator then s else prefix ++ s
  names <- (pack <$>) <$> namesMatching globStr
  return $ if head s == pathSeparator
    then names
    else T.concat . tail . splitOn (pack prefix) <$> names

-- "envUSER" produces $USER as text
envFunc :: String -> IO Text
envFunc s = pack <$> getEnv (drop 3 s)

pwdFunc :: IO Text
pwdFunc = pack <$> getEnv "HSP_PWD"

shellFunc :: String -> IO [Text]
shellFunc s = do
  pwd <- getEnv "HSP_PWD"
  T.lines . pack <$> readCreateProcess ((shell s) { cwd = Just pwd }) ""
