{-# LANGUAGE OverloadedStrings #-}
module Display where

import qualified Data.Text                     as T
import           Data.Text                      ( Text )

import           Colors
import           Types

displayPipe :: ColorF -> Pipe -> Text
displayPipe color = display
 where
  display (S s showNumbers) | not showNumbers = T.unlines (getText <$> s)
                            | showNumbers     = T.unlines (f <$> s)
   where
    f sline =
      color Magenta
        <> "["
        <> tshow (n sline)
        <> "]"
        <> color Green
        <> getText sline
        <> color Off
  display (L ls) = T.unlines (f <$> ls)
   where
    f lline =
      color Magenta
        <> "["
        <> tshow (n lline)
        <> "]"
        <> color Bold
        <> color Green
        <> "["
        <> T.concat (numberFields (getText lline))
        <> color Bold
        <> color Green
        <> "]"
        <> color Off
    numberFields = zipWith
      (\n s ->
        color Bold
          <> color Blue
          <> "["
          <> tshow n
          <> "]"
          <> color Off
          <> color Green
          <> s
          <> color Off
      )
      [0 ..]

removeBlanks :: Pipe -> Pipe
removeBlanks (S x sh) = S (filter (not . T.null . getText) x) sh
removeBlanks (L x   ) = L $ filter (not . null . getText) x
