{-# LANGUAGE OverloadedStrings #-}
module Colors where

import Data.Text (Text)
data Color = Off | Red | Green | Magenta | Bold | Yellow | Blue
type ColorF = Color -> Text

colorYes :: Color -> Text
colorYes Off     = "\ESC[0m"
colorYes Red     = "\ESC[31m"
colorYes Green   = "\ESC[32m"
colorYes Magenta = "\ESC[35m"
colorYes Bold    = "\ESC[1m" 
colorYes Yellow  = "\ESC[33m"
colorYes Blue    = "\ESC[34m"

colorNo :: Color -> Text
colorNo _ = ""