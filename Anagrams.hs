{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Move brackets to avoid $" #-}

import Data.Char
import Data.List

isAnagramOf :: String -> String -> Bool
isAnagramOf a b = (filter f $ sort $ toUpper <$> a) == (filter f $ sort $ toUpper <$> b)
  where
    ls = ['A' .. 'Z'] ++ ['0' .. '9']
    f x = x `elem` ls