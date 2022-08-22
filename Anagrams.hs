{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Data.Char (toUpper)
import Data.List (sort)

isAnagramOf :: String -> String -> Bool
isAnagramOf a b = filter f (sort $ toUpper <$> a) == filter f (sort $ toUpper <$> b)
  where
    ls = ['A' .. 'Z'] ++ ['0' .. '9']
    f x = x `elem` ls