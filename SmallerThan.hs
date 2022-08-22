module SmallerThan where

smaller :: Ord a => [a] -> [Int]
smaller [] = []
smaller (h : t) = length [a | a <- t, a < h] : smaller t
