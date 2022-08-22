stringify :: [Int] -> String
stringify [] = "null"
stringify (x : xs) = show x ++ " -> " ++ stringify xs
