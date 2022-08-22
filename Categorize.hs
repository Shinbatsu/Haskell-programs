openOrSenior :: [(Int, Int)] -> [String]
openOrSenior = map (\x -> if snd x > 7 && fst x >= 55 then "Senior" else "Open")
