circleArea :: Double -> Maybe Double
circleArea x
  | x <= 0 = Nothing
  | otherwise = Just (x * x * pi)
