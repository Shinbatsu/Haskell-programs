-- Clojure

ifEven :: Integral p => (p -> p) -> p -> p
ifEven f x =
  if even x
    then f x
    else x

genIfEvenX :: Integral p => p -> (p -> p) -> p
genIfEvenX x f = ifEven f x
