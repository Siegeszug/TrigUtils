pythagTriples :: Int -> [(Int, Int, Int)]
pythagTriples max = [ (a, b, c) | c <- [1..max], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2 ]

fct :: Int -> [Int]
fct n = [ f | f <- [2..n], n `mod` f == 0 ]

coprime :: [Int] -> [Int] -> Bool
coprime [] _ = True
coprime _ [] = True
coprime (n:l') m = (not (elem n m)) && (coprime l' m)

coprime3 :: (Int, Int, Int) -> Bool
coprime3 (a, b, c) = coprime (fct a) (fct b) ||
                     coprime (fct b) (fct c) ||
                     coprime (fct a) (fct c)
                         
primatives :: Int -> [(Int, Int, Int)]
primatives max = [ t | t <- (pythagTriples max), (coprime3 t) ]

primRatio :: Int -> Float
primRatio 0 = 1
primRatio n = x / y
  where x = fromIntegral (length (primatives n)) :: Float
        y = fromIntegral (length (pythagTriples n)) :: Float
