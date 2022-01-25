pythagTriples :: Int -> [(Int, Int, Int)]
pythagTriples max = [ (a, b, c) | c <- [1..max], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2 ]

fct :: Int -> [Int]
fct n = [ f | f <- [2..n], g <- [2..n], f * g == n ] ++ [n]

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
