--lab4 - Toader Vlad-Marian (242)
--1
factori :: Int -> [Int]
factori x = [y | y <- [1..x], x `mod` y == 0]

--2
prim :: Int -> Bool 
prim n 
  | length (factori n) == 2 =  True 
  | otherwise = False

--3
numerePrime :: Int -> [Int]
numerePrime n = [y | y <- [2..n], prim y == True]

--4
myzip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
myzip3 [] _ _ = []
myzip3 _ [] _ = []
myzip3 _ _ [] = []
myzip3 (a:as) (b:bs) (c:cs) = (a,b,c) : myzip3 as bs cs

--5
ordonataNat :: [Int] -> Bool
ordonataNat [] = True
ordonataNat [x] = True
ordonataNat (x:xs) = and [a <= b | (a, b) <- zip (x:xs) xs]

--6
ordonataNat1 :: [Int] -> Bool 
ordonataNat1 [] = True
ordonataNat1 [x] = True
ordonataNat1 (x:xs)
  | x <= head xs = ordonataNat1 xs
  | otherwise = False

  --7
ordonata :: [a] -> (a -> a -> Bool) -> Bool
ordonata [] _ = True 
ordonata [x] _ = True 
ordonata (x:xs) op
  | x `op` head xs = ordonata xs op
  | otherwise = False 

--8
(*<*) :: (Integer, Integer) -> (Integer, Integer) -> Bool
(*<*) (x,y) (z,t)
  | x `mod` z ==0 && y `mod` t ==0 = True 
  | otherwise = False

--9
compuneList :: (b -> c) -> [(a -> b)] -> [(a -> c)]
compuneList func [] = []
compuneList func (x:xs) = func.x : compuneList func xs

--10
aplicaList :: a -> [(a -> b)] -> [b]
aplicaList n [] = []
aplicaList n (x:xs) = x n : aplicaList n xs