--Toader Vlad-Marian, 242

import Data.Char

--1
vocale :: String -> Int
vocale [] = 0
vocale x = if (head x) `elem` ['a','A','e','E','i','I','o','O','u','U'] then 1 + vocale(tail x) else vocale(tail x)

nrVocale :: [String] -> Int
nrVocale [] = 0
nrVocale (x:xs)
  | x == reverse(x) = vocale(x) + nrVocale xs
  | otherwise = nrVocale xs

--2
f :: Int -> [Int] -> [Int]
f _ [] = []
f y (x:xs)
  | even x = x:y:(f y xs)
  | otherwise = x:(f y xs)

--3
divizori :: Int -> [Int]
divizori x = [y | y <- [1..x], x `mod` y == 0]

--4
listadiv :: [Int] -> [[Int]]
listadiv x = [ divizori y | y <- x ]

--5a)
inIntervalRec :: Int -> Int -> [Int] -> [Int]
inIntervalRec _ _ [] = []
inIntervalRec a b (x:xs)
  | x >= a && x <= b = x:(inIntervalRec a b xs)
  | otherwise = inIntervalRec a b xs

--5b)
inIntervalComp :: Int -> Int -> [Int] -> [Int]
inIntervalComp a b x = [ y | y<-x, y>=a && y <=b ]

--6a)
pozitiveRec :: [Int] -> Int 
pozitiveRec [] = 0
pozitiveRec (x:xs)
  | x>0 = 1+pozitiveRec(xs)
  | otherwise = pozitiveRec(xs)

--6b)
pozitiveComp :: [Int] -> Int
pozitiveComp x = length [y | y <- x, y > 0]

--7a)
poz :: Int -> [Int] -> [Int]
poz _ [] = []
poz i (x:xs)
  | odd x = i:poz(i+1) xs
  | otherwise = poz(i+1) xs

pozitiiImpareRec :: [Int] -> [Int]
pozitiiImpareRec x = poz 0 x

--7b)
pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp x = [y | (i,y) <- x `zip` [0..], odd i]

--8a)
multdigitsRec :: String -> Int
multdigitsRec [] = 1
multdigitsRec (x:xs)
  | isDigit x = digitToInt x * multdigitsRec(xs)
  | otherwise = multdigitsRec(xs)

--8b)
multdigitsComp :: String -> Int
multdigitsComp x = product [digitToInt y | y <- x, isDigit y]