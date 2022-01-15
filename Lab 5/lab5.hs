--lab 5 - Toader Vlad-Marian (242)
--1
firstEl :: [(a,b)] -> [a]
firstEl = map fst

--2
sumList :: [[Int]] -> [Int]
sumList = map sum

--3
func :: Int -> Int
func x
  | odd x = x*2
  | otherwise = x `div` 2

prel2 :: [Int] -> [Int]
prel2 = map func

--4
lssir :: Char -> [String] -> [String]
lssir x = filter (elem x) 

--5
sqr :: [Int] -> [Int]
sqr = map (^2) . filter odd

--6
sqrindex :: [Int] -> [Int]
sqrindex x = map ((^2) . snd) (filter (odd . fst) (zip [1..length x] x))

--7 - eliminarea consoanelor -> afisarea vocalelor
numaiVocale :: [String] -> [String]
numaiVocale = map (filter (`elem` "aeiouAEIOU"))

--8
mymap :: (a->b) -> [a] -> [b]
mymap _ [] = []
mymap f (x:xs) = f x : mymap f xs

myfilter :: (a->Bool) -> [a] -> [a]
myfilter _ [] = []
myfilter f (x:xs)
  | f x = x : myfilter f xs
  | otherwise = myfilter f xs

--9
sumsqr :: [Int] -> Int
sumsqr = foldr (+) 0 . map (^2) . filter odd

--10
verifTrue :: [Bool] -> Bool 
verifTrue = foldr (&&) True 

--11a
rmChar :: Char -> String -> String 
rmChar x = filter (/=x)

--11b
rmCharsRec :: String -> String -> String 
rmCharsRec [] [] = []
rmCharsRec [] x = x
rmCharsRec x [] = []
rmCharsRec y (x:xs)
  | x `elem` y = rmCharsRec (rmChar x y) xs
  | otherwise = x : rmCharsRec y xs