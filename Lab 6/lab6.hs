--laborator 6.1 - Toader Vlad-Marian (242)

import Data.Char

--1
toInt :: Char -> Char -> Int
toInt x c = ord c - ord x

lctoInt :: Char -> Int
lctoInt = toInt 'a'

uctoInt :: Char -> Int
uctoInt = toInt 'A'

chartransform :: Char -> Int -> Char
chartransform x n = chr (ord x + n)

uctransform :: Int -> Char
uctransform = chartransform 'A'

lctransform :: Int -> Char
lctransform = chartransform 'a'

shift :: Int -> Char -> Char
shift n c
  | isLower c = lctransform((lctoInt c + n) `mod` 26)
  | isUpper c = uctransform((uctoInt c + n) `mod` 26)
  | otherwise = c

rotate :: Int -> String -> String
rotate n xs
  | n < 0 || n > length xs = error "incorrect value for n"
  | otherwise = [shift n x | x <- xs]

--3
makeKey :: Int -> [(Char, Char)]
makeKey n = zip x (rotate n x) where x = ['A'..'Z']

--4
lookUp :: Char -> [(Char, Char)] -> Char
lookUp c [] = c
lookUp c (x:xs)
  | c == fst x = snd x
  | otherwise = lookUp c xs

--5
encipher :: Int -> Char -> Char
encipher n c = lookUp c $ makeKey n

--6
normalize :: String -> String
normalize [] = []
normalize (x:xs)
  | isLower x = toUpper x : normalize xs
  | isDigit x || isLetter x = x : normalize xs
  | otherwise = normalize xs

--7
encipherStr :: Int -> String -> String
encipherStr n c = rotate n $ normalize c

--8
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey = map (\ x -> (snd x, fst x))

--9
decipher :: Int -> Char -> Char 
decipher n c = lookUp c $ reverseKey $ makeKey n

decipherStr :: Int -> String -> String 
decipherStr _ [] = []
decipherStr n (x:xs)
  | x == ' ' || isDigit x = x : decipherStr n xs
  | isAlpha x && isUpper x = decipher n x : decipherStr n xs
  | otherwise = decipherStr n xs