--Toader Vlad-Marian, 242

poly::Double->Double->Double->Double->Double
poly a b c x = let
                ecuatie = a*x*x + b*x + c
                in ecuatie

eeny :: Integer -> String
eeny x = if (x`mod`2 == 0)
    then "eeny"
    else "meeny"

fizzbuzz :: Integer -> String
fizzbuzz x = if (x`mod`3 == 0 && x`mod`5 == 0)
    then "FizzBuzz"
    else if (x`mod`5 == 0)
    then "Buzz"
    else if (x`mod` 3 == 0)
        then "Fizz"
    else ""

fibonacciEcuational :: Integer -> Integer
fibonacciEcuational 0 = 0
fibonacciEcuational 1 = 1
fibonacciEcuational n =
    fibonacciEcuational (n - 1) + fibonacciEcuational (n - 2)

tribonacci :: Integer -> Integer
tribonacci 1 = 1
tribonacci 2 = 1
tribonacci 3 = 2
tribonacci n =
    tribonacci (n-1) + tribonacci (n-2) + tribonacci (n-3)

fibonacciCazuri :: Integer -> Integer
fibonacciCazuri n
    | n < 2 = n
    | otherwise = fibonacciCazuri (n - 1) + fibonacciCazuri (n - 2)


bimonial :: Integer -> Integer -> Integer
bimonial 0 k = 0
bimonial n 0 = 1
bimonial n k = bimonial(n-1)k + bimonial (n-1)(k-1)

verifL :: [Int] -> Bool
verifL x = if length(x) `mod` 2 == 0
                then True
            else False

takefinal :: [Int] -> Int -> [Int]
takefinal l n  = drop ((length l)-n)(l)

myreplicate :: Int -> Double -> [Double]
myreplicate 0 _ = []
myreplicate n v
  | n > 0 = v : myreplicate (n-1) v

sumImp :: [Int] -> Int
sumImp [] = 0
sumImp (x:xs)
  | odd x = x + sumImp xs
  | otherwise = sumImp xs

totalLen :: [String] -> Int
totalLen [] = 0
totalLen (x:xs)
  | head x == 'A' = length x + totalLen xs
  | otherwise = totalLen xs
