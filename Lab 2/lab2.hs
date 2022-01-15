--Toader Vlad-Marian, 242

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