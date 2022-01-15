-Lab 9 (suplimentar) - Toader Vlad-Marian (242)

--ex.1
--a)
sfchar :: Char -> Bool
sfchar x
  | x == '.' || x == '?' || x == '!' || x == ':' = True
  | otherwise = False

nrProp :: String -> Int
nrProp [] = 0
nrProp (x:xs)
  | sfchar x = 1 + nrProp xs
  | otherwise = nrProp xs
--b)
nrProp1 :: String -> Int
nrProp1 text = length [x | x<-text, sfchar x]

--ex.2
liniiN :: [[Int]] -> Int -> Bool
-- filter (\lin -> length lin == n) a aici am liniile cu n elemente
liniiN a n = and [all (> 0) l | l <- filter (\lin -> length lin == n) a]

--ex.3
data Punct = Pt [Int] 
  deriving Show
(+++) :: Punct -> Punct -> Punct
(+++) (Pt a) (Pt b) = Pt (a ++ b)
data Arb = Vid | F Int | N Arb Arb
  deriving Show
class ToFromArb a where
  toArb :: a -> Arb
  fromArb :: Arb -> a

instance ToFromArb Punct where
  fromArb Vid = Pt []
  fromArb (F x) = Pt [x]
  fromArb (N x y) = fromArb x +++ fromArb y
  toArb (Pt []) = Vid
  toArb (Pt (x:xs)) = N (F x) (toArb (Pt xs))
  
