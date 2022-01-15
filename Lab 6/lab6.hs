--lab 6 - Toader Vlad-Marian (242)
--ex. 1
data Fruct = Mar String Bool | Portocala String Int deriving (Show)
ionatanFaraVierme = Mar "Ionatan" False
goldenCuVierme = Mar "Golden Delicious" True
portocalaSicilia10 = Portocala "Sanguinello" 10
listaFructe = [Mar "Ionatan" False,
                Portocala "Sanguinello" 10,
                Portocala "Valencia" 22,
                Mar "Golden Delicious" True,
                Portocala "Sanguinello" 15,
                Portocala "Moro" 12,
                Portocala "Tarocco" 3,
                Portocala "Moro" 12,
                Portocala "Valencia" 2,
                Mar "Golden Delicious" False,
                Mar "Golden" False,
                Mar "Golden" True]

--a)
ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia _ = False
ePortocalaDeSicilia (Portocala x y)
  | x == "Moro" = True
  | x == "Tarocco" = True
  | x == "Sanguinello" = True
  | otherwise = False

--b)
nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia ls = foldr (+) 0 [i | (Portocala s i) <- ls, ePortocalaDeSicilia(Portocala s i)]

--c)
areViermi :: Fruct -> Bool
areViermi (Portocala _ _) = False
areViermi (Mar _ False) = False
areViermi (Mar _ True) = True

nrMereViermi :: [Fruct] -> Int
nrMereViermi = length . filter (areViermi)

--Ex. 2
type NumeA = String
type Rasa = String
data Animal = Pisica NumeA | Caine NumeA Rasa deriving (Show) --Pisica are un string =NumeA, Cainele are 2 stringuri - nume + rasa

--2a)
vorbeste :: Animal -> String
vorbeste (Pisica _) = "Meow!"
vorbeste (Caine _ _) = "Woof!"

--2b)
rasa :: Animal -> Maybe String
rasa (Pisica _) = Nothing
rasa (Caine _ r) = Just r

--Ex. 3
data Linie = L [Int] deriving (Show)
data Matrice = M [Linie] deriving (Show)


--a)
verifLinie :: Linie -> Int -> Bool
verifLinie (L l) n
  | sum l == n = True
  | otherwise = False

verifica :: Matrice -> Int -> Bool --matricea este o lista de linii (lista de liste de int-uri)
verifica (M m) n = foldr(&&) True [verifLinie l n | l <- m]

--3b
pozlin :: Linie -> Bool
pozlin (L l) =  length l == length ([x | x <- l, x>0 ])

verifdim :: Linie -> Int -> Bool
verifdim (L l) n = length l == n

doarPozN :: Matrice -> Int -> Bool
doarPozN (M m) n = foldr (&&) True [pozlin l | l <- m, verifdim l n]

--3c
corect :: Matrice -> Bool
corect (M []) = True
corect (M [y]) = True
corect (M (L x:L y:xs))
  | length x == length y = corect (M (L y:xs))
  | otherwise = False