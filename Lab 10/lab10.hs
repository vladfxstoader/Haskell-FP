--Laborator 10 - Toader Vlad-Marian (242)
--ex.1
data Expr = Const Int --integer constant
      | Expr :+: Expr --addition
      | Expr :*: Expr --multiplication
      deriving Eq

data Operation = Add | Mult deriving (Eq, Show)

data Tree = Lf Int --leaf
      | Node Operation Tree Tree --branch
      deriving (Eq, Show)

--1.1
instance Show Expr where
  show (Const x) = show x
  show (x :+: y) = show x ++ "+" ++ show y
  show (x :*: y) = show x ++ "*" ++ show y

--1.2
evalExp :: Expr -> Int
evalExp (Const x) = x
evalExp (x :+: y) = evalExp x + evalExp y
evalExp (x :*: y) = evalExp x * evalExp y
exp1 = (Const 2 :*: Const 3) :+: (Const 0 :*: Const 5)
exp2 = Const 2 :*: (Const 3 :+: Const 4)
exp3 = Const 4 :+: (Const 3 :*: Const 3)
exp4 = ((Const 1 :*: Const 2) :*: (Const 3 :+: Const 1)) :*: Const 2

--1.3
evalArb :: Tree -> Int
evalArb (Lf x) = x
evalArb (Node Add x y) = evalArb x + evalArb y
evalArb (Node Mult x y) = evalArb x * evalArb y
arb1 = Node Add (Node Mult (Lf 2) (Lf 3)) (Node Mult (Lf 0)(Lf 5))
arb2 = Node Mult (Lf 2) (Node Add (Lf 3)(Lf 4))
arb3 = Node Add (Lf 4) (Node Mult (Lf 3)(Lf 3))
arb4 = Node Mult (Node Mult (Node Mult (Lf 1) (Lf 2)) (Node Add (Lf 3)(Lf 1))) (Lf 2)

--1.4
expToArb :: Expr -> Tree
expToArb (Const x) = Lf x
expToArb (x :+: y) = Node Add (expToArb x) (expToArb y)
expToArb (x :*: y) = Node Mult (expToArb x) (expToArb y)

--ex.2
class Collection c where
  empty :: c key value
  singleton :: key -> value -> c key value
  insert
    :: Ord key
    => key -> value -> c key value -> c key value
  lookup :: Ord key => key -> c key value -> Maybe value
  delete :: Ord key => key -> c key value -> c key value
  keys :: c key value -> [key]
  values :: c key value -> [value]
  toList :: c key value -> [(key, value)]
  fromList :: Ord key => [(key,value)] -> c key value
--2.1
  keys c = [fst x | x <- toList c]
  values c = [snd x | x <- toList c]
  fromList [] = empty
  fromList (x:xs) = insert (fst x) (snd x) (fromList xs)