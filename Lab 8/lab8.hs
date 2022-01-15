--lab 8 - Toader Vlad-Marian (242)

type Nume = String
data Prop
  = Var Nume
  | F
  | T
  | Not Prop
  | Prop :|: Prop
  | Prop :&: Prop
  | Prop :->: Prop
  | Prop :<->: Prop
  deriving Eq
infixr 1 :->:
infixr 1 :<->:
infixr 2 :|:
infixr 3 :&:

p1 :: Prop
p1 = (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")

p2 :: Prop
p2 = (Var "P" :|: Var "Q") :&: (Not (Var "P") :&: Not (Var "Q"))

p3 :: Prop
p3 = (Var "P" :&: (Var "Q" :|: Var "R")) :&: ((Not (Var "P") :|: Not (Var "Q")) :&: (Not (Var "P") :|: Not (Var "R")))

instance Show Prop where
  show (Var p) = p
  show (Not p) =  "(~" ++  show  p ++ ")"
  show (p :|: q) =  "(" ++  show  p ++ "|" ++ show  q ++ ")"
  show (p :&: q) =  "(" ++  show  p ++ "&" ++ show  q ++ ")"
  show F =  "F"
  show T =  "T"

test_ShowProp :: Bool
test_ShowProp =
  show (Not (Var "P") :&: Var "Q") == "((~P)&Q)"

type Env = [(Nume, Bool)]

impureLookup :: Eq a => a -> [(a,b)] -> b
impureLookup a = fromJust . lookup a

eval :: Prop -> Env -> Bool
eval (Var p) e = impureLookup p e
eval F _ = False
eval T _ = True
eval (Not p) e = not (eval p e)
eval (p :|: q) e = (eval p e) || (eval q e)
eval (p :&: q) e = (eval p e) && (eval q e)

test_eval = eval (Var "P" :|: Var "Q") [("P", True), ("Q", False)] == True

variabile :: Prop -> [Nume]
variabile (Var p) = [p]
variabile F = []
variabile T = []
variabile (Not p) = variabile p
variabile (p :|: q) = nub (variabile p ++ variabile q)
variabile (p :&: q) = nub (variabile p ++ variabile q)

test_variabile =
  variabile (Not (Var "P") :&: Var "Q") == ["P", "Q"]

produsCartezian :: [a] -> Int -> [[a]]
produsCartezian ls 1 = [[x] | x <- ls]
produsCartezian ls n = [x : xs | x <- ls, xs <- produsCartezian ls (n - 1)]

envs :: [Nume] -> [Env]
envs [] = []
envs ls = [zip ls c | c <- produsCartezian [False, True] (length ls)]

satisfiabila :: Prop -> Bool
satisfiabila pr =  or [eval pr x | x <- envs (variabile pr)]

valida :: Prop -> Bool
valida pr = and [eval pr x | x <- envs (variabile pr)]