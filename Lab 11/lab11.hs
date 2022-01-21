--lab 11 - Toader Vlad-Marian (242)
type Name = String
data Value = VBool Bool
    | VInt Int
    | VFun (Value -> Value)
    | VError
data Hask = HTrue | HFalse
    | HIf Hask Hask Hask
    | HLit Int
    | Hask :==: Hask
    | Hask :+: Hask
    | Hask :*: Hask
    | HVar Name
    | HLam Name Hask
    | Hask :$: Hask
infix 4 :==:
infixl 6 :+:
infixl 7 :*:
infixl 9 :$:
type HEnv = [(Name, Value)]

--ex.1
instance Show Value where
    show (VBool b) = show b
    show (VInt x) = show x
    show (VFun f) = "functie"
    show VError = "eroare"

--ex.2
instance Eq Value where
    (VBool b) == (VBool c) = b == c
    (VInt b) == (VInt c) = b == c
    _ == _ = False

--ex.3
hEval :: Hask -> HEnv -> Value
hEval HTrue r = VBool True
hEval HFalse r = VBool False
hEval (HIf c d e) r = hif (hEval c r) (hEval d r) (hEval e r)
    where hif (VBool b) v w = if b then v else w
          hif _ _ _ = VError
hEval (HLit i) r = VInt i
hEval (d :==: e) r = heq (hEval d r) (hEval e r)
    where heq (VInt a) (VInt b) = VBool (a==b)
          heq (VBool a) (VBool b) = VBool (a==b)
          heq _ _ = error "failed to execute :==:"

hEval (d :+: e) r = hplus (hEval d r) (hEval e r)
    where hplus (VInt a) (VInt b) = VInt (a+b)
          hplus _ _ = error "failed to execute :+:"

hEval (d :*: e) r = hmult (hEval d r) (hEval e r)
    where hmult (VInt a) (VInt b) = VInt (a*b)
          hmult _ _ = error "failed to execute :*:"

hEval (HLam nume expr ) l = VFun (\v -> hEval expr ((nume, v) : l))

hEval (d :$: e) r = happ (hEval d r) (hEval e r)
    where happ (VFun f) = f

