module Calculus where

import Data.Maybe

data UnOp = Neg | Sin | Cos | Log
          deriving (Eq, Ord, Show)

data BinOp = Add | Mul | Div
           deriving (Eq, Ord, Show)

data Exp = Val Double | Id String | UnApp UnOp Exp | BinApp BinOp Exp Exp
         deriving (Eq, Ord, Show)

type Env = [(String, Double)]

---------------------------------------------------------------------------
-- Type classes and class instances

class Vars a where
  x, y, z :: a

instance Vars Exp where
  x = Id "x"
  y = Id "y"
  z = Id "z"

instance Vars Double where
  x = 4.3
  y = 9.2
  z = -1.7

instance Num Exp where
  negate (Val 0) = Val 0
  negate e    = UnApp Neg e
  (+) e (Val 0) = e
  (+) (Val 0) e = e
  (+) e1 e2   = BinApp Add e1 e2
  (*) (Val 1) e = e
  (*) e (Val 1) = e
  (*) (Val 0) e = Val 0
  (*) e (Val 0) = Val 0
  (*) e1 e2 = BinApp Mul e1 e2




instance Fractional Exp where
  fromRational  = undefined
  (/) (Val 0) e = Val 0
  (/) e (Val 1) = e
  (/) e1 e2     = BinApp Div e1 e2


instance Floating Exp where
  sin      = UnApp Sin
  cos      = UnApp Cos
  log      = UnApp Log
  tan x    = BinApp Div (UnApp Sin x) (UnApp Cos x)
  pi       = Val 3.14
  


---------------------------------------------------------------------------
-- Tables for the showExp funtion
showUnOpTable
  = [  (Neg, "-"  )
    ,  (Sin, "sin")
    ,  (Cos, "cos")
    ,  (Log, "log")
    ]

showBinOpTable
  = [  (Add, "+")
    ,  (Mul, "*")
    ,  (Div, "/")
    ]

-- Tables for the eval function
unOpTable
  = [  (Neg, negate)
    ,  (Sin, sin)
    ,  (Cos, cos)
    ,  (Log, log)
    ]
binOpTable
  = [  (Add, (+))
    ,  (Mul, (*))
    ,  (Div, (/))
    ]

-- Looks up a key and returns the binding value
lookUp :: Eq a => a -> [(a, b)] -> b
-- Pre: the key always have a binding value in the provided list
lookUp
  = (fromJust.).lookup

-- Shows a given expression in long form
showExp :: Exp -> String
showExp (Val val)
  = show val
showExp (Id string)
  = show string
showExp (UnApp unOp expression)
  = showUnOp ++ showExp expression
  where
    showUnOp = lookUp unOp showUnOpTable
showExp (BinApp binOp exp1 exp2)
  = showExp exp1 ++ showUnOp ++ showExp exp2
  where
    showUnOp = lookUp binOp showBinOpTable

-- Evaluates a given expression
eval :: Exp -> Env -> Double
eval (Val val) _
  = val
eval (Id string) env
  = lookUp string env
eval (UnApp unOp expression) env
  = operation (eval expression env)
  where
    operation = lookUp unOp unOpTable
eval (BinApp binOp exp1 exp2) env
  = operation (eval exp1 env) (eval exp2 env)
  where
    operation = lookUp binOp binOpTable


-- Differentates an expression and returns and expression
diff :: Exp -> String -> Exp
diff (Val _) _
  = (Val 0.0)
diff (Id x) dx
  | x==dx     = Val 1.0
  | otherwise = Val 0.0
diff (UnApp unOp expression) dx
  | unOp == Sin = cos expression * exp'
  | unOp == Cos = negate ((sin expression) * exp')
  | unOp == Log = exp'/expression
  | unOp == Neg = negate exp'
  where
    exp' = diff expression dx
diff (BinApp binOp exp1 exp2) dx
  | binOp == Add = exp1' + exp2'
  | binOp == Mul = (exp1 * exp2') + (exp1' * exp2)
  | binOp == Div = numerator / denominator
  where
    exp1' = diff exp1 dx
    exp2' = diff exp2 dx
    denominator = exp2 * exp2
    numerator   = (exp1 * exp2') + (negate 1.0 * (exp1 * exp2'))


maclaurin :: Exp -> Double -> Int -> Double
-- Pre: The variable is x ie the differentiation will take place wrt to x
maclaurin _ point 0
  = point
maclaurin expression point n
  = sum termList
  where
    termList = take n (zipWith3 nTerm diffList xPowList facList)
    facList  = scanl (*) 1 [1..]
    diffList = iterate (`diff` "x") expression
    xPowList = 1 : (iterate (*point) point)
    nTerm    = \x y z -> ((eval x env)*y)/z
    env      = [("x",0)]
---------------------------------------------------------------------------
-- Test cases...

e1, e2, e3, e4, e5, e6 :: Exp

-- 5*x
e1 = BinApp Mul (Val 5.0) (Id "x")

-- x*x+y-7
e2 = BinApp Add (BinApp Add (BinApp Mul (Id "x") (Id "x")) (Id "y"))
                (UnApp Neg (Val 7.0))

-- x-y^2/(4*x*y-y^2)::Exp
e3 = BinApp Add (Id "x")
            (UnApp Neg (BinApp Div (BinApp Mul (Id "y") (Id "y"))
            (BinApp Add (BinApp Mul (BinApp Mul (Val 4.0) (Id "x")) (Id "y"))
                        (UnApp Neg (BinApp Mul (Id "y") (Id "y"))))))

-- -cos x::Exp
e4 = UnApp Neg (UnApp Cos (Id "x"))

-- sin (1+log(2*x))::Exp
e5 = UnApp Sin (BinApp Add (Val 1.0)
                           (UnApp Log (BinApp Mul (Val 2.0) (Id "x"))))

-- log(3*x^2+2)::Exp
e6 = UnApp Log (BinApp Add (BinApp Mul (Val 3.0) (BinApp Mul (Id "x") (Id "x")))
                           (Val 2.0))
