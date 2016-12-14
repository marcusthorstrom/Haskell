module Expr where
import Parsing

import Test.QuickCheck
import Data.Char
import Data.Maybe
import Debug.Trace

-- Part 1
-- A
data Expr
  = Num Double
  | VarX
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Sin Expr
  | Cos Expr
 deriving ( Eq, Read )


-- B

instance Show Expr where
 show = showExpr

instance Arbitrary Expr where
   arbitrary = frequency [
       (5, return VarX),
       (1, do expr1 <- arbitrary
              expr2 <- arbitrary
              return (Add expr1 expr2)),
       (1, do expr1 <- arbitrary
              expr2 <- arbitrary
              return (Sub expr1 expr2)),
       (1, do expr1 <- arbitrary
              expr2 <- arbitrary
              return (Add expr1 expr2)),
       (1, do expr <- arbitrary
              return (Sin expr)),
       (1, do expr <- arbitrary
              return (Cos expr)),
       (9, do num <- arbitrary
              return (Num num))
              ]

--arbExpr :: Int -> Gen Expr


showExpr :: Expr -> String
showExpr (Num n)   = show n
showExpr (VarX)    = "x"
showExpr (Add a b) = showFactor a ++ "+" ++ showFactor b
showExpr (Sub a b) = showFactor a ++ "-" ++ showFactor b
showExpr (Mul a b) = showFactor a ++ "*" ++ showFactor b
showExpr (Sin a)   = "Sin " ++ showFactor a
showExpr (Cos a)   = "Cos " ++ showFactor a


showFactor :: Expr -> String
showFactor (Add a b) = "(" ++ showExpr (Add a b) ++ ")"
showFactor (Sub a b) = "(" ++ showExpr (Sub a b) ++ ")"
showFactor (Mul a b) = "(" ++ showExpr (Mul a b) ++ ")"
showFactor (Cos a)   = "(" ++ showExpr (Cos a) ++ ")"
showFactor (Sin a)   = "(" ++ showExpr (Sin a) ++ ")"
showFactor e = showExpr e


-- C
eval :: Expr -> Double -> Double
eval (Num d) x   = d
eval (VarX) x    = x
eval (Add a b) x = (eval a x) + (eval b x)
eval (Sub a b) x = (eval a x) - (eval b x)
eval (Mul a b) x = (eval a x) * (eval b x)
eval (Sin a) x   = sin ((eval a x))
eval (Cos a) x   = cos ((eval a x))

-- D

string :: String -> Parser String
string ""    = return ""
string (c:s) = do c' <- char c
                  s' <- string s
                  return (c':s')


num :: Parser Expr
num = fmap Num readsP <|>
      char 'x' *> return VarX


expr = foldr1 Add `fmap` chain sub (char '+')
sub = foldr1 Sub `fmap` chain term (char '-')
term = foldr1 Mul `fmap` chain sincos (char '*')
factor = char '(' *> expr <* char ')' <|> num

sincos = string "sin" *> fmap Sin factor <|>
         string "cos" *> fmap Cos factor <|>
         factor


readExpr :: String -> Maybe Expr
readExpr s = let s' = map toLower (filter (not.isSpace) s)
              in case parse expr s' of
                Just (e, "") -> Just e
                _            -> Nothing


-- F
simplify :: Expr -> Expr
simplify (Mul a b) | b == (Num 0) = (Num 0)
simplify (Mul a b) | a == (Num 0) = (Num 0)

simplify (Mul a b) | b == (Num 1) = simplify a
simplify (Mul a b) | a == (Num 1) = simplify b

simplify (Add a b) | b == (Num 0) = simplify a
simplify (Add a b) | a == (Num 0) = simplify b

simplify (Sub a b) | b == (Num 0) = simplify a

simplify (Add (Sin a) (Sin b)) | a == b = (Mul (Num 2) (Sin (simplify a)))
simplify (Add (Cos a) (Cos b)) | a == b = (Mul (Num 2) (Cos (simplify a)))

simplify (Add (Num a) (Num b)) = (Num (a+b))
simplify (Mul (Num a) (Num b)) = (Num (a*b))

simplify (Num a) = (Num a)
simplify (VarX) = VarX
simplify (Mul a b) = (Mul (simplify a) (simplify b))
simplify (Add a b) = (Add (simplify a) (simplify b))
simplify (Sub a b) = (Sub (simplify a) (simplify b))
simplify (Sin a) = (Sin (simplify a))
simplify (Cos a) = (Cos (simplify a))


-- G
-- SV: Derivera
differentiate :: Expr -> Expr
differentiate (Mul (VarX) (VarX)) = (Mul (Num 2) VarX)
differentiate (VarX) = (Num 1)
differentiate (Num a) = (Num 0)
differentiate (Add a b) = Add (differentiate a) (differentiate b)
differentiate (Mul VarX b) = b
differentiate (Mul a VarX) = a
-- Chain rule
differentiate (Sin a) = (Mul (Cos a) (differentiate a) )
differentiate (Cos a) = (Sub (Num 0) (Mul (Sin a) (differentiate a)))


-- Product rule
differentiate (Mul a b) = (Add (Mul (differentiate a) b) (Mul a (differentiate b)))
