module Expr where
--import Parsing

import Test.QuickCheck
import Data.Char(isSpace)
import Data.Maybe

-- Part 1
-- A
data Expr
  = Num Double
  | VarX
  | Add Expr Expr
  | Mul Expr Expr
  | Sin Expr
  | Cos Expr
 deriving ( Eq, Read )


-- B

instance Show Expr where
 show = showExpr

instance Arbitrary Expr where
  arbitrary = sized rExpr

showExpr :: Expr -> String
showExpr (Num n)   = show n
showExpr (VarX)    = "x"
showExpr (Add a b) = showFactor a ++ "+" ++ showFactor b
showExpr (Mul a b) = showFactor a ++ "*" ++ showFactor b
showExpr (Sin a)   = "Sin " ++ showFactor a
showExpr (Cos a)   = "Cos " ++ showFactor a


showFactor :: Expr -> String
showFactor (Add a b) = "(" ++ showExpr (Add a b) ++ ")"
showFactor (Mul a b) = "(" ++ showExpr (Mul a b) ++ ")"
showFactor e = showExpr e


-- C
eval :: Expr -> Double -> Double
eval (Num d) x   = d
eval (VarX) x    = x
eval (Add a b) x = (eval a x) + (eval b x)
eval (Mul a b) x = (eval a x) * (eval b x)
eval (Sin a) x   = sin ((eval a x))
eval (Cos a) x   = cos ((eval a x))

-- D

type Parser a = String -> Maybe (a, String)


num :: Parser Expr
num s = case reads s of
      (i,s'):_ -> Just (Num i, s')
      _        -> Nothing


chain p c f s = case p s of
  Just (n, c':s') | c' == c -> case chain p c f s' of
                Just (e, s'') -> Just (f n e, s'')
                Nothing -> Just (n, c:s')
  Nothing -> Nothing
  r                -> r

expr1 = chain num '+' Add
expr = chain term '+' Add
term = chain factor '*' Mul


factor ('(':s) = case expr s of
  Just (e,')':s') -> Just (e, s')
  _               -> Nothing

factor s       = num s


readExpr :: String -> Maybe Expr
readExpr s = case expr (filter (not.isSpace) s) of
  Just (e, "") -> Just e
  _            -> Nothing
