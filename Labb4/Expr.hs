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
  | Mul Expr Expr
  | Sin Expr
  | Cos Expr
 deriving ( Eq, Read )


-- B

instance Show Expr where
 show = showExpr

-- instance Arbitrary Expr where
--   arbitrary = sized arbExpr

--arbExpr :: Int -> Gen Expr


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

string :: String -> Parser String
string ""    = return ""
string (c:s) = do c' <- char c
                  s' <- string s
                  return (c':s')

integer :: Parser Double
integer = nat <|> fmap negate (char '-' *> nat)
--integer = oneOrMore digit >>= \ds -> return (read ds)

nat :: Parser Double
--nat = oneOrMore digit >>= return . read
nat = oneOrMore digit *> char '.' *> oneOrMore digit >>= return . read

var :: Parser Expr
var = char 'x' *> return VarX <|> num


num :: Parser Expr
num = fmap Num integer

--num k = case (read k :: [(Double, String)]) of
--        [(a, s)] -> Just (Num a, s)
--        []       -> Nothing


expr = foldr1 Add `fmap` chain term (char '+')
term = foldr1 Mul `fmap` chain sincos (char '*')
factor = char '(' *> expr <* char ')' <|> var

--var = char 'x'

sincos = string "sin" *> fmap Sin factor <|>
         string "cos" *> fmap Cos factor <|>
         factor

--sincos = sat (=='s') *> sat (=='i') *> sat (=='n') *> fmap Sin expr <|>
--         sat (=='c') *> sat (=='o') *> sat (=='s') *> fmap Cos expr <|>
--         expr

readExpr :: String -> Maybe Expr
readExpr s = let s' = map toLower (filter (not.isSpace) s)
              in case parse expr s' of
                Just (e, "") -> Just e
                _            -> Nothing


prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e1 = let e2 = readExpr (show e1) in
                        Just e1 == e2
