-- Part 1
-- A
data Expr
  = Num Double
  | VarX
  | Add Expr Expr
  | Mul Expr Expr
  | Sin Expr
  | Cos Expr
 deriving ( Eq )

instance Show Expr where
 show (Num a) = show a
 show (VarX) = "x"
 show (Add lh rh) = (show lh)++"+"++(show rh)
 show (Mul lh rh) = (show lh)++"*"++(show rh)
 show (Sin val) = "Sin("++(show val)++")"
 show (Cos val) = "Cos("++(show val)++")"


-- B
