-- E
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e1 = let e2 = readExpr (show e1) in
                        Just e1 == e2
