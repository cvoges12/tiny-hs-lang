data Expr = Lit Int |
            Add Expr Expr |
            Sub Expr Expr |
            Mul Expr Expr |
            Div Expr Expr

data SExpr = Atom String |
             List [SExpr]

eval :: Expr -> Int
eval (Lit a) = a
eval (Add a b) = (eval a) + (eval b)
eval (Sub a b) = (eval a) - (eval b)
eval (Mul a b) = (eval a) * (eval b)
eval (Div a b) = (eval a) / (eval b)

count :: Expr -> Int
count (Lit a) = 1
count (Add a b) = (count a) + (count b) + 1
count (Sub a b) = (count a) + (count b) + 1
count (Mul a b) = (count a) + (count b) + 1
count (Div a b) = (count a) + (count b) + 1
