data Expr = Val Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr
     
     
eval1 :: Expr -> Int
eval1 (Val x) = x
eval1 (Add x y) = eval1 x + eval1 y
eval1 (Sub x y) = eval1 x - eval1 y
eval1 (Mul x y) = eval1 x * eval1 y
eval1 (Div x y) = div (eval1 x) (eval1 y)

eval2 :: Expr -> Maybe Int
eval2 (Val x) = Just x
eval2 (Add x y) = evalOp (+) x y
eval2 (Sub x y) = evalOp (-) x y
eval2 (Mul x y) = evalOp (*) x y
eval2 (Div x y) = do
    e1 <- eval2 x
    e2 <- eval2 y
    if e2 == 0 then Nothing else Just (div e1 e2)
    
evalOp :: (Int -> Int -> Int) -> Expr -> Expr -> Maybe Int
evalOp op x y = do
    e1 <- eval2 x
    e2 <- eval2 y
    return $ op e1 e2
    
eval3 :: Expr -> Either String Int
eval3 (Val x) = Right x
eval3 (Add x y) = evalOp' (+) x y
eval3 (Sub x y) = evalOp' (-) x y
eval3 (Mul x y) = evalOp' (*) x y
eval3 (Div x y) = do
    e1 <- eval3 x
    e2 <- eval3 y
    if e2 == 0 then (Left "div0") else (Right (div e1 e2))
    
evalOp' :: (Int -> Int -> Int) -> Expr -> Expr -> Either String Int
evalOp' op x y = do
    e1 <- eval3 x
    e2 <- eval3 y
    Right (op e1 e2)
