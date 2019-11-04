-- x_i = (x^i)/(i!)
exps :: Float -> [Float]
exps x = zipWith (/) (iterate (* x) 1.0) fact
    where fact = scanl (*) 1 [1..]

-- Recurrencia: x_i = x/i * x_(i-1)
{-
exps :: Float -> [Float]
exps x = scanl (*) 1 (exps' x 1)
    where exps' x i = x/i : (exps' x (i+1))-}

exponencial :: Float -> Float -> Float
exponencial x e = sum $ takeWhile (>= e) $ exps x




--dona diferent de 0 (why) en alguns casos com (test 2.3)
--test x = zipWith (-) (iterate (* x) 1.0) (map (x^^) [0,1..])