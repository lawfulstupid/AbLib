data Expr = Var Char
          | Con Double
          | Add Expr Expr
          | Neg Expr
          | Mul Expr Expr
          -- | Inv Expr
          | Div Expr Expr
          | Exp Expr Expr
          | Fun String Expr

instance Show Expr where
    show (Var v) = [v]
    show (Con c) = show c
    show (Add e1 e2) = "(" ++ (show e1) ++ ") + (" ++ (show e2) ++ ")"
    show (Neg e) = "-(" ++ (show e) ++ ")"
    show (Mul e1 e2) = "(" ++ (show e1) ++ ") * (" ++ (show e2) ++ ")"
    -- show (Inv e) = "1 / (" ++ (show e) ++ ")"
    show (Div e1 e2) = "(" ++ (show e1) ++ ") / (" ++ (show e2) ++ ")"
    show (Exp e1 e2) = "(" ++ (show e1) ++ ") ^ (" ++ (show e2) ++ ")"
    show (Fun f e) = (showString f "") ++ "(" ++ (show e) ++ ")"
    
-- explicit show
showe :: Expr -> String
showe (Var v) = "(Var " ++ (show v) ++ ")"
showe (Con c) = "(Con " ++ (show c) ++ ")"
showe (Add e1 e2) = "(Add " ++ (showe e1) ++ " " ++ (showe e2) ++ ")"
showe (Neg e) = "(Neg " ++ (showe e) ++ ")"
showe (Mul e1 e2) = "(Mul " ++ (showe e1) ++ " " ++ (showe e2) ++ ")"
-- showe (Inv e) = "(Inv " ++ (showe e) ++ ")"
showe (Div e1 e2) = "(Div " ++ (showe e1) ++ " " ++ (showe e2) ++ ")"
showe (Exp e1 e2) = "(Exp " ++ (showe e1) ++ " " ++ (showe e2) ++ ")"
showe (Fun f e) = "(Fun " ++ f ++ " " ++ (showe e) ++ ")"

instance Eq Expr where
    Var v == Var v' = (v == v')
    Con c == Con c' = (c == c')
    Add e1 e2 == Add x1 x2 = ((e1 == x1) && (e2 == x2)) || ((e1 == x2) && (e2 == x1))
    Neg e == Neg e' = (e == e')
    Mul e1 e2 == Mul x1 x2 = ((e1 == x1) && (e2 == x2)) || ((e1 == x2) && (e2 == x1))
    -- Inv e == Inv e' = (e == e')
    Div e1 e2 == Div x1 x2 = (e1 == x1) && (e2 == x2)
    Exp e1 e2 == Exp x1 x2 = (e1 == x1) && (e2 == x2)
    Fun s e == Fun s' e' = (s == s') && (e == e')
    _ == _ = False

instance Num Expr where
    -- addition
    (Con c) + (Con c') = Con (c + c')
    e + (Con 0) = e
    (Con 0) + e = e
    e1 + (Neg e2) = e1 - e2
    (Neg e1) + e2 = e2 - e1
    e1 + e2 = Add e1 e2
    -- subtraction
    (Con c) - (Con c') = Con (c - c')
    e - (Con 0) = e
    (Con 0) - e = negate e
    e1 - (Neg e2) = e1 + e2
    (Neg e1) - e2 = negate (e1 + e2)
    e1 - e2 = if e1 == e2 then Con 0 else Add e1 (negate e2)
    -- negation
    negate (Con c) = Con (-c)
    negate (Neg e) = e
    negate e = Neg e
    -- multiplication
    (Con c) * (Con c') = Con (c * c')
    e * (Con 0) = Con 0
    e * (Con 1) = e
    e * (Con (-1)) = negate e
    (Con 0) * e = Con 0
    (Con 1) * e = e
    (Con (-1)) * e = negate e
    e1 * (Div (Con 1) e2) = e1 / e2
    (Div (Con 1) e1) * e2 = e2 / e1
    -- e1 * (Inv e2) = e1 / e2
    -- (Inv e1) * e2 = e2 / e1
    e1 * e2 = Mul e1 e2
    -- other
    abs (Con c) = Con (abs c)
    abs e = Fun "abs" e
    signum (Con c) = Con (signum c)
    signum e = Fun "signum" e
    fromInteger n = Con (fromInteger n)

instance Fractional Expr where
    -- division
    (Con c) / (Con c') = Con (c / c')
    (Con 0) / e = Con 0
    e / (Con 1) = e
    e / (Con (-1)) = negate e
    e / (Div e1 e2) = e * e2 / e1
    e1 / e2 = if e1 == e2 then Con 1 else Div e1 e2
    recip e = 1 / e
    fromRational r = Con (fromRational r)

instance Floating Expr where
    pi = Con (pi)
    sqrt e = e ** 0.5
    exp (Con c) = Con (exp c)
    exp (Fun "log" e) = e
    exp e = Fun "exp" e
    log (Con c) = Con (log c)
    log (Fun "exp" e) = e
    log e = Fun "log" e
    sin (Con c) = Con (sin c)
    sin e = Fun "sin" e
    cos (Con c) = Con (cos c)
    cos e = Fun "cos" e
    tan (Con c) = Con (tan c)
    tan e = Fun "tan" e
    asin (Con c) = Con (asin c)
    asin e = Fun "asin" e
    acos (Con c) = Con (acos c)
    acos e = Fun "acos" e
    atan (Con c) = Con (atan c)
    atan e = Fun "atan" e
    sinh (Con c) = Con (sinh c)
    sinh e = Fun "sinh" e
    cosh (Con c) = Con (cosh c)
    cosh e = Fun "cosh" e
    tanh (Con c) = Con (tanh c)
    tanh e = Fun "tanh" e
    asinh (Con c) = Con (asinh c)
    asinh e = Fun "asinh" e
    acosh (Con c) = Con (acosh c)
    acosh e = Fun "acosh" e
    atanh (Con c) = Con (atanh c)
    atanh e = Fun "atanh" e
    (Con c) ** (Con c') = Con (c ** c')
    (Con 0) ** e = (Con 0)
    (Con 1) ** e = (Con 1)
    e ** (Con 0) = (Con 1)
    e ** (Con 1) = e
    e1 ** e2 = Exp e1 e2
    
simplify :: Expr -> Expr
simplify (Var v) = Var v
simplify (Con c) = Con c
simplify (Add e1 e2) = (simplify e1) + (simplify e2)
simplify (Neg e) = (Con 0) - (simplify e)
simplify (Mul e1 e2) = (simplify e1) * (simplify e2)
-- simplify (Inv e) = 1 / (simplify e)
simplify (Div e1 e2) = (simplify e1) / (simplify e2)
simplify (Exp e1 e2) = (simplify e1) ** (simplify e2)
simplify (Fun f e) = fun f (simplify e)

substitute :: (Char, Expr) -> Expr -> Expr
substitute (x,e) (Var v) = if v == x then e else Var v
substitute _ (Con c) = Con c
substitute s (Add e1 e2) = Add (substitute s e1) (substitute s e2)
substitute s (Neg e) = Neg (substitute s e)
substitute s (Mul e1 e2) = Mul (substitute s e1) (substitute s e2)
-- substitute s (Inv e) = Inv (substitute s e)
substitute s (Div e1 e2) = Div (substitute s e1) (substitute s e2)
substitute s (Exp e1 e2) = Exp (substitute s e1) (substitute s e2)
substitute s (Fun f e) = Fun f (substitute s e)

evaluate :: (Char, Expr) -> Expr -> Expr
evaluate s e = simplify (substitute s e)

diff :: Char -> Expr -> Expr
diff x (Var v) = Con 1
diff x (Con c) = Con 0
diff x (Add e1 e2) = (diff x e1) + (diff x e2)
diff x (Neg e) = Neg (diff x e)
diff x (Mul e1 e2) = (e1 * (diff x e2)) + (e2 * (diff x e1))
-- diff x (Inv e) = (negate (diff x e)) / (e ** 2)
diff x (Div e1 e2) = ((e2 * (diff x e1)) - (e1 * (diff x e2))) / (e2 ** 2)
diff x (Exp e1 e2) = ((e1 ** (e2 - (Con 1))) * (diff x e1) * e2) + ((e1 ** e2) * (diff x e2) * (log e1))
diff x (Fun f e) = (diff x e) * (dfun f e) where
    dfun :: String -> Expr -> Expr
    dfun "exp" t = exp t
    dfun "log" t = 1 / t
    dfun "sin" t = cos t
    dfun "cos" t = - (sin t)
    dfun "tan" t = (sec t) ** 2
    dfun "asin" t = 1 / (sqrt (1 - t ** 2))
    dfun "acos" t = - (dfun "asin" t)
    dfun "atan" t = 1 / (1 + t ** 2)
    dfun "sinh" t = cosh t
    dfun "cosh" t = sinh t
    dfun "tanh" t = (sech t) ** 2
    dfun "asinh" t = 1 / (sqrt (1 + t ** 2))
    dfun "acosh" t = (1 / (sqrt (t - 1))) * (1 / (sqrt (t + 1)))
    dfun "atanh" t = 1 / (1 - t ** 2)
    dfun "sec" t = (tan t) * (sec t)
    dfun "csc" t = - (cot t) * (csc t)
    dfun "cot" t = - (csc t) ** 2
    dfun "sech" t = - (tanh t) * (sech t)
    dfun "csch" t = - (coth t) * (csch t)
    dfun "coth" t = - (csch t) ** 2
    dfun f t = Fun (f ++ "'") t

fun :: String -> Expr -> Expr
fun "exp" = exp
fun "log" = log
fun "sin" = sin
fun "cos" = cos
fun "tan" = tan
fun "asin" = asin
fun "acos" = acos
fun "atan" = atan
fun "sinh" = sinh
fun "cosh" = cosh
fun "tanh" = tanh
fun "asinh" = asinh
fun "acosh" = acosh
fun "atanh" = atanh
fun "sec" = sec
fun "csc" = csc
fun "cot" = cot
fun "sech" = sech
fun "csch" = csch
fun "coth" = coth
fun f = Fun f

apply :: (a -> a) -> Int -> a -> a
apply f 0 x = x
apply f n x = apply f (n-1) (f x)

sec :: Floating a => a -> a
csc :: Floating a => a -> a
cot :: Floating a => a -> a
sech :: Floating a => a -> a
csch :: Floating a => a -> a
coth :: Floating a => a -> a
sec x = 1 / (cos x)
csc x = 1 / (sin x)
cot x = 1 / (tan x)
sech x = 1 / (cosh x)
csch x = 1 / (sinh x)
coth x = 1 / (tanh x)