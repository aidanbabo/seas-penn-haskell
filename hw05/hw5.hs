import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add l r) = (eval l) + (eval r)
eval (Mul l r) = (eval l) * (eval r)

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = MinMax (max a b)
  mul (MinMax a) (MinMax b) = MinMax (min a b)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit = Mod7
  add (Mod7 a) (Mod7 b) = Mod7 ((`mod` 7) $ (+) a b)
  mul (Mod7 a) (Mod7 b) = Mod7 ((`mod` 7) $ (*) a b)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
