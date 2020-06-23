{-# LANGUAGE FlexibleInstances #-}

-- Exercise 1
fib :: Integer -> Integer
fib i 
  | i <= 0 = 0
  | i == 1 = 1
  | otherwise = fib (i-1) + fib (i-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2
fibs2 :: [Integer]
fibs2 = 0 : 1 : f 0 1
    where f a b = a+b : f b (a+b)
-- How cool kids do it
fibsCool :: [Integer]
fibsCool = 0 : 1 : zipWith (+) fibsCool (tail fibsCool)

-- Exercise 3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons c s) = c : streamToList s

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat c = Cons c (streamRepeat c)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons e s) = Cons (f e) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f s = Cons s $ streamFromSeed f (f s)

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0 4 ...
ruler :: Stream Integer 
ruler = interleaveStreams (streamRepeat 0) (streamMap (+1) ruler)

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x (interleaveStreams ys xs)

-- Exercise 6
x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0

instance Num (Stream Integer) where
    fromInteger n = Cons n $ streamRepeat 0
    negate = streamMap (*(-1))
    (+) = streamZipWith (+)
    (*) (Cons x xs) t@(Cons y ys) = Cons (x*y) $ (streamMap (*x) ys) + xs*t

streamZipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
streamZipWith f (Cons x xs) (Cons y ys) = Cons (f x y ) $ streamZipWith f xs ys

instance Fractional (Stream Integer) where
    (/) a@(Cons x xs) b@(Cons y ys) = Cons (x `div` y) $ streamMap (`div` y) $ xs - (a/b)*ys
