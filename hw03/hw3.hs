module Golf where

-- The goal here is to be short, hence all the where clauses

-- Hopscotch
skips :: [a] -> [[a]]
skips s = forEach [1..(length s)]
  where forEach l = case l of
          (x:xs) -> everyN x s : forEach xs
          [] -> []
        everyN n xs = case drop (n-1) xs of
          (y:ys) -> y : everyN n ys
          [] -> []

-- Local Maxima
localMaxima :: [Integer] -> [Integer]
localMaxima a = map second $ filter maxima $ zip3 a (drop 1 a) (drop 2 a)
  where maxima (x, y, z) = y > x && y > z
        second (_, y, _) = y

-- Histogram
-- histogram :: [Integer] -> String

occurences :: Integer -> [Integer] -> Int
occurences i xs = length $ filter (\x -> x == i ) xs
