import Control.Applicative

(*>>) :: Applicative f => f a -> f b -> f b
(*>>) _ x = x

mapA' :: Applicative f => (a -> f b) -> ([a] -> f [b])
mapA' = undefined
