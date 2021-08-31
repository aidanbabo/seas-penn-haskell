{-# LANGUAGE FlexibleInstances #-}

module JoinList where

import Data.Semigroup
import Data.Monoid

import Sized
import Scrabble
import Editor
import Buffer

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

instance Monoid m => Monoid (JoinList m a) where
    mempty = Empty
    mappend = (+++)

-- Tricky Tricky
instance Monoid m => Semigroup (JoinList m a) where
    (<>) = mappend

-- Exercise 1
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append (tag x <> tag y) x y

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

-- Exercise 2
sz :: (Monoid b, Sized b) => JoinList b a -> Int
sz = getSize . size . tag

-- O(logn)
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ n _ | n < 0 = Nothing
indexJ n x | n > sz x = Nothing
indexJ _ (Single _ x) = Just x
indexJ i (Append m l r)
  | sz l < i = indexJ (i- sz l) r
  | otherwise = indexJ i l

-- O(n)
indexJOn :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJOn i l = jlToList l !!? i

(!!?) :: [a] -> Int -> Maybe a
[]     !!? _= Nothing
_      !!? i | i < 0 = Nothing
(x:xs) !!? 0         = Just x
(x:xs) !!? i         = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty          = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n j | n < 0 = j
dropJ n j | n > sz j = Empty
dropJ n (Append m l r)
  | sz l <= n = dropJ (n - sz l) r
  | otherwise = dropJ n l <> r
dropJ _ _ = Empty

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ _ s@(Single _ _) = s
takeJ n _ | n < 0 = Empty
takeJ n j | n > sz j = j
takeJ n (Append m l r)
  | sz l <= n = l <> (takeJ (n - sz l) r)
  | otherwise = takeJ n l

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

instance Buffer (JoinList (Score, Size) String) where

  toString = unwords . jlToList

  fromString = mconcat . map (\w -> Single (scoreString w, Size 1) w) . lines

  line = indexJ

  replaceLine n s jl = takeJ n jl <> fromString s <> dropJ (n+1) jl

  numLines = sz

  value = getScore . fst . tag

main = runEditor editor joinListBuffer
    where joinListBuffer = fromString $ unlines 
           [ "This buffer is for notes you don't want to save, and for" 
           , "evaluation of steam valve coefficients." 
           , "To load a different file, type the character L followed" 
           , "by the name of the file." 
           ] :: (JoinList (Score, Size) String)

