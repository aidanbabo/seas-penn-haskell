{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee

-- Exercise 1
glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp _ ef) (GL l glf) = GL (e:l) (ef + glf)

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL l1 f1) (GL l2 f2) = GL (l1++l2) (f1+f2)

instance Semigroup GuestList where
    (<>) = mappend

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1 gl2 = case compare gl1 gl2 of 
                    LT -> gl2
                    otherwise -> gl1

-- Exercise 2
-- Data.Tree is actually Foldable...

-- Exercise 3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
