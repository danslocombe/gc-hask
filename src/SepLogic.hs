{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE RankNTypes  #-}
{-# LANGUAGE OverlappingInstances  #-}

module SepLogic where

import GHC.TypeLits
import Data.Kind (Type)

data a :-> b = a :-> b

data Heap (h :: [Nat :-> Nat]) where
  Emp   :: Heap '[]
  Cell  :: Heap '[h]
  (:*:) :: Disjoint h1 h2 => Heap h1 -> Heap h2 -> Heap (h1 ++ h2)

type family (a :: [k]) ++ (b :: [k]) :: [k] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

class Difference a b ~ a => Disjoint (a :: [k]) (b :: [k])
instance Difference a b ~ a => Disjoint (a :: [k]) (b :: [k])

type family Remove (a :: k) (as :: [k]) :: [k] where
  Remove _ '[]       = '[]
  Remove a (a ': as) = Remove a as
  Remove a (b ': as) = b ': Remove a as

type family Difference (as :: [k]) (bs :: [k]) :: [k] where
  Difference as '[] = as
  Difference as (b ': bs) = Difference (Remove b as) bs

h1 :: Heap '[1 ':-> 10]
h1 = Cell

h2 :: Heap '[2 ':-> 10]
h2 = Cell

h3 = h1 :*: h2

--h3' = h1 :*: h2 :*: h2 -- type error

test :: Heap '[1 ':-> 10, 2 ':-> 20, 3 ':-> 30]
test = Cell :*: Cell :*: Cell

test1 :: Heap '[1 ':-> 10] -> Heap '[2 ':-> 20] -> Heap '[3 ':-> 30]
test1 x y = Cell

test2 :: Heap '[]
test2 = Emp

test3 :: Heap ('[x ':-> y] ++ xs)
test3 = undefined

--gc :: Heap (xs ++ ys) -> (Heap xs, Heap ys)
--gc :: Disjoint xs ys => Heap (xs ++ ys) -> Heap xs
--decompose :: Disjoint xs ys => Heap (xs ++ ys) -> (Heap xs, Heap ys)
--decompose (as :*: bs) = (as, bs)


class Decomposable (xs :: [Nat :-> Nat]) (ys :: [Nat :-> Nat]) (zs :: [Nat :-> Nat]) where
  decompose :: Heap xs -> (Heap ys, Heap zs)

instance Decomposable as '[] as where
  decompose xs = (Emp, xs)

instance Decomposable (x ': as) '[x] as where
  decompose (Cell :*: xs) = (Cell, xs)

instance Decomposable a b c => Decomposable a c b where
  decompose a = (c, b)
    where (b, c) = decompose a


--instance as ~ (bs ++ cs) => Decomposable as bs cs where
  --decompose (xs :*: ys) = (xs, ys)


--class Decomposable2 (xs :: [Nat :-> Nat]) (ys :: [Nat :-> Nat]) (zs :: [Nat :-> Nat]) where
  --decompose2 :: Heap xs -> (Heap ys, Heap zs)

--instance Decomposable1 x y z => Decomposable2 x z y where
  --decompose2 a = (c, b)
    --where (b, c) = decompose a

--instance Decomposable1 x y z => Decomposable2 x y z where
  --decompose2 = decompose


--instance Decomposable '[x, y] '[x] '[y] where
  --decompose (Cell :*: Cell) = (Cell, Cell)


cell1 :: Heap '[1 ':-> 2]
cell1 = Cell

cell2 :: Heap '[2 ':-> 3]
cell2 = Cell

cell3 :: Heap '[3 ':-> 5]
cell3 = Cell

cell4 :: Heap '[4 ':-> 5]
cell4 = Cell

--exdan :: (Heap '[1 ':-> 2], Heap '[2 ':-> 3, 3 ':-> 5])
--exdan :: (Heap '[1 ':-> 2, 2 ':-> 3], Heap '[3 ':-> 5])
exdan :: (Heap '[1 ':-> 2, 4 ':-> 5], Heap '[2 ':-> 3, 3 ':-> 5])
exdan = decompose $ (cell1 :*: cell4 :*: cell2) :*: cell3

--instance Decomposable as bs cs => Decompose (x ': as) (x ': bs) cs where
  --decompose xs =
    --split
  --decompose (xs :*: ys) = (xs, ys)

--instance Disjoint xs ys => Decomposable (xs ++ ys) xs ys where
  --decompose (xs :*: ys) = (xs, ys)

--[] = [] * []
--[x] * [] = [x]
--x:xs * ys = 


--instance Decomposable as bs cs => Decomposable ((b ': bs) ++ cs) (b ': bs) (cs) where
  --decompose (xs :*: ys) = (xs, ys)

--gc :: Heap ('[x ':-> y] ++ xs) -> Heap xs
--gc (a :*: b) = b
