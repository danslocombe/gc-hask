{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module Idable where

class Eq (Id a) => Idable a where
  type Id a
  type IdTrans a
  ident :: a -> Id a
  idTrans :: a -> IdTrans a
  idTransRev :: Id a -> IdTrans a -> a

instance forall a b. Eq a => Idable (a, b) where
  type Id (a, b) = a
  type IdTrans (a, b) = b
  ident = fst
  idTrans = snd
  idTransRev = (,)

instance Eq a => Idable (a, b, c) where
  type Id (a, b, c) = a
  type IdTrans (a, b, c) = (b, c)
  ident (x, y, z) = x
  idTrans (x, y, z) = (y, z)
  idTransRev x (y, z) = (x, y, z)

deleteIdable :: Idable a => Id a -> [a] -> [a]
deleteIdable x xs = filter (\y -> x /= ident y) xs

lookupId :: (Foldable t, Idable a) => Id a -> t a -> Maybe (IdTrans a)
lookupId id xs = idTrans <$> foldl f Nothing xs
  where
    f x y = if ident y == id 
      then Just y
      else x

-- Assume f preserves ident
--modifyIdable :: Idable a => Id a -> (IdTrans a -> IdTrans a) -> [a] -> [a]
--modifyIdable id f xs = xs'
  --where
    --xs' = case lookupId id xs of
      --Just y -> (f y) : deleteIdable id xs
      --Nothing -> xs

modifyIdable :: (Functor t, Idable a) => Id a -> (IdTrans a -> IdTrans a) -> t a -> t a
modifyIdable idx f xs = xs'
  where
    g x = if ident x == idx
      then idTransRev idx $ f (idTrans x)
      else x
    xs' = fmap g xs

setIdable :: Idable a => Id a -> IdTrans a -> [a] -> [a]
setIdable idx y xs = (idTransRev idx y) : [ x | x <- xs, ident x /= idx ]

idIndex :: (Idable a) => Id a -> [a] -> Maybe Int
idIndex id xs = idIndex' id (zip [0..] xs)

idIndex' :: (Idable a) => Id a -> [(Int, a)] -> Maybe Int
idIndex' id [] = Nothing
idIndex' id ((i, x):xs) = if ident x == id
  then Just i
  else idIndex' id xs

