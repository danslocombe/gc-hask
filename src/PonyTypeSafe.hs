{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module PonyTypeSafe where

import GHC.TypeLits
import Data.Finite
import PonyTypes
import Data.Reflection
import GHC.TypeLits.Witnesses
import Data.Proxy
import Idable
import Unsafe.Coerce
import Data.List
import Utils


newtype Class = Class Symbol
data Env e = Env deriving Show

data ObjectDescr2 (c :: Class) = 
  ObjectDescr2 ActorId ObjectId 
  | Null2
  deriving Show

data Fields (ks :: [(Capability, Class)]) where
  EmpFields :: Fields '[]
  FieldCons :: (ObjectDescr2 c) -> Fields ks -> Fields ('(k, c) ': ks)

deriving instance Show (Fields ks)


data Path2 (act :: Nat) (ks :: [Nat]) where
  SinglePath :: (KnownNat n, KnownNat a) => Path2 a '[n]
  ChainPath :: (KnownNat x) => Path2 a xs -> Path2 a (x ': xs)

type ObjStore = [(ObjectDescr, [ObjectDescr])]

class WriteCap a where
instance WriteCap 'Iso where
instance WriteCap 'Ref where

class ReadCap a where
instance ReadCap 'Iso where
instance ReadCap 'Ref where
instance ReadCap 'Val where

type family Take (n :: Nat) (xs :: [k]) :: [k] where
  Take 0 xs = xs
  Take n '[] = '[] -- error "Tried to take from empty list"
  Take (natSing n) (x ': xs) = Take n xs

type family Lookup (i :: a) (xs :: [(a, b)]) :: b where
  Lookup x ('(x, y) ': zs) = y
  Lookup x ('(y, z) ': zs) = Lookup x zs

type family PlinkSnd (n :: Nat) (xs :: [(a, b)]) :: b where
  PlinkSnd 0 ( '( x, y ) ': xs ) = y
  PlinkSnd (n) ( '( x, y ) ': xs ) = PlinkSnd (n - 1) xs

type family Plink (n :: Nat) (xs :: [a]) :: a where
  Plink 0 ( x ': xs ) = x
  Plink (n) ( x ': xs ) = Plink (n - 1) xs

class NonEmpty a
instance NonEmpty (x ': xs)

class AtLeastTwo a
instance AtLeastTwo (x ': (y ': xs))

instance AtLeastTwo a => NonEmpty a

class WriteCapN (n :: Nat) (xs :: [ks]) where
instance (WriteCap x) => WriteCapN 0 (x ': xs)
instance (WriteCapN (n - 1) xs) => WriteCapN n (x ': xs)

instance (WriteCap x) => WriteCapN 0 ( '(x, y) ': xs)
instance (WriteCapN (n - 1) xs) => WriteCapN n ( '(x, y) ': xs)

class ReadCapN (n :: Nat) (xs :: [ks])
instance (ReadCap x) => ReadCapN 0 (x ': xs)
instance (ReadCapN (n - 1) xs) => ReadCapN n (x ': xs)

instance (ReadCap x) => ReadCapN 0 ( '(x, y) ': xs)
instance (ReadCapN (n - 1) xs) => ReadCapN n ( '(x, y) ': xs)

class ReadPath (p :: [Nat]) 
               (s :: [(Capability, Class)]) 
               (cs :: [(Class,[(Capability, Class)])])

instance ReadPath '[] s cs
instance (ReadCapN n s
         , ReadPath ns (Lookup (PlinkSnd n s) cs) cs) 
         => ReadPath (n ': ns) s cs


class WritePath (p :: [Nat])
                 (s :: [(Capability, Class)]) 
                 (cs :: [(Class,[(Capability, Class)])])
instance WritePath '[] s cs
instance (WriteCapN n s
         , WritePath ns (Lookup (PlinkSnd n s) cs) cs) 
         => WritePath (n ': ns) s cs
    
type family PathEndClass (p :: [Nat])
                         (s :: [(Capability, Class)]) 
                         (cs :: [(Class,[(Capability, Class)])])
                         :: Class where
  PathEndClass '[n] s _ = PlinkSnd n s
  PathEndClass (n ': ns) s cs = PathEndClass (ns) (Lookup (PlinkSnd n s) cs) cs

type family SubListElem (n :: Nat) (x :: a) (k :: [a]) :: [a] where
  SubListElem 0 x (_ ': xs) = x ': xs
  SubListElem n x (_ ': xs) = x ': (SubListElem (n - 1) x xs)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


readUnsafe :: Int -> Fields xs -> ObjectDescr2 c
readUnsafe 0 (x `FieldCons` xs) = unsafeCoerce x
readUnsafe n (_ `FieldCons` xs) = unsafeCoerce $ readUnsafe (n-1) xs

readN :: forall n xs k ks c. 
  (KnownNat n, ReadCapN n xs, NonEmpty xs, c ~ PlinkSnd n xs) => 
    Proxy n -> Fields xs -> ObjectDescr2 c
readN _ fs = readUnsafe nval fs
  where
    nval = fromIntegral $ natVal $ Proxy @n

readPathUnsafe :: ObjStore -> [ObjectDescr] -> [Int] -> Maybe (ObjectDescr2 c)
readPathUnsafe store flatFields [] = Nothing
readPathUnsafe store flatFields [p] = upcastOD <$> flatFields !?! p
readPathUnsafe store flatFields (p:ps) = do
  oDescr <- flatFields !?! p
  ctx <- lookup oDescr store
  readPathUnsafe store ctx ps 

readPathf :: forall aid p s e c.
  ( ToListy p
  , ReadPath (aid ': p) s e
  , KnownNat aid
  , c ~ PathEndClass p s e
  ) => Env e -> ObjStore -> Fields s -> Path2 aid p -> Maybe (ObjectDescr2 c)
readPathf _ store fs p = readPathUnsafe store flatFields flatPath
  where
    flatFields = fieldsToList fs
    flatPath = pathToList p

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


modUnsafe :: Int -> ObjectDescr -> Fields xs -> Fields xs
modUnsafe 0 o (_ `FieldCons` xs) = (upcastOD o) `FieldCons` xs
modUnsafe n o (x `FieldCons` xs) = x `FieldCons` (modUnsafe (n-1) o xs)

modifyN :: forall n xs k ks c. 
  ( KnownNat n
  , WriteCapN n xs
  , NonEmpty xs
  , c ~ PlinkSnd n xs
  ) => Proxy n -> ObjectDescr2 c -> Fields xs -> Fields xs
modifyN _ oDescr fs = modUnsafe nval (weakenOD oDescr) fs
  where
    nval = fromIntegral $ natVal $ Proxy @n

modifyPathUnsafe :: ObjStore -> Int -> [Int] -> ObjectDescr -> Maybe ObjStore
modifyPathUnsafe store cur [] _ = undefined
modifyPathUnsafe store cur [p] newVal = do 
  (oDescr, old) <- store !?! cur
  let new = (oDescr, setByIndex p newVal old)
  return $ setByIndex cur new store
modifyPathUnsafe store cur (p:ps) newVal = do
  (oDescr, fds) <- store !?! cur
  fd <- fds !?! p
  newCur <- idIndex fd store
  modifyPathUnsafe store newCur ps newVal

modifyPathUnsafeFirst :: ObjStore -> [ObjectDescr] -> [Int] -> ObjectDescr -> Maybe ObjStore
modifyPathUnsafeFirst store actor (actorField:path) new = do
  target <- actor !?! actorField
  x <- idIndex target store
  modifyPathUnsafe store x path new

-- Unhappy with this
modifyPathf :: 
  ( ToListy p
  , WritePath (aid ': p) s e
  , p ~ (p1 ': ps)
  , KnownNat p1
  , KnownNat aid
  , c ~ PathEndClass p s e
  ) => Env e -> ObjStore -> Fields s -> Path2 aid p -> ObjectDescr2 c -> Maybe ObjStore
modifyPathf _ store fs p newVal
  = modifyPathUnsafeFirst store flatFields flatPath (weakenOD newVal) 
  where
    flatFields = fieldsToList fs
    flatPath = pathToList p

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

storeA :: ObjStore
storeA = [(ObjectDescr 1 1, [ObjectDescr 1 1])]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

weakenObjStore :: TypedObjStore ks -> ObjStore
weakenObjStore EmpObjStore = []
weakenObjStore (x `ObjStoreCons` xs)
  = (odescr, fds) : weakenObjStore xs
  where
    odescr = ObjectDescr (getOwner2 x) (getObjectId2 x)
    fds = fieldsToList $ getObjFields2 x

-- Can you do this without unsafe coerce?
-- Problem is that ks is dependent :/
upcastObjStore :: ObjStore -> TypedObjStore ks
upcastObjStore [] = unsafeCoerce EmpObjStore
upcastObjStore ((ObjectDescr aid oid, fds) : xs)
  = unsafeCoerce $ o `ObjStoreCons` (upcastObjStore xs)
  where
    o = Object2
      { getOwner2 = aid
      , getObjectId2 = oid
      , getObjFields2 = (upcastFields fds)
      }

weakenOD :: ObjectDescr2 c -> ObjectDescr
weakenOD Null2 = Null
weakenOD (ObjectDescr2 x y) = ObjectDescr x y

upcastOD :: ObjectDescr -> ObjectDescr2 c
upcastOD Null = Null2
upcastOD (ObjectDescr x y) = ObjectDescr2 x y

fieldsToList :: Fields ks -> [ObjectDescr]
fieldsToList EmpFields = []
fieldsToList (o `FieldCons` os) = (weakenOD o) : (fieldsToList os)

-- Also has unsafe coerce
upcastFields :: [ObjectDescr] -> Fields ks
upcastFields [] = unsafeCoerce $ EmpFields
upcastFields (x:xs) = unsafeCoerce $ upcastOD x `FieldCons` (upcastFields xs)

pathToList :: forall a ps. (KnownNat a, ToListy ps) => Path2 a ps -> [Int]
pathToList _ = n : (map fromIntegral $ toListy (Proxy @ps))
  where n = fromIntegral (natVal (Proxy @a))

class ToListy (xs :: [Nat]) where
  toListy :: Proxy xs -> [Integer]

instance ToListy '[] where
  toListy = const []

instance forall x xs. (KnownNat x, ToListy xs) => ToListy (x ': xs) where
  toListy _ = natVal (Proxy @x) : (toListy (Proxy @xs))

data Object2 ks = Object2
  { getOwner2 :: ActorId
  , getObjectId2 :: ObjectId
  , getObjFields2 :: Fields ks
  } deriving Show

data Actor2 ks = Actor2
  { getId2 :: ActorId
  , getActFields2 :: Fields ks
  } deriving Show

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data TypedObjStore (fs :: [[(Capability, Class)]])  where
  EmpObjStore :: TypedObjStore '[]
  ObjStoreCons :: Object2 x -> TypedObjStore xs -> TypedObjStore (x ': xs)

deriving instance Show (TypedObjStore a)

data TypedActStore (fs :: [[(Capability, Class)]])  where
  EmpActStore :: TypedActStore '[]
  ActStoreCons :: Actor2 x -> TypedActStore xs -> TypedActStore (x ': xs)

deriving instance Show (TypedActStore a)

plinkObjs :: forall n a b. (KnownNat n, b ~ Plink n a) => 
             Proxy n -> TypedObjStore a -> Object2 b
plinkObjs p (x `ObjStoreCons` xs) = if (nval == 0)
  then unsafeCoerce x
  else unsafeCoerce $ withNatOp (%-) p (Proxy @1) $ plinkObjs (Proxy @(n - 1)) xs
    where
      nval = fromIntegral $ natVal $ p

plinkActs :: forall n a b. (KnownNat n, b ~ Plink n a) => 
             Proxy n -> TypedActStore a -> Actor2 b
plinkActs p (x `ActStoreCons` xs) = if (nval == 0)
  then unsafeCoerce x
  else unsafeCoerce $ withNatOp (%-) p (Proxy @1) $ plinkActs (Proxy @(n - 1)) xs
    where
      nval = fromIntegral $ natVal $ p

updateObjStore :: forall n a b x. (KnownNat n, b ~ SubListElem n x a) => 
                  Proxy n -> Object2 x -> TypedObjStore a -> TypedObjStore b
updateObjStore p aNew (a `ObjStoreCons` as) = if (nval == 0)
  then unsafeCoerce $ aNew `ObjStoreCons` as
  else unsafeCoerce $ withNatOp (%-) p (Proxy @1) $ updateObjStore (Proxy @(n-1)) aNew as
    where
      nval = fromIntegral $ natVal $ p

updateActStore :: forall n a b x. (KnownNat n, b ~ SubListElem n x a) => 
                  Proxy n -> Actor2 x -> TypedActStore a -> TypedActStore b
updateActStore p aNew (a `ActStoreCons` as) = if (nval == 0)
  then unsafeCoerce $ aNew `ActStoreCons` as
  else unsafeCoerce $ withNatOp (%-) p (Proxy @1) $ updateActStore (Proxy @(n-1)) aNew as
    where
      nval = fromIntegral $ natVal $ p

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
data Config2 e os as = Config2 
  { _getEnv :: Env e
  , getActStore :: TypedActStore as
  , getObjStore :: TypedObjStore os
  } deriving Show

readActFieldConfig ::
  (KnownNat n0
  , xs ~ (Plink n0 as)
  , KnownNat n1
  , ReadCapN n1 xs
  , NonEmpty xs
  , c ~ PlinkSnd n1 xs
  ) => Proxy n0 -> Proxy n1 -> Config2 e os as -> ObjectDescr2 c
readActFieldConfig p0 p1 cfg@Config2{..} = x
  where
    act = plinkActs p0 getActStore
    fds = getActFields2 act
    x = readN p1 fds

readPathConfig :: forall a xs as p e p1 ps c os.
  (KnownNat a
  , xs ~ (Plink a as)
  , NonEmpty xs
  , ToListy p
  , ReadPath (a ': p) xs e
  , KnownNat p1
  , p ~ (p1 ': ps)
  , c ~ PathEndClass p xs e
  ) => Path2 a p -> Config2 e os as -> Maybe (ObjectDescr2 c)
readPathConfig path cfg@Config2{..} = x
  where
    act = plinkActs (Proxy @a) getActStore
    fds = getActFields2 act
    ostore = weakenObjStore $ getObjStore
    env = _getEnv
    x = readPathf env ostore fds path 
-- 
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

modifyActFieldConfig :: forall n0 n1 xs as e os as1 c.
  (KnownNat n0
  , xs ~ (Plink n0 as)
  , KnownNat n1
  , WriteCapN n1 xs
  , NonEmpty xs
  , c ~ PlinkSnd n1 xs
  ) => Proxy n0 -> Proxy n1 -> ObjectDescr2 c -> Config2 e os as -> Config2 e os as

modifyActFieldConfig p0 p1 oDescr cfg@Config2{..} = cfg {getActStore = store'}
  where
    act :: Actor2 xs
    act = plinkActs p0 getActStore

    fds = getActFields2 act
    fds' = modifyN p1 oDescr fds

    act' :: Actor2 xs
    act' = act {getActFields2 = fds'}

    -- We are only updating the value of a field not changing the types
    store' :: TypedActStore as
    store' = unsafeCoerce $ updateActStore p0 act' getActStore

modifyPathConfig ::  forall xs as a p p1 ps e c os.
  (KnownNat a
  , xs ~ (Plink a as)
  , NonEmpty xs
  , WritePath (a ': p) xs e
  , c ~ PathEndClass p xs e
  , p ~ (p1 ': ps)
  , ToListy p
  , KnownNat p1
  ) => Path2 a p -> ObjectDescr2 c -> Config2 e os as -> Maybe (Config2 e os as)
modifyPathConfig path new cfg@Config2{..} = do

  let act = plinkActs (Proxy @a) getActStore
      fds = getActFields2 act

      weakObjStore = weakenObjStore getObjStore

  x <- modifyPathf _getEnv weakObjStore fds path new

  let store' :: TypedObjStore os
      store' = upcastObjStore  x

  return $ cfg {getObjStore = store'}
--

