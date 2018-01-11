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

module PonyTypes3 where

import GHC.TypeLits
import Data.Finite
import PonyTypes
import Data.Reflection
import GHC.TypeLits.Witnesses
import Data.Proxy
import Idable
import qualified Data.Vector.HFixed.HVec as HV
import qualified Data.Vector.HFixed as HV
import Unsafe.Coerce

data Fields (ks :: [(Capability, Class)]) where
  EmpFields :: Fields '[]
  FieldCons :: (ObjectDescr) -> Fields ks -> Fields (k ': ks)

deriving instance Show (Fields ks)


data Path3 (ks :: [Nat]) where
  SinglePath :: KnownNat n => Path3 '[n]
  ChainPath :: (KnownNat x) => Path3 xs -> Path3 (x ': xs)

type ObjStore = [(ObjectDescr, [ObjectDescr])]

class WriteCap a where
instance WriteCap 'Iso where
instance WriteCap 'Ref where

class ReadCap a where
instance ReadCap 'Iso where
instance ReadCap 'Ref where
instance ReadCap 'Val where

--modifyHead :: WriteCap a => ObjectDescr -> Fields (a:as) -> Fields (a:as)
--modifyHead x (_ `FieldCons` xs) = x `FieldCons` xs

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
    
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


readUnsafe :: Int -> Fields xs -> ObjectDescr
readUnsafe 0 (x `FieldCons` xs) = x
readUnsafe n (_ `FieldCons` xs) = readUnsafe (n-1) xs

readN :: forall n xs k ks. 
  (KnownNat n, ReadCapN n xs, NonEmpty xs) => 
    Proxy n -> Fields xs -> ObjectDescr
readN _ fs = readUnsafe nval fs
  where
    nval = fromIntegral $ natVal $ Proxy @n

-- Todo replace (!!)
readPathUnsafe :: ObjStore -> [ObjectDescr] -> [Int] -> Maybe ObjectDescr
readPathUnsafe store flatFields [] = undefined
readPathUnsafe store flatFields [p] = return $ flatFields !! p
readPathUnsafe store flatFields (p:ps) = do
  let oDescr = flatFields !! p
  ctx <- lookup oDescr store
  readPathUnsafe store ctx ps 

readPathf :: (ToListy p, ReadPath p s e, p ~ (p1 ': ps), KnownNat p1) 
  => Env e -> ObjStore -> Fields s -> Path3 p -> Maybe ObjectDescr
readPathf _ store fs p = readPathUnsafe store flatFields flatPath
  where
    flatFields = fieldsToList fs
    flatPath = pathToList p

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


modUnsafe :: Int -> ObjectDescr -> Fields xs -> Fields xs
modUnsafe 0 o (_ `FieldCons` xs) = o `FieldCons` xs
modUnsafe n o (x `FieldCons` xs) = x `FieldCons` (modUnsafe (n-1) o xs)

modifyN :: forall n xs k ks. 
           (KnownNat n, WriteCapN n xs, NonEmpty xs) =>
           Proxy n -> ObjectDescr -> Fields xs -> Fields xs
modifyN _ oDescr fs = modUnsafe nval oDescr fs
  where
    nval = fromIntegral $ natVal $ Proxy @n

-- Todo replace (!!)
modifyPathUnsafe :: ObjStore -> [Int] -> ObjectDescr -> ObjStore
modifyPathUnsafe store [] _ = undefined
modifyPathUnsafe store [p] newVal = undefined
modifyPathUnsafe store (p:ps) newVal = undefined

modifyPathf :: (ToListy p, WritePath p s e, p ~ (p1 ': ps), KnownNat p1) 
  => Env e -> ObjStore -> Fields s -> Path3 p -> ObjectDescr -> ObjStore
modifyPathf _ store fs p newVal
  = modifyPathUnsafe store flatPath newVal
  where
    flatFields = fieldsToList fs
    flatPath = pathToList p

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data Path2 (a :: [Nat]) = Path2
data Env e = Env

e :: Env '[ '( 'Class "A", '[ '( 'Iso, 'Class "A" ) ] )
          , '( 'Class "B", '[] ) ]
e = Env

fieldsA :: Fields '[ '( 'Iso , 'Class "A") ]
fieldsA = (ObjectDescr 1 1) `FieldCons` EmpFields

fieldsB :: Fields '[ '( 'Iso , 'Class "A"), '( 'Val, 'Class "B" )]
fieldsB = (ObjectDescr 1 1) `FieldCons` (Null `FieldCons` EmpFields)


pathA :: Path3 '[0]
pathA = SinglePath

pathB :: Path3 '[1]
pathB = SinglePath

pathC :: Path3 '[0, 0]
pathC = ChainPath SinglePath

storeA :: ObjStore
storeA = [(ObjectDescr 1 1, [ObjectDescr 1 1])]

as0 = Actor3 1 fieldsB `ActStoreCons` EmpActStore
os0 = Object3 1 1 fieldsA `ObjStoreCons` (Object3 1 2 EmpFields `ObjStoreCons` EmpObjStore)
cfg0 = Config2 e as0 os0

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

--data ObjStore (as :: [Class]) where
  --EmpObjStore :: ObjStore '[]
  --OSCons :: forall a b xs. (ObjectDescr2 a, Fields3 b) -> ObjStore xs -> ObjStore (a ': xs)

--lookupOS :: (b ~ (Lookup a xs), xs ~ (x1 ': x2)) => Env xs -> ObjStore (y ': ys) -> ObjectDescr2 a -> Maybe (Fields3 b)
--lookupOS _ EmpObjStore _ = Nothing
--lookupOS e ((o, fs) `OSCons` os) x 
  -- = undefined --if o == x then Just fs else undefined --lookupOS e os x

--os = (ObjectDescr2 1 1, (ObjectDescr2 1 1 `FieldCons3` EmpFields3)) `OSCons` EmpObjStore

fieldsToList :: Fields ks -> [ObjectDescr]
fieldsToList EmpFields = []
fieldsToList (o `FieldCons` os) = o : (fieldsToList os)

pathToList :: forall ps. ToListy ps => Path3 ps -> [Int]
pathToList _ = map fromIntegral $ toListy (Proxy @ps)

class ToListy (xs :: [Nat]) where
  toListy :: Proxy xs -> [Integer]

instance ToListy '[] where
  toListy = const []

instance forall x xs. (KnownNat x, ToListy xs) => ToListy (x ': xs) where
  toListy _ = natVal (Proxy @x) : (toListy (Proxy @xs))


data Object2 (n :: Nat) ks = Object2
  { getOwner2 :: ActorId
  --, getObjectId :: ObjectId
  , getObjFields :: Fields ks
  }

data Object3 ks = Object3
  { getOwner3 :: ActorId
  , getObjectId3 :: ObjectId
  , getObjFields3 :: Fields ks
  } deriving Show

data Actor3 ks = Actor3
  { getId3 :: ActorId
  , getActFields3 :: Fields ks
  } deriving Show

type family FormObjs (ks :: [Nat :-> [(Capability, Class)]]) where
  FormObjs '[] = '[]
  FormObjs ((x ':-> ks) ': xs) = (Object2 x ks) ': (FormObjs xs)

--data ObjectCollection where
  --ObjectCollection :: (FormObjs ks
  
--data Config2 (ks :: [Nat :-> [(Capability, Class)]]) where
  --Config2 :: forall ks xs. ((FormObjs ks) ~ xs) => Listy xs -> Config2 ks

data IdList as where
  EmpIDL :: IdList '[]
  IDLCons :: Id a -> IdTrans a -> IdList as -> IdList (a ': as)
  
data Listy a = Listy

newtype Class = Class Symbol --[(Capability, Class)]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data TypedObjStore (fs :: [[(Capability, Class)]])  where
  EmpObjStore :: TypedObjStore '[]
  ObjStoreCons :: Object3 x -> TypedObjStore xs -> TypedObjStore (x ': xs)

deriving instance Show (TypedObjStore a)

data TypedActStore (fs :: [[(Capability, Class)]])  where
  EmpActStore :: TypedActStore '[]
  ActStoreCons :: Actor3 x -> TypedActStore xs -> TypedActStore (x ': xs)

deriving instance Show (TypedActStore a)

plinkObjs :: forall n a b. (KnownNat n, b ~ Plink n a) => 
             Proxy n -> TypedObjStore a -> Object3 b
plinkObjs p (x `ObjStoreCons` xs) = if (nval == 0)
  then unsafeCoerce x
  else unsafeCoerce $ withNatOp (%-) p (Proxy @1) $ plinkObjs (Proxy @(n - 1)) xs
    where
      nval = fromIntegral $ natVal $ p

plinkActs :: forall n a b. (KnownNat n, b ~ Plink n a) => 
             Proxy n -> TypedActStore a -> Actor3 b
plinkActs p (x `ActStoreCons` xs) = if (nval == 0)
  then unsafeCoerce x
  else unsafeCoerce $ withNatOp (%-) p (Proxy @1) $ plinkActs (Proxy @(n - 1)) xs
    where
      nval = fromIntegral $ natVal $ p

updateObjStore :: forall n a b x. (KnownNat n, b ~ SubListElem n x a) => 
                  Proxy n -> Object3 x -> TypedObjStore a -> TypedObjStore b
updateObjStore p aNew (a `ObjStoreCons` as) = if (nval == 0)
  then unsafeCoerce $ aNew `ObjStoreCons` as
  else unsafeCoerce $ withNatOp (%-) p (Proxy @1) $ updateObjStore (Proxy @(n-1)) aNew as
    where
      nval = fromIntegral $ natVal $ p

updateActStore :: forall n a b x. (KnownNat n, b ~ SubListElem n x a) => 
                  Proxy n -> Actor3 x -> TypedActStore a -> TypedActStore b
updateActStore p aNew (a `ActStoreCons` as) = if (nval == 0)
  then unsafeCoerce $ aNew `ActStoreCons` as
  else unsafeCoerce $ withNatOp (%-) p (Proxy @1) $ updateActStore (Proxy @(n-1)) aNew as
    where
      nval = fromIntegral $ natVal $ p

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

type family SubListElem (n :: Nat) (x :: a) (k :: [a]) :: [a] where
  SubListElem 0 x (_ ': xs) = x ': xs
  SubListElem n x (_ ': xs) = x ': (SubListElem (n - 1) x xs)

weakenObjStore :: TypedObjStore ks -> ObjStore
weakenObjStore EmpObjStore = []
weakenObjStore (x `ObjStoreCons` xs)
  = (odescr, fds) : weakenObjStore xs
  where
    odescr = ObjectDescr (getOwner3 x) (getObjectId3 x)
    fds = fieldsToList $ getObjFields3 x

data Config2 e os as = Config2 
  { _getEnv :: Env e
  , getActStore :: TypedActStore as
  , getObjStore :: TypedObjStore os
  }

--modifyPathConfig :: (KnownNat n, Plink n ToListy p, WritePath p s e, p ~ (p1 ': ps), KnownNat p1) => 
                    --Path p -> Proxy n -> Config2 e os as -> Config2 e os as
--modifyPathConfig 

readActFieldConfig ::
  (KnownNat n0
  , xs ~ (Plink n0 as)
  , KnownNat n1
  , ReadCapN n1 xs
  , NonEmpty xs) =>
  Proxy n0 -> Proxy n1 -> Config2 e os as -> ObjectDescr
readActFieldConfig p0 p1 cfg@Config2{..} = x
  where
    act = plinkActs p0 getActStore
    fds = getActFields3 act
    x = readN p1 fds

readPathConfig ::
  (KnownNat n
  , xs ~ (Plink n as)
  , NonEmpty xs
  , ToListy p
  , ReadPath p xs e
  , KnownNat p1
  , p ~ (p1 ': ps))
  => Proxy n -> Config2 e os as -> Path3 p -> Maybe ObjectDescr
readPathConfig p cfg@Config2{..} path = x
  where
    act = plinkActs p getActStore
    fds = getActFields3 act
    ostore = weakenObjStore $ getObjStore
    env = _getEnv
    x = readPathf env ostore fds path 

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

modifyActFieldConfig :: forall n0 n1 xs as e os as1.
  (KnownNat n0
  , xs ~ (Plink n0 as)
  , KnownNat n1
  , WriteCapN n1 xs
  --, as1 ~ SubListElem n0 xs as
  , NonEmpty xs) =>
  Proxy n0 -> Proxy n1 -> ObjectDescr -> Config2 e os as -> Config2 e os as
modifyActFieldConfig p0 p1 oDescr cfg@Config2{..} = cfg {getActStore = store'}
  where
    act :: Actor3 xs
    act = plinkActs p0 getActStore

    fds = getActFields3 act
    fds' = modifyN p1 oDescr fds

    act' :: Actor3 xs
    act' = act {getActFields3 = fds'}

    -- We are only updating the value of a field not changing the types
    store' :: TypedActStore as
    store' = unsafeCoerce $ updateActStore p0 act' getActStore

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

--type Class = Symbol


--data Actor2 (n :: Nat) (ks :: [Capability]) = Actor2
  --{ getActFields :: Fields ks
  --}
  
--data Config2 (ks :: [Nat :-> [Capability]]) where
  --Config2 :: forall ks xs. ((FormObjs ks) ~ xs) => Listy xs -> Config2 ks

--data Actor2 (ks :: [Nat :-> [Capability]]) where
--data Actor2 (ks :: [Nat :-> Object2 Nat [Capability]]) where
  --Actor2 :: ((FormObjs ks) ~ xs) => Listy ks -> Actor2 xs
  --{ getObjects :: FormObjs ks }
  
  --Actor2 :: ActorId -> Listy ks -> Actor2 ks
