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

data Fields (ks :: [(Capability, Class)]) where
  EmpFields :: Fields '[]
  FieldCons :: (ObjectDescr) -> Fields ks -> Fields (k ': ks)
  -- Maybe add class to descr?

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
  PlinkSnd (natSing n) ( '( x, y ) ': xs ) = PlinkSnd n xs


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
  (KnownNat n, ReadCapN n xs, xs ~ (k : ks)) => 
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
    flatPath = pathToList p --undefined -- pathToList p

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


modUnsafe :: Int -> ObjectDescr -> Fields xs -> Fields xs
modUnsafe 0 o (_ `FieldCons` xs) = o `FieldCons` xs
modUnsafe n o (x `FieldCons` xs) = x `FieldCons` (modUnsafe (n-1) o xs)

modifyN :: forall n xs k ks. 
           (KnownNat n, WriteCapN n xs, xs ~ (k : ks)) =>
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

pathA :: Path3 '[0]
pathA = SinglePath

storeA :: ObjStore
storeA = [(ObjectDescr 1 1, [ObjectDescr 1 1])]

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
  -- getObjectId :: ObjectId
  , getObjFields :: Fields ks
  }

--type family FormObjs (ks :: [Nat :-> [Capability]]) where
  --FormObjs '[] = '[]
  --FormObjs ((x ':-> ks) ': xs) = (Object2 x ks) ': (FormObjs xs)

data IdList as where
  EmpIDL :: IdList '[]
  IDLCons :: Id a -> IdTrans a -> IdList as -> IdList (a ': as)
  
data Listy a = Listy

newtype Class = Class Symbol --[(Capability, Class)]

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
