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

module PonyTypes2 where

import GHC.TypeLits
import Data.Finite
import PonyTypes
import Data.Reflection
import GHC.TypeLits.Witnesses
import Data.Proxy
import Idable
import qualified Data.Vector.HFixed.HVec as HV
import qualified Data.Vector.HFixed as HV

--data IFields (ks :: [Nat :-> Capability]) where
  --IEmpFields :: IFields '[]
  --IFieldCons :: (Int , ObjectDescr) -> IFields ks -> IFields ((n ':-> Iso) ': ks)

data Fields (ks :: [Capability]) where
  EmpFields :: Fields '[]
  FieldCons :: (ObjectDescr) -> Fields ks -> Fields (k ': ks)

data Fields2 (ks :: [(Capability, Class)]) where
  EmpFields2 :: Fields2 '[]
  FieldCons2 :: (ObjectDescr) -> Fields2 ks -> Fields2 (k ': ks)
  -- Maybe add class to descr?

deriving instance Show (Fields ks)

fs1 :: Fields '[ 'Iso ]
fs1 = Null `FieldCons` EmpFields

fs2 :: Fields '[ 'Val ]
fs2 = Null `FieldCons` EmpFields

fs3 :: Fields '[ 'Iso, 'Iso ]
fs3 = Null `FieldCons` (Null `FieldCons` EmpFields)

class WriteCap a where
instance WriteCap 'Iso where
instance WriteCap 'Ref where

class ReadCap a where
instance ReadCap 'Iso where
instance ReadCap 'Ref where
instance ReadCap 'Val where

modifyHead :: WriteCap a => ObjectDescr -> Fields (a:as) -> Fields (a:as)
modifyHead x (_ `FieldCons` xs) = x `FieldCons` xs

data Basic a = Basic

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


class WriteCapN (n :: Nat) (xs :: [Capability]) where
instance (WriteCap x) => WriteCapN 0 (x ': xs)
instance (WriteCapN (n - 1) xs) => WriteCapN n (x ': xs)

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

canWrite :: (WriteCapN n xs) => Proxy n -> Fields xs -> ()
canWrite _ _ = () 

modUnsafe :: Int -> ObjectDescr -> Fields xs -> Fields xs
modUnsafe 0 o (_ `FieldCons` xs) = o `FieldCons` xs
modUnsafe n o (x `FieldCons` xs) = x `FieldCons` (modUnsafe (n-1) o xs)

readUnsafe :: Int -> Fields xs -> ObjectDescr
readUnsafe 0 (x `FieldCons` xs) = x
readUnsafe n (_ `FieldCons` xs) = readUnsafe (n-1) xs

readUnsafe2 :: Int -> Fields2 xs -> ObjectDescr
readUnsafe2 0 (x `FieldCons2` xs) = x
readUnsafe2 n (_ `FieldCons2` xs) = readUnsafe2 (n-1) xs

modifyN :: forall n xs k ks. (KnownNat n, WriteCapN n xs, xs ~ (k : ks)) => Proxy n -> ObjectDescr -> Fields xs -> Fields xs
modifyN _ oDescr fs = modUnsafe nval oDescr fs
  where
    nval = fromIntegral $ natVal $ Proxy @n

readN :: forall n xs k ks. (KnownNat n, ReadCapN n xs, xs ~ (k : ks)) => Proxy n -> Fields xs -> ObjectDescr
readN _ fs = readUnsafe nval fs
  where
    nval = fromIntegral $ natVal $ Proxy @n

data Path2 (a :: [Nat]) = Path2

data Path3 (ks :: [Nat]) where
  SinglePath :: Path3 '[n]
  ChainPath :: Path3 xs -> Path3 (x ': xs)

readN2 :: forall n xs k ks.
  (KnownNat n, ReadCapN n xs, xs ~ (k : ks)) =>
    Proxy n -> Fields2 xs -> ObjectDescr
readN2 _ fs = undefined

data Start a = Start
data Env a = Env

e :: Env '[ '( 'Class "A", '[ '( 'Iso, 'Class "A" ) ] )
          , '( 'Class "B", '[] ) ]
e = Env

fields1 :: Fields2 '[ '( 'Iso , 'Class "A") ]
fields1 = (ObjectDescr 1 1) `FieldCons2` EmpFields2

data Hide where
  Hide :: forall a. (Fields2 a) -> Hide

data ObjStore where
  EmpObjStore :: ObjStore
  OSCons :: forall a. (ObjectDescr, Fields2 a) -> ObjStore -> ObjStore

os = (ObjectDescr 1 1, (ObjectDescr 1 1 `FieldCons2` EmpFields2)) `OSCons` EmpObjStore

readPathUnsafe :: forall e s n ps. KnownNat n => 
                  Env e -> ObjStore ->  Fields2 s -> Path3 (n ': ps) -> ObjectDescr
readPathUnsafe _ _ fields SinglePath = readUnsafe2 nval fields
  where
    nval = fromIntegral $ natVal $ Proxy @n

--readPathUnsafe _ fields ChainPath cps = readPathUnsafe _ cps
  --where
    --nval = fromIntegral $ natVal $ Proxy @n
    

readPathf :: (ReadPath p s e, p ~ (p1 ': ps), KnownNat p1) => Env e -> ObjStore -> Fields2 s -> Path3 p -> ObjectDescr
readPathf = readPathUnsafe --foldl readUnsafe  
  where
    --init =

data Object2 (n :: Nat) ks = Object2
  { getOwner2 :: ActorId
  -- getObjectId :: ObjectId
  , getObjFields2 :: Fields ks
  }


type family FormObjs (ks :: [Nat :-> [Capability]]) where
  FormObjs '[] = '[]
  FormObjs ((x ':-> ks) ': xs) = (Object2 x ks) ': (FormObjs xs)

data IdList as where
  EmpIDL :: IdList '[]
  IDLCons :: Id a -> IdTrans a -> IdList as -> IdList (a ': as)
  
data Listy a = Listy

newtype Class = Class Symbol --[(Capability, Class)]
--type Class = Symbol


data Actor2 (n :: Nat) (ks :: [Capability]) = Actor2
  { getActFields2 :: Fields ks
  }
  
data Config2 (ks :: [Nat :-> [Capability]]) where
  Config2 :: forall ks xs. ((FormObjs ks) ~ xs) => Listy xs -> Config2 ks

--data Actor2 (ks :: [Nat :-> [Capability]]) where
--data Actor2 (ks :: [Nat :-> Object2 Nat [Capability]]) where
  --Actor2 :: ((FormObjs ks) ~ xs) => Listy ks -> Actor2 xs
  --{ getObjects :: FormObjs ks }
  
  --Actor2 :: ActorId -> Listy ks -> Actor2 ks
