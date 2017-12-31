{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TypeFamilyDependencies  #-}
{-# LANGUAGE KindSignatures        #-}

module PonyTypes where

import GHC.TypeLits
import Idable
import Data.Proxy

newtype ActorId = ActorId Int deriving (Eq, Show, Num)
newtype ObjectId = ObjectId Int deriving (Eq, Show, Num)
newtype ActFieldId = ActFieldId Int deriving (Eq, Show, Num)
newtype ObjFieldId = ObjFieldId Int deriving (Eq, Show, Num)

data ObjectDescr = Null | ObjectDescr ActorId ObjectId deriving (Eq, Show)

data Path = Path ActFieldId [ObjFieldId]

data ActorState = 
  ActorCollecting | ActorSending | ActorReceving | ActorBehaviour | ActorIdle
  deriving Show

data Capability = Val | Iso | Ref | Tag deriving (Eq, Show)

data Actor a = Actor
  { getActorId :: ActorId
  , getActFields :: [(ActFieldId, Capability, ObjectDescr)]
  , freshFieldId :: ActFieldId
  , getActorState :: ActorState
  , getObjects :: [Object a] 
  } deriving (Functor, Foldable)

data Object a = Object 
  { getOwner :: ActorId
  , getObjFields :: [(ObjFieldId, Capability, ObjectDescr)]
  , getObjectId :: ObjectId
  , getItem :: a
  } deriving (Functor, Foldable)

data Config a = Config 
  { getActors :: [Actor a]
  , freshActorId :: ActorId
  , freshObjectId :: ObjectId
  }

instance Idable (Actor a) where
  type Id (Actor a) = ActorId
  ident = getActorId

instance Idable (Object a) where
  type Id (Object a) = ObjectId
  ident = getObjectId

deriving instance Show a => Show (Object a)
deriving instance Show a => Show (Actor a)
deriving instance Show a => Show (Config a)

class IntExtractable a where
  intExtract :: a -> Int

instance IntExtractable ActorId where
  intExtract (ActorId x) = x
instance IntExtractable ObjectId where
  intExtract (ObjectId x) = x
instance IntExtractable ActFieldId where
  intExtract (ActFieldId x) = x
instance IntExtractable ObjFieldId where
  intExtract (ObjFieldId x) = x

data a :-> b = a :-> b

data Object2 (ks :: [Nat :-> Capability]) a where
  Object2 :: {getThing :: a} -> Object2 ks a

data LC (ks :: [Nat]) where
  LC :: LC ks

yy :: LC [2, 3, 4]
yy = LC

type family Len (as :: [k]) :: Nat where
  Len '[] = 0
  Len (x ': xs) = 1 + Len xs

ccc :: forall ks. LC ks -> Proxy (Len ks)
ccc _ = Proxy

eee :: Integer
eee = natVal (Proxy :: Proxy (Len [1,2,3]))

--ddd :: forall ks. (Proxy (ks :: [Nat])) -> Integer
--ddd _ = natVal (Proxy :: Proxy (Len ks))

fromTypes :: KnownNat n => [Proxy n] -> [Integer]
fromTypes = map natVal
