{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TypeFamilyDependencies  #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE Rank2Types        #-}

module PonyTypes where

import Data.List (intersperse)

import GHC.TypeLits
import Idable
import Data.Proxy

newtype ActorId = ActorId Int deriving (Eq, Ord, Show, Num)
newtype ObjectId = ObjectId Int deriving (Eq, Ord, Show, Num)
newtype ActFieldId = ActFieldId Int deriving (Eq, Ord, Show, Num)
newtype ObjFieldId = ObjFieldId Int deriving (Eq, Ord, Show, Num)

data ObjectDescr = Null | ObjectDescr ActorId ObjectId deriving (Eq, Show)

data Path = Path ActFieldId [ObjFieldId] deriving (Eq, Show)

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
  , getRequestQueue :: [Request]
  , getMessageQueue :: [Message]
  , getBehaviour :: BehaviourId -> Behaviour
  } deriving (Functor, Foldable)

data Object a = Object 
  { getOwner :: ActorId
  , getObjFields :: [(ObjFieldId, Capability, ObjectDescr)]
  , getObjectId :: ObjectId
  , getItem :: Maybe a
  } deriving (Functor, Foldable)

newtype BehaviourId = BehaviourId Int deriving (Eq, Show, Num)

data Behaviour = Behaviour ActFieldId [Request]

data Message
  = App BehaviourId ObjectDescr
  | Orca ObjectDescr Int
  deriving (Eq, Show)

data Request
  = AssignFieldNew ActFieldId
  | AssignObjFieldNew Path ObjFieldId
  | AssignField Path ActFieldId
  | AssignObjField Path Path ObjFieldId
  | Send ActFieldId ActorId BehaviourId

data Config a = Config 
  { getActors :: [Actor a]
  , freshActorId :: ActorId
  , freshObjectId :: ObjectId
  }

instance Idable (Actor a) where
  type Id (Actor a) = ActorId
  type IdTrans (Actor a) = Actor a
  ident = getActorId
  idTrans = id
  idTransRev = seq

instance Idable (Object a) where
  type Id (Object a) = ObjectId
  type IdTrans (Object a) = Object a
  ident = getObjectId
  idTrans = id
  idTransRev = seq


instance Show a => Show (Actor a) where
  show Actor{..} = f 
    [ "Id = " ++ show getActorId
    , "Fields = " ++ show getActFields
    , "State = " ++ show getActorState
    , "MessageQueue = " ++ show getMessageQueue
    ]
    where f = concat . (intersperse "\n")

deriving instance Show a => Show (Object a)
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
