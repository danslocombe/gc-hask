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
{-# LANGUAGE FlexibleInstances        #-}

module PonyTypes where

import Data.List (intersperse)

import GHC.TypeLits
import Idable
import Data.Proxy
import Data.Graph.Inductive.Graph hiding (Path)
import Data.Graph.Inductive.PatriciaTree
import Test.QuickCheck
import Control.Monad

newtype ActorId = ActorId Int deriving (Eq, Ord, Show, Num)
newtype ObjectId = ObjectId Int deriving (Eq, Ord, Show, Num)
newtype ActFieldId = ActFieldId Int deriving (Eq, Ord, Show, Num)
newtype ObjFieldId = ObjFieldId Int deriving (Eq, Ord, Show, Num)

data ObjectDescr = Null | ObjectDescr ActorId ObjectId deriving (Eq, Show)

data Path = Path ActFieldId [ObjFieldId] deriving (Eq, Show)

data ActorState = 
  ActorCollect | ActorSend | ActorRec | ActorExec | ActorIdle
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
  --, getRcs :: ObjectDescr -> Maybe Int
  , getRCs :: [(ObjectDescr,Int)]
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
  deriving (Show)

data Action = ActionGC ActorId
            | ActionReceive ActorId
            | ActionSetState ActorId ActorState
            | ActionExecBeh ActorId

instance Show Action where
  show (ActionGC i)
    = "Actor " ++ (show $ intExtract i) ++ " Garbage Collection"
  show (ActionReceive i)
    = "Actor " ++ (show $ intExtract i) ++ " Received"
  show (ActionSetState i x)
    = "Actor " ++ (show $ intExtract i) ++ " set to " ++ show x
  show (ActionExecBeh i)
    = "Actor " ++ (show $ intExtract i) ++ " Executed Behaviour"

type RecFun a = ActorId -> Message -> ConfigMorph a
type GCFun a = ActorId -> ConfigMorph a
type SendFun a = ActorId -> ActFieldId -> BehaviourId -> ActorId -> ConfigMorph a

type ConfigMorph a = Config a -> Maybe (Config a)


data Config a = Config 
  { getActors :: [Actor a]
  , freshActorId :: ActorId
  , freshObjectId :: ObjectId
  , lastAction :: Maybe Action
  , getRecFun :: RecFun a
  , getGCFun :: GCFun a
  , getSendFun :: SendFun a
  }

instance Idable (Actor a) where
  type Id (Actor a) = ActorId
  type IdTrans (Actor a) = Actor a
  ident = getActorId
  idTrans = id
  idTransRev = const id

instance Idable (Object a) where
  type Id (Object a) = ObjectId
  type IdTrans (Object a) = Object a
  ident = getObjectId
  idTrans = id
  idTransRev = const id


instance Show a => Show (Actor a) where
  show Actor{..} = unlines
    [ "Id = " ++ show getActorId
    , "Fields = " ++ show getActFields
    , "State = " ++ show getActorState
    , "MessageQueue = " ++ show getMessageQueue
    , "RCs = " ++ show getRCs
    ]

deriving instance Show a => Show (Object a)

instance Show a => Show (Config a) where
  show Config{..} = unlines
    [ show getActors ]


instance Arbitrary (Gr Int ()) where
  arbitrary = do
    n <- getSize 
    let actorCount = floor . sqrt . fromIntegral $ n
    nodeActors <- replicateM n (choose (1, actorCount))
    let nodes = zip [1..] nodeActors
      --(\i -> choose (1, actorCount) >>= (\x -> return (i, x))) [1..n]

    arcCountMult <- fromIntegral <$> choose (1 :: Int, 100)
    let arcCount = floor $ arcCountMult * (fromIntegral n) * 0.1

    arcs <- replicateM arcCount $ do {
      x <- choose (1, n);
      y <- choose (1, n);
      return (x, y, ())
    }
    return $ mkGraph nodes arcs
      
instance Arbitrary (Config ()) where
  arbitrary = do
    undefined
    --graph <- arbitrary
    --let nodes = labNodes graph
        --n = noNodes graph
        --edges = labEdges graph
        --objs = map (\(i, a) -> 
          --Object 
          --{ getOwner = ActorId a
          --, getObjFields = [(f, Val, :w
          --]
  

class IntExtractable a where
  intExtract :: a -> Int

instance IntExtractable Int where
  intExtract = id
instance IntExtractable ActorId where
  intExtract (ActorId x) = x
instance IntExtractable ObjectId where
  intExtract (ObjectId x) = x
instance IntExtractable ActFieldId where
  intExtract (ActFieldId x) = x
instance IntExtractable ObjFieldId where
  intExtract (ObjFieldId x) = x
instance IntExtractable BehaviourId where
  intExtract (BehaviourId x) = x

data a :-> b = a :-> b

--data Object2 (ks :: [Nat :-> Capability]) a where
  --Object2 :: {getThing :: a} -> Object2 ks a

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
