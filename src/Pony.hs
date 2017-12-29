{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE StandaloneDeriving #-}

module Pony where

import Data.List
import Data.Maybe (fromJust)
import Control.Monad

import Idable

newtype ActorId = ActorId Int deriving (Eq, Show, Num)
newtype ObjectId = ObjectId Int deriving (Eq, Show, Num)
newtype ActFieldId = ActFieldId Int deriving (Eq, Show, Num)
newtype ObjFieldId = ObjFieldId Int deriving (Eq, Show, Num)

data ObjectDescr = ObjectDescr ActorId ObjectId deriving (Eq, Show)

incId = (+1)

data Path = Path ActFieldId [ObjFieldId]

data ActorState = 
  ActorCollecting | ActorSending | ActorReceving | ActorBehaviour | ActorIdle
  deriving Show

data Actor a = Actor
  { getActorId :: ActorId
  , getActFields :: [(ActFieldId, ObjectDescr)]
  , freshFieldId :: ActFieldId
  , getActorState :: ActorState
  , getObjects :: [Object a] 
  } deriving (Functor, Foldable)

data Object a = Object 
  { getOwner :: ActorId
  , getObjFields :: [(ObjFieldId, ObjectDescr)]
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

o1 = Object 1 [] 1 ()
a1 = Actor 1 [(1, ObjectDescr 1 1)] 2 ActorIdle [o1]
cfg1 = Config [a1] 2 2

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- Things an actor can do
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

assignActFieldNew :: a -> ActorId -> ActFieldId -> Config a -> Config a
assignActFieldNew x aId fId cfg = cfg''
  where
    --(oId, cfg') = freshObjectId
    --oDescr = ObjectDescr aId oId
    (oDescr, cfg') = createObject x aId cfg
    as = getActors cfg'
    as' = modifyIdable aId (\a -> updateField fId oDescr a) as
    cfg'' = cfg' {getActors = as'}

reassignActField :: ActorId -> ActFieldId -> Path -> Config a -> Config a
reassignActField aId targetId path cfg = cfg {getActors = as'}
  where
   as = getActors cfg
   actor = fromJust $ lookupId aId as
   oId = undefined
   as' = modifyIdable aId (\a -> updateField targetId oId a) as

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

lookupObject :: ObjectDescr -> Config a -> Maybe (Object a)
lookupObject (ObjectDescr aid oid) Config{..} = do
  act <- lookupId aid getActors
  lookupId oid (getObjects act)
    
lookupPath :: Path -> ActorId -> Config a -> Maybe ObjectDescr
lookupPath (Path afd ofds) aid cfg@Config{..} = do
  act <- lookupId aid getActors
  (_, oDescrInit) <- lookupId afd $ getActFields act

  let f objDescr field = do {
    o <- lookupObject objDescr cfg;
    (_, ret) <- lookupId field $ getObjFields o;
    return ret
  }
  foldM f oDescrInit ofds

updateField :: ActFieldId -> ObjectDescr -> Actor a -> Actor a
updateField fId odescr act@Actor{..} = act {getActFields = fs'}
  where
    fs' = modifyIdable fId (const (fId, odescr)) getActFields

createObject :: a -> ActorId -> Config a -> (ObjectDescr, Config a)
createObject x aId cfg
  = (newDescr, cfg'')
  where
    (newId, cfg') = modifyFreshObjectId cfg
    newDescr = ObjectDescr aId newId
    new = Object aId [] newId x
    as' = modifyIdable aId addObjectToActor (getActors cfg')
    cfg'' = cfg' {getActors = as'}
    addObjectToActor a = a {getObjects = new : (getObjects a)}

modifyFreshObjectId :: Config a -> (ObjectId, Config a)
modifyFreshObjectId cfg@Config{..} =
  (freshObjectId, cfg {freshObjectId = incId freshObjectId})
