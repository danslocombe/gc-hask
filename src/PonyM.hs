{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

module PonyM where

import Data.List
import Data.Maybe (fromJust)
import Control.Monad

import Idable

newtype ActorId = ActorId Int deriving (Eq, Show, Num)
newtype ObjectId = ObjectId Int deriving (Eq, Show, Num)
newtype ActFieldId = ActFieldId Int deriving (Eq, Show, Num)
newtype ObjFieldId = ObjFieldId Int deriving (Eq, Show, Num)

data ObjectDescr = ObjectDescr ActorId ObjectId

incId = (+1)

data Path = Path ActFieldId [ObjFieldId]

data ActorState = ActorCollecting | ActorSending | ActorReceving | ActorBehaviour

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

--data DiGraph a b = DiGraph [(a, b)] [(a, a)]

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

-- freshId :: Config -> ObjectId
-- freshId (Config _ (DiGraph xs _)) = (+1) $ maximum $ map fst xs
--
 
--updateActField :: ActFieldId -> ObjectId -> Actor -> Actor
--updateActField fId oId (Actor aId fs state) = Actor aId fs' state
  --where
    --fs' = modifyIdable fId (const (fId, oId)) fs
 
-- 
-- createObject :: ActorId -> Config -> (ObjectId, Config)
-- createObject aId cfg@(Config as (DiGraph os r)) 
--   = (newId, Config as (DiGraph ((newId, new):os) r))
--   where
--     newId = freshId cfg
--     new = Object aId newId
-- 
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- Things an actor can do
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- assignActFieldNew :: ActorId -> FieldId -> Config -> Config
-- assignActFieldNew aId fId cfg = Config as' g
--   where
--     (newId, Config as g) = createObject aId cfg
--     as' = modifyIdable aId (\a -> updateField fId newId a) as 
-- 

reassignActField :: ActorId -> ActFieldId -> Path -> Config a -> Config a
reassignActField aId targetId path cfg = cfg {getActors = as'}
  where
   -- Get object Id
   -- Throws if actor and sourceId doesn't exist
   as = getActors cfg
   actor = fromJust $ lookupId aId as
   --oId = snd $ fromJust $ lookupId sourceId $ getFields actor
   oId = undefined
   as' = modifyIdable aId (\a -> updateField targetId oId a) as
   --as' = modifyIdable aId (\a -> updateField targetId oId a) as

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

-- 
-- -- assignObjField :: ActorId -> FieldId -> Config
-- 
