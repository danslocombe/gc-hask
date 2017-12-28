{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Pony where

import Data.List
import Data.Maybe (fromJust)

type ActorId = Int
type ObjectId = Int
type FieldId = Int

data ActorState = ActorCollecting | ActorSending | ActorReceving | ActorBehaviour

data Actor = Actor 
  { getActorId :: ActorId
  , getFields :: [(FieldId, ObjectId)]
  , getActorState :: ActorState
  }

data Object = Object 
  { getOwner :: ActorId
  , getObjectId :: ObjectId
  }

data DiGraph a b = DiGraph [(a, b)] [(a, a)]

data Config = Config [Actor] (DiGraph ObjectId Object)

freshId :: Config -> ObjectId
freshId (Config _ (DiGraph xs _)) = (+1) $ maximum $ map fst xs

updateField :: FieldId -> ObjectId -> Actor -> Actor
updateField fId oId (Actor aId fs state) = Actor aId fs' state
  where
    fs' = modifyIdable fId (const (fId, oId)) fs


createObject :: ActorId -> Config -> (ObjectId, Config)
createObject aId cfg@(Config as (DiGraph os r)) 
  = (newId, Config as (DiGraph ((newId, new):os) r))
  where
    newId = freshId cfg
    new = Object aId newId

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Things an actor can do
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
assignActFieldNew :: ActorId -> FieldId -> Config -> Config
assignActFieldNew aId fId cfg = Config as' g
  where
    (newId, Config as g) = createObject aId cfg
    as' = modifyIdable aId (\a -> updateField fId newId a) as 

assignActField :: ActorId -> FieldId -> FieldId -> Config -> Config
assignActField aId sourceId targetId (Config as g) = Config as' g
  where
    -- Get object Id
    -- Throws if actor and sourceId doesn't exist
    actor = fromJust $ lookupId aId as
    oId = snd $ fromJust $ lookupId sourceId $ getFields actor
    as' = modifyIdable aId (\a -> updateField targetId oId a) as

-- assignObjField :: ActorId -> FieldId -> Config

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

class Eq (Id a) => Idable a where
  type Id a
  ident :: a -> Id a

instance Eq a => Idable (a, b) where
  type Id (a, b) = a
  ident = fst

instance Idable Actor where
  type Id Actor = ActorId
  ident = getActorId

instance Idable Object where
  type Id Object = ObjectId
  ident = getObjectId

deleteIdable :: Idable a => Id a -> [a] -> [a]
deleteIdable x xs = filter (\y -> x /= ident y) xs

lookupId :: Idable a => Id a -> [a] -> Maybe a
lookupId id xs = lookup id $ zip (map ident xs) xs

-- Assume f preserves ident
modifyIdable :: Idable a => Id a -> (a -> a) -> [a] -> [a]
modifyIdable id f xs = xs'
  where
    xs' = case lookupId id xs of
      Just y -> (f y) : deleteIdable id xs
      Nothing -> xs
