{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Pony where

import Data.List
import Data.Maybe (fromJust)
import Control.Monad

import Idable
import PonyTypes

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Todo
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Some basic capabilities
-- Orca and App message queues
-- Scheduler
-- Random state / program generation
-- Garbage collection protocol
-- Optimizations
-- Instrumentation (Some writer monad)
-- Move some stuff to type level? Fields actors etc
-- Lenses?

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Rendering to GraphViz
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

o1 = Object 1 [(1, Iso, Null) ] 1 ()
a1 = Actor 1 [(1, Iso,  ObjectDescr 1 1), (2, Iso, Null)] 3 ActorIdle [o1]
o2 = Object 2 [] 2 ()
a2 = Actor 2 [(1, Iso, ObjectDescr 2 2), (2, Iso, Null)] 3 ActorIdle [o2]
cfg1 = Config [a1, a2] 3 3

--
cfg2 = assignActFieldNew () 1 2 cfg1
cfg3 = assignActFieldNew () 2 2 cfg2
cfg4 = fromJust $ reassignPath 1 (Path 2 []) (Path 1 []) 1 cfg3
cfg5 = fromJust $ sendObject 1 1 2 2 cfg4
--

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- Things an actor can do
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- Assign newly created object to an actor's field
assignActFieldNew :: a -> ActorId -> ActFieldId -> Config a -> Config a
assignActFieldNew x aId fId cfg = cfg''
   where
    (oDescr, cfg') = createObject x aId cfg
    as = getActors cfg'
    as' = modifyIdable aId (\a -> updateField fId oDescr a) as
    cfg'' = cfg' {getActors = as'}

-- Assign object at some relative path to a field of an actor
reassignActField :: ActorId -> ActFieldId -> Path -> Config a -> Config a
reassignActField aId targetId path cfg = cfg {getActors = as'}
  where
    as = getActors cfg
    actor = fromJust $ lookupId aId as
    oId = undefined
    as' = modifyIdable aId (\a -> updateField targetId oId a) as

-- Assign object at some relative path to a field of an object at some other path
reassignPath :: ActorId -> Path -> Path -> ObjFieldId -> Config a -> Maybe (Config a)
reassignPath aId targetPath assigneePath assigneeField cfg = do
  assigneeDescr <- lookupPath assigneePath aId cfg
  assignee <- lookupObject assigneeDescr cfg
  targetDescr <- lookupPath targetPath aId cfg
  let as = getActors cfg
      assigneeOwner = getOwner assignee
      cfg' = modifyObjectDeepDescr assigneeDescr 
        (\o -> setObjField assigneeField targetDescr o) cfg
  return cfg'

-- Assign an object from an actor's field to another actor
sendObject :: ActorId -> ActFieldId -> ActorId -> ActFieldId -> Config a -> Maybe (Config a)
sendObject senderId senderField receiverId receiverField cfg@Config{..} = do
  oDescr <- lookupPath (Path senderField []) senderId cfg
  let cfg' = modifyActor senderId (updateField senderField Null) cfg
  return $ modifyActor receiverId (updateField receiverField oDescr) cfg'

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

modifyActor :: ActorId -> (Actor a -> Actor a) -> Config a -> Config a
modifyActor id f cfg@Config{..} = cfg {getActors = as'}
  where
    g x = if getActorId x == id
      then f x
      else x
    as' = map g getActors

modifyObject :: ObjectId -> (Object a -> Object a) -> Actor a -> Actor a
modifyObject id f act@Actor{..} = act {getObjects = os'}
  where
    g x = if getObjectId x == id
      then f x
      else x
    os' = map g getObjects

modifyObjectDeep :: ActorId -> ObjectId -> (Object a -> Object a) -> Config a -> Config a
modifyObjectDeep aid oid f cfg = modifyActor aid (modifyObject oid f) cfg

modifyObjectDeepDescr :: ObjectDescr -> (Object a -> Object a) -> Config a -> Config a
modifyObjectDeepDescr (ObjectDescr aid oid) = modifyObjectDeep aid oid

-- -- -- -- -- --

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
    -- TODO: need to have correct capabilities
    fs' = modifyIdable fId (const (Iso, odescr)) getActFields

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

incId = (+1)

modifyFreshObjectId :: Config a -> (ObjectId, Config a)
modifyFreshObjectId cfg@Config{..} =
  (freshObjectId, cfg {freshObjectId = incId freshObjectId})

setObjField :: ObjFieldId -> ObjectDescr -> Object a -> Object a
setObjField fid odescr obj@Object{..} = obj {getObjFields = fs'}
  where
    fs' = modifyIdable fid (\(k, _) -> (k, odescr)) getObjFields
