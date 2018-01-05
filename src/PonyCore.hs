{-# LANGUAGE RecordWildCards #-}

module PonyCore where

import Control.Monad

import Idable
import PonyTypes

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- TODO add capabilities
traceObj :: ObjectDescr -> Config a -> [ObjectDescr]
traceObj = traceObj' []

traceObj' :: [ObjectDescr] -> ObjectDescr -> Config a -> [ObjectDescr]
traceObj' _ Null _ = []
traceObj' marked oDescr cfg = if elem oDescr marked
  then []
  else ret
  where
    -- Todo error handling here
    obj = case lookupObject oDescr cfg of
      Just y -> y
      _ -> error "HERE"
    thrd (x, y, z) = z
    children = map thrd $ getObjFields obj
    traceChild = \x -> traceObj' (oDescr:marked) x cfg
    ret = oDescr : (concat $ map traceChild children)

rcIsOne :: ObjectDescr -> Actor a -> Bool
rcIsOne odescr Actor{..} = case lookupId odescr getRCs of
  Nothing -> error "Expected RC but wasn't found !"
  Just x | x <= 0 -> error "Expected RC but was zero or less !"
  Just x -> x == 1

rcIsZero :: ObjectDescr -> Actor a -> Bool
rcIsZero odescr Actor{..} = case lookupId odescr getRCs of
  Nothing -> True
  Just x -> x == 0

owner :: ObjectDescr -> Actor a -> Bool
owner Null _ = False
owner (ObjectDescr aId _) Actor{..} = aId == getActorId

updateRC :: ObjectDescr -> Int -> Actor a -> Actor a
updateRC odescr x a@Actor{..} = a {getRCs = rcs'}
  where
    rcs' = case lookupId odescr getRCs of
      Nothing -> if x > 0
        then (odescr, x) : getRCs
        else errorMsg
      Just y -> case x + y of
        0 -> deleteIdable odescr getRCs
        z | z > 0 -> (odescr, z) : deleteIdable odescr getRCs
        otherwise -> errorMsg

    errorMsg = error $ "Tried to reduce rc below 0"
    --errorMsg = error $ "Tried to reduce rc below 0: " ++ show a


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Assign an object from an actor's field to another actor
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
sendObject :: ActorId -> ActFieldId -> ActorId -> BehaviourId -> ConfigMorph a
sendObject senderId senderField receiverId bId cfg@Config{..} = do
  oDescr <- lookupPath (Path senderField []) senderId cfg
  let cfg' = modifyActor senderId (updateField senderField Null) cfg

  let appmsg = App bId oDescr
  let addToQueue msg a = a {getMessageQueue = msg : (getMessageQueue a)}

  return $ modifyActor receiverId (addToQueue appmsg) cfg'

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
--
runBeh :: ActorId -> ObjectDescr -> Behaviour -> ConfigMorph a
runBeh aId x (Behaviour fId rs) cfg@Config{..} = return cfg'
  where
    cfg' = modifyActor aId (f . updateField fId x) cfg
    f a = a {getRequestQueue = (getRequestQueue a) ++ rs}

-- Assign newly created object to an actor's field
assignActFieldNew :: Maybe a -> ActorId -> ActFieldId -> ConfigMorph a
assignActFieldNew x aId fId cfg = return $ cfg''
   where
    (oDescr, cfg') = createObject x aId cfg
    cfg'' = modifyActor aId (updateField fId oDescr) cfg'

-- Assign newly created object to an actor's field
assignActPathNew :: Maybe a -> ActorId -> Path -> ObjFieldId -> ConfigMorph a
assignActPathNew x aId path targetField cfg = cfg''
   where
    (oDescr, cfg') = createObject x aId cfg
    cfg'' = modifyPathField path targetField aId oDescr cfg'

-- Assign object at some relative path to a field of an actor
reassignActField :: ActorId -> ActFieldId -> Path -> ConfigMorph a
reassignActField aId fId path cfg = do 
  oDescr <- lookupPath path aId cfg
  return $ modifyActor aId (updateField fId oDescr) cfg

-- Assign object at some relative path to a field of an object at some other path
reassignPath :: ActorId -> Path -> Path -> ObjFieldId -> ConfigMorph a
reassignPath aId targetPath assigneePath assigneeField cfg = do
  assigneeDescr <- lookupPath assigneePath aId cfg
  assignee <- lookupObject assigneeDescr cfg
  targetDescr <- lookupPath targetPath aId cfg
  let as = getActors cfg
      assigneeOwner = getOwner assignee
      cfg' = modifyObjectDeepDescr assigneeDescr 
        (\o -> setObjField assigneeField targetDescr o) cfg
  return cfg'

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

distribOrca :: Message -> Config a -> Config a
distribOrca msg@(Orca (ObjectDescr aId oId) x)
  = modifyActor aId (\a -> a {getMessageQueue = (getMessageQueue a) ++ [msg]}) 
  

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
lookupActor :: ActorId -> Config a -> Maybe (Actor a)
lookupActor aId Config{..} = do
  lookupId aId getActors

lookupObject :: ObjectDescr -> Config a -> Maybe (Object a)
lookupObject Null _ = Nothing
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

modifyPathField :: Path -> ObjFieldId -> ActorId -> ObjectDescr -> ConfigMorph a
modifyPathField path fId aId oDescr cfg = do
  oDescrMod <- lookupPath path aId cfg
  oMod <- lookupObject oDescrMod cfg
  -- Todo capabilities
  let fds' = setIdable fId (Iso, oDescr) $ getObjFields oMod
      oMod' = oMod {getObjFields = fds'}
  return $ modifyObjectDeepDescr oDescrMod (const oMod') cfg

updateField :: ActFieldId -> ObjectDescr -> Actor a -> Actor a
updateField fId odescr act@Actor{..} = act {getActFields = fs'}
  where
    -- TODO: need to have correct capabilities
    fs' = modifyIdable fId (const (Iso, odescr)) getActFields

createObject :: Maybe a -> ActorId -> Config a -> (ObjectDescr, Config a)
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

setState :: ActorId -> ActorState -> ConfigMorph a
setState aId newState cfg 
  = return $ modifyActor aId (\a -> a {getActorState = newState}) cfg
