{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Rank2Types #-}

module Pony where

import Data.List
import Data.Maybe (fromJust)
import Control.Monad
import Control.Monad.Trans.Writer.Lazy
import Data.Maybe

import Debug.Trace

import Idable
import PonyTypes

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Todo
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Capabilities
-- Random state / program generation
--     QuickCheck
-- Optimizations
-- Instrumentation (Some writer monad)
-- Move some stuff to type level? Fields actors etc
-- Lenses?

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Rendering to GraphViz
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

--constrBasicBehv :: ActFieldId -> Behaviour
--constrBasicBehv = updateField

beh = undefined
--beh = const $ updateField 1

o1 = Object 1 [(1, Iso, Null) ] 1 (Just ())
o2 = Object 2 [] 2 (Just ())

a1 = Actor 
  { getActorId = 1 
  , getActFields = [(1, Iso, ObjectDescr 1 1), (2, Iso, Null)]
  , freshFieldId = 3
  , getActorState = ActorIdle
  , getObjects = [o1]
  , getRequestQueue = []
  , getMessageQueue = []
  , getRCs = []
  , getBehaviour = beh
  }
a2 = Actor 
  { getActorId = 2 
  , getActFields = [(1, Iso, ObjectDescr 2 2), (2, Iso, Null)]
  , freshFieldId = 3
  , getActorState = ActorIdle
  , getObjects = [o2]
  , getRequestQueue = []
  , getMessageQueue = []
  , getRCs = []
  , getBehaviour = beh
  }
cfg1 = Config [a1, a2] 3 3 Nothing

--
cfg2 = fromJust $ assignActFieldNew (Just ()) 1 2 cfg1
cfg3 = fromJust $ assignActFieldNew (Just ()) 2 2 cfg2
cfg4 = fromJust $ reassignPath 1 (Path 2 []) (Path 1 []) 1 cfg3
cfg5 = fromJust $ sendObject 1 1 2 2 cfg4
--
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

basicActor aId bs = Actor 
  { getActorId = aId 
  , getActFields = [(1, Iso, Null), (2, Iso, Null)]
  , freshFieldId = 3
  , getActorState = ActorIdle
  , getObjects = []
  , getRequestQueue = []
  , getMessageQueue = []
  , getRCs = []
  , getBehaviour = bs
  }

mkBehPong :: ActorId -> BehaviourId -> Behaviour
mkBehPong target _ = Behaviour 1 [Send 1 target 1]

behNone ::  ActFieldId -> BehaviourId -> Behaviour
behNone i _ = Behaviour i []

ap1 = basicActor 1 (mkBehPong 2)
ap2 = basicActor 2 (mkBehPong 1)

cfgp1 = Config [ap1, ap2] 3 1 Nothing
cfgp2 = fromJust $ assignActFieldNew (Just ()) 1 1 cfgp1
cfgp3 = modifyActor 1 (\a -> a {getActorState = ActorExec, getRequestQueue = [Send 1 2 1]}) cfgp2

pong = runConfig [1,2] cfgp3

behOverwrite :: ActFieldId -> BehaviourId -> Behaviour
behOverwrite fId _ = Behaviour fId [AssignFieldNew fId]

agc1 = basicActor 1 (behNone 2)
agc2 = basicActor 2 (behOverwrite 2)

cfggc1 = Config [agc1, agc2] 3 1 Nothing
cfggc2 = fromJust $ assignActFieldNew (Just ()) 1 1 cfggc1
cfggc3 = fromJust $ assignActPathNew (Just ()) 1 (Path 1 []) 1 cfggc2
cfggc4 = fromJust $ assignActPathNew (Just ()) 1 (Path 1 []) 2 cfggc3
cfggc5 = fromJust $ doSend 1 1 1 2 cfggc4
cfggc6 = fromJust $ assignActFieldNew (Just ()) 2 2 cfggc5

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

behSendOn :: ActFieldId -> ActorId -> BehaviourId -> BehaviourId -> Behaviour
behSendOn fId target tbId _ = Behaviour fId [Send fId target tbId ]

aso1 = basicActor 1 (behNone 2)
aso2 = basicActor 2 (behSendOn 2 3 1)
aso3 = basicActor 3 (behOverwrite 2)

cfgso1 = Config [aso1, aso2, aso3] 4 1 Nothing
cfgso2 = fromJust $ assignActFieldNew (Just ()) 1 1 cfgso1
cfgso3 = fromJust $ assignActPathNew (Just ()) 1 (Path 1 []) 1 cfgso2
cfgso4 = fromJust $ assignActPathNew (Just ()) 1 (Path 1 []) 2 cfgso3
cfgso5 = fromJust $ doSend 1 1 1 2 cfgso4

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- GC
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


updateRCSend :: [ObjectDescr] -> Actor a -> (Actor a, [Message])
updateRCSend ws a = foldl f (a, []) ws
  where
    -- Maybe should throw an error?
    f (act, msgs) Null = (act, msgs)

    f (act, msgs) odescr =
      if owner odescr act
        then (updateRC odescr 1 act, msgs)
        else if rcIsOne odescr act
          then (updateRC odescr 255 act, Orca odescr 256 : msgs)
          else (updateRC odescr (-1) act, msgs)

updateRCRec :: [ObjectDescr] -> Actor a -> Actor a
updateRCRec ws a = foldl f a ws
  where
    f act Null = act
    f act odescr = if owner odescr act
      then updateRC odescr (-1) act
      else updateRC odescr 1 act

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

type ConfigMorph a = Config a -> Maybe (Config a)

-- Todo
--scanlM :: Monad m => (a -> b -> m a) -> a -> [b] -> m [a]
--sss :: Monad m => (a -> b -> m a) -> m a -> [b] -> [m a]
--sss f init [] = [init]
--sss f init (x:xs) = init : (sss f (f init x) xs)

runConfig :: [ActorId] -> Config a -> [Config a]
runConfig aids cfg = scanl ((fromJust .) . (flip execState)) cfg aidInf
  where
    aidInf = concat $ repeat aids

execState :: ActorId -> ConfigMorph a
execState aId cfg@Config{..} = do
  act <- lookupId aId getActors
  let (f, action) = case (getActorState act) of {

    ActorIdle -> (case getMessageQueue act of {
      [] -> (doGC aId, ActionGC aId);
      m:ms -> ((doRec aId m) . 
             (modifyActor aId (\a -> a {getMessageQueue = ms}))
             >=> setState aId ActorExec, ActionReceive aId);
    });

    ActorExec -> (case getRequestQueue act of {
      [] -> (setState aId ActorIdle, ActionSetState aId ActorIdle);
      r:rs -> (execOne aId, ActionExecBeh aId);
    }
    );

    -- Probably not needed
    ActorRec -> undefined;
    ActorSend -> undefined;
    ActorCollect -> undefined;
  }

  f cfg {lastAction = Just action}
  --f $ modifyActor aId (\a -> a {getActorState = newState) cfg

-- Execute a single request
-- Take from req queue and run execReq
execOne :: ActorId -> ConfigMorph a
execOne aId cfg@Config{..} = do
  act <- lookupId aId getActors
  case getRequestQueue act of
    [] -> return cfg
    (r:rs) -> let as' = modifyIdable aId (const (act {getRequestQueue = rs})) getActors
              in execReq aId r (cfg {getActors = as'})


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
execReq :: ActorId -> Request -> ConfigMorph a
execReq aId req cfg@Config{..} = x cfg
  where
    x = case req of
      AssignFieldNew fId -> assignActFieldNew Nothing aId fId
      AssignObjFieldNew p oFid -> undefined
      AssignField p aFid -> reassignActField aId aFid p
      AssignObjField p assignP oFid -> reassignPath aId p assignP oFid

      Send aFid target bId -> doSend aId aFid bId target

-- Send an object
-- Update the RCs and then sendObject
doSend :: ActorId -> ActFieldId -> BehaviourId -> ActorId -> ConfigMorph a
doSend aId afId bId target cfg = do
  objDescr <- lookupPath (Path afId []) aId cfg
  act <- lookupId aId (getActors cfg)
  let trace = traceObj objDescr cfg
      (act', orcas) = updateRCSend trace act
      -- Update actor
      cfg0 = modifyActor aId (const act') cfg
      -- Add orcas
      cfg1 = foldl (flip distribOrca) cfg0 orcas
  sendObject aId afId target bId cfg1

distribOrca :: Message -> Config a -> Config a
distribOrca msg@(Orca (ObjectDescr aId oId) x)
  = modifyActor aId (\a -> a {getMessageQueue = (getMessageQueue a) ++ [msg]}) 
  
-- Receive an object
doRec :: ActorId -> Message -> ConfigMorph a
doRec aId (App bId oDescr) cfg@Config{..} = do
  act <- lookupId aId getActors

  let Behaviour fId rs = getBehaviour act bId
      act' = updateField fId oDescr act
      act'' = act' {getRequestQueue = (getRequestQueue act') ++ rs}
      trace = traceObj oDescr cfg
      act''' = updateRCRec trace act''

  return $ modifyActor aId (const (act''')) cfg

doRec aId (Orca oDescr x) cfg 
  = return $ modifyActor aId (updateRC oDescr x) cfg

doGC :: ActorId -> ConfigMorph a
doGC aId cfg@Config{..} = do
  act <- lookupId aId getActors
  let owned = map (\o -> (ObjectDescr aId (getObjectId o))) (getObjects act)
      rced = mapMaybe 
        (\(o, x) -> if x > 0 then Just o else Nothing) 
        (getRCs act)
      unreachable0 = union owned rced

      fieldObjs  = map (\(_, _, x) -> x) (getActFields act)
      locallyReachable = nub $ concat (map (\o -> traceObj o cfg) fieldObjs)
      reachable0 = locallyReachable

      reachable1 = intersect owned rced

      collectable = unreachable0 \\ (union reachable0 reachable1)

      (gcLocal, gcRemote) = partition 
        (\(ObjectDescr aId' _) -> aId == aId') collectable

      gcLocalIds = map (\(ObjectDescr _ i) -> i) gcLocal

      objs' = [x | x <- getObjects act, not $ getObjectId x `elem` gcLocalIds]

      cfg' = modifyActor aId (\a -> a {getObjects = objs'}) cfg

      orcas = [Orca odescr (-rc) | (odescr, rc) <- getRCs act, odescr `elem` gcRemote]

      cfg'' = foldl (flip distribOrca) cfg' orcas

      rcs' = [(x, y) | (x, y) <- getRCs act, not (x `elem` gcRemote)]

      cfg''' = modifyActor aId (\a -> a {getRCs = rcs'}) cfg''

  return cfg'''
    
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

-- Assign an object from an actor's field to another actor
sendObject :: ActorId -> ActFieldId -> ActorId -> BehaviourId -> ConfigMorph a
sendObject senderId senderField receiverId bId cfg@Config{..} = do
  oDescr <- lookupPath (Path senderField []) senderId cfg
  let cfg' = modifyActor senderId (updateField senderField Null) cfg

  let appmsg = App bId oDescr
  let addToQueue msg a = a {getMessageQueue = msg : (getMessageQueue a)}

  return $ modifyActor receiverId (addToQueue appmsg) cfg'

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
