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
import PonyCore

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
cfg1 = Config [a1, a2] 3 3 Nothing vanillaRec vanillaGC vanillaSend

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

cfgp1 = Config [ap1, ap2] 3 1 Nothing vanillaRec vanillaGC vanillaSend
cfgp2 = fromJust $ assignActFieldNew (Just ()) 1 1 cfgp1
cfgp3 = modifyActor 1 (\a -> a {getActorState = ActorExec, getRequestQueue = [Send 1 2 1]}) cfgp2

pong = runConfig [1,2] cfgp3

behOverwrite :: ActFieldId -> BehaviourId -> Behaviour
behOverwrite fId _ = Behaviour fId [AssignFieldNew fId]

agc1 = basicActor 1 (behNone 2)
agc2 = basicActor 2 (behOverwrite 2)

cfggc1 = Config [agc1, agc2] 3 1 Nothing vanillaRec vanillaGC vanillaSend
cfggc2 = fromJust $ assignActFieldNew (Just ()) 1 1 cfggc1
cfggc3 = fromJust $ assignActPathNew (Just ()) 1 (Path 1 []) 1 cfggc2
cfggc4 = fromJust $ assignActPathNew (Just ()) 1 (Path 1 []) 2 cfggc3
cfggc5 = fromJust $ vanillaSend 1 1 1 2 cfggc4
cfggc6 = fromJust $ assignActFieldNew (Just ()) 2 2 cfggc5

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

behSendOn :: ActFieldId -> ActorId -> BehaviourId -> BehaviourId -> Behaviour
behSendOn fId target tbId _ = Behaviour fId [Send fId target tbId ]

aso1 = basicActor 1 (behNone 2)
aso2 = basicActor 2 (behSendOn 2 3 1)
aso3 = basicActor 3 (behOverwrite 2)

cfgso1 = Config [aso1, aso2, aso3] 4 1 Nothing vanillaRec vanillaGC vanillaSend
cfgso2 = fromJust $ assignActFieldNew (Just ()) 1 1 cfgso1
cfgso3 = fromJust $ assignActPathNew (Just ()) 1 (Path 1 []) 1 cfgso2
cfgso4 = fromJust $ assignActPathNew (Just ()) 1 (Path 1 []) 2 cfgso3
cfgso5 = fromJust $ vanillaSend 1 1 1 2 cfgso4

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

runConfig :: [ActorId] -> Config a -> [Config a]
runConfig aids cfg = scanl ((fromJust .) . (flip execState)) cfg aidInf
  where
    aidInf = concat $ repeat aids

execState :: ActorId -> ConfigMorph a
execState aId cfg@Config{..} = do
  act <- lookupId aId getActors

  let recThenChangeState m ms = (getRecFun aId m) . (modifyActor aId (\a -> a {getMessageQueue = ms}))
        >=> setState aId ActorExec
    

  let (f, action) = case (getActorState act) of {

    ActorIdle -> (case getMessageQueue act of {
      [] -> (getGCFun aId, ActionGC aId);
      m:ms -> (recThenChangeState m ms, ActionReceive aId);
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

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- GC
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

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

-- Execute a single request
-- Take from req queue and run execReq
execOne :: ActorId -> ConfigMorph a
execOne aId cfg@Config{..} = do
  act <- lookupId aId getActors
  case getRequestQueue act of
    [] -> return cfg
    (r:rs) -> let as' = modifyIdable aId (const (act {getRequestQueue = rs})) getActors
              in execReq aId r (cfg {getActors = as'})


execReq :: ActorId -> Request -> ConfigMorph a
execReq aId req cfg@Config{..} = x cfg
  where
    x = case req of
      AssignFieldNew fId -> assignActFieldNew Nothing aId fId
      AssignObjFieldNew p oFid -> undefined
      AssignField p aFid -> reassignActField aId aFid p
      AssignObjField p assignP oFid -> reassignPath aId p assignP oFid

      Send aFid target bId -> getSendFun aId aFid bId target

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- Send an object
-- Update the RCs and then sendObject
vanillaSend :: ActorId -> ActFieldId -> BehaviourId -> ActorId -> ConfigMorph a
vanillaSend aId afId bId target cfg = do
  objDescr <- lookupPath (Path afId []) aId cfg
  act <- lookupId aId (getActors cfg)
  let trace = traceObj objDescr cfg
      (act', orcas) = updateRCSend trace act
      -- Update actor
      cfg0 = modifyActor aId (const act') cfg
      -- Add orcas
      cfg1 = foldl (flip distribOrca) cfg0 orcas
  sendObject aId afId target bId cfg1

-- Receive an object
vanillaRec :: ActorId -> Message -> ConfigMorph a
vanillaRec aId (App bId oDescr) cfg@Config{..} = do
  act <- lookupId aId getActors

  let Behaviour fId rs = getBehaviour act bId
      act' = updateField fId oDescr act
      act'' = act' {getRequestQueue = (getRequestQueue act') ++ rs}
      trace = traceObj oDescr cfg
      act''' = updateRCRec trace act''

  return $ modifyActor aId (const (act''')) cfg

vanillaRec aId (Orca oDescr x) cfg 
  = return $ modifyActor aId (updateRC oDescr x) cfg

vanillaGC :: ActorId -> ConfigMorph a
vanillaGC aId cfg@Config{..} = do
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

-- Send an object
-- Update the RCs and then sendObject
lazyvalSend :: ActorId -> ActFieldId -> BehaviourId -> ActorId -> ConfigMorph a
lazyvalSend aId afId bId target cfg = do
  objDescr <- lookupPath (Path afId []) aId cfg
  act <- lookupId aId (getActors cfg)
  let trace = traceObj objDescr cfg
      (act', orcas) = updateRCSend trace act
      -- Update actor
      cfg0 = modifyActor aId (const act') cfg
      -- Add orcas
      cfg1 = foldl (flip distribOrca) cfg0 orcas
  sendObject aId afId target bId cfg1

-- Receive an object
lazyvalRec :: ActorId -> Message -> ConfigMorph a
lazyvalRec aId (App bId oDescr) cfg@Config{..} = do
  act <- lookupId aId getActors

  let Behaviour fId rs = getBehaviour act bId
      act' = updateField fId oDescr act
      act'' = act' {getRequestQueue = (getRequestQueue act') ++ rs}
      trace = traceObj oDescr cfg
      act''' = updateRCRec trace act''

  return $ modifyActor aId (const (act''')) cfg

lazyvalRec aId (Orca oDescr x) cfg 
  = return $ modifyActor aId (updateRC oDescr x) cfg

lazyvalGC :: ActorId -> ConfigMorph a
lazyvalGC aId cfg@Config{..} = do
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
