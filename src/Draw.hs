{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Draw where

import Data.Graph.Inductive.Graph hiding (Path)
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Example
import Data.GraphViz  hiding (Path)
import qualified Data.GraphViz.Printing as GVP
import Data.GraphViz.Attributes.Complete
import Data.Text.Lazy (pack)
import Data.Maybe (fromJust)
import Data.List

import Idable
import PonyTypes
import PonyCore
import Pony
import Data.Functor.Identity
import System.Process


pongx = zip [0..] pong
pongy = map (\(y, p) -> ("dot/pong/p" ++ show y, p)) pongx
pongz = take 16 pongy
execpong = mapM_ (uncurry drawFile) pongz


drawN :: Int -> [ActorId] -> FilePath -> Config a -> IO ()
drawN n ids fp cfg = mapM_ (uncurry drawFile) z
  where
    cfgsI = zip [0..] (runConfig ids cfg)
    y = map (\(y, p) -> (fp ++ show y, p)) cfgsI
    z = take n y

drawFile :: FilePath -> Config a -> IO ()
drawFile filename cfg = do
  let fileDot = filename ++ ".dot"
      filePs = filename ++ ".png"
      (Identity toWrite) = draw cfg 
  writeFile fileDot (show toWrite)
  callCommand $ "dot -Tpng " ++ fileDot ++ " -o " ++ filePs

draw :: Applicative m => Config a -> m GVP.Doc 
draw cfg = GVP.text . GVP.renderDot . toDot . (defaultVis cfg) . toGraph $ cfg

data NodeId = NIdBot | NIdObj Mutability ObjectId ActorId | NIdAct ActorId | NIdMsg ObjectId ActorId | NIdOrca ObjectId ActorId deriving (Eq, Show)

data DEdgeType = EdgeNormal | EdgeUnconstrain | EdgeInvisConstrain

-- instance Ord NodeId where
--   NIdObj x _ <= y = case y of
--     NIdObj z _ -> x <= z
--     NIdMsg _ _ -> True
--     NIdOrca _ _ -> True
--     NIdAct _ -> True
--     NIdBot -> False
--   NIdMsg x _ <= y = case y of
--     NIdObj _ _ -> False
--     NIdMsg z _ -> x <= z
--     NIdOrca _ _ -> True
--     NIdAct _ -> True
--     NIdBot -> False
--   NIdOrca x _ <= y = case y of
--     NIdObj _ _ -> False
--     NIdMsg _ _ -> False
--     NIdOrca z _ -> x <= z
--     NIdAct _ -> True
--     NIdBot -> False
--   NIdAct x <= y = case y of
--     NIdObj _ _ -> False
--     NIdMsg _ _ -> False
--     NIdOrca _ _ -> True
--     NIdAct z -> x <= z
--     NIdBot -> False
--
--  NIdBot <= _ = False

instance Ord NodeId where
  NIdBot <= _ = True
  _ <= NIdBot = False

  NIdAct x <= y = case y of
    NIdAct z -> x <= z
    _ -> True

  NIdMsg x _ <= y = case y of
    NIdAct _ -> False
    NIdMsg z _ -> x <= z
    _ -> True

  NIdObj _ x _ <= y = case y of
    NIdAct _ -> False
    NIdMsg _ _ -> False
    NIdObj _ z _ -> x <= z
    _ -> True

defaultVis :: (Graph gr) => Config a -> gr NodeId DEdgeType -> DotGraph Node
defaultVis cfg = graphToDot (dotparams cfg)

renderRC :: (ObjectDescr, Int) -> String
renderRC (ObjectDescr _ i, n) = show (intExtract i) ++ " -> " ++ show n

renderRCs xs = concat $ intersperse "\n" (renderRC <$> xs)

renderMessage (App bId (ObjectDescr _ i))
  = "App "
  ++ (show $ intExtract i)
  ++ " b_id "
  ++ (show $ intExtract bId)

renderMessage (Orca (ObjectDescr _ i) x)
  = "Orca "
  ++ (show $ intExtract i)
  ++ ":" ++ show x

renderMessages [] = ""
renderMessages xs = "[" ++ concat (intersperse ", " $ renderMessage <$> xs) ++ "]"

dotparams :: Config a -> GraphvizParams Int NodeId DEdgeType ActorId NodeId
dotparams cfg = Params 
  { isDirected       = True
  , globalAttributes = [GraphAttrs([RankDir FromTop, masterLabel])]
  , clusterBy        = clustBy
  , isDotCluster     = const True
  , clusterID        = Num . Int . intExtract
  , fmtCluster       = clFmt
  , fmtNode          = fmtNode
  , fmtEdge          = fmtEdge
  }
  where
    clustBy (n, l@(NIdObj _ _ x)) = C x $ N (n, l)
    clustBy (n, l@(NIdAct x)) = C x $ N (n, l)
    clustBy (n, l@(NIdMsg _ x)) = C x $ N (n, l)
    clustBy (n, l@(NIdOrca _ x)) = C x $ N (n, l)
    clustBy (n, l@(NIdBot)) = N (n, l)

    masterLabel = toLabel $ unlines [labelRcs]
    labelRcs = case lastAction cfg of
      Nothing -> "Initial State"
      Just x -> show x

    act aId = fromJust $ lookupActor aId cfg
    clusterLabel aId
      = unlines
      [ renderRCs (getRCs $ act aId)
      , renderMessages (getMessageQueue $ act aId)
      ]

    clFmt aId = 
      [GraphAttrs [toLabel $ clusterLabel aId, Rank SourceRank]]

    label s x = Label $ StrLabel $ pack $ (s ++ show (intExtract x))
    --labelClear s = Label $ StrLabel $ pack $ s

    fmtNode (n, l@(NIdObj Mutable x _))
      = [ label "Obj: " x
        ]
    fmtNode (n, l@(NIdObj Imm x _))
      = [ label "ImmObj: " x
        ]
    fmtNode (n, l@(NIdAct x)) 
      = [ Shape DiamondShape
        , label "Actor: " x
        ]
    fmtNode (n, l@(NIdMsg x _))
      = [ Shape BoxShape
        --, label "App " x
        , toLabel "App"
        ]
    fmtNode (n, l@(NIdOrca x _))
      = [ Shape BoxShape
        --, label "Orca: " x
        , toLabel "Orca"
        ]
    fmtNode (n, l@(NIdBot)) = [Style [SItem Invisible []]]
    fmtEdge (0, _, _)  = [Style [SItem Invisible []]]
    fmtEdge (_, _, EdgeUnconstrain)  = [Constraint False]
    fmtEdge (_, _, EdgeInvisConstrain)  = [Style [SItem Invisible []]]
    fmtEdge (_, _, _)  = []

toGraph :: Config a -> Gr NodeId DEdgeType
toGraph Config{..} = mkGraph nodes (arcs)
  where
    actCount = length getActors
    messageCount = sum (length . getMessageQueue <$> getActors)

    actIdStart = 0
    messageIdStart = actIdStart + actCount
    objIdStart = messageIdStart + messageCount

    msgs = concatMap (\a -> 
        map (\x -> (x, getActorId a)) (getMessageQueue a)
      ) getActors
    objs = concatMap (\a -> getObjects a) getActors

    -- Nodes
    nodeAct
      = map (\a -> (ActorId actIdStart + getActorId a, 
                    NIdAct (getActorId a))
        ) getActors

    filterNode (i, (App _ _, a)) 
      = (messageIdStart + i, NIdMsg (ObjectId i) a)
    filterNode (i, (Orca _ _, a)) 
      = (messageIdStart + i, NIdOrca (ObjectId i) a)

    nodeMessages = map filterNode (zip [1..] msgs)

    nodeObj
      = map (\o -> (ObjectId objIdStart + getObjectId o,
                    NIdObj (getMutability o) (getObjectId o) (getOwner o))
      ) objs

    g :: (IntExtractable a) => (a, b) -> (Int, b)
    g = (\(x, y) -> (intExtract x, y))
    nodes = [(0, NIdBot)] ++ map g nodeObj ++ nodeMessages ++ map g nodeAct

    -- Arcs Obj
    f0 o (_, _, ObjectDescr _ x) 
      = [( ObjectId objIdStart + getObjectId o
         , ObjectId objIdStart + x
         , EdgeNormal)]
    f0 _ _ = []
    arcsObj = concatMap (\o -> concatMap (f0 o) (getObjFields o)) objs
    -- Arcs Actor
    f1 a (_, _, ObjectDescr _ x) 
      = [(ActorId actIdStart + (getActorId a)
         , ObjectId objIdStart + x
         , EdgeUnconstrain)]
    f1 _ _ = []
    arcsAct = concatMap (\a -> concatMap (f1 a) (getActFields a)) getActors

    -- Arcs Message
    fmsg (i, (App _ (ObjectDescr _ oId), act)) 
      = [(messageIdStart + i, intExtract (ObjectId objIdStart + oId), EdgeNormal),
         (intExtract (ActorId actIdStart + act), messageIdStart + i, EdgeNormal)]
    fmsg (i, (Orca (ObjectDescr _ oId) _, act)) 
      = [(messageIdStart + i, intExtract (ObjectId objIdStart + oId), EdgeNormal),
         (intExtract (ActorId actIdStart + act), messageIdStart + i, EdgeNormal)]
    arcsMsg = concatMap fmsg (zip [1..] msgs)

    h :: (IntExtractable a, IntExtractable b) => (a, b, c) -> (Int, Int, c)
    h = (\(x, y, z) -> (intExtract x, intExtract y, z))

    arcsBot = map (\a -> (ActorId 0, getActorId a, EdgeInvisConstrain)) getActors
    arcsBot2 = [] :: [(Int, Int, DEdgeType)] -- map (\o -> (ActorId 0, getObjectId o)) objs
    --arcsBot2 = map (\o -> (ActorId 0, getObjectId o)) objs
    arcsActMult = [(x, y, EdgeInvisConstrain) | x <- getActorId <$> getActors, y <- getActorId <$> getActors, x < y]

    arcs 
      =  map h arcsBot 
      ++ map h arcsBot2
      ++ map h arcsActMult
      ++ map h arcsObj
      ++ map h arcsAct
      ++ map h arcsMsg

    --arcs = map h arcsBot ++ map h arcsBot2 ++ map h arcsObj ++ map h arcsAct ++ map h arcsMsg

