{-# LANGUAGE RecordWildCards #-}

module Draw where

import Data.Graph.Inductive.Graph hiding (Path)
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Example
import Data.GraphViz  hiding (Path)
import qualified Data.GraphViz.Printing as GVP
import Data.GraphViz.Attributes.Complete
import Data.Text.Lazy (pack)

import Idable
import PonyTypes
import Pony
import Data.Functor.Identity
import System.Process


pongx = zip [0..] pong
pongy = map (\(y, p) -> ("dot/pong/p" ++ show y, p)) pongx
pongz = take 16 pongy
execpong = mapM_ (uncurry drawFile) pongz

drawFile :: FilePath -> Config a -> IO ()
drawFile filename cfg = do
  let fileDot = filename ++ ".dot"
      filePs = filename ++ ".png"
      (Identity toWrite) = draw cfg 
  writeFile fileDot (show toWrite)
  callCommand $ "dot -Tpng " ++ fileDot ++ " -o " ++ filePs

draw :: Applicative m => Config a -> m GVP.Doc 
draw = GVP.text . GVP.renderDot . toDot . defaultVis . toGraph

data NodeId = NIdBot | NIdObj ActorId | NIdAct ActorId | NIdMsg ActorId deriving (Eq, Show)

instance Ord NodeId where
  NIdObj x <= y = case y of
    NIdObj z -> x <= z
    NIdMsg _ -> True
    NIdAct _ -> True
    NIdBot -> False
  NIdMsg x <= y = case y of
    NIdObj _ -> False
    NIdMsg z -> x <= z
    NIdAct _ -> True
    NIdBot -> False
  NIdAct x <= y = case y of
    NIdObj _ -> False
    NIdMsg _ -> False
    NIdAct z -> x <= z
    NIdBot -> False

  NIdBot <= _ = False

defaultVis :: (Graph gr) => gr NodeId el -> DotGraph Node
defaultVis = graphToDot dotparams

dotparams :: GraphvizParams Int NodeId el ActorId NodeId
dotparams = Params 
  { isDirected       = True
  , globalAttributes = [GraphAttrs([RankDir FromTop])]
  , clusterBy        = clustBy
  , isDotCluster     = const True
  , clusterID        = Num . Int . intExtract
  , fmtCluster       = clFmt
  , fmtNode          = fmtNode
  , fmtEdge          = fmtEdge
  }
  where
    clustBy (n, l@(NIdObj x)) = C x $ N (n, l)
    clustBy (n, l@(NIdAct x)) = C x $ N (n, l)
    clustBy (n, l@(NIdMsg x)) = C x $ N (n, l)
    clustBy (n, l@(NIdBot)) = N (n, l)

    clFmt m = [GraphAttrs [toLabel $ "", Rank SourceRank]]

    label s x = Label $ StrLabel $ pack $ (s ++ show (intExtract x))
    fmtNode (n, l@(NIdObj x))
      = [ label "Obj: " x
        ]
    fmtNode (n, l@(NIdAct x)) 
      = [ Shape DiamondShape
        , label "Actor: " x
        ]
    fmtNode (n, l@(NIdMsg x))
      = [ Shape BoxShape
        , label "App: " x
        ]
    fmtNode (n, l@(NIdBot)) = [Style [SItem Invisible []]]
    fmtEdge (0, _, _)  = [Style [SItem Invisible []]]
    fmtEdge (_, _, _)  = []

toGraph :: Config a -> Gr NodeId ()
toGraph Config{..} = mkGraph nodes (labUEdges arcs)
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
    nodeMessages
      = map (\(i, (_, a)) -> (messageIdStart + i,
                         NIdMsg a)
      ) (zip [1..] msgs)
    nodeObj
      = map (\o -> (ObjectId objIdStart + getObjectId o,
                    NIdObj (getOwner o))
      ) objs

    g :: (IntExtractable a) => (a, b) -> (Int, b)
    g = (\(x, y) -> (intExtract x, y))
    nodes = [(0, NIdBot)] ++ map g nodeObj ++ nodeMessages ++ map g nodeAct

    -- Arcs
    f0 o (_, _, ObjectDescr _ x) = [(ObjectId objIdStart + getObjectId o, ObjectId objIdStart + x)]
    f0 _ _ = []
    arcsObj = concatMap (\o -> concatMap (f0 o) (getObjFields o)) objs
    f1 a (_, _, ObjectDescr _ x) = [(ActorId actIdStart + (getActorId a), ObjectId objIdStart + x)]
    f1 _ _ = []
    arcsAct = concatMap (\a -> concatMap (f1 a) (getActFields a)) getActors

    --arcsMsg0 = map (\((i, (_, owner)) -> ()) (zip [1..] msgs)
    fmsg (i, (App _ (ObjectDescr _ oId), act)) 
      = [(messageIdStart + i, intExtract (ObjectId objIdStart + oId)),
         (messageIdStart + i, intExtract (ActorId actIdStart + act))]
    fmsg _ = []
    arcsMsg = concatMap fmsg (zip [1..] msgs)

    h :: (IntExtractable a, IntExtractable b) => (a, b) -> (Int, Int)
    h = (\(x, y) -> (intExtract x, intExtract y))

    arcsBot = map (\a -> (ActorId 0, getActorId a)) getActors

    arcs = map h arcsBot ++ map h arcsObj ++ map h arcsAct ++ map h arcsMsg

