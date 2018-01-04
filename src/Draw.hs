{-# LANGUAGE RecordWildCards #-}

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

data NodeId = NIdBot | NIdObj ObjectId ActorId | NIdAct ActorId | NIdMsg ObjectId ActorId | NIdOrca ObjectId ActorId deriving (Eq, Show)

instance Ord NodeId where
  NIdObj x _ <= y = case y of
    NIdObj z _ -> x <= z
    NIdMsg _ _ -> True
    NIdOrca _ _ -> True
    NIdAct _ -> True
    NIdBot -> False
  NIdMsg x _ <= y = case y of
    NIdObj _ _ -> False
    NIdMsg z _ -> x <= z
    NIdOrca _ _ -> True
    NIdAct _ -> True
    NIdBot -> False
  NIdOrca x _ <= y = case y of
    NIdObj _ _ -> False
    NIdMsg _ _ -> False
    NIdOrca z _ -> x <= z
    NIdAct _ -> True
    NIdBot -> False
  NIdAct x <= y = case y of
    NIdObj _ _ -> False
    NIdMsg _ _ -> False
    NIdOrca _ _ -> True
    NIdAct z -> x <= z
    NIdBot -> False

  NIdBot <= _ = False

defaultVis :: (Graph gr) => Config a -> gr NodeId el -> DotGraph Node
defaultVis cfg = graphToDot (dotparams cfg)

renderRC :: (ObjectDescr, Int) -> String
renderRC (ObjectDescr _ i, n) = show (intExtract i) ++ " -> " ++ show n

renderRCs xs = concat $ intersperse "\n" (renderRC <$> xs)

dotparams :: Config a -> GraphvizParams Int NodeId el ActorId NodeId
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
    clustBy (n, l@(NIdObj _ x)) = C x $ N (n, l)
    clustBy (n, l@(NIdAct x)) = C x $ N (n, l)
    clustBy (n, l@(NIdMsg _ x)) = C x $ N (n, l)
    clustBy (n, l@(NIdOrca _ x)) = C x $ N (n, l)
    clustBy (n, l@(NIdBot)) = N (n, l)

    masterLabelText = case lastAction cfg of
      Nothing -> ""
      Just x -> show x
    masterLabel = Label $ StrLabel $ pack masterLabelText

    clFmt aId = 
      [GraphAttrs [toLabel (renderRCs $ getRCs $ fromJust $ lookupActor aId cfg)
      , Rank SourceRank]]

    label s x = Label $ StrLabel $ pack $ (s ++ show (intExtract x))
    fmtNode (n, l@(NIdObj x _))
      = [ label "Obj: " x
        ]
    fmtNode (n, l@(NIdAct x)) 
      = [ Shape DiamondShape
        , label "Actor: " x
        ]
    fmtNode (n, l@(NIdMsg x _))
      = [ Shape BoxShape
        , label "App: " x
        ]
    fmtNode (n, l@(NIdOrca x _))
      = [ Shape BoxShape
        , label "Orca: " x
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

    filterNode (i, (App _ _, a)) 
      = (messageIdStart + i, NIdMsg (ObjectId i) a)
    filterNode (i, (Orca _ _, a)) 
      = (messageIdStart + i, NIdOrca (ObjectId i) a)

    nodeMessages = map filterNode (zip [1..] msgs)

    nodeObj
      = map (\o -> (ObjectId objIdStart + getObjectId o,
                    NIdObj (getObjectId o) (getOwner o))
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
    fmsg (i, (Orca (ObjectDescr _ oId) _, act)) 
      = [(messageIdStart + i, intExtract (ObjectId objIdStart + oId)),
         (messageIdStart + i, intExtract (ActorId actIdStart + act))]
    arcsMsg = concatMap fmsg (zip [1..] msgs)

    h :: (IntExtractable a, IntExtractable b) => (a, b) -> (Int, Int)
    h = (\(x, y) -> (intExtract x, intExtract y))

    arcsBot = map (\a -> (ActorId 0, getActorId a)) getActors

    arcs = map h arcsBot ++ map h arcsObj ++ map h arcsAct ++ map h arcsMsg

