{-# LANGUAGE RecordWildCards #-}

module Draw where

import Data.Graph.Inductive.Graph hiding (Path)
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Example
import Data.GraphViz  hiding (Path)
import qualified Data.GraphViz.Printing as GVP

import Idable
import PonyTypes
import Pony

draw :: Applicative m => Config a -> m GVP.Doc 
draw = GVP.text . GVP.renderDot . toDot . defaultVis . toGraph

data NodeId = NIdObj ActorId | NIdAct ActorId | NIdMsg ActorId deriving (Eq, Show)

instance Ord NodeId where
  NIdObj x <= y = case y of
    NIdObj z -> x <= z
    NIdMsg _ -> True
    NIdAct _ -> True
  NIdMsg x <= y = case y of
    NIdObj _ -> False
    NIdMsg z -> x <= z
    NIdAct _ -> True
  NIdAct x <= y = case y of
    NIdObj _ -> False
    NIdMsg _ -> False
    NIdAct z -> x <= z

defaultVis :: (Graph gr) => gr NodeId el -> DotGraph Node
defaultVis = graphToDot dotparams

dotparams :: GraphvizParams Int NodeId el ActorId NodeId
dotparams = Params 
  { isDirected       = True
  , globalAttributes = []
  , clusterBy        = clustBy
  , isDotCluster     = const True
  , clusterID        = Num . Int . intExtract
  , fmtCluster       = clFmt
  , fmtNode          = const []
  , fmtEdge          = const []
  }
  where
    clustBy (n, l@(NIdObj x)) = C x $ N (n, l)
    clustBy (n, l@(NIdAct x)) = C x $ N (n, l)
    clustBy (n, l@(NIdMsg x)) = C x $ N (n, l)
    clFmt m = [GraphAttrs [toLabel $ ""]]

toGraph :: Config a -> Gr NodeId ()
toGraph Config{..} = mkGraph nodes (labUEdges arcs)
  where
    objs = concatMap (\a -> getObjects a) getActors

    -- Nodes
    nodeObj = map (\o -> (getObjectId o, NIdObj (getOwner o))) objs
    nodeAct = map (\a -> (-getActorId a, NIdAct (getActorId a))) getActors

    g :: (IntExtractable a) => (a, b) -> (Int, b)
    g = (\(x, y) -> (intExtract x, y))
    nodes = map g nodeObj ++ map g nodeAct

    -- Arcs
    f0 o (_, _, ObjectDescr _ x) = [(getObjectId o, x)]
    f0 _ _ = []
    arcsObj = concatMap (\o -> concatMap (f0 o) (getObjFields o)) objs
    f1 a (_, _, ObjectDescr _ x) = [(-(getActorId a), x)]
    f1 _ _ = []
    arcsAct = concatMap (\a -> concatMap (f1 a) (getActFields a)) getActors
    h :: (IntExtractable a, IntExtractable b) => (a, b) -> (Int, Int)
    h = (\(x, y) -> (intExtract x, intExtract y))
    arcs = map h arcsObj ++ map h arcsAct

