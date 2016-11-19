{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE StrictData      #-}
{-# LANGUAGE TemplateHaskell #-}

module DCEL where

import           Control.Lens
import qualified Data.Vector as V
import           Data.Vector (Vector, (!?))

type EdgePointer   = Int
type VertexPointer = Int
type FacePointer   = Int

type family Coords a :: *

class Vertex a where
  coords    :: a -> VertexPointer -> Maybe (Coords a)
  incident  :: a -> VertexPointer -> Maybe (EdgePointer)
  newVertex :: a -> VertexPointer

class Face a where
  inner :: a -> FacePointer -> Maybe EdgePointer
  outer :: a -> FacePointer -> Maybe EdgePointer

class HalfEdge a where
  source  :: a -> EdgePointer -> Maybe VertexPointer
  face    :: a -> EdgePointer -> Maybe FacePointer
  twin    :: a -> EdgePointer -> Maybe EdgePointer
  next    :: a -> EdgePointer -> Maybe EdgePointer
  prev    :: a -> EdgePointer -> Maybe EdgePointer
  newEdge :: a -> (EdgePointer, EdgePointer)

class (Vertex a, Face a, HalfEdge a) => DCEL a where
  addVertex :: a -> Coords a -> EdgePointer -> Maybe a

data Point = Point { _xCoord :: Double, _yCoord :: Double}
  deriving (Eq)
makeLenses ''Point

instance Show Point where
  show (Point x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

data Vertex2D = Vertex2D
  { _vertex :: Point
  , _vEdge  :: EdgePointer
  } deriving (Eq)
makeLenses ''Vertex2D

instance Show Vertex2D where
  show (Vertex2D p e) = show e ++ "-" ++ show p

data Face2D = Face2D
  { _fInner :: Maybe EdgePointer
  , _fOuter :: Maybe EdgePointer
  } deriving (Eq)
makeLenses ''Face2D

instance Show Face2D where
  show (Face2D Nothing Nothing)   = "(_, _)"
  show (Face2D Nothing (Just o))  = "(_, " ++ show o ++ ")"
  show (Face2D (Just i) Nothing)  = "(" ++ show i ++ ", _)"
  show (Face2D (Just i) (Just o)) = "(" ++ show i ++ ", " ++ show o ++ ")"

data HalfEdge2D = HalfEdge2D
  { _hSource :: VertexPointer
  , _hFace   :: FacePointer
  , _hTwin   :: EdgePointer
  , _hNext   :: Maybe EdgePointer
  , _hPrev   :: Maybe EdgePointer
  } deriving (Eq)
makeLenses ''HalfEdge2D

instance Show HalfEdge2D where
  show he = "{s=" ++ show (he ^. hSource)
         ++ ", f=" ++ show (he ^. hFace)
         ++ ", t=" ++ show (he ^. hTwin)
         ++ ", n=" ++ show (he ^. hNext)
         ++ ", p=" ++ show (he ^. hPrev)
         ++ "}"

data VectorDCEL = VectorDCEL
  { _vertices :: Vector Vertex2D
  , _faces    :: Vector Face2D
  , _hEdges   :: Vector HalfEdge2D
  } deriving (Show, Eq)
makeLenses ''VectorDCEL

type instance Coords VectorDCEL = Point

instance Vertex VectorDCEL where
  coords dcel n = dcel ^? vertices . ix n . vertex
  incident dcel n = dcel ^? vertices . ix n . vEdge
  newVertex dcel = V.length $ dcel ^. vertices

instance Face VectorDCEL where
  inner dcel n = dcel ^? faces . ix n >>= _fInner
  outer dcel n = dcel ^? faces . ix n >>= _fOuter

instance HalfEdge VectorDCEL where
  source dcel n = dcel ^? hEdges . ix n . hSource
  face dcel n   = dcel ^? hEdges . ix n . hFace
  twin dcel n   = dcel ^? hEdges . ix n . hTwin
  next dcel n   = dcel ^? hEdges . ix n >>= _hNext
  prev dcel n   = dcel ^? hEdges . ix n >>= _hPrev
  newEdge dcel  = let n = V.length $ dcel ^. hEdges
                  in  (n, n + 1)

instance DCEL VectorDCEL where
  addVertex dcel point hPtr = do
    -- Get the pointer to the vertex u that the new edge will originate from.
    uIdx <- twin dcel hPtr
    f    <- face dcel hPtr
    h    <- dcel ^? hEdges . ix hPtr
        -- Create a new pointer to the new vertex.
    let vIdx = newVertex dcel
        -- Create new pointers to the new half edges.
        (h1Idx, h2Idx) = newEdge dcel
        n = next dcel hPtr
        v = Vertex2D point h2Idx
        h1 = HalfEdge2D uIdx f h2Idx (Just h2Idx) (Just hPtr)
        h2 = HalfEdge2D vIdx f h1Idx n (Just h1Idx)
        -- Set the next pointer of hEdge to point to h1.
        h' = h & hNext .~ Just h1Idx
        -- Set the prev pointer of (next hEdge) to h2.
        edges = (dcel ^. hEdges) & ix hPtr .~ h'
        edges' = case n of
                   Nothing  -> edges
                   Just i   -> edges & ix i . hPrev .~ Just h2Idx
        edges'' = edges' `V.snoc` h1 `V.snoc` h2
    return $ dcel & vertices %~ flip V.snoc v
                  & hEdges .~ edges''
