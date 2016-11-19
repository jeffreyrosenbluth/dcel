{-# LANGUAGE InstanceSigs    #-}
{-# LANGUAGE StrictData      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module DCEL where

import           Control.Lens
import           Data.Vector  (Vector, (!?))
import qualified Data.Vector  as V

type EdgePointer   = Int
type VertexPointer = Int
type FacePointer   = Int

type family Coords a :: *

class Vertex a where
  coords    :: a -> VertexPointer -> Maybe (Coords a)
  incident  :: a -> VertexPointer -> Maybe (EdgePointer)
  newVertex :: a -> VertexPointer

class Face a where
  faceEdge :: a -> FacePointer -> Maybe EdgePointer
  newFaces :: a -> (FacePointer, FacePointer)

class HalfEdge a where
  source  :: a -> EdgePointer -> Maybe VertexPointer
  face    :: a -> EdgePointer -> Maybe FacePointer
  twin    :: a -> EdgePointer -> Maybe EdgePointer
  next    :: a -> EdgePointer -> Maybe EdgePointer
  prev    :: a -> EdgePointer -> Maybe EdgePointer
  newEdge :: a -> (EdgePointer, EdgePointer)

class (Vertex a, Face a, HalfEdge a) => DCEL a where
  -- | addVertex dcel p h.
  --   Add a new vertex v and a new edge (h1, h2) connecting u where u is
  --   source(h) if it exists in the dcel. If g is next(h)
  --   then set g's prev pointer to a new half edge h2 originating
  --   at v. Set the next pointer of the twin of h to h1.
  --   In other words when following the path starting with twin(h) we next
  --   go to v and then back to u.
  addVertex :: a -> Coords a -> EdgePointer -> Maybe a
  splitFace :: a -> VertexPointer -> EdgePointer -> Maybe a

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

newtype Face2D = Face2D {_fEdge :: EdgePointer } deriving (Eq)
makeLenses ''Face2D

instance Show Face2D where
  show (Face2D f)   = "f=" ++ show f

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
  faceEdge dcel n = dcel ^? faces . ix n . fEdge
  newFaces dcel = let n = V.length $ dcel ^. faces
                  in  (n, n+1)

instance HalfEdge VectorDCEL where
  source dcel n = dcel ^? hEdges . ix n . hSource
  face dcel n   = dcel ^? hEdges . ix n . hFace
  twin dcel n   = dcel ^? hEdges . ix n . hTwin
  next dcel n   = dcel ^? hEdges . ix n >>= _hNext
  prev dcel n   = dcel ^? hEdges . ix n >>= _hPrev
  newEdge dcel  = let n = V.length $ dcel ^. hEdges
                  in  (n, n + 1)

instance DCEL VectorDCEL where
  addVertex :: VectorDCEL -> Point -> EdgePointer -> Maybe VectorDCEL
  addVertex dcel point hRef = do
    -- Get the pointer to the vertex u that the new edge will originate from.
    uRef <- twin dcel hRef
    f    <- face dcel hRef
    h    <- dcel ^? hEdges . ix hRef
        -- Create a new pointer to the new vertex.
    let vRef = newVertex dcel
        -- Create new pointers, their associated half edges and the new
        -- vertex.
        (h1Ref, h2Ref) = newEdge dcel
        n = next dcel hRef
        v = Vertex2D point h2Ref
        h1 = HalfEdge2D uRef f h2Ref (Just h2Ref) (Just hRef)
        h2 = HalfEdge2D vRef f h1Ref n (Just h1Ref)
        -- Set the next pointer of hEdge to point to h1.
        h' = h & hNext .~ Just h1Ref
        edges = (dcel ^. hEdges) & ix hRef .~ h'
        -- Set the prev pointer of (next hEdge) to h2.
        edges' = case n of
                   Nothing  -> edges
                   Just i   -> edges & ix i . hPrev .~ Just h2Ref
        edges'' = edges' `V.snoc` h1 `V.snoc` h2
    return $ dcel & vertices %~ flip V.snoc v
                  & hEdges .~ edges''

  splitFace :: VectorDCEL -> VertexPointer -> EdgePointer -> Maybe VectorDCEL
  splitFace dcel vRef hRef = do
    uRef <- twin dcel hRef
    let (f1Ref, f2Ref) = newFaces dcel
        (h1Ref, h2Ref) = newEdge dcel
        n = next dcel hRef
        f1 = Face2D f1Ref
        f2 = Face2D f2Ref
        -- h1 = HalfEdge2D uRef _ h2Ref _ hRef
        -- h2 = HalfEdge2D vRef _ h1Ref n _
    return dcel
