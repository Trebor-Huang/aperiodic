module Geometry where
import Substitution
import Data.List (elemIndex)
import Data.Maybe (fromJust, mapMaybe)
import Utils (bfs)

-- Used for a subgroup of 2D rotations
class Monoid m => SO2 m where
  invert :: m -> m

class (SO2 rot, Monoid vec) => Vector2 rot vec | vec -> rot where
  -- | rotate r v rotates an object v by r
  rotate :: rot -> vec -> vec

  negV :: vec -> vec

  (+.) :: vec -> vec -> vec
  (+.) = (<>)

  (-.) :: vec -> vec -> vec
  x -. y = x +. negV y
infixl 6 -.

-- data V2 a = V2 a a deriving (Show, Eq, Functor)
-- instance Num a => Semigroup (V2 a) where
--   V2 a1 b1 <> V2 a2 b2 = V2 (a1 + a2) (b1 + b2)

-- instance Num a => Monoid (V2 a) where
--   mempty = V2 1 0

-- | Converts `vec` to Float vectors for drawing
class Draw2D vec where
  toVec :: vec -> (Float, Float)

data Tiles tile edge rot vec = Tiles {
  edges :: tile -> [edge],
  -- Matching edge colors must have `vec` pointing in the opposite direction,
  -- use `rot` to change rotation
  edgeMap :: tile -> edge -> (rot, vec)
}

drawTile :: (Vector2 rot vec, Eq edge)
  => Tiles tile edge rot vec
  -- input tile, edge and the rotation of the neighbor's matching edge
  -- and the position of the start of this edge
  -> tile -> edge -> rot -> vec
  -- outputs the needed rotation, and (edge, position at its endpoint)
  -> (rot, [(edge, vec)])
drawTile tiles t e r1 v = let
  -- List of edges
  es = edges tiles t
  em = edgeMap tiles t
  -- The original vector for the edge e
  r0 = fst (em e)
  ei = fromJust $ elemIndex e es
  -- the rotation we need to apply
  r = r1 <> invert r0
  -- the rotated coordinates
  coords = scanl (+.) mempty $
    map (\ e' -> let (r', u) = em e' in rotate (r <> r') u) es
  -- the amount to move
  dv = v -. coords !! ei
  finalCoords = map (+. dv) coords
  in
    (r, zip (last es : init es) finalCoords)

draw :: (Vector2 rot vec, Draw2D vec, Eq edge, Eq vec)
  => (Signature tile subtile -> edge -> Maybe (Signature tile subtile, edge))
  -> Tiles tile edge rot vec
  -- The "in-bounds" predicate
  -> ((Float, Float) -> Bool)
  -> Signature tile subtile
  -> edge
  -- A list of polygons
  -> [(Signature tile subtile, [(Float, Float)])]
draw adjrec tiles inBounds sig e0 =
  map (\((sig, _, _, _, c), _) -> (sig, c)) $
    bfs
      (\(_, _, _, _, c) -> any inBounds c)
      (\(_, _, _, c, _) -> c)
      (go sig e0 mempty mempty)
      branch
  where
    go sig@(t0 :< _) e0 r v =
      let (r', cs) = drawTile tiles t0 e0 r v
       in (sig, e0, r', cs, map (toVec . snd) cs)

    branch (sig@(t0 :< _), e0, r0, cs, _) =
      mapMaybe
        ( \ed -> do
            -- new signature and the edge it enters from
            (sig', ed') <- adjrec sig ed
            -- now we need to figure out what is at the base of the edge
            pos <- ed `lookup` cs
            return (go sig' ed' (r0 <> fst (edgeMap tiles t0 ed)) pos, ())
        )
        $ filter (/= e0)
        $ edges tiles t0

-- Update things incrementally
data DrawState tile subtile edge vec rot = DrawState {
  drawnTiles :: [[vec]],
  -- tile signature, one of its edge, the position of its base and its rotation
  activeEdges :: [(Signature tile subtile, edge, rot, vec)]
}

-- Whenever an active edge enters the viewport (...??)
