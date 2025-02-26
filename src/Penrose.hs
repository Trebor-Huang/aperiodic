module Penrose where
import Substitution
import Geometry
import Automata
import qualified Data.Set as S
import Kleenex

data PenroseTile = U | V deriving (Show, Eq, Ord, Enum, Bounded)
data PenroseEdge = E0 | E1 | E2 | E3 deriving (Show, Eq, Ord, Enum, Bounded)
data PenroseSubtile = S0 | S1 | S2 deriving (Show, Eq, Ord, Enum, Bounded)

penrose3A :: FSA (Alphabet PenroseTile PenroseSubtile PenroseEdge) (Maybe PenroseTile)
penrose3A = FST {..}
  where
    states = S.fromList [Nothing, Just U, Just V]
    transitions = [
        (Nothing, Begin U E0, (), Just U),
        (Nothing, Begin U E1, (), Just U),
        (Nothing, Begin U E2, (), Just U),
        (Nothing, Begin U E3, (), Just U),
        (Nothing, Begin V E0, (), Just V),
        (Nothing, Begin V E1, (), Just V),
        (Nothing, Begin V E2, (), Just V),
        (Nothing, Begin V E3, (), Just V),
        (Just U, Inflate U S1, (), Just U),
        (Just V, Inflate U S0, (), Just U),
        (Just U, Inflate U S2, (), Just U),
        (Just V, Inflate V S0, (), Just V),
        (Just U, Inflate V S1, (), Just V)
      ]

penrose3 :: SubstSystem PenroseTile PenroseSubtile PenroseEdge Int ()
penrose3 = SubstSystem {..}
  where
    subtile V S0 = V
    subtile V S1 = U
    subtile U S0 = V
    subtile U S1 = U
    subtile U S2 = U
    subtile _ _ = error "Unrecognized subtile"

    substMap _ V = fromPairs [
        (Left (E0, 1), Left (E3, 2)),
        (Left (E0, 0), Right (S0, E3)),
        (Left (E1, 0), Right (S1, E0)),
        (Left (E1, 1), Right (S1, E1)),
        (Left (E2, 2), Right (S1, E2)),
        (Left (E2, 1), Right (S1, E3)),
        (Left (E2, 0), Right (S0, E0)),
        (Left (E3, 0), Right (S0, E1)),
        (Left (E3, 1), Right (S0, E2))
      ]
    substMap _ U = fromPairs [
        (Left (E0, 2), Right (S1, E2)),
        (Left (E0, 1), Right (S1, E3)),
        (Left (E0, 0), Left (E1, 1)),
        (Left (E1, 0), Right (S2, E3)),
        (Left (E2, 0), Right (S2, E0)),
        (Left (E2, 1), Right (S2, E1)),
        (Left (E3, 0), Right (S0, E1)),
        (Left (E3, 1), Right (S0, E2)),
        (Left (E3, 2), Right (S1, E1)),
        (Right (S1, E0), Right (S0, E3)),
        (Right (S0, E0), Right (S2, E2))
      ]

newtype Mod10 = Mod10 Int
instance Semigroup Mod10 where
  (Mod10 r) <> (Mod10 v) = Mod10 ((r + v) `mod` 10)
instance Monoid Mod10 where
  mempty = Mod10 0

instance SO2 Mod10 where
  invert (Mod10 u) = Mod10 ((- u) `mod` 10)

data Golden a = Golden a a a a deriving (Show, Eq, Functor)

instance Num a => Semigroup (Golden a) where
  Golden a1 b1 c1 d1 <> Golden a2 b2 c2 d2 = Golden (a1 + a2) (b1 + b2) (c1 + c2) (d1 + d2)

instance Num a => Monoid (Golden a) where
  mempty = Golden 1 0 0 0

instance (Num a) => Vector2 Mod10 (Golden a) where
  rotate (Mod10 n) (Golden a b c d) =
    let n' = if even n then n `div` 2 else ((n + 5) `div` 2) `mod` 5
        sgn = if even n then 1 else -1
        [a', b', c', d', e'] = take 5 $ drop n' $ cycle $ map (sgn *) [a, b, c, d, 0] in
      Golden (a' - e') (b' - e') (c' - e') (d' - e')

  negV = fmap negate

instance (Integral a) => Draw2D (Golden a) where
  toVec (Golden a b c d) = (
    fromIntegral a +
    fromIntegral b * cos (2*pi / 5) +
    fromIntegral (c + d) * cos (4*pi / 5),
    fromIntegral (d - c) * sin (4*pi / 5) -
    fromIntegral b * sin (2*pi / 5))

penrose3G :: Tiles PenroseTile PenroseEdge Mod10 (Golden Int)
penrose3G = Tiles {..}
  where
    pos1 = Golden 1 0 0 0
    neg1 = Golden (-1) 0 0 0

    edges U = [E0, E1, E2, E3]
    edges V = [E0, E1, E2, E3]

    edgeMap U E0 = (Mod10 5, neg1)
    edgeMap U E1 = (Mod10 7, neg1)
    edgeMap U E2 = (Mod10 5, pos1)
    edgeMap U E3 = (Mod10 7, pos1)

    edgeMap V E0 = (Mod10 5, neg1)
    edgeMap V E1 = (Mod10 1, pos1)
    edgeMap V E2 = (Mod10 0, neg1)
    edgeMap V E3 = (Mod10 6, pos1)

penrose3T = induceFromFunction (adjRec penrose3) penrose3A


penrose3S = toKMC (numberStates (normalizeMachine penrose3T))

