{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Spectre where
import qualified Data.Set as S
import Geometry
import Substitution
import Automata

newtype Mod12 = Mod12 Int
instance Semigroup Mod12 where
  (Mod12 r) <> (Mod12 v) = Mod12 ((r + v) `mod` 12)
instance Monoid Mod12 where
  mempty = Mod12 0

instance SO2 Mod12 where
  invert (Mod12 u) = Mod12 ((- u) `mod` 12)

-- (x/2 + y√3/2, z/2 + w√3/2); parity of (x,w) agree, parity of (y,z) agree.
-- equivalently, spanning with {(1, 0), (√3/2, 1/2), (1/2, √3/2), (0, 1)}
data TrigNum a = TrigNum a a a a deriving (Eq, Show, Functor)

instance Integral a => Draw2D (TrigNum a) where
  toVec (TrigNum a b c d) = (
      fromIntegral a +
      fromIntegral b * 0.5 * sqrt 3 +
      fromIntegral c * 0.5,
      fromIntegral b * 0.5 +
      fromIntegral c * 0.5 * sqrt 3 +
      fromIntegral d
    )

instance Num a => Semigroup (TrigNum a) where
  TrigNum a b c d <> TrigNum a' b' c' d' = TrigNum (a+a') (b+b') (c+c') (d+d')

instance Num a => Monoid (TrigNum a) where
  mempty = TrigNum 0 0 0 0

instance (Num a) => Vector2 Mod12 (TrigNum a) where
  rotate r (TrigNum a b c d) = mconcat $ zipWith vectors
    (map (\i -> r <> Mod12 i) [0,1,2,3]) [a,b,c,d]
    where
      vectors :: Num a => Mod12 -> a -> TrigNum a
      vectors (Mod12 0) a = TrigNum a 0 0 0
      vectors (Mod12 1) a = TrigNum 0 a 0 0
      vectors (Mod12 2) a = TrigNum 0 0 a 0
      vectors (Mod12 3) a = TrigNum 0 0 0 a
      vectors (Mod12 4) a = TrigNum (-a) 0 a 0
      vectors (Mod12 5) a = TrigNum 0 (-a) 0 a
      vectors (Mod12 6) a = TrigNum (-a) 0 0 0
      vectors (Mod12 7) a = TrigNum 0 (-a) 0 0
      vectors (Mod12 8) a = TrigNum 0 0 (-a) 0
      vectors (Mod12 9) a = TrigNum 0 0 0 (-a)
      vectors (Mod12 10) a = TrigNum a 0 (-a) 0
      vectors (Mod12 11) a = TrigNum 0 a 0 (-a)
      vectors (Mod12 n) a = vectors (Mod12 (n `mod` 12)) a

  negV = fmap negate

data SpectreTile = G | D | J | L | X | P | S | F | Y | Spectre
  deriving (Show, Eq, Ord, Enum, Bounded)
data SpectreEdge
  = H0 | H1 | H2 | H3 | H4 | H5
  | S0 | S1 | S2 | S3 | S4 | S5 | S6 | S7 | S8 | S9 | S10 | S11 | S12 | S13
  deriving (Show, Eq, Ord, Enum, Bounded)
data SpectreSubtile
  = SH0 | SH1 | SH2 | SH3 | SH4 | SH5 | SH6 | SH7
  | SS0 | SS1
  deriving (Show, Eq, Ord, Enum, Bounded)

spectre3A :: FSA (Alphabet SpectreTile SpectreSubtile SpectreEdge) (Maybe SpectreTile)
spectre3A = FST {..}
  where
    states = S.fromList $ Nothing : map Just [minBound..maxBound]
    transitions = [] ++ []
