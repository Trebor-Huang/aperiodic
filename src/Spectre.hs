{-# LANGUAGE FlexibleInstances #-}
module Spectre (
  SpectreTile(..), SpectreSubtile(..), SpectreEdge(..),
  spectre3, spectre3A, spectre3G, spectre3T, spectre3S,
  TrigNum(..), Mod12(..)) where
import qualified Data.Set as S
import Geometry
import Substitution
import Automata
import Kleenex

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

-- The order of constructors is used later (as enums), so don't change them!
data SpectreTile = Gamma | Delta | Theta | Lambda | Xi | Pi | Sigma | Phi | Psi | Spectre
  deriving (Show, Eq, Ord, Enum, Bounded)
-- The edge of hexagons are counterclockwise
-- (with H5/H6 sometimes referring to eta, which can be paired against itself)
-- and the edge of spectres are clockwise.
data SpectreEdge
  = H0 | H1 | H2 | H3 | H4 | H5 | H6
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
    transitions =
      -- Start with spectres
      [(Nothing, Begin Spectre sh, (), Just Spectre) | sh <- [S0 .. S13]] ++
      -- Spectres to hexagons
      [(Just Spectre, Inflate hex ss, (), Just hex) |
        hex <- [Gamma .. Psi],
        ss <- if hex == Gamma then [SS0, SS1] else [SS0]] ++
      -- hexagons to hexagons
      [(Just hex1, Inflate hex2 ss, (), Just hex2) |
        hex2 <- [Gamma .. Psi],
        (hex1, ss) <- zip (subtiles !! fromEnum hex2) [SH0 .. SH7]]

subtiles :: [[SpectreTile]]
subtiles = [
    [Phi, Xi, Gamma, Sigma, Pi, Delta, Theta],
    [Phi, Pi, Gamma, Sigma, Xi, Delta, Phi, Xi],
    [Phi, Pi, Gamma, Sigma, Psi, Delta, Phi, Pi],
    [Phi, Pi, Gamma, Sigma, Psi, Delta, Phi, Xi],
    [Phi, Psi, Gamma, Sigma, Psi, Delta, Phi, Pi],
    [Phi, Psi, Gamma, Sigma, Psi, Delta, Phi, Xi],
    [Lambda, Pi, Gamma, Sigma, Xi, Delta, Phi, Xi],
    [Phi, Pi, Gamma, Sigma, Psi, Delta, Phi, Psi],
    [Phi, Psi, Gamma, Sigma, Psi, Delta, Phi, Psi]
  ]

spectre3G :: Tiles SpectreTile SpectreEdge Mod12 (TrigNum Int)
spectre3G = Tiles {..}
  where
    edges Spectre = [S0 .. S13]
    edges r = error $ "This tile should not exist in the final result: " <> show r

    -- angles = [2, 10, 1, 9, 3, 11, 8, 0, 9, 1, 10, 6, 9, 5]
    angles = [10, 2, 11, 3, 9, 1, 4, 0, 3, 11, 2, 6, 3, 7]

    edgeMap Spectre s = (Mod12 (angles !! fromEnum s),
      if even (fromEnum s) then TrigNum 1 0 0 0 else TrigNum (-1) 0 0 0)
    edgeMap r _ = error $ "This tile should not exist in the final result: " <> show r

spectre3 :: SubstSystem SpectreTile SpectreSubtile SpectreEdge Int ()
spectre3 = SubstSystem {..}
  where
    subtile _ SS0 = Spectre
    subtile _ SS1 = Spectre
    subtile tile sub = subtiles !! fromEnum tile !! fromEnum sub

    -- Expanding to spectres
    -- substMap Delta = fromPairs []

    substMap _ tile = fromPairs []

spectre3T = induceFromFunction (adjRec spectre3) spectre3A


spectre3S = toKMC (numberStates (normalizeMachine spectre3T))
