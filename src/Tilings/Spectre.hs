{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Tilings.Spectre (
  Tile(..), Subtile(..), Edge(..),
  system, automaton, geometry, transducer, streamer,
  TrigNum(..), Mod12(..)) where
import qualified Data.Set as S
import Geometry
import Substitution
import Automata
import Kleenex
import Control.Monad (join)
import Data.Bifunctor (bimap)
import Data.List (delete)

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
  rotate r (TrigNum x y z w) = mconcat $ zipWith vectors
    (map (\i -> r <> Mod12 i) [0,1,2,3]) [x,y,z,w]
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
data Tile = Gamma | Delta | Theta | Lambda | Xi | Pi | Sigma | Phi | Psi | Spectre
  deriving (Eq, Ord, Enum, Bounded)
instance Show Tile where
  show Gamma = "Γ"
  show Delta = "Δ"
  show Theta = "Θ"
  show Lambda = "Λ"
  show Xi = "Ξ"
  show Pi = "Π"
  show Sigma = "Σ"
  show Phi = "Φ"
  show Psi = "Ψ"
  show Spectre = "S"

-- The edge of hexagons are counterclockwise
-- (and for the edge eta, we split it into H5(+) and H6(-) in the couterclockwise direction)
-- and the edge of spectres are clockwise.
data Edge
  = H0 | H1 | H2 | H3 | H4 | H5 | H6
  | S0 | S1 | S2 | S3 | S4 | S5 | S6 | S7 | S8 | S9 | S10 | S11 | S12 | S13
  deriving (Show, Eq, Ord, Enum, Bounded)
data Subtile
  = SH0 | SH1 | SH2 | SH3 | SH4 | SH5 | SH6 | SH7
  | SS0 | SS1
  deriving (Show, Eq, Ord, Enum, Bounded)

automaton :: FSA (Alphabet Tile Subtile Edge) (Maybe Tile)
automaton = FST {..}
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

subtiles :: [[Tile]]
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

geometry :: Tiles Tile Edge Mod12 (TrigNum Int)
geometry = Tiles {..}
  where
    edges Spectre = [S0 .. S13]
    edges r = error $ "This tile should not exist in the final result: " <> show r

    -- angles = [2, 10, 1, 9, 3, 11, 8, 0, 9, 1, 10, 6, 9, 5]
    angles = [5, 9, 6, 10, 1, 9, 0, 8, 11, 3, 9, 1, 10, 2]

    edgeMap Spectre s =
      let i = fromEnum s - fromEnum S0 in
        (
          Mod12 (angles !! i),
          if even i then TrigNum 1 0 0 0 else TrigNum (-1) 0 0 0
        )
    edgeMap r _ = error $ "This tile should not exist in the final result: " <> show r

data SpectreStage = HexStage | SpectreStage deriving (Eq, Show)

system :: SubstSystem Tile Subtile Edge Int SpectreStage
system = SubstSystem {..}
  where
    subtile _ SS0 = Spectre
    subtile _ SS1 = Spectre
    subtile tile sub = subtiles !! fromEnum tile !! fromEnum sub

    stageMap x = if x < SS0 then HexStage else SpectreStage

    -- Expanding to spectres, we have two special cases
    substMap SpectreStage Gamma = fromPairs [
        -- outer left edge
        (Left (H2, 2), Right (SS0, S0)),
        (Left (H2, 1), Right (SS0, S1)),
        (Left (H2, 0), Right (SS0, S2)),
        (Left (H1, 0), Right (SS0, S3)),
        (Left (H1, 1), Right (SS0, S4)),
        (Left (H1, 2), Right (SS0, S5)),
        (Left (H0, 0), Right (SS0, S6)),
        (Left (H0, 1), Right (SS0, S7)),
        (Left (H0, 2), Right (SS0, S8)),
        (Left (H5, 2), Right (SS0, S9)),
        -- middle edge
        (Right (SS1, S7), Right (SS0, S10)),
        (Right (SS1, S6), Right (SS0, S11)),
        (Right (SS1, S5), Right (SS0, S12)),
        (Right (SS1, S4), Right (SS0, S13)),
        -- outer right edge
        (Left (H4, 4), Right (SS1, S0)),
        (Left (H4, 5), Right (SS1, S1)),
        (Left (H3, 0), Right (SS1, S2)),
        (Left (H3, 1), Right (SS1, S3)),

        (Left (H5, 1), Right (SS1, S8)),
        (Left (H5, 0), Right (SS1, S9)),
        (Left (H4, 0), Right (SS1, S10)),
        (Left (H4, 1), Right (SS1, S11)),
        (Left (H4, 2), Right (SS1, S12)),
        (Left (H4, 3), Right (SS1, S13))
      ]
    substMap SpectreStage Sigma = fromPairs [
        (Left (H3, 1), Right (SS0, S0)),
        (Left (H2, 2), Right (SS0, S1)),
        (Left (H2, 1), Right (SS0, S2)),
        (Left (H2, 0), Right (SS0, S3)),
        -- spur
        (Left (H1, 0), Left (H0, 5)),
        (Left (H1, 1), Left (H0, 4)),

        (Left (H0, 3), Right (SS0, S4)),
        (Left (H0, 2), Right (SS0, S5)),
        (Left (H0, 1), Right (SS0, S6)),
        (Left (H0, 0), Right (SS0, S7)),
        (Left (H5, 0), Right (SS0, S8)),
        (Left (H5, 1), Right (SS0, S9)),
        (Left (H4, 2), Right (SS0, S10)),
        (Left (H4, 1), Right (SS0, S11)),
        (Left (H4, 0), Right (SS0, S12)),
        (Left (H3, 0), Right (SS0, S13))
      ]
    substMap SpectreStage tile
      = fromPairs $ zipWith
        (\i n -> (Left n, Right (SS0, toEnum (i + fromEnum S0))))
        [0..] (numbers !! fromEnum tile)
      where
        -- Subedge is couterclockwise for positive colors
        numbers = [
            [], -- Gamma
            [
              (H3, 1),
              (H2, 2), (H2, 1), (H2, 0),
              (H1, 1), (H1, 0),
              (H0, 0), (H0, 1),
              (H5, 0), (H5, 1),
              (H4, 2), (H4, 1), (H4, 0),
              (H3, 0)
            ], -- Delta
            [
              (H3, 0),
              (H2, 2), (H2, 1), (H2, 0),
              (H1, 1), (H1, 0),
              (H0, 0), (H0, 1), (H0, 2),
              (H6, 0), (H5, 0),
              (H4, 2), (H4, 1), (H4, 0)
            ], -- Theta
            [
              (H3, 1),
              (H2, 2), (H2, 1), (H2, 0),
              (H1, 1), (H1, 0),
              (H0, 0), (H0, 1), (H0, 2),
              (H5, 0),
              (H4, 2), (H4, 1), (H4, 0),
              (H3, 0)
            ], -- Lambda
            [
              (H3, 0),
              (H2, 1), (H2, 0),
              (H1, 0), (H1, 1), (H1, 2),
              (H0, 0), (H0, 1), (H0, 2),
              (H6, 0), (H5, 0),
              (H4, 2), (H4, 1), (H4, 0)
            ], -- Xi
            [
              (H3, 1),
              (H2, 1), (H2, 0),
              (H1, 0), (H1, 1), (H1, 2),
              (H0, 0), (H0, 1), (H0, 2),
              (H5, 0),
              (H4, 2), (H4, 1), (H4, 0),
              (H3, 0)
            ], -- Pi
            [], -- Sigma
            [
              (H3, 1),
              (H2, 2), (H2, 1), (H2, 0),
              (H1, 1), (H1, 0),
              (H0, 0), (H0, 1), (H0, 2),
              (H6, 0), (H5, 0),
              (H4, 1), (H4, 0),
              (H3, 0)
            ], -- Phi
            [
              (H3, 1),
              (H2, 1), (H2, 0),
              (H1, 0), (H1, 1), (H1, 2),
              (H0, 0), (H0, 1), (H0, 2),
              (H6, 0), (H5, 0),
              (H4, 1), (H4, 0),
              (H3, 0)
            ], -- Psi
            [] -- Spectre
          ]

    -- Expanding between hexagons, note that the chirality is reversed!!
    -- It doesn't affect our algorithm though, because at this stage no geometry
    -- is actually involved.
    substMap HexStage tile = fromPairs $
      map (bimap Right Right)
        (if tile /= Gamma then internalPairs8 else internalPairs7) ++
      zipWith (\p q -> (p, Right q))
        (externalEdge !! fromEnum tile)
        (case tile of
          Gamma -> internalEdgeGamma
          Delta -> internalEdgeDelta
          Theta -> internalEdgeTheta
          Lambda -> internalEdgeLambda
          Xi -> internalEdgeXi
          Pi -> internalEdgePi
          Sigma -> internalEdgeSigma
          Phi -> internalEdgePhi
          Psi -> internalEdgePsi
          t -> error $ "Unexpected tile " <> show t)

    -- the internal pairs when there are 7 hexes
    internalPairs7 = [
        ((SH0, H2), (SH1, H0)),
        ((SH0, H1), (SH3, H5)),
        ((SH0, H0), (SH2, H5)),
        ((SH1, H1), (SH3, H4)),
        ((SH2, H4), (SH3, H0)),
        ((SH2, H3), (SH5, H1)),
        ((SH2, H2), (SH4, H1)),
        ((SH3, H1), (SH5, H0)),
        ((SH3, H2), (SH6, H0)),
        ((SH4, H0), (SH5, H2)),
        ((SH5, H5), (SH6, H1))
      ]
    -- extra pairs when there are 8 hexes
    internalPairs8 = [
        ((SH5, H4), (SH7, H1)),
        ((SH6, H2), (SH7, H0))
      ] ++ internalPairs7

    -- the internal edges that touch the outside, in clockwise order
    internalEdgeGamma = [
        (SH4, H2),
        (SH2, H1), (SH2, H0),
        (SH0, H6), (SH0, H5), (SH0, H4), (SH0, H3),
        (SH1, H6), (SH1, H5), (SH1, H4), (SH1, H3), (SH1, H2),
        (SH3, H3),
        (SH6, H6), (SH6, H5), (SH6, H4), (SH6, H3), (SH6, H2),
        (SH5, H4), (SH5, H3),
        (SH4, H5), (SH4, H4), (SH4, H3)
      ]
    internalEdgePsi = [
        (SH4, H2),
        (SH2, H1), (SH2, H0),
        (SH0, H6), (SH0, H5), (SH0, H4), (SH0, H3),
        (SH1, H6), (SH1, H5), (SH1, H4), (SH1, H3), (SH1, H2),
        (SH3, H3),
        (SH6, H6), (SH6, H5), (SH6, H4), (SH6, H3),
        (SH7, H6), (SH7, H5), (SH7, H4), (SH7, H3), (SH7, H2),
        (SH5, H3),
        (SH4, H6), (SH4, H5), (SH4, H4), (SH4, H3)
      ]
    internalEdgeTheta = delete (SH7, H6) internalEdgeLambda
    internalEdgeLambda = delete (SH1, H6) internalEdgePsi
    internalEdgeXi = delete (SH7, H6) internalEdgePsi
    internalEdgePi = internalEdgePsi
    internalEdgePhi = internalEdgeLambda
    -- for Delta and Sigma the starting point is not the same
    internalEdge8' = last internalEdgePsi : init internalEdgePsi
    internalEdgeDelta = delete (SH1, H6) internalEdge8'
    internalEdgeSigma = delete (SH0, H6) internalEdgeDelta


    -- the external edges
    externalEdge = map (join . zipWith
        (\i n -> map (\k -> Left (i, k)) $ if (n :: Int) > 0 then [0..n-1] else [(-n-1),(-n-2)..0])
        [H0 .. H6])
      [
        [-beta, -alpha, alpha, -gamma, -delta, beta], -- Gamma
        [-zeta, gamma, beta, -epsilon, alpha, -gamma], -- Delta
        [-beta, gamma, beta, theta, beta, eta, -eta], -- Theta
        [-beta, gamma, beta, -epsilon, alpha, -theta], -- Lambda
        [-beta, -alpha, epsilon, theta, beta, eta, -eta], -- Xi
        [-beta, -alpha, epsilon, -epsilon, alpha, -theta], -- Pi
        [delta, zeta, beta, -epsilon, alpha, -gamma], -- Sigma
        [-beta, gamma, beta, -epsilon, epsilon, eta, -eta], -- Phi
        [-beta, -alpha, epsilon, -epsilon, epsilon, eta, -eta] -- Psi
      ]
      where
        alpha = 4
        beta = 2
        gamma = 6
        delta = 5
        epsilon = 5
        zeta = 3
        eta = 3
        theta = 7

transducer = induceFromFunction (adjRec system) automaton


streamer = toKMC (numberStates (normalizeMachine transducer))
