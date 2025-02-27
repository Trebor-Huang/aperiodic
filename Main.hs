{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Main where
import Geometry
import Substitution
import qualified Tilings.Spectre as T
import Automata
import Kleenex (adjSST)

import GHC.Clock (getMonotonicTime)
import System.Exit (exitSuccess)
import KMC.SymbolicSST (SST(sstS))
import qualified Data.Set as S
import Diagrams.Prelude hiding ((:<))
import Diagrams.Backend.SVG
import Data.Colour.CIE
import Data.Colour.CIE.Illuminant

sig :: Signature T.Tile T.Subtile
sig = T.Spectre :<
  [(T.Psi, T.SS0)] ++
  repeat (T.Psi, T.SH1)

boundX = 100.0
boundY = 80.0

calculated :: [(Signature T.Tile T.Subtile, [(Double, Double)])]
calculated =
  draw (adjSST T.streamer) T.geometry inBounds sig T.S0
  where
    inBounds (x, y) =
      x < boundX*0.5 && x > - (boundX*0.5) &&
      y < boundY*0.5 && y > - (boundY*0.5)

rendered :: Diagram B
rendered = map makePolygon calculated # mconcat #
  rectEnvelope (mkP2 (- (boundX * 0.5)) (- (boundY * 0.5))) (V2 boundX boundY)
  where
    fromHue h = cieLAB d65 80 (70 * cos h) (70 * sin h)

    chooseColor T.Delta _ = fromHue 0
    chooseColor T.Theta _ = fromHue 0.7
    chooseColor T.Lambda _ = fromHue 1.4
    chooseColor T.Xi _ = fromHue 2.1
    chooseColor T.Pi _ = fromHue 2.8
    chooseColor T.Phi _ = fromHue (-0.7)
    chooseColor T.Psi _ = fromHue (-1.4)
    chooseColor T.Gamma _ = fromHue (-2.1)
    -- chooseColor T.Gamma T.SS0 = cieLAB d65 85 (70 * cos (-2.1)) (70 * sin (-2.1))
    -- chooseColor T.Gamma T.SS1 = cieLAB d65 75 (70 * cos (-2.1)) (70 * sin (-2.1))
    chooseColor T.Sigma _ = fromHue (-2.8)
    chooseColor _ _ = white

    average :: [(Double, Double)] -> (Double, Double)
    average f = let l = length f in
      (sum (map fst f) / fromIntegral l, sum (map snd f) / fromIntegral l)

    makePolygon :: (Signature T.Tile T.Subtile, [(Double, Double)]) -> Diagram B
    makePolygon (_:<ts, coords) =
      text (show (fst (head ts))) #
      font "CMU Serif" #
      moveTo (p2 $ average coords)
      `atop`
      fromVertices (map p2 coords) #
      mapLoc closeLine #
      strokeLocLoop #
      lw ultraThin #
      fc (uncurry chooseColor (head ts))

main :: IO ()
main = do
  putStrLn $ "Inference complete, " <> show (length $ states $ fst T.transducer) <> " states"
  putStrLn $ "Streaming transducer has " <> show (S.size $ sstS T.streamer) <> " states"
  t1 <- getMonotonicTime
  putStrLn "Starting calculation..."
  putStrLn $ "Render complete, " <> show (length calculated) <> " tiles in total"
  t2 <- getMonotonicTime
  putStrLn $ "Time used: " <> show (t2 - t1)
  renderSVG "./output/render.svg" (dims (V2 800 0)) rendered
