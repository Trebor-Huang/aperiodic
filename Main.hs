{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Main where
import Graphics.Gloss
import Geometry
import Substitution
import qualified Spectre as T
import Automata
import GHC.Clock (getMonotonicTime)
import Kleenex (adjSST)
import System.Exit (exitSuccess)

rendered =
  draw (adjRec T.system) T.geometry inBounds
    (T.Spectre :< [(T.Psi, T.SS0), (T.Xi, T.SH4)] ++ cycle [(T.Sigma, T.SH4), (T.Xi, T.SH3)]) T.S0
  where
    factor = 10
    inBounds (x, y) =
      x < factor * 4 && x > - (factor * 4) &&
      y < factor * 3 && y > - (factor * 3)

main :: IO ()
main = do
  putStrLn $ "Inference complete, " <> show (length $ states $ fst T.transducer) <> " states"
  t1 <- getMonotonicTime
  putStrLn "Starting calculation..."
  putStrLn $ "Render complete, " <> show (length rendered) <> " tiles in total"
  t2 <- getMonotonicTime
  putStrLn $ "Time used: " <> show (t2 - t1)
  -- exitSuccess
  display FullScreen white
    (scale 40 40 $ pictures $ zipWith makePolygon [0..] rendered)
  where
    chooseColor _ = green

    makePolygon :: Int -> (Signature T.Tile T.Subtile, Path) -> Picture
    makePolygon _ (sig@(t:<ts), p) = scale 0.5 0.5 $ pictures [
        -- color (chooseColor sig) (polygon p),
        lineLoop p,
        uncurry translate (average p) $
        scale 0.002 0.002 $
          text (show (fst (head ts)))
      ]

    average :: [(Float, Float)] -> (Float, Float)
    average f = let l = length f in
      (sum (map fst f) / fromIntegral l, sum (map snd f) / fromIntegral l)
