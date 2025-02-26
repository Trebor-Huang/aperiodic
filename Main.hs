module Main where
import Graphics.Gloss
import Geometry
import Substitution
import Penrose
import Automata
import GHC.Clock (getMonotonicTime)
import Kleenex (adjSST)
import System.Exit (exitSuccess)

rendered =
  draw (adjSST penrose3S) penrose3G inBounds
    (V :< repeat (V, S0)) E0
  where
    factor = 5
    inBounds (x, y) =
      x < factor * 4 && x > - (factor * 4) &&
      y < factor * 3 && y > - (factor * 3)

main :: IO ()
main = do
  putStrLn $ "Inference complete, " <> show (length $ states $ fst penrose3T) <> " states"
  t1 <- getMonotonicTime
  putStrLn "Starting calculation..."
  putStrLn $ "Render complete, " <> show (length rendered) <> " tiles in total"
  t2 <- getMonotonicTime
  putStrLn $ "Time used: " <> show (t2 - t1)
  exitSuccess
  display FullScreen white
    (scale 40 40 $ pictures $ zipWith makePolygon [0..] rendered)
  where
    chooseColor (t :< ts) =
      case t of
        V -> cyan
        U -> blue
      -- case ts !! 20 of
      --   (V, S0) -> blue
      --   (V, S1) -> yellow
      --   (U, S0) -> orange
      --   (U, S1) -> red
      --   (U, S2) -> green
      --   _ -> black

    makePolygon :: Int -> (Signature PenroseTile PenroseSubtile, Path) -> Picture
    makePolygon _ (sig, p) = scale 0.5 0.5 $ pictures [
        color (chooseColor sig) (polygon p),
        lineLoop p
        -- translate (sum (map fst p) / 4) (sum (map snd p) / 4) $
        -- scale 0.002 0.002 $
        --   text (show i)
      ]
