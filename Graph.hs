module Graph where

import Graphics.Rendering.Plot
import Data.Packed.Vector

graph :: (Double -> Double) -> (Double, Double) -> Int -> FilePath -> IO ()
graph f (xmin, xmax) samples filename = writeFigure PNG filename (512, 512) $ do
  plot $ (fromList (take samples (iterate (+ (xmax - xmin)/(fromIntegral samples)) xmin))
         , [line f black])
  yrange Linear 0 1
  xrange Linear xmin xmax

density :: [Double] -> (Double, Double) -> Int -> FilePath -> IO ()
density ys (xmin, xmax) samples filename = writeFigure PNG filename (512, 512) $ do
  plot $ [(Hist, fromList (take samples (iterate (+ (xmax - xmin)/(fromIntegral samples)) xmin))
          , fromList ys)]
  xrange Linear xmin xmax
