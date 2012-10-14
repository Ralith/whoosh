module Whoosh where

import System.Random
import Control.Monad.State

data Gun = Gun { kineticEnergy  :: Double
               , muzzleVelocity :: Double
               }

instance Show Gun where
    show (Gun ke mv) = "gun having " ++
                       "muzzle velocity of " ++ show mv ++ " m/s, " ++
                       "kinetic energy of " ++ show ke ++ " joules"

main = do
  gen <- newStdGen
  putStrLn $ show $ evalState genGun gen

-- Generate a Gaussian (0, 1) variate.
boxMuller :: StdGen -> (Double, StdGen)
boxMuller gen = (sqrt (-2 * log u1) * cos (2 * pi * u2), gen'')
    where (u1, gen')  = randomR (0, 1) gen
          (u2, gen'') = randomR (0, 1) gen'

normal :: Double -> Double -> State StdGen Double
normal mean stddev = state (\gen -> let (val, gen') = boxMuller gen in
                                    (val * stddev + mean, gen'))

genGun :: State StdGen Gun
genGun = do
  mv <- normal 715 200
  ke <- normal 1800 400
  return Gun { kineticEnergy = ke
             , muzzleVelocity = mv
             }
