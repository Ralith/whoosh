module Whoosh where

import System.Random
import Control.Monad.State

data Gun = Gun { muzzleVelocity :: Double -- m/s
               , barrelLength :: Double -- m
               , bulletLength :: Double -- m
               , caliber :: Double -- m
               }

instance Show Gun where
    show (Gun mv barl bull cal)
        = "gun having " ++
          "muzzle velocity of " ++ show mv ++ "m/s, " ++
          "a " ++ show (barl*10) ++ "cm barrel, " ++
          "firing " ++ show (cal*100) ++ "x" ++ show (bull*100) ++ "mm bullets"

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

mass :: Floating a => a -> a -> a
mass velocity kineticEnergy = 2 * kineticEnergy / velocity ** 2 -- kg

densityOfLead :: Floating a => a
densityOfLead = 11340 -- kg/m^3

diameterOfCylinder :: Floating a => a -> a -> a -> a
diameterOfCylinder mass density length = 2 * sqrt (mass/(density * pi * length))

genGun :: State StdGen Gun
genGun = do
  mv <- normal 715 200
  ke <- normal 1800 400
  let typicalMass = mass mv ke
      barrelLen = mv / 100
  return Gun { muzzleVelocity = mv
             , barrelLength = barrelLen
             , bulletLength = barrelLen / 10
             , caliber = diameterOfCylinder typicalMass densityOfLead barrelLen
             }
