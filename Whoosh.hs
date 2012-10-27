module Whoosh where

import System.Random
import Control.Monad.State
import Text.Printf

data Gun = Gun { muzzleVelocity :: Double -- m/s
               , barrelLength :: Double -- m
               , cartridge :: Cartridge
               }

data Bullet = Bullet { bulletCaliber :: Double
                     , bulletLength :: Double
                     }

data Cartridge = Cartridge { bullet :: Bullet
                           , powderMass :: Double
                           }

fmtDouble :: Double -> String
fmtDouble x = printf "%0.2f" x

instance Show Gun where
    show (Gun mv barl cart)
        = "gun having " ++
          "a " ++ fmtDouble (barl*100) ++ "cm barrel, " ++
          "typically firing a " ++ show cart ++ " " ++
          "at " ++ fmtDouble mv ++ "m/s"
        where

instance Show Cartridge where
    show (Cartridge b p)
        = "cartridge containing a " ++ show b ++ " and " ++ fmtDouble (p*1000) ++ " grams of powder"

instance Show Bullet where
    show (Bullet cal len)
        = fmtDouble (cal*100) ++ "x" ++ fmtDouble (len*100) ++ "mm bullet"

-- main = do
--   gen <- newStdGen
--   putStrLn $ show $ evalState genGun gen

-- Generate a Gaussian (0, 1) variate.
boxMuller :: (Random a, Floating a) => StdGen -> (a, StdGen)
boxMuller gen = (sqrt (-2 * log u1) * cos (2 * pi * u2), gen'')
    where (u1, gen')  = randomR (0, 1) gen
          (u2, gen'') = randomR (0, 1) gen'

normal :: (Random a, Floating a) => a -> a -> State StdGen a
normal mean stddev = state (\gen -> let (val, gen') = boxMuller gen in
                                    (val * stddev + mean, gen'))

lognormal :: (Random a, Floating a) => a -> a -> State StdGen a
lognormal minimum shape
    = state (\gen -> let (x, gen') = boxMuller gen in
                     (exp (minimum + shape * x), gen'))
