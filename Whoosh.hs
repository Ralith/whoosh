module Whoosh where

import System.Random
import Control.Monad.State
import Text.Printf

import Distributions

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
fmtDouble = printf "%0.2f"

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
        = fmtDouble (cal*1000) ++ "x" ++ fmtDouble (len*1000) ++ "mm bullet"

main :: IO ()
main = do
  gen <- newStdGen
  putStrLn $ show $ evalState genGun gen


normal :: (Floating a, Random a) => Rational -> Rational -> State StdGen a
normal mu sigma = state $ sample (Normal mu sigma)

lognormal :: (Floating a, Random a) => Rational -> Rational -> Rational -> Rational -> State StdGen a
lognormal mu sigma zero scale = state $ sample (LogNormal mu sigma zero scale)

uniform :: (Floating a, Random a) => a -> a -> State StdGen a
uniform a b = state $ randomR (a, b)

genEnergy :: State StdGen Double
genEnergy = lognormal 1 0.75 100 1000

genMass :: State StdGen Double
genMass = lognormal 0.1 0.5 1 10

genGun :: State StdGen (Double, Double)
genGun = do
  baseEnergy <- genEnergy
  mass <- genMass
  massFactor <- lognormal 1 1 0 100
  return (baseEnergy + massFactor * mass, mass)

genBullet :: Double -> State StdGen Bullet
genBullet mass = do
  return Bullet { bulletCaliber = 0, bulletLength = 0 }
