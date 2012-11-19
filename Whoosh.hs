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
                     , bulletRho :: Double
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
    show (Bullet cal len rho)
        = fmtDouble (cal*1000) ++ "x" ++ fmtDouble (len*1000) ++ "mm bullet"
          ++ "with ρ=" ++ fmtDouble (rho*1000) ++ "mm"

main :: IO ()
main = do
  gen <- newStdGen
  putStrLn $ show $ evalState genGun gen
  putStrLn $ show $ evalState (genBullet 0.01) gen


normal :: (Floating a, Random a) => Rational -> Rational -> State StdGen a
normal mu sigma = state $ sample (Normal mu sigma)

lognormal :: (Floating a, Random a) => Rational -> Rational -> Rational -> Rational -> State StdGen a
lognormal mu sigma zero scale = state $ sample (LogNormal mu sigma zero scale)

uniform :: (Floating a, Random a) => a -> a -> State StdGen a
uniform a b = state $ randomR (a, b)

genEnergy :: State StdGen Double
genEnergy = lognormal 1 0.75 100 1000

genMass :: State StdGen Double
genMass = lognormal 0.1 0.5 0.001 (1/100)

genGun :: State StdGen (Double, Double)
genGun = do
  baseEnergy <- genEnergy
  mass <- genMass
  massFactor <- lognormal 1 1 0 100
  return (baseEnergy + massFactor * mass, mass)

genBullet :: Double -> State StdGen Bullet
genBullet mass = do
  startingRhoFactor <- lognormal 0.5 0.75 1 1
  let density = 11340 -- Lead, kg/m^3
      a = min 20 startingRhoFactor
      v = mass / density
      -- Given rho=2*R*a and the formula for the volume of an ogive, we can obtain:
      f r = -((24*pi*a^3-12*pi*a^2)*r^3*asin((sqrt(4*a-1)*abs(r))/(2*a*r))+sqrt(4*a-1)*(-24*pi*a^2+16*pi*a-4*pi)*r^2*abs(r)+sqrt(4*a-1)*(6*pi*a-3*pi)*sqrt(4*a^2-4*a+1)*r^3)/3 - v
      f' r = -((24*pi*a^3-12*pi*a^2)*r^2*abs(r)*asin((sqrt(4*a-1)*abs(r))/(2*a*r))+sqrt(4*a-1)*(6*pi*a-3*pi)*sqrt(4*a^2-4*a+1)*r^2*abs(r)+sqrt(4*a-1)*(-24*pi*a^2+16*pi*a-4*pi)*r^3)/(abs(r))
      radius = newton f f' 0.01 1e-10
  return Bullet { bulletCaliber = 2*radius, bulletLength = sqrt(4*a-1)*abs(radius), bulletRho = 2*radius*a }

newton :: (Floating a, Ord a) => (a -> a) -> (a -> a) -> a -> a -> a
newton f f' guess tol = helper guess (step guess)
    where
      helper x0 x1 =
          if abs (x1 - x0) < tol then x1
          else helper x1 (step x1)
      step x = x - (f x) / (f' x)
