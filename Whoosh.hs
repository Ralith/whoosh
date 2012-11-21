module Whoosh ( Bullet(..)
              , genBullet
              , genMass
              ) where

import System.Random
import Control.Monad.State
import Text.Printf

import Distributions

data Gun = Gun { muzzleVelocity :: Double -- m/s
               , barrelLength :: Double -- m
               , cartridge :: Cartridge
               }
         deriving (Show)

data Bullet = Bullet { bulletCaliber :: Double -- m
                     , bulletCylinderLen :: Double -- m
                     , bulletOgiveLen :: Double -- m
                     , bulletRho :: Double -- m
                     , bulletMass :: Double -- kg
                     }
            deriving (Show)

data Cartridge = Cartridge { bullet :: Bullet
                           , powderMass :: Double -- kg
                           }
                 deriving (Show)

fmtDouble :: Double -> String
fmtDouble = printf "%0.2f"

normal :: (Floating a, Random a) => Rational -> Rational -> State StdGen a
normal mu sigma = state $ sample (Normal mu sigma)

lognormal :: (Floating a, Random a) => Rational -> Rational -> Rational -> Rational -> State StdGen a
lognormal mu sigma zero scale = state $ sample (LogNormal mu sigma zero scale)

uniform :: (Floating a, Random a) => Rational -> Rational -> State StdGen a
uniform a b = state $ randomR (fromRational a, fromRational b)

genEnergy :: State StdGen Double
genEnergy = lognormal 1 0.75 100 1000

genMass :: State StdGen Double
genMass = lognormal 0.01 0.55 0.001 (1/150)

genGun :: State StdGen (Double, Double)
genGun = do
  baseEnergy <- genEnergy
  mass <- genMass
  massFactor <- lognormal 1 1 0 100
  return (baseEnergy + massFactor * mass, mass)

genBullet :: Double -> State StdGen Bullet
genBullet mass = do
  startingRatio <- lognormal 0.1 0.5 0.5 1.2
  cylinderFrac <- uniform (1/4) (1/2)
  let density = 11340 -- Lead, kg/m^3
      ratio = min 20 startingRatio
      totalVol = mass / density
      cylinderVol = totalVol * cylinderFrac
      ogiveVol = totalVol * (1 - cylinderFrac)
      (radius, ogiveLen, rho) = ogive ogiveVol ratio
  return Bullet { bulletCaliber = 2*radius
                , bulletCylinderLen = cylinderVol / (pi * radius^2)
                , bulletOgiveLen = ogiveLen
                , bulletRho = rho
                , bulletMass = mass
                }

ogive :: (Floating a, Ord a) => a -> a -> (a, a, a)
ogive volume a =
    -- Given a = L/(2*R) and the formula for the volume of an ogive, we can obtain:
    let f r = -((192*pi*a^6+48*pi*a^4-12*pi*a^2-3*pi)*r^3*abs(r)*asin((4*abs(a)*abs(r))/((4*a^2+1)*r))+(48*pi*a^2-12*pi)*sqrt(16*a^4-8*a^2+1)*abs(a)*r^3*abs(r)+(-384*pi*a^4+64*pi*a^2-24*pi)*abs(a)*r^4)/(24*abs(r)) - volume
        f' r = -((192*pi*a^6+48*pi*a^4-12*pi*a^2-3*pi)*r^2*asin((4*abs(a)*abs(r))/((4*a^2+1)*r))+(-384*pi*a^4+64*pi*a^2-24*pi)*abs(a)*r*abs(r)+(48*pi*a^2-12*pi)*sqrt(16*a^4-8*a^2+1)*abs(a)*r^2)/(8)
        r = newton f f' 0.01 1e-10
        rho = (r^2+(2*a*r)^2)/(2*r)
    in (r, sqrt (2*rho*r-r^2), rho)

newton :: (Floating a, Ord a) => (a -> a) -> (a -> a) -> a -> a -> a
newton f f' guess tol = helper guess (step guess)
    where
      helper x0 x1 =
          if abs (x1 - x0) < tol then x1
          else helper x1 (step x1)
      step x = x - (f x) / (f' x)
