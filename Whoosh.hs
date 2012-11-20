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

data Bullet = Bullet { bulletCaliber :: Double
                     , bulletCylinderLen :: Double
                     , bulletOgiveLen :: Double
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
    show (Bullet cal cyl ogive rho)
        = fmtDouble (cal*1000) ++ "x" ++ fmtDouble ((cyl+ogive)*1000) ++ "mm bullet"
          ++ " with ρ=" ++ fmtDouble (rho*1000) ++ "mm"

normal :: (Floating a, Random a) => Rational -> Rational -> State StdGen a
normal mu sigma = state $ sample (Normal mu sigma)

lognormal :: (Floating a, Random a) => Rational -> Rational -> Rational -> Rational -> State StdGen a
lognormal mu sigma zero scale = state $ sample (LogNormal mu sigma zero scale)

uniform :: (Floating a, Random a) => Rational -> Rational -> State StdGen a
uniform a b = state $ randomR (fromRational a, fromRational b)

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
  startingRatio <- lognormal 0.2 0.5 0.5 1
  cylinderFrac <- uniform (1/5) (1/2)
  let density = 11340 -- Lead, kg/m^3
      ratio = min 20 startingRatio
      totalVol = mass / density
      cylinderVol = totalVol * cylinderFrac
      ogiveVol = totalVol * (1 - cylinderFrac)
      (radius, ogiveLen, rho) = ogive ogiveVol ratio
  return Bullet { bulletCaliber = 2*radius
                , bulletCylinderLen = cylinderVol / (pi * radius^2)
                , bulletOgiveLen = ogiveLen
                , bulletRho = rho }

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
