module Distributions where

import System.Random

data Distribution
    = Normal Rational Rational
    | LogNormal Rational Rational Rational Rational
    | Gibbs [(Rational, Distribution)]

mean :: Floating a => Distribution -> a
mean (Normal mu _) = fromRational mu
mean (LogNormal mu sigma z c)
    = (fromRational z) + (fromRational c) * exp ((fromRational mu) + (fromRational sigma)**2/2)
mean (Gibbs ds) = foldr (\(weight, d) -> (+ (fromRational weight) * (mean d))) 0 ds

median :: Floating a => Distribution -> a
median (Normal mu _) = fromRational mu
median (LogNormal mu _ z c) = (fromRational z) + (fromRational c) * exp (fromRational mu)
median (Gibbs ds) = foldr (\(weight, d) -> (+ (fromRational weight) * (median d))) 0 ds -- TODO: Verify

-- mode :: Floating a => Distribution -> a
-- mode (Normal mu _) = fromRational mu
-- mode (LogNormal mu sigma) = exp ((fromRational mu) - (fromRational sigma)**2)
-- mode (Gibbs ds) = 

pdf :: Floating a => Distribution -> a -> a
pdf (Normal mu sigma) x
    = exp (- (x - (fromRational mu))**2/(2*(fromRational sigma)**2))
      / ((fromRational sigma) * sqrt (2*pi))
pdf (LogNormal mu sigma z c) x
    = exp (- (log ((x - fromRational z)/(fromRational c)) - (fromRational mu))**2/(2*(fromRational sigma)**2))
      / ((fromRational c) * ((x - fromRational z)/(fromRational c)) * sqrt (2*pi*(fromRational sigma)**2))
pdf (Gibbs ds) x = foldl (\accum (weight, d) -> accum + (fromRational weight) * pdf d x) 0 ds

-- Generate a Gaussian (0, 1) variate.
boxMuller :: (RandomGen g, Random a, Floating a) => g -> (a, g)
boxMuller gen = (sqrt (-2 * log u1) * cos (2 * pi * u2), gen'')
    where (u1, gen')  = randomR (0, 1) gen
          (u2, gen'') = randomR (0, 1) gen'

sample :: (RandomGen g, Floating a, Random a) => Distribution -> g -> (a, g)
sample (Normal mu sigma) g
    = let (x, g') = boxMuller g in
      ((fromRational mu)+x*(fromRational sigma), g')
sample (LogNormal mu sigma z c) g
    = let (x, g') = boxMuller g in
      ((fromRational z) + (fromRational c) * exp ((fromRational mu) + (fromRational sigma) * x), g')
sample (Gibbs ds) g
    = let (x, g') = randomR (0, 1) g in
      sample (pick x ds) g'
    where
      pick :: Float -> [(Rational, Distribution)] -> Distribution
      pick x ((weight, d):ds)
          | x <= (fromRational weight) = d
          | otherwise                  = pick (x - fromRational weight) ds
