module Distributions where

import System.Random

data Distribution a
    = Normal a a
    | LogNormal a a a a
    | Gibbs [(Double, Distribution a)]

mean :: Floating a => Distribution a -> a
mean (Normal mu _) = mu
mean (LogNormal mu sigma z c)
    = z + c * exp (mu + sigma**2/2)
mean (Gibbs ds) = foldr (\(weight, d) -> (+ realToFrac weight * mean d)) 0 ds

median :: Floating a => Distribution a -> a
median (Normal mu _) = mu
median (LogNormal mu _ z c) = z + c * exp mu
median (Gibbs ds) = foldr (\(weight, d) -> (+ realToFrac weight * median d)) 0 ds -- TODO: Verify

-- mode :: Floating a => Distribution -> a
-- mode (Normal mu _) = fromDouble mu
-- mode (LogNormal mu sigma) = exp (mu - sigma**2)
-- mode (Gibbs ds) = 

pdf :: Floating a => Distribution a -> a -> a
pdf (Normal mu sigma) x
    = exp (- (x - mu)**2/(2*sigma**2))
      / (sigma * sqrt (2*pi))
pdf (LogNormal mu sigma z c) x
    = exp (- (log ((x - z)/c) - mu)**2/(2*sigma**2))
      / (c * ((x - z)/c) * sqrt (2*pi*sigma**2))
pdf (Gibbs ds) x = foldl (\accum (weight, d) -> accum + realToFrac weight * pdf d x) 0 ds

-- Generate a Gaussian (0, 1) variate.
boxMuller :: (RandomGen g, Random a, Floating a) => g -> (a, g)
boxMuller gen = (sqrt (-2 * log u1) * cos (2 * pi * u2), gen'')
    where (u1, gen')  = randomR (0, 1) gen
          (u2, gen'') = randomR (0, 1) gen'

sample :: (Floating a, Random a, RandomGen g) => Distribution a -> g -> (a, g)
sample (Normal mu sigma) g
    = let (x, g') = boxMuller g in
      (mu+x*sigma, g')
sample (LogNormal mu sigma z c) g
    = let (x, g') = boxMuller g in
      (z + c * exp (mu + sigma * x), g')
sample (Gibbs ds) g
    = let (x, g') = randomR (0, 1) g in
      sample (pick x ds) g'
    where
      pick :: Double -> [(Double, Distribution a)] -> Distribution a
      pick x ((weight, d):ds)
          | x <= weight = d
          | otherwise   = pick (x - weight) ds
