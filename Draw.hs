module Main where

import System.Random
import Control.Monad.State

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

import Whoosh

main :: IO ()
main = do
  gen <- newStdGen
  defaultMain $ scale 50 $ foldr1 (|||) $ map bullet $ evalState (replicateM 10 $ genMass >>= genBullet) gen

bullet (Bullet cal cylLen ogiveLen rho) =
    (ogive (cal/2) rho ogiveLen) === (rect cal cylLen)

ogive radius rho len =
    -- dot norm((rho - radius, olen)) norm((rho, 0))
    let (x0, y0) = normalize (rho - radius, len)
        (x1, y1) = normalize (rho, 0)
        rarc = (arc (Rad 0) (Rad $ acos (x0 * x1 + y0 * y1)))
               # lineCap LineCapRound
               # scale rho
               # translateX (radius - rho)
        larc = scaleX (-1) rarc
    in larc ||| rarc

normalize :: Floating t => (t, t) -> (t, t)
normalize (x, y) = let len = sqrt (x^2 + y^2)
                   in (x/len, y/len)