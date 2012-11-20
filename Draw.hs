{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import System.Random
import Control.Monad.State

import Diagrams.Prelude
import Diagrams.TwoD.Arc
import Diagrams.Backend.Cairo.CmdLine

import Whoosh

main :: IO ()
main = do
  gen <- newStdGen
  defaultMain $ hcat $ map bullet $ evalState (replicateM 20 $ genMass >>= genBullet) gen

bullet (Bullet cal cylLen ogiveLen rho) =
    ((ogive (cal/2) rho ogiveLen cylLen) # fc black # lw 0) <> strutX (cal * 1.1)

dimension size = centerX $ (vrule (size/10) ||| hrule size ||| vrule (size/10)) <> strutX (size * 1.1)

ogive radius rho len baseDepth =
    -- dot norm((rho - radius, olen)) norm((rho, 0))
    let (x0, y0) = normalize (rho - radius, len)
        (x1, y1) = normalize (rho, 0)
        rarc = (bezierFromSweep (Rad $ acos (x0 * x1 + y0 * y1))) # scale rho
        larc = rarc # reflectX
        base = (straight $ r2 (0, -baseDepth))
             : (straight $ r2 (2*radius, 0))
             : (straight $ r2 (0, baseDepth))
             : []
    in pathLike (p2 (-radius, baseDepth)) True (base ++ rarc ++ map reverseSegment larc)

normalize :: Floating t => (t, t) -> (t, t)
normalize (x, y) = let len = sqrt (x^2 + y^2)
                   in (x/len, y/len)