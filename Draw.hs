{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import System.Random
import Control.Monad.State
import Text.Printf

import Diagrams.Prelude
import Diagrams.TwoD.Arc
import Diagrams.Backend.SVG.CmdLine

import Whoosh

main :: IO ()
main = do
  gen <- newStdGen
  defaultMain $ hcat $ map (scale 10 . bullet) $ evalState (replicateM 100 $ genMass >>= genBullet >>= (frob genMV)) gen

frob :: Functor f => (a -> f b) -> a -> f (a, b)
frob = liftM2 fmap (,)

bullet ((Bullet cal cylLen ogiveLen rho mass), mv) = let magic = 500 in
    (scale magic $ (ogive (cal/2) rho ogiveLen cylLen) # fc black # lw 0)
    === (dimension (cal*magic) (printf "%0.2fmm" (cal*1000)))
    === (text (printf "%0.2fg" (mass * 1000)) # fc black <> strutY 1.3 <> strutX 3)
    === (text (printf "%0.2fm/s" mv) # fc black <> strutY 1.3 <> strutX 3)

dimension size annotation =
    ((centerX $ hrule size) <> strutY 1 <> strutX (size * 1.1)) # lw 0.5
    === (text annotation # fc black <> strutY 1.3 <> strutX (fromIntegral $ length annotation))

ogive radius rho len baseDepth =
    -- dot norm((rho - radius, olen)) norm((rho, 0))
    let (x0, y0) = normalize (rho - radius, len)
        (x1, y1) = normalize (rho, 0)
        rarc = (bezierFromSweep (Rad $ acos (x0 * x1 + y0 * y1))) # scale rho
        larc = rarc # reflectX
        base = [ straight $ r2 (0, -baseDepth)
               , straight $ r2 (2*radius, 0)
               , straight $ r2 (0, baseDepth)
               ]
    in pathLike (p2 (-radius, baseDepth)) True (base ++ rarc ++ map reverseSegment larc)

normalize :: Floating t => (t, t) -> (t, t)
normalize (x, y) = let len = sqrt (x^2 + y^2)
                   in (x/len, y/len)