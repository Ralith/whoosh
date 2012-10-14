module Whoosh where

import System.Random
import Control.Monad.State

data Gun = Gun deriving Show

main = do
  gen <- getStdGen
  putStrLn $ show $ evalState genGun gen

genGun :: State StdGen Gun
genGun = return Gun
