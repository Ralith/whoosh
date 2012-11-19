import System.Random
import Control.Monad.State

import Whoosh

main :: IO ()
main = do
  gen <- newStdGen
  putStrLn $ show $ evalState genGun gen
  putStrLn $ show $ evalState (genBullet 0.01) gen
