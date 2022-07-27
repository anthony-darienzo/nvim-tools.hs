module Main where

import NvimTools ( discoverNvims, pathToSocket, updateAndClose)
import System.Environment (getEnv)
import Control.Monad ( (>=>) )
{-
  f >=> g = \x -> (f x) >>= g
-}

run :: (a -> Int -> b) -> Int -> [a] -> [b]
run f i0 [] = []
run f i0 [ x ] = [ f x i0 ]
run f i0 (x:xs) = f x i0 : run f (i0 + 1) xs

main :: IO ()
main = do
  putStrLn "RUNNING NEOVIM SOCKET TEST"
  tmpdir  <- getEnv "TMPDIR"
  sockets <- (discoverNvims >=> traverse pathToSocket) tmpdir

  mapM_ updateAndClose sockets

  putStrLn "FINISHED NEOVIM SOCKET TEST"