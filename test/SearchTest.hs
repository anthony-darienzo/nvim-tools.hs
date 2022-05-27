{- | Test to see if globbing for neovim sockets works -}
module Main where

import System.Environment (getEnv)
import NvimTools ( discoverNvims )

main :: IO ()
main = do
  putStrLn "BEGINNING NEOVIM SEARCH TEST"

  tmpdir  <- getEnv "TMPDIR"
  nvims   <- discoverNvims tmpdir
  putStr "Discovered the following neovim instances: "
  if null nvims
    then putStrLn "No instances found!"
    else mapM_ putStrLn nvims

  putStrLn "END NEOVIM SEARCH TEST"