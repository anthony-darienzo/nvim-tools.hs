module Main where

import NvimTools ( discoverNvims, discoverVimrs, pathToSocket, updateAndClose, prepareMsgPackCmd)
import System.Environment (getEnv)
import Control.Monad ( (>=>) )
{-
  f >=> g = \x -> (f x) >>= g
-}
import Data.Maybe ( catMaybes )

import qualified Data.ByteString.Lazy as BS

run :: (a -> Int -> b) -> Int -> [a] -> [b]
run f i0 [] = []
run f i0 [ x ] = [ f x i0 ]
run f i0 (x:xs) = f x i0 : run f (i0 + 1) xs

main :: IO ()
main = do
  putStrLn "RUNNING NEOVIM SOCKET TEST"
  tmpdir  <- getEnv "TMPDIR"
  nvim_sockets <- (discoverNvims >=> traverse pathToSocket >=> pure . catMaybes) tmpdir
  vimr_sockets <- (discoverVimrs >=> traverse pathToSocket >=> pure . catMaybes) tmpdir

  mapM_ updateAndClose nvim_sockets
  mapM_ updateAndClose vimr_sockets 

  putStrLn "Example Cmd"
  cmd <- prepareMsgPackCmd "ExampleCMD" []
  BS.putStr cmd
  putStrLn ""

  putStrLn "FINISHED NEOVIM SOCKET TEST"