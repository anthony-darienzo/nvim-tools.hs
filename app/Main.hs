module Main (main) where

import NvimTools ( discoverNvims, pathToSocket, updateAndClose)

import System.Environment (getEnv)
import Control.Monad ( (>=>) )
{-
  f >=> g == \x -> (f x) >>= g
-}

run :: (a -> Int -> b) -> Int -> [a] -> [b]
run f i0 [] = []
run f i0 [ x ] = [ f x i0 ]
run f i0 (x:xs) = f x i0 : run f (i0 + 1) xs

{- | Find neovim instances, tell them all to call UpdateAppearance -}
main :: IO ()
main = do
    tmpdir  <- getEnv "TMPDIR"
    sockets <- (discoverNvims >=> traverse pathToSocket) tmpdir
    if null sockets
        then putStrLn "No neovim instances found!"
        else
            sequence_ (run updateAndClose 0 sockets) 
            >> putStrLn "Updated neovims!"