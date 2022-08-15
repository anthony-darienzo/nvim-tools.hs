module Extras where

import System.IO ( hPutStrLn, stderr )

nicePrefix :: String
nicePrefix = "nvim-tools: "

nicePutStrLn :: String -> IO ()
nicePutStrLn s = putStrLn $ nicePrefix <> s

niceErr :: String -> IO ()
niceErr s = hPutStrLn stderr $ nicePrefix <> s
