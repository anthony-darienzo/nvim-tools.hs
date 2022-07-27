module Main (main) where

import NvimTools
  ( discoverNvims
  , pathToSocket
  , updateAndClose
  , moveToLine
  , conditionalMoveToLine )
import System.Environment ( getEnv, getArgs )
import System.IO ( hPutStrLn, stderr )
import Control.Monad ( (>=>) )
{-
  f >=> g == \x -> (f x) >>= g
-}
import Text.Read ( readMaybe )

{- | Find neovim instances, tell them all to call UpdateAppearance -}
updateAppearance :: IO ()
updateAppearance = do
    tmpdir  <- getEnv "TMPDIR"
    sockets <- (discoverNvims >=> traverse pathToSocket) tmpdir
    if null sockets
        then hPutStrLn stderr "No neovim instances found!"
        else
            mapM_ updateAndClose sockets
            >> putStrLn "Updated neovims!"

{- | Move cursor to line. If asSocket is true, path is a unix socket path to a
 - neovim instance. Otherwise, path is the name of the filename to search for. -}
moveCursor :: String -> FilePath -> Bool -> IO ()
moveCursor line_arg path asSocket =
  case (readMaybe line_arg :: Maybe Integer) of
    Nothing -> hPutStrLn stderr $
      "In moveCursor, failed to identify desired line number. Received: "
      <> show line_arg
    Just n -> if asSocket
      then do
        socket <- pathToSocket path
        moveToLine socket n
      else do
        tmpdir  <- getEnv "TMPDIR"
        sockets <- (discoverNvims >=> traverse pathToSocket) tmpdir
        if null sockets
          then hPutStrLn stderr "No neovim instances found!"
          else
            mapM_ (\ s -> conditionalMoveToLine s n path) sockets
            >> putStrLn "Updated neovims!"

printHelpDialog :: IO ()
printHelpDialog = putStr $ unlines
  [ "nvim-update: neovim RPC update tools"
  , ""
  , "Using neovim's RPC API, this command does two things:"
  , "    - \"nvim-update updateAppearance\" will instruct all neovim instances to"
  , "        update their colorscheme."
  , "    - \"nvim-update moveCursor <LINE> --socket <SOCKET_PATH>\" will tell the "
  , "        neovim instance at SOCKET_PATH to move the cursor to LINE."
  , "    - \"nvim-update moveCursor <LINE> --file <FILENAME>\" will tell all neovim"
  , "        instances currently viewing FILENAME to move the cursor to LINE."
  , "    - \"nvim-update --help\" displays this dialog."
  , "    - \"nvim-update\" will also display this dialog."
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    []          -> printHelpDialog
    ["--help"]  -> printHelpDialog

    ["updateAppearance"] -> updateAppearance

    ["moveCursor", line, "--socket", socket_path]   -> moveCursor line socket_path True
    ["moveCursor", line, "--file", filename]        -> moveCursor line filename False

    _ -> hPutStrLn stderr $ "Invalid command \"" <> unwords args <> "\". Use flag --help for help."