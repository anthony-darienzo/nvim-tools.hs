module Main (main) where

import NvimTools
  ( discoverSockets
  , pathToSocket
  , updateAndClose
  , moveToLine
  , conditionalMoveToLine )
import System.Environment ( getEnv, getArgs )
import Extras ( nicePutStrLn, niceErr )
import Control.Monad ( (>=>), unless )
{-
  f >=> g == \x -> (f x) >>= g
-}
import Text.Read ( readMaybe )
import Data.Maybe (catMaybes, isNothing)

{- | Find neovim instances, tell them all to call UpdateAppearance -}
updateAppearance :: IO ()
updateAppearance = do
    tmpdir  <- getEnv "XDG_RUNTIME_DIR"
    nicePutStrLn tmpdir
    maybe_sockets <- (discoverSockets >=> traverse pathToSocket) tmpdir
    let sockets = catMaybes maybe_sockets
    if null sockets
        then nicePutStrLn "No neovim instances found!"
        else
            mapM_ updateAndClose sockets
            >> nicePutStrLn "Updated neovims!"

{- | Move cursor to line. If asSocket is true, path is a unix socket path to a
 - neovim instance. Otherwise, path is the name of the filename to search for. -}
moveCursor :: String -> FilePath -> Bool -> IO ()
moveCursor line_arg path asSocket =
  case (readMaybe line_arg :: Maybe Integer) of
    Nothing -> niceErr $
      "In moveCursor, failed to identify desired line number. Received: "
        <> show line_arg
    Just n -> if asSocket
      then do
        socket <- pathToSocket path
        maybe ( nicePutStrLn "No update" ) (`moveToLine` n) socket -- if socket is not Nothing. There must be a better way
      else do
        tmpdir  <- getEnv "TMPDIR"
        maybe_sockets <- (discoverSockets >=> traverse pathToSocket) tmpdir
        let sockets = catMaybes maybe_sockets
        if null sockets
          then nicePutStrLn "No neovim instances found!"
          else
            mapM_ (\ s -> conditionalMoveToLine s n path) sockets
            >> nicePutStrLn "Updated neovims!"

printHelpDialog :: IO ()
printHelpDialog = putStr $ unlines
  [ "nvim-update: neovim RPC update tools"
  , ""
  , "Using neovim's RPC API, this command does two things:"
  , "    - \"nvim-tools updateAppearance\" will instruct all neovim instances to"
  , "        update their colorscheme."
  , "    - \"nvim-tools moveCursor <LINE> --socket <SOCKET_PATH>\" will tell the "
  , "        neovim instance at SOCKET_PATH to move the cursor to LINE."
  , "    - \"nvim-tools moveCursor <LINE> --file <FILENAME>\" will tell all neovim"
  , "        instances currently viewing FILENAME to move the cursor to LINE."
  , "    - \"nvim-tools --help\" displays this dialog."
  , "    - \"nvim-tools \" will also display this dialog."
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

    _ -> niceErr $ "Invalid command \"" <> unwords args <> "\". Use flag --help for help."
