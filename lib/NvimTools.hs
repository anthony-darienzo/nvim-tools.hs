module NvimTools where

import System.FilePath.Glob ( compile, globDir1 )
import Data.MessagePack ( toObject, pack, unpack )
import System.Random ( randomRIO )
import Network.Socket 
  ( Socket
  , SocketType (Stream)
  , SockAddr (SockAddrUnix)
  , AddrInfo (AddrInfo)
  , Family (AF_UNIX)
  , defaultProtocol
  , connect
  , socket
  , close' )
import Network.Socket.ByteString.Lazy ( sendAll )
import Data.ByteString.Builder ( lazyByteString )

pathToSocket :: FilePath -> IO Socket
pathToSocket file = do
  let addr = SockAddrUnix file
  sock <- socket AF_UNIX Stream defaultProtocol
  connect sock addr
  return sock

{- | Given the TMPDIR, output a List of neovim socket paths -}
discoverNvims :: FilePath -> IO [ FilePath ]
discoverNvims = globDir1 (compile "nvim*/*")

execLuaCmdName :: String
execLuaCmdName = "nvim_execute_lua"

sendLuaCommand :: Socket -> String -> IO ()
sendLuaCommand s cmd = do
  msg_id <- randomRIO (0,aBigInt)
  let msg = pack 
        [ toObject (0 :: Int)
        , toObject msg_id
        , toObject execLuaCmdName
        , toObject [toObject cmd, toObject [()]] ]
  sendAll s msg
  close' s

luaUpdateCmdArg :: String
luaUpdateCmdArg = "require('ui.colors').UpdateAppearance()"

aBigInt :: Int
aBigInt = 2 ^ 15

{- | Given neovim instance, tell it to call UpdateAppearance -}
updateAndClose :: Socket -> IO ()
updateAndClose s = sendLuaCommand s luaUpdateCmdArg

getMoveCmd :: Integer -> String
getMoveCmd line = "vim.fn.cursor(" <> show line <> ",0,0)"

moveToLine :: Socket -> Integer -> IO ()
moveToLine s line = sendLuaCommand s $ getMoveCmd line

getConditionalCmd :: Integer -> FilePath -> String
getConditionalCmd line target_file = unwords
  [ "if vim.fn.expand('%:p') == \"" <> target_file <> "\" then"
  , getMoveCmd line
  , "end" ]

conditionalMoveToLine :: Socket -> Integer -> FilePath -> IO ()
conditionalMoveToLine s line target_file = sendLuaCommand s $ getConditionalCmd line target_file