module NvimTools where

import System.FilePath.Glob ( compile, globDir1 )
import Data.MessagePack (toObject, pack, unpack)
import Network.Socket (
  Socket,
  SocketType (Stream),
  SockAddr (SockAddrUnix),
  AddrInfo (AddrInfo),
  Family (AF_UNIX),
  defaultProtocol,
  connect,
  socket, 
  close'
  )
import Network.Socket.ByteString.Lazy (sendAll)
import Data.ByteString.Builder (lazyByteString)

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

luaCmdArg :: String
luaCmdArg = "require('ui.colors').UpdateAppearance()"

{- | Given neovim instance, tell it to call UpdateAppearance -}
updateAndClose :: Socket -> Int -> IO ()
updateAndClose s msg_id = do
  let msg = pack 
        [ toObject (0 :: Int)
        , toObject msg_id
        , toObject execLuaCmdName
        , toObject [toObject luaCmdArg, toObject [()] ] ]
  sendAll s msg
  close' s