module NvimTools where

import System.FilePath.Glob ( compile, globDir1 )
import System.Posix.Files ( fileAccess, fileExist )
import System.FilePath ( takeFileName )

import Data.Vector ( (!?) )
import Data.Int (Int64)
import Data.ByteString.Lazy ( ByteString )

import Data.MessagePack 
  ( MessagePack
  , Object, toObject, defaultConfig, fromObject
  , pack, unpack )

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
  , close'
  , getPeerName )
import Network.Socket.ByteString.Lazy ( sendAll, recv )

import Extras (nicePutStrLn, niceErr)
import Control.Monad ( void )

data RPCResponse = Reply 
  { restype :: Int64
  , msgid :: Int64
  , body :: Object } deriving Show

pathToSocket :: FilePath -> IO (Maybe Socket)
pathToSocket file = do
  nicePutStrLn $ "Attempting to connect to socket at path: " <> file
  nicePutStrLn "Checking existence"
  exists <- fileExist file
  has_exec <- if exists
    then fileAccess file False False True
    else return False
  nicePutStrLn "Checking permissions"
  if not has_exec
    then do
        nicePutStrLn ( "Permission check failed. Skipping socket connection for " <> file )
        return Nothing
    else do
      nicePutStrLn "Permission check passed. Proceeding!"
      let addr = SockAddrUnix file
      sock <- socket AF_UNIX Stream defaultProtocol
      connect sock addr
      return (Just sock)

discoverNvims :: FilePath -> IO [ FilePath ]
discoverNvims = globDir1 (compile "nvim.*.0")

discoverVimrs :: FilePath -> IO [ FilePath ]
discoverVimrs = globDir1 (compile "vimr*.sock")

{- | Given the TMPDIR, output a List of neovim socket paths -}
discoverSockets tmp = do
    nvims <- discoverNvims tmp
    vimrs <- discoverVimrs tmp
    return $ nvims <> vimrs

aBigInt :: Int
aBigInt = 2 ^ 15

execLuaCmdName :: String
execLuaCmdName = "nvim_execute_lua"

luaUpdateCmdArg :: String
luaUpdateCmdArg = "require('ui.colors').UpdateAppearance()"

getMoveCmd :: Integer -> String
getMoveCmd line = "vim.fn.cursor(" <> show line <> ",0,0)"

getConditionalCmd :: Integer -> FilePath -> String
getConditionalCmd line target_file = unwords
  [ "require('notify') [[Incoming moveCursor request for file: " <> name <> ".]]"
  , "if vim.fn.expand('%:p') == \"" <> target_file <> "\" then"
  , getMoveCmd line
  , "end" ]
  where name = takeFileName target_file

obj :: (MessagePack a) => a -> Object
obj = toObject defaultConfig

prepareMsgPackCmd :: String -> [Object] -> IO (ByteString, Int)
prepareMsgPackCmd cmd args = do
  msg_id <- randomRIO (0,aBigInt)
  let msgtype = obj (0 :: Int)
      msgid   = obj msg_id
      method  = obj execLuaCmdName
      params  = obj $ obj cmd : args
  let msg = pack [msgtype,  msgid,  method, params]
  return (msg, msg_id)

voidCmd :: String -> IO (ByteString, Int)
voidCmd cmd = prepareMsgPackCmd cmd [ obj [()] ]

resDecode :: ByteString -> Maybe RPCResponse 
resDecode bs = do
  objs <- unpack bs
  r <- objs !? 0
  m <- objs !? 1
  b <- objs !? 2
  rt <- fromObject r
  mi <- fromObject m
  bd <- fromObject b
  return $ Reply rt mi bd

parseNeovimResponse :: RPCResponse -> Maybe String
parseNeovimResponse r = do
  x <- fromObject . body $ r
  -- neovim returns an array, the 2nd element is the message. (1st is status?)
  s <- fromObject =<< x !? 1
  return s

req :: Socket -> String -> Int64 -> IO (Maybe RPCResponse)
req s cmd maxl = do
  (msg,id) <- voidCmd cmd
  sendAll s msg
  sname <- getPeerName s
  res   <- recv s maxl
  close' s
  return $ resDecode res

sendLuaCommand :: Socket -> String -> IO ()
sendLuaCommand s cmd = do
  sname <- getPeerName s
  nicePutStrLn $
    "Sending a Lua command to socket with name: " <> show sname
  void $ req s cmd 4096

sendLuaCommandVerbose :: Socket -> String -> IO ()
sendLuaCommandVerbose s cmd = do
  sname <- getPeerName s
  nicePutStrLn $
    "Sending a Lua command to socket with name: " <> show sname
  bs <- req s cmd 4096
  case parseNeovimResponse =<< bs of
    Nothing -> return ()
    Just s  -> putStrLn s 

{- | Given neovim instance, tell it to call UpdateAppearance -}
updateAndClose :: Socket -> IO ()
updateAndClose s = sendLuaCommandVerbose s luaUpdateCmdArg

moveToLine :: Socket -> Integer -> IO ()
moveToLine s line = sendLuaCommand s $ getMoveCmd line

conditionalMoveToLine :: Socket -> Integer -> FilePath -> IO ()
conditionalMoveToLine s line target_file = sendLuaCommandVerbose s $ getConditionalCmd line target_file
