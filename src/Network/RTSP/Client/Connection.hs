{-# LANGUAGE ScopedTypeVariables #-}

-- | TODO documentation

module Network.RTSP.Client.Connection where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Control.Monad.Trans.Resource
import qualified Data.ByteString              as S
import qualified Data.ByteString.Char8        as C
import           Data.IORef
import           Network.Socket               (AddrInfo (..), AddrInfoFlag (..),
                                               Socket (..), SocketType (..),
                                               defaultHints)
import qualified Network.Socket               as NS
import           Network.Socket.ByteString    (recv, sendAll)

import           Network.RTSP.Client.Response
import           Network.RTSP.Client.Types
import           Network.RTSP.Client.Utils

-- | Open RTSP server connection, initialize session with connection.
--
connect :: RtspClient ()
connect = do
  -- TODO check for connection is closed

  srv_host <- liftM (Just . C.unpack . host) get
  srv_port <- liftM (Just . show . port) get
  addrs <- liftIO $ NS.getAddrInfo hints srv_host srv_port

  conn'@(_, conn) <- allocate (newConnection addrs) closeConnection
  recv' <- allocate (newReceiver conn) closeReceiver

  modify $ \ss -> ss { connection = Just conn'    -- update session
                     , receiver = Just recv' }
  where
    hints = Just defaultHints { addrFlags = [AI_ADDRCONFIG]
                                 , addrSocketType = Stream }

close :: RtspClient ()
close = do
  get >>= liftIO. release' . connection
  get >>= liftIO. release' . receiver
  modify $ \ss -> ss { connection = Nothing       -- update session
                     , receiver = Nothing }

-- | Create new connection to server, using list of addresses.
--
newConnection :: [NS.AddrInfo] -> IO Connection
newConnection addrs =
  tryConnect addrs $ \addr@AddrInfo{addrFamily = a, addrSocketType = t, addrProtocol = p} ->
  bracketOnError (NS.socket a t p) NS.sClose $ \s -> socketConnection s addr 4096
  where
    -- | Attempt to make IO operation on address list.
    tryConnect :: [AddrInfo] -> (AddrInfo -> IO a) -> IO a
    tryConnect [] _ = undefined       -- here we got undefined because list from
                                      -- getAddrInfo can not be empty
    tryConnect (a:as) cb =
      cb a `catch` \(e :: IOException) -> case as of
        [] -> throwIO e
        _ -> tryConnect as cb

-- | Make connection with stack from read, write and close functions.
--
makeConnection :: IO S.ByteString         -- ^ read
               -> (S.ByteString -> IO ()) -- ^ write
               -> IO ()                   -- ^ close
               -> IO Connection
makeConnection r w c = do
  istack <- newIORef []
  _ <- mkWeakIORef istack c
  return $! Connection { connectionRead = join $ atomicModifyIORef istack $ \stack ->
                          case stack of
                            x:xs -> (xs, return x)
                            [] -> ([], r)
                       , connectionUnread =  -- unread only if string not null
                            \x -> unless (S.null x) $ atomicModifyIORef istack $ \stack -> (x:stack, ())
                       , connectionWrite = w
                       , connectionClose = c
                       }

-- | Create socket connection.
--
socketConnection :: Socket -> AddrInfo -> Int -> IO Connection
socketConnection sc addr chunksize = do
  NS.setSocketOption sc NS.NoDelay 1
  NS.connect sc (addrAddress addr)
  makeConnection
    (recv sc chunksize)
    (sendAll sc)
    (NS.sClose sc)

-- | Close connection to server.
--
closeConnection :: Connection -> IO ()
closeConnection = connectionClose

-- | Create receiver thread.
--
newReceiver :: Connection -> IO Receiver
newReceiver conn = do
  chan <- newChan
  parent <- myThreadId -- linking current thread for exceptions
  thread <- async $ sessionReceiver parent conn chan
  return Receiver { receiverThread = thread
                  , receiverChan = chan }

-- | Kill receiver thread.
--
closeReceiver :: Receiver -> IO ()
closeReceiver = cancel . receiverThread
