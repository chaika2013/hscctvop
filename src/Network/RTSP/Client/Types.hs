{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

-- | TODO documentation
--

module Network.RTSP.Client.Types where

import           Control.Applicative
import           Control.Concurrent.Async
import           Control.Concurrent.Chan
import           Control.Exception
import           Control.Monad.Base
import           Control.Monad.IO.Class
import           Control.Monad.State.Class
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.State
import qualified Data.ByteString              as S
import qualified Data.ByteString.Lazy         as L
import           Data.Typeable                (Typeable)
import           Network.HTTP.Types

-- | RTSP client monad.
--
newtype RtspClient a = RtspClient
                       { runRtspClient :: StateT Session (ResourceT IO) a
                       } deriving ( Functor, Applicative, Monad, MonadIO
                                  , MonadThrow, MonadResource
                                  , MonadState Session)

instance MonadBase IO RtspClient where
  liftBase = RtspClient . liftBase

-- | RTSP exceptions.
--
data RtspException = InvalidUrlException String String
                   | ResponseTimeout
                   | StatusCodeException Status
                   deriving (Show, Typeable)

instance Exception RtspException

-- | RTSP session data.
--
data Session = Session
               { host       :: S.ByteString
               -- ^ RTSP server address.
               --
               , port       :: Int
               -- ^ RTSP server port.
               --
               , path       :: S.ByteString
               -- ^ Everything after port including query string.
               --
               , reqCSeq    :: CSeq
               -- ^ Request numbering.
               --
               , connection :: Releasable Connection
               -- ^ Session connection functions.
               --
               , receiver   :: Releasable Receiver
               -- ^ Response receiver (or stream receiver if RTSP over TCP).
               --
               , responseTimeout :: Maybe Int
               -- ^ Response timeout in microseconds. Default value is 30
               -- seconds.
               --
               }

-- | Session connection to RTSP server.
--
data Connection = Connection
                  { connectionRead   :: IO S.ByteString
                  -- ^ Read function.
                  --
                  , connectionUnread :: S.ByteString -> IO ()
                  -- ^ Push data to stack for further reading.
                  --
                  , connectionWrite  :: S.ByteString -> IO ()
                  -- ^ Write function.
                  --
                  , connectionClose  :: IO ()
                  -- ^ Close connection.
                  --
                  }

-- | Receiver thread data with channel.
--
data Receiver = Receiver
                { receiverThread :: Async ()
                -- ^ Session receiver thread with response passing.
                --
                , receiverChan   :: Chan Response
                -- ^ Channel for passing responses from server.
                --
                }

-- | Request to server.
--
type Request = L.ByteString

-- | Reponse from session connection.
--
data Response = Response
                { resStatus  :: Status
                -- ^ Response status code.
                --
                , resHeaders :: ResponseHeaders
                -- ^ Response headers.
                --
                , resBody    :: Maybe S.ByteString
                -- ^ Response body.
                --
                , resCSeq    :: CSeq
                -- ^ Response sequence number.
                --
                } deriving (Show)

-- | RTP packet data.
--
data Packet = RtpPacket | RtcpPacket
            deriving (Show)

-- | Type for resource.
--
type Releasable a = Maybe (ReleaseKey, a)

-- | Simple types.
--
type CSeq = Int
