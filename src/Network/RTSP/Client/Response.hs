-- | TODO documentation

module Network.RTSP.Client.Response where

import           Control.Concurrent.Chan
import           Data.Attoparsec.ByteString

import           Network.RTSP.Client.Types

import           Control.Monad

-- | Session connection receiver thread.
--
sessionReceiver :: Connection -> Chan Response -> IO ()
sessionReceiver conn chan = forever $ do
  bs <- connectionRead conn
  case parse response bs of
    Done rest res -> do
      connectionUnread conn rest
      print res
    Partial c -> undefined
    Fail _ _ _ -> undefined -- here we have an error, something must be passed
                            -- to main thread!!!

-- | RTSP response parser. If Nothing is returned, it means that we got
-- RTP/RTCP interleaved packet instead of response.
--
response :: Parser (Either RtpPacket Response)
response = undefined
