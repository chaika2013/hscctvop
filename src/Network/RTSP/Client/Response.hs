{-# LANGUAGE OverloadedStrings #-}

-- | TODO documentation

module Network.RTSP.Client.Response where

import           Control.Applicative
import           Control.Concurrent.Chan
import           Control.Monad
import qualified Data.Attoparsec.ByteString       as A
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8            as C
import qualified Data.CaseInsensitive             as CI
import           Network.HTTP.Types

import           Network.RTSP.Client.Types

-- | Session connection receiver thread.
--
sessionReceiver :: Connection -> Chan Response -> IO ()
sessionReceiver cn chan = loop $ parse response
  where
    loop parse' = do
      bs <- connectionRead cn
      case parse' bs of
        
        Done rest r -> do
          connectionUnread cn rest
          case r of
            Right res -> writeChan chan $! res
            Left _pkt -> undefined
          loop $ parse response
          
        Partial cont -> do
          print ("Partial" :: String)
          loop cont

        Fail{} -> do
          -- TODO Throw exception to main thread!!!
          print ("Failure" :: String)
          undefined

-- | RTSP response parser.
--
-- If Left is returned, it means that we got RTP/RTCP interleaved packet instead
-- of response.
--
response :: Parser (Either Packet Response)
response = (Left <$> packet) <|> (Right <$> response')
  where

    -- | Parse RTSP response.
    --
    response' :: Parser Response
    response' = do
      status <- statusLine
      headers <- messageHeaders
      body <- responseBody headers
      case decimalHeader headers hCSeq of
        Nothing -> fail "CSeq not found in response"
        Just cseq ->
          return Response { resStatus = status
                          , resHeaders = headers
                          , resBody = body
                          , resCSeq = cseq
                          }

    isToken w =
      w <= 127 && A.notInClass "\0-\31()<>@,;:\\\"/[]?={} \t" w

    hCSeq = "CSeq"

    statusLine =
      Status
      <$> (string "RTSP/1.0 " *> decimal <* space)
      <*> (A.takeTill isEndOfLine <* endOfLine)

    messageHeader =
      (,)
      <$> (CI.mk <$> (A.takeWhile isToken <* char8 ':' <* A.skipWhile isHorizontalSpace))
      <*> (A.takeTill isEndOfLine <* endOfLine)
    messageHeaders = many messageHeader <* endOfLine

    responseBody hs = case decimalHeader hs hContentLength of
      Just len -> liftM Just $ A.take len
      Nothing -> return Nothing

    decimalHeader headers name =
      fst <$> join (C.readInt <$> lookup name headers)

    -- | Parse and return packet.
    --
    packet :: Parser Packet
    packet = fail "TODO parse packet RTP/RTCP packet"
