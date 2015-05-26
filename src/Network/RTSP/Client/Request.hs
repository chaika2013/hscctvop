{-# LANGUAGE OverloadedStrings #-}

-- | TODO documentation

module Network.RTSP.Client.Request where

import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8     as C
import qualified Data.ByteString.Lazy      as L
import qualified Data.CaseInsensitive      as CI
import           Data.Monoid               hiding ((<>))
import           Network.HTTP.Types

import           Network.RTSP.Client.Types
import           Network.RTSP.Client.Utils

-- | Send request to server.
--
sendRequest :: Method -> RtspClient CSeq
sendRequest method = get >>= \ss -> do
  let send = connectionWrite . component' $ connection ss
  let req = makeRequest method ss
  liftIO . mapM_ send . L.toChunks $ req
  updateCSeq

-- | Update current cseq and return previous one.
--
updateCSeq :: RtspClient CSeq
updateCSeq = get >>= \ss -> do
  let cseq' = reqCSeq ss
  put $ ss{reqCSeq = cseq' + 1}
  return cseq'

-- | Make request.
--
makeRequest :: Method -> Session -> Request
makeRequest method ss =
  toLazyByteString request
  where
    request :: Builder
    request =
      byteString method
      <> byteString " "
      <> requestUri
      <> byteString " RTSP/1.0\r\n"
      <> requestHeaders
      <> headerPair ("CSeq", C.pack . show $ reqCSeq ss)
      <> byteString "\r\n"

    requestUri =
      requestProtocol
      <> byteString (host ss)
      <> requestPort
      <> requestPath

    requestProtocol =
      byteString "rtsp://"

    requestPort
      | port ss == 554 = mempty
      | otherwise = byteString ":"
                    <> byteString (C.pack . show $ port ss)

    requestPath = byteString (path ss)

    requestHeaders = mempty

    headerPair (k, v) =
      byteString (CI.original k)
      <> byteString ": "
      <> byteString v
      <> byteString "\r\n"
