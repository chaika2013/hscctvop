{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | TODO documentation
--

module Network.RTSP.Client.Session where

import           Control.Monad.State
import           Control.Monad.Trans.Resource
import qualified Data.ByteString.Char8        as C
import           Data.Default                 (Default (..), def)
import           Network.HTTP.Types
import           Network.URI                  (URI (..), URIAuth (..),
                                               escapeURIString, isAllowedInURI,
                                               parseURI)
import Control.Concurrent.Chan
import System.Timeout

import           Network.RTSP.Client.Request
import           Network.RTSP.Client.Types
import           Network.RTSP.Client.Utils

instance Default Session where
  def = Session
        { host = "localhost"
        , port = 554
        , path = "/"
        , reqCSeq = 1
        , connection = Nothing
        , receiver = Nothing
        , responseTimeout = Just 30000000
        }

instance Show Session where
  show ss = unlines
            [ "Session {"
            , "  host        = " ++ show (host ss)
            , "  port        = " ++ show (port ss)
            , "  path        = " ++ show (path ss)
            , "}"
            ]

-- | Run RTSP session.
runRtspSession :: RtspClient a -> IO a
runRtspSession = runResourceT . flip evalStateT def . runRtspClient

-- | Setup new 'URI' for communication.
--
setUri :: String -> RtspClient ()
setUri s =
  case parseURI (encode s) of
    Just uri -> do
      auth <- parseAuth uri
      port' <- parsePort auth
      path' <- parsePath uri
      modify $ \ss -> ss { host = C.pack $ uriRegName auth
                         , port = port'
                         , path = path' }
    Nothing -> failUri "Invalid URL"
  where
    encode = escapeURIString isAllowedInURI

    parseAuth URI{uriAuthority = auth'} =
      case auth' of
        Just auth -> do
          unless (null $ uriUserInfo auth) $
            failUri "URL auth not supported; use applyBasicAuth instead"
          return auth
        Nothing -> failUri "URL must be absolute"

    parsePort URIAuth{uriPort = port'} =
      case port' of
        [] -> return 554
        (':':rest) -> case readDec rest of
          Just port'' -> return port''
          Nothing -> failUri "Invalid port"
        _ -> failUri "Invalid port"

    parsePath URI{uriPath = path', uriQuery = query', uriFragment = frag'} =
      return . C.concat . map C.pack $ [path', query', frag']

    failUri = throwM . InvalidUrlException s

-- | Communicate with server.
--
-- Send request and receive response from server. Response is received with
-- timeout. Default timeout value is 60 seconds.
--
communicate :: Method -> RtspClient Response
communicate method = do
  chan <- liftM (receiverChan . component' . receiver) get
  wait <- liftM (notToNeg . responseTimeout) get
  res' <- sendRequest method >>= liftIO . timeout wait . readResponse chan
  case res' of
    Just res -> case resStatus res of
      Status 200 _ -> return res
      s -> throwM $ StatusCodeException s
    Nothing ->
      throwM ResponseTimeout
  where
    readResponse chan cseq = do
      res <- readChan chan
      if resCSeq res == cseq then return res
        else readResponse chan cseq
  
