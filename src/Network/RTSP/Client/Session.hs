{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | TODO documentation
--

module Network.RTSP.Client.Session where

import           Control.Monad.State
import           Control.Monad.Trans.Resource
import qualified Data.ByteString.Char8        as C
import           Data.Default                 (Default (..), def)
import           Network.URI                  (URI (..), URIAuth (..),
                                               escapeURIString, isAllowedInURI,
                                               parseURI)

import           Network.RTSP.Client.Utils
import           Network.RTSP.Client.Types

instance Default Session where
  def = Session
        { scheme = RTSP
        , host = "localhost"
        , port = 554
        , path = "/"
        , cseq = 1
        , connection = Nothing
        , receiver = Nothing
        }

instance Show Session where
  show ss = unlines
            [ "Session {"
            , "  scheme      = " ++ show (scheme ss)
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
      scheme' <- parseScheme uri
      auth <- parseAuth uri
      port' <- parsePort scheme' auth
      path' <- parsePath uri
      modify $ \ss -> ss { scheme = scheme'
                         , host = C.pack $ uriRegName auth
                         , port = port'
                         , path = path' }
    Nothing -> failUri "Invalid URL"
  where
    encode = escapeURIString isAllowedInURI

    parseScheme URI{uriScheme = scheme'} =
      case scheme' of
        "rtsp:" -> return RTSP
        "http:" -> return RTSP_OVER_HTTP
        _ -> failUri "Unsupported scheme"

    parseAuth URI{uriAuthority = auth'} =
      case auth' of
        Just auth -> do
          unless (null $ uriUserInfo auth) $
            failUri "URL auth not supported; use applyBasicAuth instead"
          return auth
        Nothing -> failUri "URL must be absolute"

    parsePort scheme' URIAuth{uriPort = port'} =
      case port' of
        [] -> return $ defaultPort scheme'
        (':':rest) -> case readDec rest of
          Just port'' -> return port''
          Nothing -> failUri "Invalid port"
        _ -> failUri "Invalid port"

    parsePath URI{uriPath = path', uriQuery = query', uriFragment = frag'} =
      return . C.concat . map C.pack $ [path', query', frag']

    defaultPort scheme' =
      case scheme' of
        RTSP -> 554
        RTSP_OVER_HTTP -> 80

    failUri = throwM . InvalidUrlException s


