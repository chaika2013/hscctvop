{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Network             (withSocketsDo)
import           Network.RTSP.Client

import Control.Monad.IO.Class
import Control.Concurrent

import Control.Exception

main :: IO ()
main = withSocketsDo $
  handle (\e -> print (e :: SomeException)) $ runRtspSession $ do
    setUri "rtsp://172.16.7.21/axis-media/media.amp"
    connect

    communicate "DESCRIBE" >>= liftIO . print
    
--    liftIO $ threadDelay 1000000

    close
