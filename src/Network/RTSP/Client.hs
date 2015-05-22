-- | TODO documentation
--

module Network.RTSP.Client
       ( runRtspSession
       , setUri
       , connect
       , close
       , sendRequest
       ) where

import Network.RTSP.Client.Connection
import Network.RTSP.Client.Request
import Network.RTSP.Client.Session
