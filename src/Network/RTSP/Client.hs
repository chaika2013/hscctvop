-- | TODO documentation
--

module Network.RTSP.Client
       ( runRtspSession
       , setUri
       , connect
       , communicate
       , close
       ) where

import Network.RTSP.Client.Connection
import Network.RTSP.Client.Session
