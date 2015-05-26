-- | TODO documentation

module Network.RTSP.Client.Utils where

import           Control.Monad.Trans.Resource (release)
import qualified Data.ByteString.Char8        as C
import           Data.Monoid                  (Monoid (..), mappend)

import           Network.RTSP.Client.Types

-- | Read int from string.
--
readDec :: String -> Maybe Int
readDec = fmap fst . C.readInt . C.pack

-- | Helper function to release releasable component.
--
release' :: Releasable a -> IO ()
release' Nothing = return ()
release' (Just (key, _)) = release key

-- | Helper function to get resource from releasable component.
--
component' :: Releasable a -> a
component' Nothing = undefined    -- we must check if component exists before
                                  -- calling this function
component' (Just (_, comp)) = comp

-- | Helper function converts Nothing to -1.
--
notToNeg :: Maybe Int -> Int
notToNeg (Just d) = d
notToNeg Nothing = -1

-- | Monoid append helper.
--
infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
