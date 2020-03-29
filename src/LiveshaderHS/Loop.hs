module LiveshaderHS.Loop where

import Control.Monad.State
import Data.Time.Clock

import LiveshaderHS.OpenGL

liveshader :: IO ()
liveshader = do
  makeWindow
  rs <- initOGL
  t0 <- getCurrentTime
  void $ flip runStateT rs $ forever $ do
    dt <- elapsedTime t0
    renderFrame dt

elapsedTime :: MonadIO m => UTCTime -> m Float
elapsedTime t0 = do
  t <- liftIO getCurrentTime
  let dt = diffUTCTime t0 t
  pure (realToFrac dt)
