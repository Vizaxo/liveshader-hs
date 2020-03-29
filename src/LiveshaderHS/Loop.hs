module LiveshaderHS.Loop where

import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Data.Time.Clock
import System.FSNotify

import LiveshaderHS.OpenGL
import LiveshaderHS.STMState
import LiveshaderHS.Types

liveshader :: FilePath -> IO ()
liveshader shaderDir = do
  makeWindow
  rs <- initOGL shaderDir

  s <- newTVarIO rs
  recompileOnChange s shaderDir

  t0 <- getCurrentTime
  void $ runSTMStateT s $ forever $ do
    dt <- elapsedTime t0
    renderFrame dt

elapsedTime :: MonadIO m => UTCTime -> m Float
elapsedTime t0 = do
  t <- liftIO getCurrentTime
  let dt = diffUTCTime t0 t
  pure (realToFrac dt)

setDirty :: TVar RenderState -> IO ()
setDirty s = atomically $ do
  rs <- readTVar s
  writeTVar s (set dirty True rs)

recompileOnChange :: TVar RenderState -> FilePath -> IO ()
recompileOnChange s shaderDir = do
  mgr <- startManager
  void $ watchDir mgr shaderDir (const True) (const (setDirty s))
