module LiveshaderHS.Loop where

import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Data.Time.Clock
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import System.FSNotify

import LiveshaderHS.OpenGL
import LiveshaderHS.STMState
import LiveshaderHS.Types

liveshader :: MonadIO m => FilePath -> m ()
liveshader shaderDir = do
  makeWindow
  initOGL
  rs <- initRenderState shaderDir

  s <- liftIO (newTVarIO rs)
  recompileOnChange s shaderDir
  GLFW.windowSizeCallback $= updateWindowSize s

  t0 <- liftIO (getCurrentTime)
  void $ runSTMStateT s $ do
    rs <- get
    mapM_ clearBuffer (rs^.buffers)
    forever $ do
      (iTime, t) <- elapsedTime t0
      renderFrame iTime t

updateWindowSize :: TVar RenderState -> GL.Size -> IO ()
updateWindowSize s size = do
  GL.viewport $= ((GL.Position 0 0), size)
  atomically $ do
    rs <- readTVar s
    writeTVar s (set windowSize size rs)



elapsedTime :: MonadIO m => UTCTime -> m (Float, UTCTime)
elapsedTime t0 = do
  t <- liftIO getCurrentTime
  let iTime = diffUTCTime t t0
  pure (realToFrac iTime, t)

setDirty :: MonadIO m => TVar RenderState -> m ()
setDirty s = liftIO $ atomically $ do
  rs <- readTVar s
  writeTVar s (set dirty True rs)

recompileOnChange :: MonadIO m => TVar RenderState -> FilePath -> m ()
recompileOnChange s shaderDir = liftIO $ do
  mgr <- startManager
  void $ watchDir mgr shaderDir (const True) (const (setDirty s))
