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

liveshader :: FilePath -> IO ()
liveshader shaderDir = do
  makeWindow
  rs <- initOGL shaderDir

  s <- newTVarIO rs
  recompileOnChange s shaderDir
  GLFW.windowSizeCallback $= updateWindowSize s

  t0 <- getCurrentTime
  void $ runSTMStateT s $ forever $ do
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

setDirty :: TVar RenderState -> IO ()
setDirty s = atomically $ do
  rs <- readTVar s
  writeTVar s (set dirty True rs)

recompileOnChange :: TVar RenderState -> FilePath -> IO ()
recompileOnChange s shaderDir = do
  mgr <- startManager
  void $ watchDir mgr shaderDir (const True) (const (setDirty s))
