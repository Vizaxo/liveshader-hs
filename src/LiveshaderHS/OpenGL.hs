module LiveshaderHS.OpenGL where

import Control.Lens
import Control.Monad.State
import Data.Time.Clock
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Graphics.GLUtil
import Foreign.Storable

import LiveshaderHS.Types

makeWindow :: IO ()
makeWindow = do
  GLFW.initialize
  GLFW.openWindowHint GLFW.OpenGLVersionMajor 3
  GLFW.openWindowHint GLFW.OpenGLVersionMinor 3
  GLFW.openWindowHint GLFW.OpenGLProfile GLFW.OpenGLCoreProfile
  GLFW.openWindow (GL.Size 400 600) [] GLFW.Window
  GLFW.windowTitle $= "Liveshader HS"

  -- Disable vsync
  GLFW.swapInterval $= 0

  GLFW.disableSpecial GLFW.MouseCursor
  GLFW.mousePos $= (GL.Position 0 0)

initOGL :: IO RenderState
initOGL = do
  GL.polygonMode $= (GL.Fill, GL.Fill)
  GL.cullFace $= Nothing
  GL.depthFunc $= Just GL.Less

  vao <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vao

  shaderProg <- makeShaderProgram

  vao <- makeVAO $ do
      let pos = GL.VertexArrayDescriptor 2 GL.Float (fromIntegral $ sizeOf (undefined :: GL.Vector2 Float)) offset0
          posAttribute  = getAttrib shaderProg "pos"
      vbo <- makeBuffer GL.ArrayBuffer vertices
      GL.vertexAttribArray posAttribute $= GL.Enabled
      GL.vertexAttribPointer posAttribute $= (GL.ToFloat, pos)

  GL.currentProgram $= Just (program shaderProg)
  t0 <- getCurrentTime
  pure (RenderState shaderProg vao t0)


vertices :: [GL.Vector2 Float]
vertices = [ GL.Vector2 (-1.0) (-1.0)
           , GL.Vector2 1.0 (-1.0)
           , GL.Vector2 1.0 1.0
           , GL.Vector2 1.0 1.0
           , GL.Vector2 (-1.0) 1.0
           , GL.Vector2 (-1.0) (-1.0)
           ]

makeShaderProgram :: IO ShaderProgram
makeShaderProgram = loadShaderProgram
  [ (GL.VertexShader, "shaders/vertex.glsl")
  , (GL.FragmentShader, "shaders/fragment.glsl")
  ]

elapsedTime :: (MonadState RenderState m, MonadIO m) => m Float
elapsedTime = do
  t0 <- gets (^. startTime)
  t <- liftIO getCurrentTime
  let dt = diffUTCTime t0 t
  pure (realToFrac dt)

renderFrame :: (MonadState RenderState m, MonadIO m) => m ()
renderFrame = do
  renderState <- get
  dt <- elapsedTime

  liftIO $ setUniform (renderState ^. shaderProg) "iTime" (dt :: Float)
  GL.clearColor $= GL.Color4 0.0 0.0 0.0 0.0
  liftIO $ GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  GL.bindVertexArrayObject $= Just (renderState ^. vao)

  liftIO $ GL.drawArrays GL.Triangles 0 (fromIntegral (length vertices))
  liftIO $ GLFW.swapBuffers