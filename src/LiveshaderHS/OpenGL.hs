module LiveshaderHS.OpenGL where

import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Graphics.GLUtil
import Foreign.Storable

import LiveshaderHS.STMState
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

initOGL :: FilePath -> IO RenderState
initOGL shaderDir = do
  GL.polygonMode $= (GL.Fill, GL.Fill)
  GL.cullFace $= Nothing
  GL.depthFunc $= Just GL.Less

  vao <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vao

  shaderProg <- makeShaderProgram shaderDir
  windowSize <- GL.get GLFW.windowSize

  vao <- makeVAO $ do
      let pos = GL.VertexArrayDescriptor 2 GL.Float (fromIntegral $ sizeOf (undefined :: GL.Vector2 Float)) offset0
          posAttribute  = getAttrib shaderProg "pos"
      vbo <- makeBuffer GL.ArrayBuffer vertices
      GL.vertexAttribArray posAttribute $= GL.Enabled
      GL.vertexAttribPointer posAttribute $= (GL.ToFloat, pos)

  pure (RenderState shaderProg vao False shaderDir windowSize)


vertices :: [GL.Vector2 Float]
vertices = [ GL.Vector2 (-1.0) (-1.0)
           , GL.Vector2 1.0 (-1.0)
           , GL.Vector2 1.0 1.0
           , GL.Vector2 1.0 1.0
           , GL.Vector2 (-1.0) 1.0
           , GL.Vector2 (-1.0) (-1.0)
           ]

makeShaderProgram :: FilePath -> IO ShaderProgram
makeShaderProgram shaderDir = loadShaderProgram
  [ (GL.VertexShader, shaderDir ++ "/vertex.glsl")
  , (GL.FragmentShader, shaderDir ++ "/fragment.glsl")
  ]

recompileIfDirty :: (MonadState RenderState m, MonadIO m) => m ()
recompileIfDirty = do
  rs <- get
  when (rs ^. dirty) $ do
    liftIO (putStr "Recompiling shaders...")
    liftIO (try (makeShaderProgram (rs ^. shaderDir))) >>= \case
      Right sp -> do
        modify (set shaderProg sp . set dirty False)
        liftIO (putStrLn " Recompiled")
      Left e -> do
        modify (set dirty False)
        liftIO (print (e :: IOException))
        liftIO (putStrLn " Shader compilation failed")

uniformExists :: GL.UniformLocation -> Bool
uniformExists (GL.UniformLocation (-1)) = False
uniformExists _ = True

-- Set a uniform without giving a warning or error if it is not active
safeSetUniform :: (GL.Uniform a, MonadState RenderState m, MonadIO m)
  => String -> a -> m ()
safeSetUniform name v = do
  rs <- get
  uLocation <- GL.get (GL.uniformLocation (rs^.shaderProg&program) name)
  when (uniformExists uLocation) $ GL.uniform uLocation $= v

renderFrame :: (MonadState RenderState m, MonadIO m) => Float -> m ()
renderFrame dt = do
  recompileIfDirty

  rs <- get

  GL.currentProgram $= Just (program (rs ^. shaderProg))
  GL.bindVertexArrayObject $= Just (rs ^. vao)

  (GL.Position mouseX mouseY) <- GL.get GLFW.mousePos
  safeSetUniform "iTime" (dt :: Float)
  safeSetUniform "iMousePos"
    (GL.Vector2 (fromIntegral mouseX) (fromIntegral mouseY) :: GL.Vector2 Float)

  GL.clearColor $= GL.Color4 0.0 0.0 0.0 0.0
  liftIO $ GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  liftIO $ GL.drawArrays GL.Triangles 0 (fromIntegral (length vertices))
  liftIO $ GLFW.swapBuffers
