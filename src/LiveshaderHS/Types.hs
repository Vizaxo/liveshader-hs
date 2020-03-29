module LiveshaderHS.Types where

import Control.Lens
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.GLUtil

data RenderState = RenderState
  { _shaderProg :: ShaderProgram
  , _vao :: GL.VertexArrayObject
  , _dirty :: Bool
  , _shaderDir :: FilePath
  }
makeLenses ''RenderState
