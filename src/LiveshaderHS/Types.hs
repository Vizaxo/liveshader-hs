module LiveshaderHS.Types where

import Control.Lens
import Data.Time
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.GLUtil

data RenderState = RenderState
  { _shaderProg :: ShaderProgram
  , _vao :: GL.VertexArrayObject
  , _startTime :: UTCTime
  }
makeLenses ''RenderState
