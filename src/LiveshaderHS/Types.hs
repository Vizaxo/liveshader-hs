module LiveshaderHS.Types where

import Control.Lens
import Data.Time.Clock
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.GLUtil

data LastWrittenTexture = A | B

data RenderBuffer = RenderBuffer
  { _textureA :: GL.TextureObject
  , _textureB :: GL.TextureObject
  , _lastWritten :: LastWrittenTexture
  , _fbo :: GL.FramebufferObject
  }
makeLenses ''RenderBuffer

getLastWrittenTexture :: RenderBuffer -> GL.TextureObject
getLastWrittenTexture b = case b^.lastWritten of
  A -> b^.textureA
  B -> b^.textureB

getPreviousTexture :: RenderBuffer -> GL.TextureObject
getPreviousTexture b = case b^.lastWritten of
  A -> b^.textureB
  B -> b^.textureA

swapLastWritten :: LastWrittenTexture -> LastWrittenTexture
swapLastWritten A = B
swapLastWritten B = A

data RenderState = RenderState
  { _shaderProg :: ShaderProgram
  , _vao :: GL.VertexArrayObject
  , _dirty :: Bool
  , _shaderDir :: FilePath
  , _windowSize :: GL.Size
  , _lastRenderTime :: UTCTime
  , _texture0 :: GL.TextureObject
  , _buffers :: [RenderBuffer]
  }
makeLenses ''RenderState
