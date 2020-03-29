{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Monad.State

import LiveshaderHS.OpenGL

main :: IO ()
main = do
  makeWindow
  rs <- initOGL
  runStateT renderFrame rs
  readLn @Char
  pure ()
