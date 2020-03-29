module Main where

import System.Environment

import LiveshaderHS.Loop

main :: IO ()
main = getArgs >>= \case
  [path] -> liveshader path
  _ -> do
    name <- getProgName
    putStrLn ("Usage: " ++ name ++ " path_to_shader_directory")
