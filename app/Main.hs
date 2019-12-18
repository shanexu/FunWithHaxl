{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import HaxlBlog

defaultDB = "blog.sqlite"

main :: IO ()
main = do
  args <- getArgs
  path <-
    case args of
      x:_ -> putStrLn ("using " ++ x) >> return x
      _ ->
        putStrLn ("using default \"" ++ defaultDB ++ "\"") >>
        return defaultDB
  run path (getPostIds >>= mapM getPostContent) >>= print
