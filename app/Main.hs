{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import qualified HaxlBlog as H
import qualified BlogDB as R

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
  putStrLn "raw:"
  R.run path (R.getPostIds >>= mapM R.getPostContent) >>= print
  putStrLn "haxl:"
  H.run path (H.getPostIds >>= mapM H.getPostContent) >>= print
