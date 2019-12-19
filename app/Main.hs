{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import qualified HaxlBlog as H
import qualified BlogDB as R

defaultDB = "blog.sqlite"

main :: IO ()
main = do
  putStrLn "raw:"
  R.run (R.getPostIds >>= mapM R.getPostContent) >>= print
  putStrLn "haxl:"
  H.run (H.getPostIds >>= mapM H.getPostContent) >>= print
