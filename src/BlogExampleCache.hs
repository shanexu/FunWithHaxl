module BlogExampleCache
  ( runDump
  , example
  ) where

import BlogDataSource
import HaxlBlog
import Haxl.Core

runDump :: Haxl a -> IO a
runDump h = do
  db <- initDataSource
  env <- initEnv (stateSet db stateEmpty) ()
  r <- runHaxl env h
  runHaxl env dumpCacheAsHaskell >>= putStr
  return r

example = do
  a <- getPostContent 1
  b <- getPostContent 2
  return (a ++ b)

loadCache :: GenHaxl u ()
loadCache = do
  cacheRequest (FetchPostContent 2) (Right ("example content 2"))
  cacheRequest (FetchPostContent 1) (Right ("example content 1"))
