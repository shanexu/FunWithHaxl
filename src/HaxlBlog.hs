{-# LANGUAGE
    StandaloneDeriving, GADTs, TypeFamilies,
    FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving,
    OverloadedStrings, DeriveDataTypeable
 #-}

module HaxlBlog (
  PostId, PostContent,
  getPostIds, getPostContent,
  Haxl,
  run
) where


import BlogDataSource
import Haxl.Core

type Haxl a = GenHaxl () a

run :: String -> Haxl a -> IO a
run path h = do
  db <- initDataSource path
  env <- initEnv (stateSet db stateEmpty) ()
  runHaxl env h
