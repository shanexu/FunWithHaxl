{-# LANGUAGE OverloadedStrings #-}

module BlogDB where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Typeable
import qualified Data.Text as T
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Exception

-- -----------------------------------------------------------------------------
-- A monad

type Blog a = ReaderT Connection IO a

run :: String -> Blog a -> IO a
run path m = do
  db <- open path
  runReaderT m db

-- -----------------------------------------------------------------------------
-- An API

type PostId = Int
type PostContent = String

getPostIds     :: Blog [PostId]
getPostContent :: PostId -> Blog PostContent
-- more operations...


-- -----------------------------------------------------------------------------
-- Implementation

sql :: FromRow r => Query -> Blog [r]
sql query = do
  db <- ask
  liftIO $ do
    print query
    query_ db query


getPostIds = do
  r <- sql "select postid from postinfo;" :: Blog [[PostId]]
  return $ concat r

getPostContent x = do
  r <- sql (Query (T.pack $ "select content from postcontent where postid = " ++ show x ++ ";")) :: Blog [[PostContent]]
  (return . head . head) r

