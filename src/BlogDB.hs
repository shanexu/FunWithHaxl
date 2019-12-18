{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BlogDB where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Typeable
import qualified Data.Text as T
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Exception
import Control.Arrow (left)

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

getPostIds :: Blog [PostId]
getPostContent :: PostId -> Blog PostContent
-- more operations...


-- -----------------------------------------------------------------------------
-- Implementation

sql ::
     forall r. FromRow r
  => Query
  -> Blog (Either String [r])
sql query = do
  db <- ask
  liftIO $ do
    print query
    left show <$> (try $ query_ db query :: IO (Either SomeException [r]))

getPostIds = do
  r <- sql "select postid from postinfo;" :: Blog (Either String [[PostId]])
  case r of
    Right rows -> return $ concat rows
    Left s -> liftIO $ throwIO (BlogDBException s)

getPostContent x = do
  r <- sql (Query (T.pack $ "select content from postcontent where postid = " ++ show x ++ ";")) :: Blog (Either String [[PostContent]])
  case r of
    Right rows -> (return . head . head) rows
    Left s -> liftIO $ throwIO (BlogDBException s)

newtype BlogDBException = BlogDBException String
  deriving (Show, Typeable)

instance Exception BlogDBException
