{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BlogDataSource
  ( PostId, PostContent
  , getPostIds
  , getPostContent
  , initDataSource
  , BlogRequest(..)
  , BlogDBException(..)
  ) where


import Data.Hashable
import Data.Typeable
import qualified Data.Map as Map
import Control.Monad
import Data.Maybe
import Data.List
import Data.Text (Text, pack)
import Haxl.Core
import Database.SQLite.Simple
import Database.SQLite.Simple.Internal
import Control.Exception
import Control.Arrow (left)


-- -----------------------------------------------------------------------------
-- Types

type PostId = Int
type PostContent = String


-- -----------------------------------------------------------------------------
-- Request type

data BlogRequest a where
  FetchPosts :: BlogRequest [PostId]
  FetchPostContent :: PostId -> BlogRequest PostContent

deriving instance Show (BlogRequest a)
deriving instance Typeable BlogRequest

deriving instance Eq (BlogRequest a)

instance Hashable (BlogRequest a) where
  hashWithSalt salt FetchPosts = hashWithSalt salt (0::Int)
  hashWithSalt salt (FetchPostContent p) = hashWithSalt salt (1::Int, p)

instance ShowP BlogRequest where
  showp = show

-- -----------------------------------------------------------------------------
-- Requests

getPostIds :: GenHaxl u [PostId]
getPostIds = dataFetch FetchPosts

getPostContent :: PostId -> GenHaxl u PostContent
getPostContent = dataFetch . FetchPostContent

-- more operations ...


-- -----------------------------------------------------------------------------
-- Data source implementation

instance StateKey BlogRequest where
  data State BlogRequest = BlogDataState Connection

initDataSource :: String -> IO (State BlogRequest)
initDataSource path = BlogDataState <$> open path

instance DataSourceName BlogRequest where
  dataSourceName _ = "BlogDataSource"

instance DataSource u BlogRequest where
  fetch (BlogDataState db) _flags _userEnv =
    SyncFetch $ batchFetch db



-- -----------------------------------------------------------------------------
-- Group requests by type

batchFetch :: Connection -> [BlockedFetch BlogRequest] -> IO ()
batchFetch db = doFetch db . foldr collect emptyBatches

type Batches
  = ( [ResultVar [PostId]]              -- FetchPosts
    , [(PostId, ResultVar PostContent)] -- FetchPostContent
    )

emptyBatches :: Batches
emptyBatches = ([],[])

collect :: BlockedFetch BlogRequest -> Batches -> Batches
collect (BlockedFetch FetchPosts v) (as,bs) = (v:as,bs)
collect (BlockedFetch (FetchPostContent x) v) (as,bs) = (as,(x,v):bs)


-- -----------------------------------------------------------------------------
-- Fetch data for each batch

doFetch :: Connection -> Batches -> IO ()
doFetch db (as,bs) = do
  sqlMultiFetch db as id
    "select postid from postinfo;"
    (fromRow :: RowParser [PostId])
    concat
    (\_ ids -> Just ids)

  sqlMultiFetch db bs snd
    ("select postid,content from postcontent where postid in " ++
       idList (map fst bs) ++ ";")
    (fromRow :: RowParser (PostId, PostContent))
    Map.fromList
    (\(x,_) -> Map.lookup x)

sqlMultiFetch ::
  forall a x y z.
  FromRow y => Connection
  -> [x]
  -> (x -> ResultVar a)
  -> String
  -> RowParser y
  -> ([y] -> z)
  -> (x -> z -> Maybe a)
  -> IO ()
sqlMultiFetch _ [] _ _ _ _ _ = return ()
sqlMultiFetch db requests getvar query parser collate extract = do
  results <- sql parser db query :: IO (Either String [y])
  case results of
    Left s -> failAll (BlogDBException s)
    Right rows -> do
      let fetched = collate rows
      forM_ requests $ \q ->
        case extract q fetched of
          Nothing -> putFailure (getvar q) (BlogDBException "missing result")
          Just r -> putSuccess (getvar q) r
  where
    failAll e = forM_ requests $ \q -> putFailure (getvar q) e

idList :: [PostId] -> String
idList ids = "(" ++ intercalate "," (map show ids) ++ ")"

sql :: forall r. FromRow r => RowParser r -> Connection -> String -> IO (Either String [r])
sql parser db query = do
  putStrLn query
  left show <$> (Control.Exception.try $ queryWith_ parser db (Query $ pack query) :: IO (Either SomeException [r]))

newtype BlogDBException = BlogDBException String
  deriving (Show, Typeable)

instance Exception BlogDBException where
  toException = transientErrorToException
  fromException = transientErrorFromException

