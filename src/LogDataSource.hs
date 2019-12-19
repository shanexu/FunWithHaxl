{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE
    StandaloneDeriving, GADTs, TypeFamilies,
    FlexibleInstances, MultiParamTypeClasses,
    OverloadedStrings, DeriveDataTypeable
 #-}

module LogDataSource
  ( writeLog
  , initDataSource
  , LogRequest(..)
  ) where


import Data.Hashable
import Data.Typeable
import Haxl.Core

-- -----------------------------------------------------------------------------
-- Request type

data LogRequest a where
  WriteLog :: String -> LogRequest ()

deriving instance Show (LogRequest a)
deriving instance Typeable LogRequest

instance ShowP LogRequest where showp = show

deriving instance Eq (LogRequest a)

instance Hashable (LogRequest a) where
  hashWithSalt salt (WriteLog str) = hashWithSalt salt str


writeLog :: String -> GenHaxl u ()
writeLog = uncachedRequest . WriteLog


instance StateKey LogRequest where
  data State LogRequest  = LogDataState

initDataSource :: IO (State LogRequest)
initDataSource = return LogDataState

instance DataSourceName LogRequest where
  dataSourceName _ = "LogDataSource"

instance DataSource u LogRequest where
  fetch _state _flags _userEnv =
    SyncFetch $ mapM_ doOne
   where
    doOne :: BlockedFetch LogRequest -> IO ()
    doOne (BlockedFetch (WriteLog str) var) = do
      putStrLn str
      putSuccess var ()


