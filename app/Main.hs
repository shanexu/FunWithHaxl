{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Database.SQLite.Simple
import Database.SQLite.Simple.Internal

main :: IO ()
main = do
  someFunc
  conn <- connection
  [[x]] <- queryWith_ (fromRow::RowParser [Int])  conn "select 2 + 2"
  print x

connection :: IO Connection
connection = open "/tmp/user.db"

