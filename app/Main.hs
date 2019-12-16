{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Database.SQLite.Simple

main :: IO ()
main = do
  someFunc
  conn <- connection
  [[x]] <- query_ conn "select 2 + 2" :: IO [[Int]]
  print x

connection :: IO Connection
connection = open "/tmp/user.db"

