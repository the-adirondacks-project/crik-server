module Main where

import Database.PostgreSQL.Simple (connectPostgreSQL)

import Lib

main :: IO ()
main = do
  connectPostgreSQL "postgresql://localhost/tap"
  print "connected"
