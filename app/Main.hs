module Main where

import Control.Monad.IO.Class (liftIO)
import Database.PostgreSQL.Simple (connectPostgreSQL, query, Connection, Only(Only))
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Web.Scotty (scotty, get, param, status, json)
import Data.Maybe (listToMaybe)
import Network.HTTP.Types.Status (status404)

import Types.Video (VideoId(VideoId), Video)

maybeGetPort :: IO (Maybe Int)
maybeGetPort = do
  maybeRawPort <- lookupEnv "PORT"
  case maybeRawPort of
    Nothing -> return Nothing
    Just rawPort -> return (readMaybe rawPort :: Maybe Int)

getPort :: IO (Int)
getPort = do
  maybePort <- maybeGetPort
  case maybePort of
    Nothing -> return 8015
    Just x -> return x

main :: IO ()
main = do
  -- Connection info gets passed via environment variables
  psql <- connectPostgreSQL ""
  port <- getPort
  scotty port $ do
    get "/videos/:id" $ do
      id :: Int <- param "id"
      maybeVideo <- liftIO $ getVideo psql (VideoId id)
      case maybeVideo of
        Nothing -> status status404
        Just video -> json video

  print "connected"
getVideo :: Connection -> VideoId -> IO (Maybe Video)
getVideo connection videoId = do
  rows <- query connection "select * from videos where id = ?" (Only videoId)
  return (listToMaybe rows)
