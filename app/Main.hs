module Main where

import Control.Monad.IO.Class (liftIO)
import Database.PostgreSQL.Simple (connectPostgreSQL)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Web.Scotty (scotty, get, param, status, json, middleware)
import Network.HTTP.Types.Status (status404)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Database.Video (getAllVideos, getVideoById)
import Types.Video (VideoId(VideoId))

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
  psqlConnection <- connectPostgreSQL ""
  port <- getPort
  scotty port $ do
    middleware logStdoutDev
    get "/videos" $ do
      videos <- liftIO $ getAllVideos psqlConnection
      json videos
    get "/videos/:id" $ do
      id :: Int <- param "id"
      maybeVideo <- liftIO $ getVideoById psqlConnection (VideoId id)
      case maybeVideo of
        Nothing -> status status404
        Just video -> json video

  print "connected"
