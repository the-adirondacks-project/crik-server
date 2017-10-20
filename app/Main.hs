module Main where

import Control.Monad.IO.Class (liftIO)
import Database.PostgreSQL.Simple (connectPostgreSQL)
import Network.HTTP.Types.Status (status404)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Web.Scotty (scotty, get, param, status, json, middleware)

import Database.Video (getAllVideos, getVideoById)
import Database.VideoFile (getVideoFile, getVideoFiles)
import Database.VideoLibrary (getVideoLibraryById, getAllVideoLibraries)
import Types.Video (VideoId(VideoId))
import Types.VideoFile (VideoFileId(VideoFileId))
import Types.VideoLibrary (VideoLibraryId(VideoLibraryId))

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
    get "/api/videos" $ do
      videos <- liftIO $ getAllVideos psqlConnection
      json videos
    get "/api/videos/:id" $ do
      id :: Int <- param "id"
      maybeVideo <- liftIO $ getVideoById psqlConnection (VideoId id)
      case maybeVideo of
        Nothing -> status status404
        Just video -> json video
    get "/api/videos/:id/files" $ do
      id :: Int <- param "id"
      videos <- liftIO $ getVideoFiles psqlConnection (Just (VideoId id)) Nothing
      json videos
    get "/api/videos/files" $ do
      videos <- liftIO $ getVideoFiles psqlConnection Nothing Nothing
      json videos
    get "/api/videos/files/:id" $ do
      id :: Int <- param "id"
      maybeVideo <- liftIO $ getVideoFile psqlConnection Nothing (VideoFileId id)
      case maybeVideo of
        Nothing -> status status404
        Just video -> json video
    get "/api/videos/:videoId/files/:videoFileId" $ do
      videoId :: Int <- param "videoId"
      videoFileId :: Int <- param "videoFileId"
      maybeVideo <- liftIO $ getVideoFile psqlConnection (Just (VideoId videoId)) (VideoFileId videoFileId)
      case maybeVideo of
        Nothing -> status status404
        Just video -> json video
    get "/api/video_libraries" $ do
      videos <- liftIO $ getAllVideoLibraries psqlConnection
      json videos
    get "/api/videos/:id" $ do
      id :: Int <- param "id"
      maybeVideoLibrary <- liftIO $ getVideoLibraryById psqlConnection (VideoLibraryId id)
      case maybeVideoLibrary of
        Nothing -> status status404
        Just videoLibrary -> json videoLibrary
