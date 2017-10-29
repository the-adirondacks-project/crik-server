module Routes.Video
(
  setupVideoRoutes
) where

import Control.Monad.IO.Class (liftIO)
import Database.PostgreSQL.Simple (Connection)
import Network.HTTP.Types.Status (status404)
import Web.Scotty (ScottyM, get, json, jsonData, param, post, put, status)

import Database.Video (getAllVideos, getVideoById, insertVideo, updateVideo)
import Database.VideoFile (getVideoFile, getVideoFiles)
import Types.Video (VideoId(VideoId))
import Types.VideoFile (VideoFileId(VideoFileId))

setupVideoRoutes :: Connection -> ScottyM ()
setupVideoRoutes psqlConnection = do
  get "/api/videos" $ do
    videos <- liftIO $ getAllVideos psqlConnection
    json videos
  post "/api/videos" $ do
    newVideo <- jsonData
    insertedVideo <- liftIO $ insertVideo psqlConnection newVideo
    json insertedVideo
  get "/api/videos/:id" $ do
    id :: Int <- param "id"
    maybeVideo <- liftIO $ getVideoById psqlConnection (VideoId id)
    case maybeVideo of
      Nothing -> status status404
      Just video -> json video
  put "/api/videos/:id" $ do
    id :: Int <- param "id"
    videoUpdate <- jsonData
    maybeUpdatedVideo <- liftIO $ updateVideo psqlConnection (VideoId id) videoUpdate
    case maybeUpdatedVideo of
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
