module Routes.Video
(
  setupVideoRoutes
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (asks, lift)
import Data.Text.Lazy (Text)
import Database.PostgreSQL.Simple (Connection)
import Network.HTTP.Types.Status (status404)
import Web.Scotty.Trans (ScottyError, ScottyT, get, json, jsonData, param, post, put, status)

import Config (Config(..), ConfigM(..))
import Database.Video (getAllVideos, getVideoById, insertVideo, updateVideo)
import Database.VideoFile (getVideoFile, getVideoFiles)
import Types.Video (VideoId(VideoId))
import Types.VideoFile (VideoFileId(VideoFileId))

-- Well for some reason making this ScottyT Text ConfigM () works but (ScottyError e) => ScottyT e ConfigM () does not.
-- I think because I am not giving it an error type when I use it so I'm just doing this for now until I figure out what
-- I want to do for errors.
setupVideoRoutes :: ScottyT Text ConfigM ()
setupVideoRoutes = do
  get "/api/videos" $ do
    connection <- lift $ asks psqlConnection
    videos <- liftIO $ getAllVideos connection
    json videos
  post "/api/videos" $ do
    newVideo <- jsonData
    connection <- lift $ asks psqlConnection
    insertedVideo <- liftIO $ insertVideo connection newVideo
    json insertedVideo
  get "/api/videos/:id" $ do
    id :: Int <- param "id"
    connection <- lift $ asks psqlConnection
    maybeVideo <- liftIO $ getVideoById connection (VideoId id)
    case maybeVideo of
      Nothing -> status status404
      Just video -> json video
  put "/api/videos/:id" $ do
    id :: Int <- param "id"
    videoUpdate <- jsonData
    connection <- lift $ asks psqlConnection
    maybeUpdatedVideo <- liftIO $ updateVideo connection (VideoId id) videoUpdate
    case maybeUpdatedVideo of
      Nothing -> status status404
      Just video -> json video
  get "/api/videos/:id/files" $ do
    id :: Int <- param "id"
    connection <- lift $ asks psqlConnection
    videos <- liftIO $ getVideoFiles connection (Just (VideoId id)) Nothing
    json videos
  get "/api/videos/files" $ do
    connection <- lift $ asks psqlConnection
    videos <- liftIO $ getVideoFiles connection Nothing Nothing
    json videos
  get "/api/videos/files/:id" $ do
    id :: Int <- param "id"
    connection <- lift $ asks psqlConnection
    maybeVideo <- liftIO $ getVideoFile connection Nothing (VideoFileId id)
    case maybeVideo of
      Nothing -> status status404
      Just video -> json video
  get "/api/videos/:videoId/files/:videoFileId" $ do
    videoId :: Int <- param "videoId"
    videoFileId :: Int <- param "videoFileId"
    connection <- lift $ asks psqlConnection
    maybeVideo <- liftIO $ getVideoFile connection (Just (VideoId videoId)) (VideoFileId videoFileId)
    case maybeVideo of
      Nothing -> status status404
      Just video -> json video
