module Routes.Video
(
  VideoAPI
, getVideo
, getVideoFileForVideoHandler
, getVideoFileHandler
, getVideoFilesForVideoHandler
, getVideoFilesHandler
, getVideos
, newVideoHandler
, setupVideoRoutes
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (asks, lift)
import Data.Text.Lazy (Text)
import Database.PostgreSQL.Simple (Connection)
import Network.HTTP.Types.Status (status404)
import Web.Scotty.Trans (ScottyError, ScottyT, get, json, jsonData, param, post, put, status)

import Data.Proxy (Proxy(Proxy))
import Servant.API (Capture, Get, JSON, Post, ReqBody, (:>), (:<|>))
import Servant (err404, throwError)

import Config (Config(..), ConfigM(..))
import Database.Video (getAllVideos, getVideoById, insertVideo, updateVideo)
import Database.VideoFile (getVideoFile, getVideoFiles)
import Types.Video (NoId(NoId), Video, VideoId(VideoId))
import Types.VideoFile (VideoFile, VideoFileId(VideoFileId))

type VideoAPI = "videos" :> (
    Get '[JSON] [Video VideoId] :<|>
    ReqBody '[JSON] (Video NoId) :> Post '[JSON] (Video VideoId) :<|>
    Capture "videoId" Int :> Get '[JSON] (Video VideoId) :<|>
    Capture "videoId" Int :> "files" :> Get '[JSON] [VideoFile] :<|>
    Capture "videoId" Int :> "files" :> Capture "videoFileId" Int :> Get '[JSON] VideoFile :<|>
    "files" :> (
      Get '[JSON] [VideoFile] :<|>
      Capture "videoFileId" Int :> Get '[JSON] VideoFile
    )
  )

getVideo :: Int -> ConfigM (Video VideoId)
getVideo videoId = do
  connection <- asks psqlConnection
  maybeVideo <- liftIO $ getVideoById connection (VideoId videoId)
  case maybeVideo of
    Nothing -> throwError err404
    Just x -> return x

newVideoHandler :: (Video NoId) -> ConfigM (Video VideoId)
newVideoHandler newVideo = do
  connection <- asks psqlConnection
  liftIO $ insertVideo connection newVideo

getVideos :: ConfigM ([Video VideoId])
getVideos = do
  connection <- asks psqlConnection
  liftIO $ getAllVideos connection

getVideoFilesHandler :: ConfigM ([VideoFile])
getVideoFilesHandler = do
  connection <- asks psqlConnection
  liftIO $ getVideoFiles connection Nothing Nothing

getVideoFileHandler :: Int -> ConfigM (VideoFile)
getVideoFileHandler videoFileId = do
  connection <- asks psqlConnection
  maybeVideoFile <- liftIO $ getVideoFile connection Nothing (VideoFileId videoFileId)
  case maybeVideoFile of
    Nothing -> throwError err404
    Just x -> return x

getVideoFilesForVideoHandler :: Int -> ConfigM ([VideoFile])
getVideoFilesForVideoHandler videoId = do
  connection <- asks psqlConnection
  liftIO $ getVideoFiles connection (Just (VideoId videoId)) Nothing

getVideoFileForVideoHandler :: Int -> Int -> ConfigM (VideoFile)
getVideoFileForVideoHandler videoId videoFileId = do
  connection <- asks psqlConnection
  maybeVideoFile <- liftIO $ getVideoFile connection (Just (VideoId videoId)) (VideoFileId videoFileId)
  case maybeVideoFile of
    Nothing -> throwError err404
    Just x -> return x

-- Well for some reason making this ScottyT Text ConfigM () works but (ScottyError e) => ScottyT e ConfigM () does not.
-- I think because I am not giving it an error type when I use it so I'm just doing this for now until I figure out what
-- I want to do for errors.
setupVideoRoutes :: ScottyT Text ConfigM ()
setupVideoRoutes = do
  put "/api/videos/:id" $ do
    id :: Int <- param "id"
    videoUpdate <- jsonData
    connection <- lift $ asks psqlConnection
    maybeUpdatedVideo <- liftIO $ updateVideo connection (VideoId id) videoUpdate
    case maybeUpdatedVideo of
      Nothing -> status status404
      Just video -> json video
