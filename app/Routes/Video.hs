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
, updateVideoHandler
, videoServer
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Servant (ServerT, enter, err404, throwError)
import Servant.API (Capture, Get, JSON, Post, Put, ReqBody, (:>), (:<|>)((:<|>)))

import Config (Config(..), ConfigM(..))
import Database.Video (getAllVideos, getVideoById, insertVideo, updateVideo)
import Database.VideoFile (getVideoFile, getVideoFiles)
import Types.Video (NoId, Video, VideoId(VideoId))
import Types.VideoFile (VideoFile, VideoFileId(VideoFileId))

type VideoAPI = "videos" :> (
    Get '[JSON] [Video VideoId] :<|>
    ReqBody '[JSON] (Video NoId) :> Post '[JSON] (Video VideoId) :<|>
    Capture "videoId" Int :> ReqBody '[JSON] (Video NoId) :> Put '[JSON] (Video VideoId) :<|>
    Capture "videoId" Int :> Get '[JSON] (Video VideoId) :<|>
    Capture "videoId" Int :> "files" :> Get '[JSON] [VideoFile] :<|>
    Capture "videoId" Int :> "files" :> Capture "videoFileId" Int :> Get '[JSON] VideoFile :<|>
    "files" :> (
      Get '[JSON] [VideoFile] :<|>
      Capture "videoFileId" Int :> Get '[JSON] VideoFile
    )
  )

videoServer :: ServerT VideoAPI ConfigM
videoServer =
  getVideos :<|>
  newVideoHandler :<|>
  updateVideoHandler :<|>
  getVideo :<|>
  getVideoFilesForVideoHandler :<|>
  getVideoFileForVideoHandler :<|>
  getVideoFilesHandler :<|>
  getVideoFileHandler

getVideo :: Int -> ConfigM (Video VideoId)
getVideo videoId = do
  connection <- asks psqlConnection
  maybeVideo <- liftIO $ getVideoById connection (VideoId videoId)
  case maybeVideo of
    Nothing -> throwError err404
    Just x -> return x

newVideoHandler :: Video NoId -> ConfigM (Video VideoId)
newVideoHandler newVideo = do
  connection <- asks psqlConnection
  liftIO $ insertVideo connection newVideo

updateVideoHandler :: Int -> Video NoId -> ConfigM (Video VideoId)
updateVideoHandler videoId videoUpdate = do
  connection <- asks psqlConnection
  maybeUpdatedVideo <- liftIO $ updateVideo connection (VideoId videoId) videoUpdate
  case maybeUpdatedVideo of
    Nothing -> throwError err404
    Just x -> return x

getVideos :: ConfigM [Video VideoId]
getVideos = do
  connection <- asks psqlConnection
  liftIO $ getAllVideos connection

getVideoFilesHandler :: ConfigM [VideoFile]
getVideoFilesHandler = do
  connection <- asks psqlConnection
  liftIO $ getVideoFiles connection Nothing Nothing

getVideoFileHandler :: Int -> ConfigM VideoFile
getVideoFileHandler videoFileId = do
  connection <- asks psqlConnection
  maybeVideoFile <- liftIO $ getVideoFile connection Nothing (VideoFileId videoFileId)
  case maybeVideoFile of
    Nothing -> throwError err404
    Just x -> return x

getVideoFilesForVideoHandler :: Int -> ConfigM [VideoFile]
getVideoFilesForVideoHandler videoId = do
  connection <- asks psqlConnection
  liftIO $ getVideoFiles connection (Just (VideoId videoId)) Nothing

getVideoFileForVideoHandler :: Int -> Int -> ConfigM VideoFile
getVideoFileForVideoHandler videoId videoFileId = do
  connection <- asks psqlConnection
  maybeVideoFile <- liftIO $ getVideoFile connection (Just (VideoId videoId)) (VideoFileId videoFileId)
  case maybeVideoFile of
    Nothing -> throwError err404
    Just x -> return x
