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
import Servant (ServerT, err404, throwError)
import Servant.API (Capture, Get, JSON, Post, Put, ReqBody, (:>), (:<|>)((:<|>)))

import Crik.API (VideoAPI)
import Config (Config(..), ConfigM(..))
import Database.Video (getAllVideos, getVideoById, insertVideo, updateVideo)
import Database.VideoFile (getVideoFile, getVideoFiles, insertVideoFile)
import Crik.Types
import Crik.Types.Video (Video(..), VideoId(VideoId))
import Crik.Types.VideoFile (VideoFile, VideoFileId(VideoFileId))

videoServer :: ServerT VideoAPI ConfigM
videoServer =
  getVideo :<|>
  getVideos :<|>
  newVideoHandler :<|>
  updateVideoHandler :<|>
  getVideoFilesForVideoHandler

getVideo :: VideoId -> ConfigM (Video VideoId)
getVideo videoId = do
  connection <- asks psqlConnection
  maybeVideo <- liftIO $ getVideoById connection videoId
  case maybeVideo of
    Nothing -> throwError err404
    Just x -> return x

fromMaybeId (Video _ videoName) = Video NoId videoName

newVideoHandler :: Video (NoId) -> ConfigM (Video VideoId)
newVideoHandler newVideo = do
  connection <- asks psqlConnection
  liftIO $ insertVideo connection (fromMaybeId newVideo)

updateVideoHandler :: VideoId -> Video (Maybe VideoId) -> ConfigM (Video VideoId)
updateVideoHandler videoId videoUpdate = do
  connection <- asks psqlConnection
  maybeUpdatedVideo <- liftIO $ updateVideo connection videoId videoUpdate
  case maybeUpdatedVideo of
    Nothing -> throwError err404
    Just x -> return x

getVideos :: ConfigM [Video VideoId]
getVideos = do
  connection <- asks psqlConnection
  liftIO $ getAllVideos connection

getVideoFilesHandler :: ConfigM [VideoFile VideoFileId]
getVideoFilesHandler = do
  connection <- asks psqlConnection
  liftIO $ getVideoFiles connection Nothing Nothing

getVideoFileHandler :: Int -> ConfigM (VideoFile VideoFileId)
getVideoFileHandler videoFileId = do
  connection <- asks psqlConnection
  maybeVideoFile <- liftIO $ getVideoFile connection Nothing (VideoFileId videoFileId)
  case maybeVideoFile of
    Nothing -> throwError err404
    Just x -> return x

getVideoFilesForVideoHandler :: VideoId -> ConfigM [VideoFile VideoFileId]
getVideoFilesForVideoHandler videoId = do
  connection <- asks psqlConnection
  liftIO $ getVideoFiles connection (Just videoId) Nothing

getVideoFileForVideoHandler :: Int -> Int -> ConfigM (VideoFile VideoFileId)
getVideoFileForVideoHandler videoId videoFileId = do
  connection <- asks psqlConnection
  maybeVideoFile <- liftIO $ getVideoFile connection (Just (VideoId videoId)) (VideoFileId videoFileId)
  case maybeVideoFile of
    Nothing -> throwError err404
    Just x -> return x

createVideoFileHandler :: VideoFile NoId -> ConfigM (VideoFile VideoFileId)
createVideoFileHandler newVideoFile = do
  connection <- asks psqlConnection
  liftIO $ insertVideoFile connection newVideoFile
