module Routes.Video
(
  VideoAPI
, getVideo
, getFilesForVideoHandler
, getVideos
, newVideoHandler
, updateVideoHandler
, videoServer
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Text (Text)
import Servant (ServerT, err404, throwError)
import Servant.API (Capture, Get, JSON, Post, Put, ReqBody, (:>), (:<|>)((:<|>)))

import Crik.API (VideoAPI)
import Config (Config(..), ConfigM(..))
import Database.Video hiding (getVideoByName)
import qualified Database.Video as DB
import Database.File
import Crik.Types
import Crik.Types.Video (Video(..), VideoId(VideoId))
import Crik.Types.File

videoServer :: ServerT VideoAPI ConfigM
videoServer =
  getVideo :<|>
  getVideoByName :<|>
  getVideos :<|>
  newVideoHandler :<|>
  updateVideoHandler :<|>
  getFilesForVideoHandler

getVideo :: VideoId -> ConfigM (Video VideoId)
getVideo videoId = do
  connection <- asks psqlConnection
  maybeVideo <- liftIO $ getVideoById connection videoId
  case maybeVideo of
    Nothing -> throwError err404
    Just x -> return x

getVideoByName :: Text -> ConfigM (Video VideoId)
getVideoByName name = do
  connection <- asks psqlConnection
  maybeVideo <- liftIO $ DB.getVideoByName connection name
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

getFilesForVideoHandler :: VideoId -> ConfigM [File FileId]
getFilesForVideoHandler videoId = do
  connection <- asks psqlConnection
  liftIO $ getFiles connection (Just videoId) Nothing
