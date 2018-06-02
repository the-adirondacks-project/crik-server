module Routes.File
(
  fileServer
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Servant (ServerT, err404, throwError)
import Servant.API ((:<|>)((:<|>)))

import Config
import Crik.API
import Crik.Types
import Crik.Types.Video
import Crik.Types.VideoFile
import Database.VideoFile

fileServer :: ServerT FileAPI ConfigM
fileServer =
  getFileHandler :<|>
  getFilesHandler :<|>
  createFileHandler

getFileHandler :: VideoFileId -> ConfigM (VideoFile VideoFileId)
getFileHandler fileId = do
  connection <- asks psqlConnection
  maybeVideoFile <- liftIO $ getVideoFile connection Nothing fileId
  case maybeVideoFile of
    Nothing -> throwError err404
    Just x -> return x

getFilesHandler :: ConfigM [VideoFile VideoFileId]
getFilesHandler = do
  connection <- asks psqlConnection
  liftIO $ getVideoFiles connection Nothing Nothing

createFileHandler :: VideoFile NoId -> ConfigM (VideoFile VideoFileId)
createFileHandler newVideoFile = do
  connection <- asks psqlConnection
  liftIO $ insertVideoFile connection newVideoFile
