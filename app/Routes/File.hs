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
import Crik.Types.File
import Database.File

fileServer :: ServerT FileAPI ConfigM
fileServer =
  getFileHandler :<|>
  getFilesHandler :<|>
  createFileHandler

getFileHandler :: FileId -> ConfigM (File FileId)
getFileHandler fileId = do
  connection <- asks psqlConnection
  maybeFile <- liftIO $ getFile connection Nothing fileId
  case maybeFile of
    Nothing -> throwError err404
    Just x -> return x

getFilesHandler :: ConfigM [File FileId]
getFilesHandler = do
  connection <- asks psqlConnection
  liftIO $ getFiles connection Nothing Nothing

createFileHandler :: File NoId -> ConfigM (File FileId)
createFileHandler newFile = do
  connection <- asks psqlConnection
  liftIO $ insertFile connection newFile
