module Routes.VideoLibrary
(
  LibraryAPI
, libraryServer
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.List ((\\))
import Data.Text (Text, stripPrefix, pack, unpack)
import Prelude hiding (FilePath)
import Servant (ServerT, err404, err422, err500, throwError)
import Servant.API ((:<|>)((:<|>)))
import System.Directory (listDirectory)

import Crik.API
import Config (Config(..), ConfigM(..))
import Database.Library
import Database.File
import Crik.Types (NoId(NoId))
import Crik.Types.Library
import Crik.Types.File

libraryServer :: ServerT LibraryAPI ConfigM
libraryServer =
  getLibrary :<|>
  getLibraries :<|>
  createLibraryHandler :<|>
  updateLibraryHandler :<|>
  getNewFilesInLibrary :<|>
  getAllFilesInLibrary

getLibraries :: ConfigM [Library LibraryId]
getLibraries = do
  connection <- asks psqlConnection
  liftIO $ getAllLibraries connection

getLibrary :: LibraryId -> ConfigM (Library LibraryId)
getLibrary libraryId = do
  connection <- asks psqlConnection
  maybeLibrary <- liftIO $ getLibraryById connection libraryId
  case maybeLibrary of
    Nothing -> throwError err404
    Just x -> return x

getNewFilesInLibrary :: LibraryId -> ConfigM [Text]
getNewFilesInLibrary libraryId = do
  connection <- asks psqlConnection
  maybeLibrary <- liftIO $ getLibraryById connection libraryId
  case maybeLibrary of
    Just library -> do
      case getFilePathFromFileURI (libraryUrl library) of
        Just filePath -> do
          actualFiles <- liftIO $ findAllFilesInLibrary filePath
          files <- liftIO $ getFiles connection Nothing Nothing
          return (actualFiles \\ (map (unFileStorageId . fileStorageId) files))
        Nothing -> throwError err422
    Nothing -> throwError err404

getAllFilesInLibrary :: LibraryId -> ConfigM [Text]
getAllFilesInLibrary libraryId = do
  connection <- asks psqlConnection
  maybeLibrary <- liftIO $ getLibraryById connection libraryId
  case maybeLibrary of
    Just library -> do
      case getFilePathFromFileURI (libraryUrl library) of
        Just filePath -> do
          liftIO $ findAllFilesInLibrary filePath
        Nothing -> throwError err422
    Nothing -> throwError err404

createLibraryHandler :: Library NoId -> ConfigM (Library LibraryId)
createLibraryHandler newLibrary = do
  connection <- asks psqlConnection
  liftIO $ insertLibrary connection newLibrary

updateLibraryHandler ::
  LibraryId -> Library NoId -> ConfigM (Library LibraryId)
updateLibraryHandler libraryId libraryUpdate = do
  connection <- asks psqlConnection
  maybeUpdatedLibrary <- liftIO $
    updateLibrary connection libraryId libraryUpdate
  case maybeUpdatedLibrary of
    Nothing -> throwError err404
    Just x -> return x

newtype FilePath = FilePath { unFilePath :: Text }

findAllFilesInLibrary :: FilePath -> IO [Text]
findAllFilesInLibrary filePath = do
    files <- listDirectory ((unpack . unFilePath) filePath)
    return (map pack files)

getFilePathFromFileURI :: Text -> Maybe (FilePath)
getFilePathFromFileURI uri =
  case maybePath of
    Just path -> Just $ FilePath path
    Nothing -> Nothing
  where maybePath = stripPrefix filePathPrefix uri

filePathPrefix :: Text
filePathPrefix = "file://"
