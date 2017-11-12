module Routes.VideoLibrary
(
  VideoLibraryAPI
, videoLibraryServer
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.List ((\\))
import Data.Text (Text, stripPrefix, pack, unpack)
import Prelude hiding (FilePath)
import Servant (ServerT, enter, err404, err422, err500, throwError)
import Servant.API ((:<|>)((:<|>)))
import System.Directory (listDirectory)

import API (VideoLibraryAPI)
import Config (Config(..), ConfigM(..))
import Database.VideoLibrary (getAllVideoLibraries, getVideoLibraryById)
import Database.VideoFile (getVideoFiles)
import Types.VideoLibrary (VideoLibrary(..), VideoLibraryId(VideoLibraryId))
import Types.VideoFile (VideoFile(videoFileStorageId), VideoFileStorageId(unVideoFileStorageId))

videoLibraryServer :: ServerT VideoLibraryAPI ConfigM
videoLibraryServer =
  getVideoLibraries :<|>
  getVideoLibrary :<|>
  getNewFilesInLibrary :<|>
  getAllFilesInLibrary

getVideoLibraries :: ConfigM [VideoLibrary]
getVideoLibraries = do
  connection <- asks psqlConnection
  liftIO $ getAllVideoLibraries connection

getVideoLibrary :: Int -> ConfigM VideoLibrary
getVideoLibrary videoLibraryId = do
  connection <- asks psqlConnection
  maybeVideoLibrary <- liftIO $ getVideoLibraryById connection (VideoLibraryId videoLibraryId)
  case maybeVideoLibrary of
    Nothing -> throwError err404
    Just x -> return x

getNewFilesInLibrary :: Int -> ConfigM [Text]
getNewFilesInLibrary videoLibraryId = do
  connection <- asks psqlConnection
  maybeVideoLibrary <- liftIO $ getVideoLibraryById connection (VideoLibraryId videoLibraryId)
  case maybeVideoLibrary of
    Just videoLibrary -> do
      case getFilePathFromFileURI (videoLibraryUrl videoLibrary) of
        Just filePath -> do
          files <- liftIO $ findAllFilesInLibrary filePath
          videoFiles <- liftIO $ getVideoFiles connection Nothing Nothing
          return (files \\ (map (unVideoFileStorageId . videoFileStorageId) videoFiles))
        Nothing -> throwError err422
    Nothing -> throwError err404

getAllFilesInLibrary :: Int -> ConfigM [Text]
getAllFilesInLibrary videoLibraryId = do
  connection <- asks psqlConnection
  maybeVideoLibrary <- liftIO $ getVideoLibraryById connection (VideoLibraryId videoLibraryId)
  case maybeVideoLibrary of
    Just videoLibrary -> do
      case getFilePathFromFileURI (videoLibraryUrl videoLibrary) of
        Just filePath -> do
          liftIO $ findAllFilesInLibrary filePath
        Nothing -> throwError err422
    Nothing -> throwError err404

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
