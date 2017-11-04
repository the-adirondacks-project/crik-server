module Routes.VideoLibrary
(
  setupVideoLibrariesRoutes
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks, lift)
import Data.List ((\\))
import Data.Text (Text, isPrefixOf, stripPrefix, pack, unpack)
import qualified Data.Text.Lazy as LT (Text)
import Database.PostgreSQL.Simple (Connection)
import Network.HTTP.Types.Status (status404, status422, status500)
import Prelude hiding (FilePath)
import System.Directory (listDirectory)
import Web.Scotty.Trans (ScottyT, get, json, param, status)

import Config (Config(..), ConfigM(..))
import Database.VideoLibrary (getAllVideoLibraries, getVideoLibraryById)
import Database.VideoFile (getVideoFiles)
import Types.VideoLibrary (VideoLibrary(..), VideoLibraryId(VideoLibraryId))
import Types.VideoFile (VideoFile(videoFileStorageId), VideoFileStorageId(unVideoFileStorageId))

setupVideoLibrariesRoutes :: ScottyT LT.Text ConfigM ()
setupVideoLibrariesRoutes = do
  get "/api/video_libraries" $ do
    connection <- lift $ asks psqlConnection
    videoLibraries <- liftIO $ getAllVideoLibraries connection
    json videoLibraries
  get "/api/video_libraries/:videoLibraryId" $ do
    videoLibraryId :: Int <- param "videoLibraryId"
    connection <- lift $ asks psqlConnection
    videoLibraries <- liftIO $ getVideoLibraryById connection (VideoLibraryId videoLibraryId)
    json videoLibraries
  get "/api/video_libraries/:videoLibraryId/all_files" $ do
    videoLibraryId :: Int <- param "videoLibraryId"
    connection <- lift $ asks psqlConnection
    maybeVideoLibrary <- liftIO $ getVideoLibraryById connection (VideoLibraryId videoLibraryId)
    case maybeVideoLibrary of
      Just videoLibrary -> do
        case getFilePathFromFileURI (videoLibraryUrl videoLibrary) of
          Just filePath -> do
            files <- liftIO $ findAllFilesInLibrary filePath
            json files
          Nothing -> status status422
      Nothing -> status status404
  get "/api/video_libraries/:videoLibraryId/new_files" $ do
    videoLibraryId :: Int <- param "videoLibraryId"
    connection <- lift $ asks psqlConnection
    maybeVideoLibrary <- liftIO $ getVideoLibraryById connection (VideoLibraryId videoLibraryId)
    case maybeVideoLibrary of
      Just videoLibrary -> do
        case getFilePathFromFileURI (videoLibraryUrl videoLibrary) of
          Just filePath -> do
            maybeFiles <- liftIO $ findAllFilesInLibrary filePath
            case maybeFiles of
              Just files -> do
                videoFiles <- liftIO $ getVideoFiles connection Nothing Nothing
                json (files \\ (map (unVideoFileStorageId . videoFileStorageId) videoFiles))
              Nothing -> status status500
          Nothing -> status status422
      Nothing -> status status404
  get "/api/videos/:id" $ do
    id :: Int <- param "id"
    connection <- lift $ asks psqlConnection
    maybeVideoLibrary <- liftIO $ getVideoLibraryById connection (VideoLibraryId id)
    case maybeVideoLibrary of
      Nothing -> status status404
      Just videoLibrary -> json videoLibrary

newtype FilePath = FilePath { unFilePath :: Text }

findAllFilesInLibrary :: FilePath -> IO (Maybe [Text])
findAllFilesInLibrary filePath = do
    files <- listDirectory ((unpack . unFilePath) filePath)
    return $ Just (map pack files)

getFilePathFromFileURI :: Text -> Maybe (FilePath)
getFilePathFromFileURI uri =
  case maybePath of
    Just path -> Just $ FilePath path
    Nothing -> Nothing
  where maybePath = stripPrefix filePathPrefix uri

filePathPrefix :: Text
filePathPrefix = "file://"
