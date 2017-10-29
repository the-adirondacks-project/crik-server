
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, MonadReader, asks, lift, runReaderT)
import Data.List ((\\))
import Data.Text (Text, isPrefixOf, stripPrefix, pack, unpack)
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL)
import Network.HTTP.Types.Status (status404, status422, status500)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Prelude hiding (FilePath)
import System.Directory (listDirectory)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Web.Scotty.Trans (get, json, jsonData, middleware, param, post, put, scottyT, status)

import Config (Config(..), ConfigM(..))
import Routes.Video (setupVideoRoutes)
import Database.Video (getAllVideos, getVideoById, insertVideo, updateVideo)
import Database.VideoFile (getVideoFile, getVideoFiles)
import Database.VideoLibrary (getVideoLibraryById, getAllVideoLibraries)
import Types.Video (VideoId(VideoId))
import Types.VideoFile (VideoFileId(VideoFileId), videoFileStorageId, unVideoFileStorageId)
import Types.VideoLibrary (VideoLibraryId(VideoLibraryId), VideoLibrary(videoLibraryUrl))

maybeGetPort :: IO (Maybe Int)
maybeGetPort = do
  maybeRawPort <- lookupEnv "PORT"
  case maybeRawPort of
    Nothing -> return Nothing
    Just rawPort -> return (readMaybe rawPort :: Maybe Int)

getPort :: IO (Int)
getPort = do
  maybePort <- maybeGetPort
  case maybePort of
    Nothing -> return 8015
    Just x -> return x

getConfig :: IO (Config)
getConfig = do
  psqlConnection <- connectPostgreSQL ""
  return $ Config psqlConnection

main :: IO ()
main = do
  -- Connection info gets passed via environment variables
  config <- getConfig
  port <- getPort
  scottyT port (\m -> runReaderT (runConfigM m) config) $ do
    middleware logStdoutDev
    setupVideoRoutes
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
