
import Control.Monad.IO.Class (liftIO)
import Data.List ((\\))
import Data.Text (Text, isPrefixOf, stripPrefix, pack, unpack)
import Database.PostgreSQL.Simple (connectPostgreSQL)
import Network.HTTP.Types.Status (status404, status422, status500)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Prelude hiding (FilePath)
import System.Directory (listDirectory)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Web.Scotty (get, json, jsonData, middleware, param, post, scotty, status)

import Database.Video (getAllVideos, getVideoById, insertVideo)
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

main :: IO ()
main = do
  -- Connection info gets passed via environment variables
  psqlConnection <- connectPostgreSQL ""
  port <- getPort
  scotty port $ do
    middleware logStdoutDev
    get "/api/videos" $ do
      videos <- liftIO $ getAllVideos psqlConnection
      json videos
    post "/api/videos" $ do
      newVideo <- jsonData
      insertedVideo <- liftIO $ insertVideo psqlConnection newVideo
      json insertedVideo
    get "/api/videos/:id" $ do
      id :: Int <- param "id"
      maybeVideo <- liftIO $ getVideoById psqlConnection (VideoId id)
      case maybeVideo of
        Nothing -> status status404
        Just video -> json video
    get "/api/videos/:id/files" $ do
      id :: Int <- param "id"
      videos <- liftIO $ getVideoFiles psqlConnection (Just (VideoId id)) Nothing
      json videos
    get "/api/videos/files" $ do
      videos <- liftIO $ getVideoFiles psqlConnection Nothing Nothing
      json videos
    get "/api/videos/files/:id" $ do
      id :: Int <- param "id"
      maybeVideo <- liftIO $ getVideoFile psqlConnection Nothing (VideoFileId id)
      case maybeVideo of
        Nothing -> status status404
        Just video -> json video
    get "/api/videos/:videoId/files/:videoFileId" $ do
      videoId :: Int <- param "videoId"
      videoFileId :: Int <- param "videoFileId"
      maybeVideo <- liftIO $ getVideoFile psqlConnection (Just (VideoId videoId)) (VideoFileId videoFileId)
      case maybeVideo of
        Nothing -> status status404
        Just video -> json video
    get "/api/video_libraries" $ do
      videoLibraries <- liftIO $ getAllVideoLibraries psqlConnection
      json videoLibraries
    get "/api/video_libraries/:videoLibraryId" $ do
      videoLibraryId :: Int <- param "videoLibraryId"
      videoLibraries <- liftIO $ getVideoLibraryById psqlConnection (VideoLibraryId videoLibraryId)
      json videoLibraries
    get "/api/video_libraries/:videoLibraryId/all_files" $ do
      videoLibraryId :: Int <- param "videoLibraryId"
      maybeVideoLibrary <- liftIO $ getVideoLibraryById psqlConnection (VideoLibraryId videoLibraryId)
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
      maybeVideoLibrary <- liftIO $ getVideoLibraryById psqlConnection (VideoLibraryId videoLibraryId)
      case maybeVideoLibrary of
        Just videoLibrary -> do
          case getFilePathFromFileURI (videoLibraryUrl videoLibrary) of
            Just filePath -> do
              maybeFiles <- liftIO $ findAllFilesInLibrary filePath
              case maybeFiles of
                Just files -> do
                  videoFiles <- liftIO $ getVideoFiles psqlConnection Nothing Nothing
                  json (files \\ (map (unVideoFileStorageId . videoFileStorageId) videoFiles))
                Nothing -> status status500
            Nothing -> status status422
        Nothing -> status status404
    get "/api/videos/:id" $ do
      id :: Int <- param "id"
      maybeVideoLibrary <- liftIO $ getVideoLibraryById psqlConnection (VideoLibraryId id)
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
