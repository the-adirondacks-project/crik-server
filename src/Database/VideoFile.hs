module Database.VideoFile
(
  getVideoFile
, getVideoFiles
, insertVideoFile
) where

import Control.Exception (throw)
import Data.ByteString (ByteString, concat)
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple (Connection, Only(Only), query, query_)
import Database.PostgreSQL.Simple.Types (Query(Query))
import Prelude hiding (concat)

import Database.Error (DatabaseException(..))
import Types (NoId)
import Types.Video (VideoId)
import Types.VideoFile (VideoFileId, VideoFile)

getVideoFiles :: Connection -> Maybe VideoId -> Maybe VideoFileId -> IO ([VideoFile VideoFileId])
getVideoFiles connection Nothing Nothing = do
  rows <- query_ connection (Query $ concat ["select ", allColumns, " from video_files"])
  return (rows)
getVideoFiles connection (Just videoId) Nothing = do
  rows <- query connection (Query $ concat ["select ", allColumns, " from video_files where video_id = ?"])
    (Only videoId)
  return (rows)
getVideoFiles connection Nothing (Just videoFileId) = do
  rows <- query connection (Query $ concat ["select ", allColumns, " from video_files where id = ?"]) (Only videoFileId)
  return (rows)
getVideoFiles connection (Just videoId) (Just videoFileId) = do
  rows <- query connection (Query $ concat ["select ", allColumns, " from video_files where video_id = ? and id = ?"])
    (videoId, videoFileId)
  return (rows)

getVideoFile :: Connection -> Maybe VideoId -> VideoFileId -> IO (Maybe (VideoFile VideoFileId))
getVideoFile connection maybeVideoId videoFileId = do
  rows <- getVideoFiles connection maybeVideoId (Just videoFileId)
  return (listToMaybe rows)

insertVideoFile :: Connection -> VideoFile NoId -> IO (VideoFile VideoFileId)
insertVideoFile connection videoFilePost = do
  rows <- query connection
    (Query $ concat ["insert into video_files (", insertColumns, ") values (?, ?, ?, ?) returning ", allColumns])
    videoFilePost
  case rows of
    [] -> throw $ InsertReturnedNothing
      "insertVideoFile returned nothing when it should have returned the inserted video"
    [x] -> return x
    _ -> throw $ InsertReturnedMultiple "insertVideoFile returned multiple rows when it should have returned just one"

allColumns :: ByteString
allColumns = concat ["id, ", insertColumns]

insertColumns :: ByteString
insertColumns = "video_id, url, video_library_id, storage_id"
