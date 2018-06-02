{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Database.File
(
  getFile
, getFiles
, insertFile
) where

import Control.Exception (throw)
import Data.ByteString (ByteString, concat)
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple (Connection, Only(Only), query, query_)
import Database.PostgreSQL.Simple.Types (Query(Query))
import Prelude hiding (concat)

import Database.Error (DatabaseException(..))
import Crik.Types
import Crik.Types.Video
import Crik.Types.File
import Database.Instance

getFiles :: Connection -> Maybe VideoId -> Maybe FileId -> IO ([File FileId])
getFiles connection Nothing Nothing = do
  rows <- query_ connection (Query $ concat ["select ", allColumns, " from video_files"])
  return (rows)
getFiles connection (Just videoId) Nothing = do
  rows <- query connection (Query $ concat ["select ", allColumns, " from video_files where video_id = ?"])
    (Only videoId)
  return (rows)
getFiles connection Nothing (Just fileId) = do
  rows <- query connection (Query $ concat ["select ", allColumns, " from video_files where id = ?"]) (Only fileId)
  return (rows)
getFiles connection (Just videoId) (Just fileId) = do
  rows <- query connection (Query $ concat ["select ", allColumns, " from video_files where video_id = ? and id = ?"])
    (videoId, fileId)
  return (rows)

getFile :: Connection -> Maybe VideoId -> FileId -> IO (Maybe (File FileId))
getFile connection maybeVideoId fileId = do
  rows <- getFiles connection maybeVideoId (Just fileId)
  return (listToMaybe rows)

insertFile :: Connection -> File NoId -> IO (File FileId)
insertFile connection filePost = do
  rows <- query connection
    (Query $ concat ["insert into video_files (", insertColumns, ") values (?, ?, ?, ?) returning ", allColumns])
    filePost
  case rows of
    [] -> throw $ InsertReturnedNothing
      "insertFile returned nothing when it should have returned the inserted video"
    [x] -> return x
    _ -> throw $ InsertReturnedMultiple "insertFile returned multiple rows when it should have returned just one"

allColumns :: ByteString
allColumns = concat ["id, ", insertColumns]

insertColumns :: ByteString
insertColumns = "video_id, url, video_library_id, storage_id"
