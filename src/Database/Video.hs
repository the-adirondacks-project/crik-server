{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Video
(
  getAllVideos
, getVideoById
, getVideoByName
, insertVideo
, updateVideo
) where

import Control.Exception (throw)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, Only(Only), query, query_)

import Database.Error (DatabaseException(..))
import Crik.Types
import Crik.Types.Video
import Database.Instance

getAllVideos :: Connection -> IO ([Video VideoId])
getAllVideos connection = do
  rows <- query_ connection "select * from videos"
  return (rows)

getVideoById :: Connection -> VideoId -> IO (Maybe (Video VideoId))
getVideoById connection videoId = do
  rows <- query connection "select * from videos where id = ?" (Only videoId)
  return (listToMaybe rows)

getVideoByName :: Connection -> Text -> IO (Maybe (Video VideoId))
getVideoByName connection name = do
  rows <- query connection "select * from videos where name = ?" (Only name)
  return (listToMaybe rows)

insertVideo :: Connection -> Video NoId -> IO (Video VideoId)
insertVideo connection videoPost = do
  rows <- query connection "insert into videos (name) values (?) returning id, name" videoPost
  case rows of
    [] -> throw $ InsertReturnedNothing "insertVideo returned nothing when it should have returned the inserted video"
    [x] -> return x
    _ -> throw $ InsertReturnedMultiple "insertVideo returned multiple rows when it should have returned just one"

updateVideo :: Connection -> VideoId -> Video (Maybe VideoId) -> IO (Maybe (Video VideoId))
updateVideo connection videoId videoPost = do
  rows <- query connection "update videos set name = ? where id = ? returning id, name" (videoName videoPost, videoId)
  case rows of
    [] -> return Nothing
    [x] -> return $ Just x
    _ -> throw $ InsertReturnedMultiple "updateVideo returned multiple rows when it should have returned just one"
