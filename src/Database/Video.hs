module Database.Video
(
  getAllVideos
, getVideoById
) where

import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple (Connection, Only(Only), query, query_)

import Types.Video (VideoId, Video)

getAllVideos :: Connection -> IO ([Video])
getAllVideos connection = do
  rows <- query_ connection "select * from videos"
  return (rows)

getVideoById :: Connection -> VideoId -> IO (Maybe Video)
getVideoById connection videoId = do
  rows <- query connection "select * from videos where id = ?" (Only videoId)
  return (listToMaybe rows)
