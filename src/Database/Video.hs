module Database.Video
(
  getVideoById
) where

import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple (query, Connection, Only(Only))

import Types.Video (VideoId, Video)

getVideoById :: Connection -> VideoId -> IO (Maybe Video)
getVideoById connection videoId = do
  rows <- query connection "select * from videos where id = ?" (Only videoId)
  return (listToMaybe rows)
