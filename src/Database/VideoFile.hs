module Database.VideoFile
(
  getVideoFile
, getVideoFiles
) where

import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple (Connection, Only(Only), query, query_)

import Types.Video (VideoId)
import Types.VideoFile (VideoFileId, VideoFile)

getVideoFiles :: Connection -> Maybe VideoId -> Maybe VideoFileId -> IO ([VideoFile])
getVideoFiles connection Nothing Nothing = do
  rows <- query_ connection "select id, video_id, url from video_files"
  return (rows)
getVideoFiles connection (Just videoId) Nothing = do
  rows <- query connection "select id, video_id, url from video_files where video_id = ?" (Only videoId)
  return (rows)
getVideoFiles connection Nothing (Just videoFileId) = do
  rows <- query connection "select id, video_id, url from video_files where id = ?" (Only videoFileId)
  return (rows)
getVideoFiles connection (Just videoId) (Just videoFileId) = do
  rows <- query connection "select id, video_id, url from video_files where video_id = ? and id = ?"
    (videoId, videoFileId)
  return (rows)

getVideoFile :: Connection -> Maybe VideoId -> VideoFileId -> IO (Maybe VideoFile)
getVideoFile connection maybeVideoId videoFileId = do
  rows <- getVideoFiles connection maybeVideoId (Just videoFileId)
  return (listToMaybe rows)
