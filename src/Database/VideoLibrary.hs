module Database.VideoLibrary
(
  getAllVideoLibraries
, getVideoLibraryById
) where

import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple (Connection, Only(Only), query, query_)

import Types.VideoLibrary (VideoLibraryId, VideoLibrary)

getAllVideoLibraries :: Connection -> IO ([VideoLibrary])
getAllVideoLibraries connection = do
  rows <- query_ connection "select * from video_libraries"
  return (rows)

getVideoLibraryById :: Connection -> VideoLibraryId -> IO (Maybe VideoLibrary)
getVideoLibraryById connection videoLibraryId = do
  rows <- query connection "select * from video_libraries where id = ?" (Only videoLibraryId)
  return (listToMaybe rows)
