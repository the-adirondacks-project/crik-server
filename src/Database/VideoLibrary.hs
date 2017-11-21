module Database.VideoLibrary
(
  getAllVideoLibraries
, getVideoLibraryById
, insertVideoLibrary
) where

import Control.Exception (throw)
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple (Connection, Only(Only), query, query_)

import Database.Error (DatabaseException(..))
import Types (NoId)
import Types.VideoLibrary (VideoLibraryId, VideoLibrary)

getAllVideoLibraries :: Connection -> IO ([VideoLibrary VideoLibraryId])
getAllVideoLibraries connection = do
  rows <- query_ connection "select * from video_libraries"
  return (rows)

getVideoLibraryById :: Connection -> VideoLibraryId -> IO (Maybe (VideoLibrary VideoLibraryId))
getVideoLibraryById connection videoLibraryId = do
  rows <- query connection "select * from video_libraries where id = ?" (Only videoLibraryId)
  return (listToMaybe rows)

insertVideoLibrary :: Connection -> VideoLibrary NoId -> IO (VideoLibrary VideoLibraryId)
insertVideoLibrary connection videoLibraryPost = do
  rows <- query connection "insert into video_libraries (url) values (?) returning id, url" videoLibraryPost
  case rows of
    [] -> throw $ InsertReturnedNothing
      "insertVideoLibrary returned nothing when it should have returned the inserted videoLibrary"
    [x] -> return x
    _ -> throw $ InsertReturnedMultiple
      "insertVideoLibrary returned multiple rows when it should have returned just one"
