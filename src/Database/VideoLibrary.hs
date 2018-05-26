module Database.VideoLibrary
(
  getAllVideoLibraries
, getVideoLibraryById
, insertVideoLibrary
, updateVideoLibrary
) where

import Control.Exception (throw)
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple (Connection, Only(Only), query, query_)

import Database.Error (DatabaseException(..))
import Crik.Types (NoId)
import Crik.Types.VideoLibrary (VideoLibraryId, VideoLibrary(..))
import Database.Instance

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

updateVideoLibrary :: Connection -> VideoLibraryId -> VideoLibrary NoId -> IO (Maybe (VideoLibrary VideoLibraryId))
updateVideoLibrary connection videoLibraryId videoLibraryPost = do
  rows <- query connection "update video_libraries set url = ? where id = ? returning id, url"
    (videoLibraryUrl videoLibraryPost, videoLibraryId)
  case rows of
    [] -> return Nothing
    [x] -> return $ Just x
    _ -> throw $ InsertReturnedMultiple
      "updateVideoLibrary returned multiple rows when it should have returned just one"
