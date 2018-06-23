{-# LANGUAGE RecordWildCards #-}

module Database.Library
(
  getAllLibraries
, getLibraryById
, getLibraryByName
, insertLibrary
, updateLibrary
) where

import Control.Exception (throw)
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple (Connection, Only(Only), query, query_)
import Data.Text (Text)

import Database.Error (DatabaseException(..))
import Crik.Types (NoId)
import Crik.Types.Library
import Database.Instance

getAllLibraries :: Connection -> IO ([Library LibraryId])
getAllLibraries connection = do
  rows <- query_ connection "select * from libraries"
  return (rows)

getLibraryById :: Connection -> LibraryId -> IO (Maybe (Library LibraryId))
getLibraryById connection libraryId = do
  rows <- query connection "select * from libraries where id = ?" (Only libraryId)
  return (listToMaybe rows)

getLibraryByName :: Connection -> Text -> IO (Maybe (Library LibraryId))
getLibraryByName connection name = do
  print name
  rows <- query connection "select * from libraries where name = ?" (Only name)
  return (listToMaybe rows)

insertLibrary :: Connection -> Library NoId -> IO (Library LibraryId)
insertLibrary connection Library{..} = do
  rows <- query connection
    "insert into libraries (url, name) values (?, ?) returning id, url, name"
      (libraryUrl, libraryName)
  case rows of
    [] -> throw $ InsertReturnedNothing
      "insertLibrary returned nothing when it should have returned the inserted Library"
    [x] -> return x
    _ -> throw $ InsertReturnedMultiple
      "insertLibrary returned multiple rows when it should have returned just one"

updateLibrary :: Connection -> LibraryId -> Library NoId -> IO (Maybe (Library LibraryId))
updateLibrary connection libraryId (Library _ url name) = do
  rows <- query connection "update libraries set url = ?, name = ? where id = ? returning id, url"
    (url, name, libraryId)
  case rows of
    [] -> return Nothing
    [x] -> return $ Just x
    _ -> throw $ InsertReturnedMultiple
      "updateLibrary returned multiple rows when it should have returned just one"
