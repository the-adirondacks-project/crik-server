{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Instance () where

import Database.PostgreSQL.Simple.FromField (FromField(fromField))
import Database.PostgreSQL.Simple.FromRow (field, FromRow(fromRow))
import Database.PostgreSQL.Simple.ToField (ToField(toField))
import Database.PostgreSQL.Simple.ToRow (ToRow(toRow))
import Database.PostgreSQL.Simple.Types (Query(Query))

import Crik.Types
import Crik.Types.Video
import Crik.Types.File
import Crik.Types.Library

-- Videos
instance FromField VideoId where
  fromField field rawData = do
    videoId <- fromField field rawData
    return $ VideoId videoId

instance ToField VideoId where
  toField VideoId{..} = toField unVideoId

instance (FromField t) => FromRow (Video t) where
  fromRow = Video <$> field <*> field

instance ToRow (Video VideoId) where
  toRow Video{..} = [toField videoId, toField videoName]

instance ToRow (Video NoId) where
  toRow Video{..} = [toField videoName]

-- Video Files
instance FromField FileId where
  fromField field rawData = do
    videoFileId <- fromField field rawData
    return $ FileId videoFileId

instance ToField FileId where
  toField FileId{..} = toField unFileId

instance FromField FileStorageId where
  fromField field rawData = do
    videoFileStorageId <- fromField field rawData
    return $ FileStorageId videoFileStorageId

instance ToField FileStorageId where
  toField FileStorageId{..} = toField unFileStorageId

instance FromField id => FromRow (File id) where
  fromRow = File <$> field <*> field <*> field <*> field <*> field

instance ToRow (File FileId) where
  toRow File{..} = [
      toField fileId,
      toField videoId,
      toField fileUrl,
      toField libraryId,
      toField fileStorageId
    ]

instance ToRow (File NoId) where
  toRow File{..} = [
      toField videoId,
      toField fileUrl,
      toField libraryId,
      toField fileStorageId
    ]

-- Video Libraries
instance FromField LibraryId where
  fromField field rawData = do
    videoLibraryId <- fromField field rawData
    return $ LibraryId videoLibraryId

instance ToField LibraryId where
  toField LibraryId{..} = toField unLibraryId

instance (FromField t) => FromRow (Library t) where
  fromRow = Library <$> field <*> field <*> field

instance ToRow (Library NoId) where
  toRow Library{..} = [toField libraryUrl, toField libraryName]

instance ToRow (Library LibraryId) where
  toRow Library{..} = [toField libraryId, toField libraryUrl, toField libraryName]
