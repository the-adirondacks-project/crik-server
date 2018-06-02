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
import Crik.Types.VideoFile
import Crik.Types.VideoLibrary

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
instance FromField VideoFileId where
  fromField field rawData = do
    videoFileId <- fromField field rawData
    return $ VideoFileId videoFileId

instance ToField VideoFileId where
  toField VideoFileId{..} = toField unVideoFileId

instance FromField VideoFileStorageId where
  fromField field rawData = do
    videoFileStorageId <- fromField field rawData
    return $ VideoFileStorageId videoFileStorageId

instance ToField VideoFileStorageId where
  toField VideoFileStorageId{..} = toField unVideoFileStorageId

instance FromField id => FromRow (VideoFile id) where
  fromRow = VideoFile <$> field <*> field <*> field <*> field <*> field

instance ToRow (VideoFile VideoFileId) where
  toRow VideoFile{..} = [
      toField videoFileId,
      toField videoId,
      toField videoFileUrl,
      toField videoLibraryId,
      toField videoFileStorageId
    ]

instance ToRow (VideoFile NoId) where
  toRow VideoFile{..} = [
      toField videoId,
      toField videoFileUrl,
      toField videoLibraryId,
      toField videoFileStorageId
    ]

-- Video Libraries
instance FromField VideoLibraryId where
  fromField field rawData = do
    videoLibraryId <- fromField field rawData
    return $ VideoLibraryId videoLibraryId

instance ToField VideoLibraryId where
  toField VideoLibraryId{..} = toField unVideoLibraryId

instance (FromField t) => FromRow (VideoLibrary t) where
  fromRow = VideoLibrary <$> field <*> field

instance ToRow (VideoLibrary NoId) where
  toRow VideoLibrary{..} = [toField videoLibraryUrl]

instance ToRow (VideoLibrary VideoLibraryId) where
  toRow VideoLibrary{..} = [toField videoLibraryId, toField videoLibraryUrl]
