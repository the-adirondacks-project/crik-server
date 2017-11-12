{-# LANGUAGE DeriveGeneric #-}

module Types.VideoFile
(
  VideoFile(..)
, VideoFileId(..)
, VideoFileStorageId(..)
) where

import Data.Aeson (ToJSON(toJSON, toEncoding), FromJSON(parseJSON), (.=), object, pairs)
import Data.Aeson.TH (deriveJSON, defaultOptions, unwrapUnaryRecords, fieldLabelModifier)
import Data.Semigroup (Semigroup ((<>)))
import Database.PostgreSQL.Simple.FromField (FromField(fromField))
import Database.PostgreSQL.Simple.FromRow (field, FromRow(fromRow))
import Database.PostgreSQL.Simple.ToField (ToField(toField))
import Database.PostgreSQL.Simple.ToRow (ToRow(toRow))
import Data.Text (Text)
import GHC.Generics (Generic)

import Types.Video (VideoId)
import Types.VideoLibrary (VideoLibraryId)

newtype VideoFileId = VideoFileId { unVideoFileId :: Int } deriving (Generic)

$(deriveJSON defaultOptions{unwrapUnaryRecords=True} ''VideoFileId)

instance FromField VideoFileId where
  fromField field rawData = do
    videoFileId <- fromField field rawData
    return $ VideoFileId videoFileId

instance ToField VideoFileId where
  toField VideoFileId{..} = toField unVideoFileId

newtype VideoFileStorageId = VideoFileStorageId { unVideoFileStorageId :: Text } deriving (Generic)

$(deriveJSON defaultOptions{unwrapUnaryRecords=True} ''VideoFileStorageId)

instance FromField VideoFileStorageId where
  fromField field rawData = do
    videoFileStorageId <- fromField field rawData
    return $ VideoFileStorageId videoFileStorageId

instance ToField VideoFileStorageId where
  toField VideoFileStorageId{..} = toField unVideoFileStorageId

data VideoFile = VideoFile {
  videoFileId :: VideoFileId,
  videoId :: VideoId,
  videoFileUrl :: Text,
  videoLibraryId :: VideoLibraryId,
  videoFileStorageId :: VideoFileStorageId
} deriving (Generic)

instance FromRow VideoFile where
  fromRow = VideoFile <$> field <*> field <*> field <*> field <*> field

instance ToRow VideoFile where
  toRow VideoFile{..} = [
      toField videoFileId,
      toField videoId,
      toField videoFileUrl,
      toField videoLibraryId,
      toField videoFileStorageId
    ]

instance ToJSON VideoFile where
  toJSON VideoFile{..} = object [
      "id" .= videoFileId,
      "videoId" .= videoId,
      "url" .= videoFileUrl,
      "libraryId" .= videoLibraryId,
      "storageId" .= videoFileStorageId
    ]
  toEncoding VideoFile{..} = pairs (
      "id" .= videoFileId <>
      "videoId" .= videoId <>
      "url" .= videoFileUrl <>
      "libraryId" .= videoLibraryId <>
      "storageId" .= videoFileStorageId
    )
