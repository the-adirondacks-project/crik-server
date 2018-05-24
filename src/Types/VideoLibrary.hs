{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.VideoLibrary
(
  VideoLibraryId(..)
, VideoLibrary(..)
) where

import Data.Aeson (FromJSON(parseJSON), Value(Object), ToJSON(toJSON, toEncoding), (.=), (.:), object, pairs)
import Data.Aeson.TH (deriveJSON, defaultOptions, unwrapUnaryRecords)
import Data.Aeson.Types (typeMismatch)
import Data.Semigroup (Semigroup ((<>)))
import Database.PostgreSQL.Simple.FromField (FromField(fromField))
import Database.PostgreSQL.Simple.FromRow (field, FromRow(fromRow))
import Database.PostgreSQL.Simple.ToField (ToField(toField))
import Database.PostgreSQL.Simple.ToRow (ToRow(toRow))
import Data.Text (Text)
import GHC.Generics (Generic)

import Types (NoId(NoId))
import Types.Video (VideoId)

newtype VideoLibraryId = VideoLibraryId { unVideoLibraryId :: Int } deriving (Generic)

instance FromField VideoLibraryId where
  fromField field rawData = do
    videoLibraryId <- fromField field rawData
    return $ VideoLibraryId videoLibraryId

instance ToField VideoLibraryId where
  toField VideoLibraryId{..} = toField unVideoLibraryId

$(deriveJSON defaultOptions{unwrapUnaryRecords=True} ''VideoLibraryId)

data VideoLibrary id = VideoLibrary { videoLibraryId :: id, videoLibraryUrl :: Text } deriving (Generic)

instance (FromField t) => FromRow (VideoLibrary t) where
  fromRow = VideoLibrary <$> field <*> field

instance ToRow (VideoLibrary NoId) where
  toRow VideoLibrary{..} = [toField videoLibraryUrl]

instance ToRow (VideoLibrary VideoLibraryId) where
  toRow VideoLibrary{..} = [toField videoLibraryId, toField videoLibraryUrl]

instance ToJSON (VideoLibrary VideoLibraryId) where
  toJSON VideoLibrary{..} = object ["id" .= videoLibraryId, "url" .= videoLibraryUrl]
  toEncoding VideoLibrary{..} = pairs ("id" .= videoLibraryId <> "url" .= videoLibraryUrl)

instance FromJSON (VideoLibrary VideoLibraryId) where
  parseJSON (Object v) = do
    id <- v .: "id"
    url <- v .: "url"
    return (VideoLibrary (VideoLibraryId id) url)
  parseJSON invalid = typeMismatch "VideoLibrary" invalid

instance FromJSON (VideoLibrary NoId) where
  parseJSON (Object v) = do
    url <- v .: "url"
    return (VideoLibrary NoId url)
  parseJSON invalid = typeMismatch "VideoLibrary" invalid
