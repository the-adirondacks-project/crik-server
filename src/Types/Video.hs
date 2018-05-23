{-# LANGUAGE DeriveGeneric #-}

module Types.Video
(
  NoId(..)
, VideoId(..)
, Video(..)
) where

import Data.Aeson (ToJSON(toJSON, toEncoding), FromJSON(..), (.=), object, pairs)
import Data.Aeson.Types
import Data.Aeson.TH (deriveJSON, defaultOptions, unwrapUnaryRecords)
import Data.Semigroup (Semigroup ((<>)))
import Database.PostgreSQL.Simple.FromField (FromField(fromField))
import Database.PostgreSQL.Simple.FromRow (field, FromRow(fromRow))
import Database.PostgreSQL.Simple.ToField (ToField(toField))
import Database.PostgreSQL.Simple.ToRow (ToRow(toRow))
import Data.Text (Text)
import GHC.Generics (Generic)

import Types (NoId(NoId))

newtype VideoId = VideoId { unVideoId :: Int } deriving (Show, Generic)

instance FromField VideoId where
  fromField field rawData = do
    videoId <- fromField field rawData
    return $ VideoId videoId

instance ToField VideoId where
  toField VideoId{..} = toField unVideoId

$(deriveJSON defaultOptions{unwrapUnaryRecords=True} ''VideoId)

data Video id = Video { videoId :: id , videoName :: Text } deriving (Show, Generic)

instance (FromField t) => FromRow (Video t) where
  fromRow = Video <$> field <*> field

instance ToRow (Video VideoId) where
  toRow Video{..} = [toField videoId, toField videoName]

instance ToRow (Video NoId) where
  toRow Video{..} = [toField videoName]

instance ToJSON (Video VideoId) where
  toJSON Video{..} = object [
      "id" .= videoId,
      "name" .= videoName
    ]
  toEncoding Video{..} = pairs (
      "id" .= videoId <>
      "name" .= videoName
    )

instance FromJSON (Video VideoId) where
  parseJSON (Object v) = do
    id <- v .: "id"
    name <- v .: "name"
    return (Video (VideoId id) name)
  parseJSON invalid = typeMismatch "Video" invalid

instance FromJSON (Video (Maybe VideoId)) where
  parseJSON (Object v) = do
    id <- v .:? "id"
    name <- v .: "name"
    return (Video (VideoId <$> id) name)
  parseJSON invalid = typeMismatch "Video" invalid

instance FromJSON (Video NoId) where
  parseJSON (Object v) = do
    name <- v .: "name"
    return (Video NoId name)
  parseJSON invalid = typeMismatch "Video" invalid
