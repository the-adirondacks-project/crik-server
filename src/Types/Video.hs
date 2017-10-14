module Types.Video
(
  VideoId(..)
, Video(..)
) where

import Data.Aeson (ToJSON(toJSON, toEncoding), FromJSON(parseJSON), (.=), object, pairs)
import Data.Semigroup (Semigroup ((<>)))
import Database.PostgreSQL.Simple.FromField (FromField(fromField))
import Database.PostgreSQL.Simple.FromRow (field, FromRow(fromRow))
import Database.PostgreSQL.Simple.ToField (ToField(toField))
import Database.PostgreSQL.Simple.ToRow (ToRow(toRow))
import Data.Text (Text)

newtype VideoId = VideoId { unVideoId :: Int }

instance FromField VideoId where
  fromField field rawData = do
    videoId <- fromField field rawData
    return $ VideoId videoId

instance ToField VideoId where
  toField VideoId{..} = toField unVideoId

instance ToJSON VideoId where
  toJSON VideoId{..} = toJSON unVideoId
  toEncoding VideoId{..} = toEncoding unVideoId

instance FromJSON VideoId where
  parseJSON value = do
    videoId <- parseJSON value
    return (VideoId videoId)

data Video = Video { videoId :: VideoId, videoName :: Text }

instance FromRow Video where
  fromRow = Video <$> field <*> field

instance ToRow Video where
  toRow Video{..} = [toField videoId, toField videoName]

instance ToJSON Video where
  toJSON Video{..} = object ["id" .= videoId, "name" .= videoName]
  toEncoding Video{..} = pairs ("id" .= videoId <> "name" .= videoName)

