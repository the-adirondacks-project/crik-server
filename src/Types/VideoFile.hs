module Types.VideoFile
(
  VideoFileId(..)
, VideoFile(..)
) where

import Data.Aeson (ToJSON(toJSON, toEncoding), FromJSON(parseJSON), (.=), object, pairs)
import Data.Semigroup (Semigroup ((<>)))
import Database.PostgreSQL.Simple.FromField (FromField(fromField))
import Database.PostgreSQL.Simple.FromRow (field, FromRow(fromRow))
import Database.PostgreSQL.Simple.ToField (ToField(toField))
import Database.PostgreSQL.Simple.ToRow (ToRow(toRow))
import Data.Text (Text)

import Types.Video (VideoId)

newtype VideoFileId = VideoFileId { unVideoFileId :: Int }

instance FromField VideoFileId where
  fromField field rawData = do
    videoFileId <- fromField field rawData
    return $ VideoFileId videoFileId

instance ToField VideoFileId where
  toField VideoFileId{..} = toField unVideoFileId

instance ToJSON VideoFileId where
  toJSON VideoFileId{..} = toJSON unVideoFileId
  toEncoding VideoFileId{..} = toEncoding unVideoFileId

instance FromJSON VideoFileId where
  parseJSON value = do
    videoFileId <- parseJSON value
    return (VideoFileId videoFileId)

data VideoFile = VideoFile { videoFileId :: VideoFileId, videoId :: VideoId, videoFileUrl :: Text }

instance FromRow VideoFile where
  fromRow = VideoFile <$> field <*> field <*> field

instance ToRow VideoFile where
  toRow VideoFile{..} = [toField videoFileId, toField videoId, toField videoFileUrl]

instance ToJSON VideoFile where
  toJSON VideoFile{..} = object ["id" .= videoFileId, "url" .= videoFileUrl]
  toEncoding VideoFile{..} = pairs ("id" .= videoFileId <> "url" .= videoFileUrl)
