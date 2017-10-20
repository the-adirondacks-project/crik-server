module Types.Video
(
  VideoId(..)
, Video(..)
) where


import Data.Aeson (ToJSON(toJSON, toEncoding), (.=), object, pairs)
import Data.Aeson.TH (deriveJSON, defaultOptions, unwrapUnaryRecords, fieldLabelModifier)
import Data.Char (toLower)
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

$(deriveJSON defaultOptions{unwrapUnaryRecords=True} ''VideoId)

data Video = Video { videoId :: VideoId, videoName :: Text }

instance FromRow Video where
  fromRow = Video <$> field <*> field

instance ToRow Video where
  toRow Video{..} = [toField videoId, toField videoName]

$(deriveJSON defaultOptions{fieldLabelModifier=(drop 5 . map toLower)} ''Video)
