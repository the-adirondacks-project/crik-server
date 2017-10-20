module Types.VideoLibrary
(
  VideoLibraryId(..)
, VideoLibrary(..)
) where

import Data.Aeson.TH (deriveJSON, defaultOptions, unwrapUnaryRecords)
import Data.Aeson (ToJSON(toJSON, toEncoding), (.=), object, pairs)
import Data.Semigroup (Semigroup ((<>)))
import Database.PostgreSQL.Simple.FromField (FromField(fromField))
import Database.PostgreSQL.Simple.FromRow (field, FromRow(fromRow))
import Database.PostgreSQL.Simple.ToField (ToField(toField))
import Database.PostgreSQL.Simple.ToRow (ToRow(toRow))
import Data.Text (Text)

import Types.Video (VideoId)

newtype VideoLibraryId = VideoLibraryId { unVideoLibraryId :: Int }

instance FromField VideoLibraryId where
  fromField field rawData = do
    videoLibraryId <- fromField field rawData
    return $ VideoLibraryId videoLibraryId

instance ToField VideoLibraryId where
  toField VideoLibraryId{..} = toField unVideoLibraryId

$(deriveJSON defaultOptions{unwrapUnaryRecords=True} ''VideoLibraryId)

data VideoLibrary = VideoLibrary { videoLibraryId :: VideoLibraryId, videoLibraryUrl :: Text }

instance FromRow VideoLibrary where
  fromRow = VideoLibrary <$> field <*> field

instance ToRow VideoLibrary where
  toRow VideoLibrary{..} = [toField videoLibraryId, toField videoLibraryUrl]

instance ToJSON VideoLibrary where
  toJSON VideoLibrary{..} = object ["id" .= videoLibraryId, "url" .= videoLibraryUrl]
  toEncoding VideoLibrary{..} = pairs ("id" .= videoLibraryId <> "url" .= videoLibraryUrl)
