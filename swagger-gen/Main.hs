{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

import Types.Video
import Types.VideoLibrary
import Types.VideoFile
import qualified Data.ByteString.Lazy.Char8 as BL8
import Control.Lens
import Data.Text (Text)
import Servant.Swagger
import Servant.API
import Data.Swagger
import Data.Proxy (Proxy(..))
import API
import Data.Aeson.Encode.Pretty

myOptions = defaultSchemaOptions{unwrapUnaryRecords=True}

instance ToSchema VideoFile
instance ToSchema VideoFileId where declareNamedSchema = genericDeclareNamedSchema myOptions
instance ToSchema (Video VideoId)
instance ToSchema (Video NoId) where
  declareNamedSchema _ = do
    nameSchema <- declareSchemaRef (Proxy :: Proxy Text)
    return $ NamedSchema (Just "NewVideo") $ mempty
      & type_ .~ SwaggerObject
      & properties .~ [ ("name", nameSchema) ]
      & required .~ [ "name" ]
instance ToSchema VideoId
instance ToSchema VideoLibrary
instance ToSchema VideoLibraryId where declareNamedSchema = genericDeclareNamedSchema myOptions
instance ToSchema VideoFileStorageId where declareNamedSchema = genericDeclareNamedSchema myOptions
instance ToParamSchema VideoId
instance ToParamSchema VideoLibraryId

--newFiles = subOperations (Proxy :: Proxy ("video_libraries" :> NewFiles)) (Proxy :: Proxy VideoLibraryAPI)

responseSchema = (mempty :: Response) & description .~ "HGurra"

apiSwagger :: Swagger
apiSwagger = toSwagger (Proxy :: Proxy VideoLibraryAPI)

addResponse swagger = setResponseFor
  (subOperations (Proxy :: Proxy AllFiles) (Proxy :: Proxy VideoLibraryAPI))
  422 (return responseSchema) swagger

main :: IO ()
main = do
  BL8.writeFile "swagger.json" $ encodePretty $ (addResponse apiSwagger)
