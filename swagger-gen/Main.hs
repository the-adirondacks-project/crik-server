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

apiSwagger :: Swagger
apiSwagger = toSwagger (Proxy :: Proxy API)

addResponse statusCode statusDescription subAPI fullAPI swagger =
  setResponseFor (subOperations subAPI fullAPI) statusCode (return responseSchema) swagger
  where responseSchema = (mempty :: Response) & description .~ statusDescription

main :: IO ()
main = do
  let swagger = addResponse 422 "foo" (Proxy :: Proxy AllFiles) (Proxy :: Proxy API) apiSwagger
  BL8.writeFile "swagger.json" $ encodePretty swagger
