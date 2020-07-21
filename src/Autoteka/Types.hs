{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving, DeriveAnyClass #-}

module Autoteka.Types where

import           Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import           Data.Time.LocalTime (LocalTime)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T (encodeUtf8)
import           GHC.Generics
import           Servant.API (ToHttpApiData(..))


data AccessToken = AccessToken
  { access_token :: Text
  , expires_in :: Int -- seconds
  , token_type :: Text -- enum Bearer
  }
  deriving (Show, Generic, FromJSON, ToJSON)

instance ToHttpApiData AccessToken where
  toUrlPiece _ = error "Unexpected use of AccessToken"
  toHeader x = "Bearer " <> T.encodeUtf8 (access_token x)


data PreviewRequest
  = RegNumber { regNumber :: Text }
  | VIN { vin :: Text }
  deriving (Show, Generic, ToJSON)


newtype PreviewId = PreviewId Integer
  deriving newtype (Show, FromJSON, ToJSON)

instance ToHttpApiData PreviewId where toUrlPiece = T.pack . show


data Preview = Preview
  { p_status :: Text
  , p_previewId :: PreviewId
  , p_regNumber :: Maybe Text
  , p_vin :: Maybe Text
  , p_data :: Maybe Aeson.Value
  }
  deriving (Show, Generic)

instance ToJSON Preview where toEncoding = Aeson.genericToEncoding opts
instance FromJSON Preview where parseJSON = Aeson.genericParseJSON opts

data ReportRequest
  = ReportRequest { previewId :: PreviewId }
  deriving (Show, Generic, ToJSON)


newtype ReportId = ReportId Integer
  deriving newtype (Show, FromJSON, ToJSON)

instance ToHttpApiData ReportId where toUrlPiece = T.pack . show

data Report = Report
  { r_reportId :: ReportId
  , r_status :: Text
  , r_pdfLink :: Maybe Text
  , r_data :: Maybe Aeson.Value
  }
  deriving (Show, Generic)

instance ToJSON Report where toEncoding = Aeson.genericToEncoding opts
instance FromJSON Report where parseJSON = Aeson.genericParseJSON opts


data Package = Package
  { p_createdTime :: LocalTime
  , p_expireTime :: LocalTime
  , p_reportsCnt :: Int
  , p_reportsCntRemain :: Int
  }
  deriving (Show, Generic)

instance ToJSON Package where toEncoding = Aeson.genericToEncoding opts
instance FromJSON Package where parseJSON = Aeson.genericParseJSON opts

opts :: Aeson.Options
opts = Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 2 }
