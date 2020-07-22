module Autoteka
  ( Autoteka.mkClientEnv
  , getToken
  , AccessToken
  , PreviewRequest(..)
  , createPreview
  , PreviewId(..)
  , getPreview
  , Preview(..)
  , createReport
  , getReport
  , Report(..)
  , getActivePackage
  , Package(..)
  )
  where

import           Data.Text (Text)
import           Data.Proxy (Proxy(..))
import           Network.HTTP.Client (Manager)
import           Servant.API
import           Servant.Client as Servant

import Autoteka.Types
import Autoteka.Internals (Result, resultValue)


mkClientEnv :: Manager -> ClientEnv
mkClientEnv mgr = Servant.mkClientEnv mgr
  (BaseUrl Https "api.avito.ru" 443 "")


type AutotekaAPI
  = "token"
    :> QueryParam' '[Required] "grant_type" Text
    :> QueryParam' '[Required] "client_id" Text
    :> QueryParam' '[Required] "client_secret" Text
    :> Get '[JSON] AccessToken
  :<|> "autoteka" :> "v1" :> "request-preview-by-regnumber"
    :> Header' '[Required] "Authorization" AccessToken
    :> ReqBody '[JSON] PreviewRequest
    :> Post '[JSON] (Result '["preview", "previewId"] PreviewId)
  :<|> "autoteka" :> "v1" :> "previews"
    :> Header' '[Required] "Authorization" AccessToken
    :> Capture "previewId" PreviewId
    :> Get '[JSON] (Result '["preview"] Preview)
  :<|> "autoteka" :> "v1" :> "reports"
    :> Header' '[Required] "Authorization" AccessToken
    :> ReqBody '[JSON] ReportRequest
    :> Post '[JSON] (Result '["report"] Report)
  :<|> "autoteka" :> "v1" :> "reports"
    :> Header' '[Required] "Authorization" AccessToken
    :> Capture "reportId" ReportId
    :> Get '[JSON] (Result '["report"] Report)
  :<|> "autoteka" :> "v1" :> "packages" :> "active_package"
    :> Header' '[Required] "Authorization" AccessToken
    :> Get '[JSON] (Result '["package"] Package)



-- FIXME: check token_type? in getToken
-- result.preview.status = processing | notFound | success
-- FIXME: error handling



getToken :: Text -> Text -> ClientM AccessToken
createPreview :: AccessToken -> PreviewRequest -> ClientM PreviewId
getPreview :: AccessToken -> PreviewId -> ClientM Preview
createReport :: AccessToken -> PreviewId -> ClientM Report
getReport :: AccessToken -> ReportId -> ClientM Report
getActivePackage :: AccessToken -> ClientM Package


( getToken
  , createPreview, getPreview
  , createReport, getReport
  , getActivePackage
  )
  = ( getToken' "client_credentials"
    , \tok req -> resultValue <$> createPreview' tok req
    , \tok pid -> resultValue <$> getPreview' tok pid
    , \tok pid -> resultValue <$> createReport' tok (ReportRequest pid)
    , \tok rid -> resultValue <$> getReport' tok rid
    , \tok     -> resultValue <$> getActivePackage' tok
    )
  where
    getToken'
      :<|> createPreview' :<|> getPreview'
      :<|> createReport' :<|> getReport'
      :<|> getActivePackage'
        = client (Proxy :: Proxy AutotekaAPI)
