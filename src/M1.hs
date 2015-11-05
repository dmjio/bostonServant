{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module M1 where

import Data.Aeson
import Data.Monoid
import Control.Lens
import Data.ByteString.Lazy (ByteString)
import Servant
import Servant.Docs
import Servant.Client 
import Servant.JQuery
import Servant.Mock
import Servant.JuicyPixels
import Servant.Swagger hiding (Http)
import qualified Servant.Swagger as Swagger

import Codec.Picture
import Control.Monad.Trans.Either
import Control.Concurrent
import Control.Concurrent.STM.TVar
import Network.Wai
import Network.Wai.Handler.Warp
import Lackey

type GetAnInt = "anint" :> Get '[JSON] Int
type PostAnInt = "anint" :> ReqBody '[JSON] Int :> Post '[JSON] ()

type TheAPI = GetAnInt :<|> PostAnInt

instance ToSample Int Int where
    toSample _ = Just 1797

instance ToSample () () where
    toSample _ = Just ()

api :: Proxy TheAPI
api = Proxy

baseUrl :: BaseUrl
baseUrl = BaseUrl Http "localhost" 8001

getAnInt :: EitherT ServantError IO Int
postAnInt :: Int -> EitherT ServantError IO ()
getAnInt :<|> postAnInt  = client api baseUrl

mockServer :: IO ()
mockServer = run 8001 $ serve api $ mock api

apiDocs :: String
apiDocs = markdown $ docs api

jqueryClient :: String
jqueryClient = jsForAPI api

rubyClient :: String
rubyClient = rubyForAPI api

swaggerJSON :: ByteString
swaggerJSON = encode $ swagger api routeInfo (BasePath "/") info [Swagger.Http] Nothing []
  where
    info = mempty & swaggerInfoTitle .~ APITitle "Sample API"
                  & swaggerVersion .~ APIVersion "1.0"
                  & swaggerAPIDescription .~ APIDescription "Sample generated Swagger API"
                  & license ?~ APILicense "MIT" (Just "https://opensource.org/licenses/MIT")
                  & contact ?~ Contact (ContactName "Julian")
                                       (ContactURL "https://haskell-servant.github.io/")
                                       (ContactEmail "julian@gmail.com")

routeInfo = getAnIntInfo <> postAnIntInfo

intTags = [ Tag (TagName "int") (TagDescription "API for managing integers") ]

instance ToSwaggerModel Int where
    toSwagModel Proxy =
      emptyModel
        & swagModelName .~ ModelName "Int"
        & swagProperties .~ [ ("int", IntegerSwag) ]
        & swagDescription ?~ Description "Int Model"
        & swagModelExample ?~ Number 1
        & swagModelRequired .~ ["int"] 

getAnIntInfo :: SwaggerRouteInfo TheAPI
getAnIntInfo = swaggerPathInfo (Proxy :: Proxy GetAnInt) (Proxy :: Proxy TheAPI) $
  emptyRouteDescription
      & swagRouteTags .~ intTags
      & swagRouteSummary .~ PathSummary "Retrieve an Int"
      & swagRouteOperationId ?~ OperationId "getAnInt"
      & swagRouteDescription .~ PathDescription "Gets an Int"

postAnIntInfo :: SwaggerRouteInfo TheAPI
postAnIntInfo = swaggerPathInfo (Proxy :: Proxy PostAnInt) (Proxy :: Proxy TheAPI) $
  emptyRouteDescription
      & swagRouteTags .~ intTags
      & swagRouteSummary .~ PathSummary "Adds an Int "
      & swagRouteOperationId ?~ OperationId "addAnInt"
      & swagRouteDescription .~ PathDescription "POSTs an Int"


