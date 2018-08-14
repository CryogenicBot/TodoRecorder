{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Api
    ( startApp
    , app
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Control.Monad.Except
import Control.Monad.Reader
import Servant

import RouteUtilities
import Configuration
import UsersRoute
import RecordsRoute

type API = UsersApi :<|> RecordsApi

startApp :: ConfigIO ()
startApp = do 
  config <- ask
  liftIO $ run 1234 $ app config

app :: Configuration -> Application
app = serve api . server

api :: Proxy API
api = Proxy

server :: Configuration -> Server API
server config = hoistServer api (convertApiHandler config) $ combinedServers

combinedServers :: ServerT API ApiHandler
combinedServers = usersServer :<|> recordsServer