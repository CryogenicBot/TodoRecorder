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

import UsersRoute
import Configuration

type API = UsersApi

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

combinedServers = usersServer

convertApiHandler :: Configuration -> ApiHandler a -> Handler a
convertApiHandler config = Handler . flip runReaderT config . runApiHandler
