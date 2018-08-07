-- set up simple users route that will return a list of users in the system
-- set up more endpoints related to user
  -- set up retrieval of records
  -- set up creating records
{-# LANGUAGE DataKinds, TypeOperators, GeneralizedNewtypeDeriving, DeriveGeneric #-}

module UsersRoute where

import Servant
import Control.Monad.Trans (liftIO, lift)
import Control.Monad.Reader
import Control.Monad.Except
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics

import Models
import DatabaseOps
import Configuration

data UserCreateResponse = MissingField | Successful | Error deriving (Eq, Show, Generic)

instance ToJSON UserCreateResponse
instance FromJSON UserCreateResponse

newtype ApiHandler a = ApiHandler {
    runApiHandler :: ReaderT Configuration (ExceptT ServantErr IO) a
} deriving (Functor, Applicative, Monad, MonadReader Configuration, MonadError ServantErr, MonadIO)

type UsersRoute = GetUsers :<|> CreateUsers

type GetUsers = "users" :> Get '[JSON] [User]
type CreateUsers = "users" :> ReqBody '[JSON] UserCreateRequest :> Post '[JSON] UserCreateResponse

getUsers :: ApiHandler [User]
getUsers = runConfigIOAction dBgetUsers

createUser :: UserCreateRequest -> ApiHandler UserCreateResponse
createUser user = do 
  runConfigIOAction $ dBcreateUser user
  return Successful

runConfigIOAction :: ConfigIO a -> ApiHandler a
runConfigIOAction action = ask >>= liftIO . runReaderT action
