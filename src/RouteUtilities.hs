{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RouteUtilities where

import Control.Monad.Trans (liftIO)
import Control.Monad.Reader
import Control.Monad.Except
import Servant

import Configuration
import Models

newtype ApiHandler a = ApiHandler {
    runApiHandler :: ReaderT Configuration (ExceptT ServantErr IO) a
} deriving (Functor, Applicative, Monad, MonadReader Configuration, MonadError ServantErr, MonadIO)

runConfigIOAction :: ConfigIO a -> ApiHandler a
runConfigIOAction action = ask >>= liftIO . runReaderT action

convertApiHandler :: Configuration -> ApiHandler a -> Handler a
convertApiHandler config = Handler . flip runReaderT config . runApiHandler