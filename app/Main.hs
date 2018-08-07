{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (eitherDecodeStrict')
import qualified Data.ByteString as ByteString
import Control.Monad.Reader

import Api
import Configuration
import DatabaseOps

main :: IO ()
main = do
  configFile <- getConfigurationFromFile
  pool <- initConnectionPool configFile
  let fullConfig = Configuration configFile pool
  flip runReaderT fullConfig $ do
    startApp

getConfigurationFromFile :: IO ConfigurationFile
getConfigurationFromFile = do
  eitherConfig <- ByteString.readFile "config.json" >>= return . eitherDecodeStrict'
  case eitherConfig of
    Right config -> return config
    Left errorMessage -> error errorMessage