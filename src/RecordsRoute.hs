{-# LANGUAGE DataKinds, TypeOperators, DeriveGeneric, OverloadedStrings #-}

module RecordsRoute(
    RecordsApi
  , recordsServer
) where

import Servant
import Data.Text (Text(..), pack, unpack)
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics

import RouteUtilities
import Models
import DatabaseOps

type CreateRecord = "records" :> ReqBody '[JSON] RecordCreateRequest :> Post '[JSON] RecordCreateResponse

type RecordsApi = CreateRecord

recordsServer :: ServerT RecordsApi ApiHandler
recordsServer = createRecord

data RecordCreateResponse = MissingField | Successful | InvalidRequest deriving (Eq, Show, Generic)
instance ToJSON RecordCreateResponse
instance FromJSON RecordCreateResponse

createRecord :: RecordCreateRequest -> ApiHandler RecordCreateResponse
createRecord req = do
  missingFields <- return $ checkMissingFields req
  if missingFields then do
    return MissingField
  else do
    runConfigIOAction . dBcreateRecord $ req
    return Successful

checkMissingFields :: RecordCreateRequest -> Bool
checkMissingFields (RecordCreateRequest _ "" _) = True
checkMissingFields (RecordCreateRequest _ _ "") = True
checkMissingFields _ = False