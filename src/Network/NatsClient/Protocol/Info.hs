{-# LANGUAGE DeriveGeneric #-}

module Network.NatsClient.Protocol.Info where

import GHC.Generics
import Data.Aeson

data ServerInfo = ServerInfo
  { server_id :: String,
    server_name :: Maybe String,
    version :: String,
    go :: String,
    host :: String,
    port :: Int,
    headers :: Bool,
    max_payload :: Int,
    proto :: Int,
    client_id :: Maybe Int,
    auth_required :: Maybe Bool,
    tls_required :: Maybe Bool,
    tls_verify :: Maybe Bool,
    tls_available :: Maybe Bool,
    connect_urls :: Maybe [String],
    ldm :: Maybe Bool,
    git_commit :: Maybe String,
    jetstream :: Maybe Bool,
    ip :: Maybe String,
    client_ip :: Maybe String,
    nonce :: Maybe String,
    domain :: Maybe String
  }
  deriving (Eq, Show, Generic)

instance FromJSON ServerInfo
instance ToJSON ServerInfo
