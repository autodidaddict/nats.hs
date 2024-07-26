{-# LANGUAGE DeriveGeneric #-}

module Protocol.Connection (ConnectSettings (..)) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics


data ConnectSettings = ConnectSettings
  { verbose :: Bool,
    pedantic :: Bool,
    tls_required :: Bool,
    auth_token :: Maybe Text,
    user :: Maybe Text,
    pass :: Maybe Text,
    name :: Maybe Text,
    lang :: Text,
    version :: Text,
    protocol :: Maybe Int,
    echo :: Maybe Bool,
    sig :: Maybe Text,
    jwt :: Maybe Text,
    no_responders :: Maybe Bool,
    headers :: Maybe Bool,
    nkey :: Maybe String
  }
  deriving (Eq, Show, Generic)

instance FromJSON ConnectSettings

instance ToJSON ConnectSettings
