{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Parsers where

import Options.Applicative
import Control.Applicative (Alternative (empty))
import Control.Lens ((.~), (^.), (&), Const (..), Identity, anyOf)
import Data.Generic.HKD
import Data.Maybe (isJust, isNothing)
import Data.Monoid (Ap(..))
import GHC.Generics (Generic)
import Network.URI

import Options

parseHostname :: Parser (Partial Options)
parseHostname =
  let setting = strOption $ long "hostname" <> metavar "HOST" <> help "server hostname"
  in fmap (\host -> emptyOptions & field @"hostname" .~ pure host) setting

parsePort :: Parser (Partial Options)
parsePort =
  let settings = option auto $ long "port" <> value (pure 80) <> metavar "PORT" <> help "server port"
  in fmap (\i -> emptyOptions & field @"port" .~ i) settings

parseDevmode :: Parser (Partial Options)
parseDevmode =
  let settings = switch $ long "devmode" <> help "enable developer mode"
  in fmap (\i -> emptyOptions & field @"devMode" .~ pure i) settings

parseUser :: Parser (Partial Options)
parseUser =
  let settings = fmap User <$> optional (strOption (long "username" <> metavar "USER" <> help "username"))
  in fmap (maybe emptyOptions (\u -> emptyOptions & field @"userName" .~ pure u)) settings

parseDBUri :: Parser (Partial Options)
parseDBUri =
  let settings = optional (strOption (long "db_uri" <> metavar "URI" <> help "db url"))
  in fmap (maybe emptyOptions (\str -> emptyOptions & field @"dbUrl" .~ Last (parseURI str))) settings

parseCertPath :: Parser (Partial Options)
parseCertPath =
  let settings = optional (strOption (long "cert_path" <> metavar "PATH" <> help "ssl cert path"))
  in fmap (\mstr -> emptyOptions & field @"certPath" .~ pure mstr) settings

parseMetadataUrl :: Parser (Partial Options)
parseMetadataUrl =
  let settings = optional (strOption (long "metadata_url" <> metavar "URI" <> help "db url"))
  in fmap (maybe emptyOptions (\str -> emptyOptions & field @"metadataDB" .~ Last (parseURI str))) settings


parseCLI :: Parser (Partial Options)
parseCLI = getAp $ foldMap Ap
  [ parseHostname
  , parsePort
  , parseDevmode
  , parseUser
  , parseDBUri
  , parseCertPath
  ]

parseEnv :: Parser (Partial Options)
parseEnv = getAp $ foldMap Ap [parseDBUri, parseMetadataUrl, parseCertPath, empty]
