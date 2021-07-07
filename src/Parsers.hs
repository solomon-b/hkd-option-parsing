{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Parsers where

import Options.Applicative
import Control.Applicative (Alternative (empty))
import Control.Lens ((.~), (^.), (&), Const (..), Identity, anyOf)
import Data.Generic.HKD
import Data.Maybe (isJust, isNothing)
import Data.Monoid (Ap(..), Last(..))
import GHC.Generics (Generic)
import Network.URI

import Options

type OptionParser = ([String], Parser (Partial Options))

mergeParsers :: [OptionParser] -> OptionParser
mergeParsers parsers =
  let parser = getAp $ foldMap (Ap . snd) parsers
      fields = foldMap fst parsers
  in (fields, parser)

parseHostname :: OptionParser
parseHostname =
  let setting = strOption $ long "hostname" <> metavar "HOST" <> help "server hostname"
      parser = fmap (\host -> emptyOptions & field @"hostname" .~ pure host) setting
  in (pure "hostname", parser)

parsePort :: OptionParser
parsePort =
  let settings = option auto $ long "port" <> value 80 <> metavar "PORT" <> help "server port"
      parser = fmap (\i -> emptyOptions & field @"port" .~ pure i) settings
  in (pure "port", parser)

parseDevmode :: OptionParser
parseDevmode =
  let settings = switch $ long "devmode" <> help "enable developer mode"
      parser = fmap (\i -> emptyOptions & field @"devMode" .~ pure i) settings
  in (pure "devmode", parser)

parseUser :: OptionParser
parseUser =
  let settings = fmap User <$> optional (strOption (long "username" <> metavar "USER" <> help "username"))
      parser = fmap (maybe emptyOptions (\u -> emptyOptions & field @"userName" .~ pure u)) settings
  in (pure "username", parser)

parseDBUri :: OptionParser
parseDBUri =
  let settings = optional (strOption (long "db_uri" <> metavar "URI" <> help "db url"))
      parser = fmap (maybe emptyOptions (\str -> emptyOptions & field @"dbUrl" .~ Last (parseURI str))) settings
  in (pure "db_uri", parser)

parseCertPath :: OptionParser
parseCertPath =
  let settings = optional (strOption (long "cert_path" <> metavar "PATH" <> help "ssl cert path"))
      parser = fmap (\mstr -> emptyOptions & field @"certPath" .~ pure mstr) settings
  in (pure "cert_path", parser)

parseMetadataUrl :: OptionParser
parseMetadataUrl =
  let settings = optional (strOption (long "metadata_url" <> metavar "URI" <> help "db url"))
      parser = fmap (maybe emptyOptions (\str -> emptyOptions & field @"metadataDB" .~ Last (parseURI str))) settings
  in (pure "metadata_url", parser)

parseCLI :: OptionParser
parseCLI =
  let parsers = [ parseHostname
        , parsePort
        , parseDevmode
        , parseUser
        , parseDBUri
        , parseCertPath
        ]
  in mergeParsers parsers

parseEnv :: OptionParser
parseEnv =
  let parsers = [parseDBUri, parseMetadataUrl, parseCertPath]
  in mergeParsers parsers
