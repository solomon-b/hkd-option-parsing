{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module CliParser where

import Options.Applicative
import Control.Applicative (Alternative (empty))
import Control.Lens ((.~), (^.), (&), Const (..), Identity, anyOf)
import Data.Generic.HKD
import Data.Maybe (isJust, isNothing)
import Data.Monoid (Last(..), Ap(..))
import GHC.Generics (Generic)
import Network.URI

import Options

parseHostname :: Parser (Partial Options)
parseHostname =
  let p = strOption $ long "hostname" <> metavar "HOST" <> help "server hostname"
  in fmap (\host -> emptyOptions & field @"hostname" .~ pure host) p

parsePort :: Parser (Partial Options)
parsePort =
  let p = option auto $ long "port" <> value (pure 80) <> metavar "PORT" <> help "server port"
  in fmap (\i -> emptyOptions & field @"port" .~ i) p

parseDevmode :: Parser (Partial Options)
parseDevmode =
  let p = switch $ long "devmode" <> help "enable developer mode"
  in fmap (\i -> emptyOptions & field @"devMode" .~ pure i) p

parseUser :: Parser (Partial Options)
parseUser =
  let p = fmap User <$> optional (strOption (long "username" <> metavar "USER" <> help "username"))
  in fmap (maybe emptyOptions (\u -> emptyOptions & field @"userName" .~ pure u)) p

parseDBUri :: Parser (Partial Options)
parseDBUri =
  let p = optional (strOption (long "db uri" <> metavar "URI" <> help "db url"))
  in fmap (maybe emptyOptions (\str -> emptyOptions & field @"dbUrl" .~ Last (parseURI str))) p

parseCLI :: Parser (Partial Options)
parseCLI = getAp $ foldMap Ap
  [ parseHostname
  , parsePort
  , parseDevmode
  , parseUser
  , parseDBUri
  ]
