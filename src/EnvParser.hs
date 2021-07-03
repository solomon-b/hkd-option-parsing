{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module EnvParser where

import Options.Applicative
import Control.Applicative (Alternative (empty))
import Control.Lens
import Data.Generic.HKD
import Data.Maybe (isJust, isNothing)
import Data.Monoid (Ap(..))
import GHC.Generics (Generic)

import Data.Foldable
import Network.URI
import System.Environment (lookupEnv, setEnv)
import Text.Read (readMaybe)

import Options

-- TODO: Modify this to use optparse Parsers
parseEnvVar :: (String -> Partial Options) -> String -> IO (Partial Options)
parseEnvVar parser envKey =
  lookupEnv envKey >>= pure . \case
    Just x -> parser x
    Nothing -> emptyOptions

parseOptEnvVar :: (Maybe String -> Partial Options) -> String -> IO (Partial Options)
parseOptEnvVar parser envKey =
  lookupEnv envKey >>= pure . parser

parseEnv :: IO (Partial Options)
parseEnv = do
  dbUrl      <- "db_url" & parseEnvVar (\str -> emptyOptions & field @"dbUrl" .~ Last (parseURI str))
  metadataDb <- "metadata_url" & parseEnvVar (\str -> emptyOptions & field @"metadataDB" .~ Last (parseURI str))
  certPath   <- "cert_path" & parseOptEnvVar (\str -> emptyOptions & field @"certPath"  .~ pure str)
  pure $ fold [dbUrl, metadataDb, certPath]
