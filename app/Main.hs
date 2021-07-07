{-# LANGUAGE LambdaCase #-}
module Main where

import Options.Applicative
import Control.Applicative (Alternative (empty))
import Control.Lens ((.~), (^.), (&), Const (..), Identity, anyOf)
import Data.Bifunctor (first)
import Data.Generic.HKD
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Monoid (Ap(..))
import System.Environment (getEnvironment, lookupEnv)

import Options
import Parsers

main :: IO ()
main = do
  let opts = info (parseCLI <**> helper) mempty
  options <- execParser opts
  envVars <- parseEnvVars
  print envVars
  --print $ options <> envVars

execEnvParser :: IO (Partial Options)
execEnvParser = do
  dbUrl      <- "db_url" & parseEnvVar parseDBUri
  metadataDb <- "metadata_url" & parseEnvVar parseMetadataUrl
  certPath   <- "cert_path" & parseEnvVar parseCertPath
  pure $ fromMaybe mempty $ getAp $ foldMap Ap [dbUrl, metadataDb, certPath]

runParser :: Parser a -> [String] -> Maybe a
runParser p txt =
  let pinfo = info (p <**> helper) mempty
  in getParseResult $ execParserPure defaultPrefs pinfo txt

catTuples :: [(a, a)] -> [a]
catTuples = foldr (\(a, b) xs -> a:b:xs) mempty

parseEnvVars :: IO (Maybe (Partial Options))
parseEnvVars = do
  env <- catTuples . fmap (first ("--" <>)) <$> getEnvironment
  pure $ runParser parseEnv env
  --lookupEnv envKey >>= \case
  --  Just x -> pure $ runParser parseEnv ["--" <> envKey, x, "--cert_path", "/home/certs"]
  --  Nothing -> pure $ Just emptyOptions
