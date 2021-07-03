module Main where

import Options.Applicative
import Control.Applicative (Alternative (empty))
import Control.Lens ((.~), (^.), (&), Const (..), Identity, anyOf)
import Data.Generic.HKD
import Data.Maybe (isJust, isNothing)
import Data.Monoid (Last (..))

import CliParser
import EnvParser

main :: IO ()
main = do
  let opts = info (parseCLI <**> helper) mempty
  options <- execParser opts
  envVars <- parseEnv
  print $ options <> envVars
