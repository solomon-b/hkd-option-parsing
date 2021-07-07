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
  options <- execParseCLI
  envVars <- execParseEnv
  print $ options <> envVars

runParser :: Parser a -> [String] -> Maybe a
runParser p txt =
  let pinfo = info (p <**> helper) mempty
  in getParseResult $ execParserPure defaultPrefs pinfo txt

catTuples :: [(a, a)] -> [a]
catTuples = foldr (\(a, b) xs -> a:b:xs) mempty

filterEnv :: [String] -> [(String, String)] -> [(String, String)]
filterEnv fields = filter (flip elem fields . fst)

execParseEnv :: IO (Partial Options)
execParseEnv = do
  let (fields, parser) = parseEnv
  env <- catTuples . fmap (first ("--" <>)) . filterEnv fields <$> getEnvironment
  pure $ fromMaybe emptyOptions $ runParser parser env

execParseCLI :: IO (Partial Options)
execParseCLI =
  let opts = info (snd parseCLI <**> helper) mempty
  in execParser opts
