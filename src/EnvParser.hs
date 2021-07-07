{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module EnvParser where

import Options.Applicative
import Control.Applicative (Alternative (empty))
import Control.Lens
import Control.Monad
import Data.Generic.HKD
import Data.Maybe (isJust, isNothing)
import Data.Monoid (Ap(..))
import GHC.Generics (Generic)

import Data.Foldable
import Network.URI
import System.Environment (lookupEnv, setEnv)
import Text.Read (readMaybe)

import Options

