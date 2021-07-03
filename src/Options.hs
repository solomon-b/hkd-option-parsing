{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
module Options where

import Control.Applicative (Alternative (empty))
import Control.Lens ((.~), (^.), (&), Const (..), Identity, anyOf)
import Data.Generic.HKD
import Data.Maybe (isJust, isNothing)
import Data.Monoid (Last (..))
import GHC.Generics (Generic)
import Network.URI

newtype User = User String deriving (Show, Generic)

data Options = Options
  { hostname   :: String
  , port       :: Int
  , devMode    :: Bool
  , userName   :: User
  , dbUrl      :: URI
  , certPath   :: String
  , metadataDB :: URI
} deriving (Show, Generic)

type Partial a = HKD a  Last          -- Fields may be missing.
type Bare    a = HKD a  Identity      -- All must be present.
type Labels  a = HKD a (Const String) -- Every field holds a string.

emptyOptions :: Partial Options
emptyOptions = mempty

partialOptions :: Partial Options
partialOptions = emptyOptions & field @"hostname" .~ pure "localhost"

partialOptions' :: Partial Options
partialOptions' = emptyOptions & field @"port" .~ pure 22
