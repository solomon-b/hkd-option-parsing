{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving    #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE FlexibleInstances #-}
module Options where

import Control.Applicative (Alternative (empty))
import Control.Lens ((.~), (^.), (&), Const (..), Identity, anyOf)
import Data.Generic.HKD
import Data.Maybe (isJust, isNothing)
import GHC.Generics (Generic, Generic1)
import Network.URI

newtype Last a = Last { getLast :: Maybe a }
        deriving ( Eq
                 , Ord
                 , Read
                 , Show
                 , Generic
                 , Generic1
                 , Functor
                 , Applicative
                 , Monad
                 )

instance {-# OVERLAPPING #-} Semigroup (Last (Maybe a)) where
 (<>) (Last Nothing) b = b
 (<>) a (Last Nothing) = a
 (<>) a@(Last (Just _)) (Last (Just Nothing)) = a
 (<>) (Last (Just Nothing)) b@(Last (Just _)) = b
 (<>) (Last (Just (Just _))) b@(Last (Just (Just _))) = b

instance {-# OVERLAPPABLE #-} Semigroup (Last a) where
        a <> Last Nothing = a
        _ <> b            = b

instance Monoid (Last a) where
        mempty = Last Nothing

newtype User = User String deriving (Show, Generic)

data Options = Options
  { hostname   :: String
  , port       :: Int
  , devMode    :: Bool
  , userName   :: User
  , dbUrl      :: URI
  , certPath   :: Maybe String
  , metadataDB :: URI
} deriving (Show, Generic)

type Partial a = HKD a  Last

emptyOptions :: Partial Options
emptyOptions = mempty

partialOptions :: Partial Options
partialOptions = emptyOptions & field @"hostname" .~ pure "localhost"

partialOptions' :: Partial Options
partialOptions' = emptyOptions & field @"port" .~ pure 22
