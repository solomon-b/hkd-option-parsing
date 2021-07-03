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

-- This solution is so horrible.  When using the `Partial` alias from
-- Higgledy, we wrap our data in `Last`. This represents data capture
-- that can fail.
--
-- In this demo we use `Partial Options` to construct incomplete
-- `Options` for each data source and then `<>` them together to
-- produce the complete `Option` value.
--
-- We then use `construct` to convert from `Partial Options` to `Maybe
-- Options`. This is really nice because it gets rid of all the
-- unnecessary Maybes.

-- However, this causes problems if your Options type contains a field
-- that is truly optional (Maybe MyField) AND it can be supplied by
-- multiple data sources. In this scenario the optional `MyField`
-- looks like this while in `Partial`:
--
-- Last (Maybe MyField)
--
-- So if we are merging two of these values together and the second
-- one is `Last (Just Nothing)` then `<>` will choose that value over
-- a `Last (Just (Just x))`:
--
-- > Last (Just $ Just True) <> Last (Just Nothing)
-- Last {getLast = Just Nothing}
--
-- The behavior we want is this:
-- > Last (Just $ Just True) <> Last (Just Nothing)
-- Last {getLast = Just (Just True)}
--
-- This Semigroup instance accomplishes that but looks like a potential
-- major footgun.
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

type Partial a = HKD a Last

emptyOptions :: Partial Options
emptyOptions = mempty

partialOptions :: Partial Options
partialOptions = emptyOptions & field @"hostname" .~ pure "localhost"

partialOptions' :: Partial Options
partialOptions' = emptyOptions & field @"port" .~ pure 22
