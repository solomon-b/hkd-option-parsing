{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving    #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
module Options where

import Data.Functor.Product
import Data.Coerce
import Data.Functor.Compose
import Control.Applicative (Alternative (empty))
import Control.Lens ((.~), (^.), (&), Const (..), Identity, anyOf)
import Data.Generic.HKD
import Data.Maybe (isJust, isNothing)
import GHC.Generics (Generic, Generic1)
import Network.URI
import Options.Applicative (Parser)

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

instance MaybeMonoid a => Semigroup (Last a) where
  (<>) = coerce (maybeMappend @a)

instance forall a. MaybeMonoid a => Monoid (Last a) where
  mempty = coerce (maybeMempty @a)
  mappend = coerce (maybeMappend @a)

class MaybeMonoid a where
  maybeMempty :: Maybe a
  default maybeMempty :: Maybe a
  maybeMempty = Nothing

  maybeMappend :: Maybe a -> Maybe a -> Maybe a
  default maybeMappend :: Maybe a -> Maybe a -> Maybe a
  maybeMappend Nothing Nothing = Nothing
  maybeMappend Nothing (Just a) = Just a
  maybeMappend (Just a) Nothing = Just a
  maybeMappend (Just a) (Just b) = Just b

instance MaybeMonoid (Maybe a) where
  -- NOTE:
  maybeMempty = Just Nothing

  maybeMappend Nothing b = b
  maybeMappend a Nothing = a
  maybeMappend a@((Just _)) ((Just Nothing)) = a
  maybeMappend (Just Nothing) b@(Just _) = b
  maybeMappend (Just (Just _)) b@(Just (Just _)) = b

instance MaybeMonoid String
instance MaybeMonoid Int
instance MaybeMonoid Bool
instance MaybeMonoid User
instance MaybeMonoid URI

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

--class Applicative f => Parser f where
--  parseOne :: Options -> f String
--
--instance (Parser f, Parser g) => Parser (Product f g) where
--  parseOne = undefined
