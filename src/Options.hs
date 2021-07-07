{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving    #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Options where

import Barbies
import Barbies.Constraints
import Data.Functor.Identity
import Data.Functor.Product

import Data.Functor.Compose
import Control.Applicative (Alternative (empty))
import Control.Lens ((.~), (^.), (&), Const (..), Identity, anyOf)
import Data.Generic.HKD
import Data.Maybe (isJust, isNothing)
import Data.Monoid (Last(..))
import GHC.Generics (Generic, Generic1)
import Network.URI

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

instance Monoid (Either String a) where
  mempty = Left mempty

emptyOptions :: Partial Options
emptyOptions = mempty

data OptionsF f = OptionsF
  { hostnameF   :: f String
  , portF       :: f Int
  , devModeF    :: f Bool
  , userNameF   :: f User
  , dbUrlF      :: f URI
  , certPathF   :: Maybe (f String)
  , metadataDBF :: f URI
} deriving (Generic, FunctorB, TraversableB, ApplicativeB)

deriving instance AllBF Show f OptionsF => Show (OptionsF f)
deriving instance AllBF Eq   f OptionsF => Eq   (OptionsF f)

--type AllBF c f OptionsF = (c String, c Int , c Bool, c User, c URI, c (f String), c URI)

instance ConstraintsB OptionsF where
  type AllB c OptionsF = (c String, c Int , c Bool, c User, c URI, c (Maybe String), c URI)

  baddDicts (OptionsF a b c d e f g) =
    OptionsF (Pair Dict a) (Pair Dict b) (Pair Dict c) (Pair Dict d) (Pair Dict e) (Pair Dict f) (Pair Dict g)


instance AllBF Semigroup f OptionsF => Semigroup (OptionsF f) where
  (<>) (OptionsF a b c d e f g) (OptionsF a' b' c' d' e' f' g') =
    OptionsF (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g')

instance AllBF Monoid f OptionsF => Monoid (OptionsF f) where
  mempty = OptionsF mempty mempty mempty mempty mempty mempty mempty

type PartialOptions = OptionsF Last


--emptyOptionsF :: PartialOptions
--emptyOptionsF = mempty
