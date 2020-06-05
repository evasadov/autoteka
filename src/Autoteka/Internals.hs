{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Autoteka.Internals
  ( Result
  , resultValue
  )
  where

import qualified Data.Aeson as Aeson
import           Data.Aeson (FromJSON, (.:))
import qualified Data.Text as T (pack)
import           Data.Proxy (Proxy(..))
import           GHC.TypeLits (Symbol, KnownSymbol, symbolVal)


newtype Result (fld :: [Symbol]) typ = Result typ

resultValue :: Result f t -> t
resultValue (Result t) = t


instance (KnownSymbol fld, FromJSON typ) => FromJSON (Result '[fld] typ) where
  parseJSON (Aeson.Object obj)
    = (obj .: "result") >>= (.: fld) >>= pure . Result
    where
      fld = T.pack $ symbolVal (Proxy :: Proxy fld)
  parseJSON _ = fail "Object expected"


instance
  (KnownSymbol f1, KnownSymbol f2, FromJSON typ)
   => FromJSON (Result '[f1, f2] typ)
  where
    parseJSON (Aeson.Object obj)
      = (obj .: "result")
      >>= (.: f1)
      >>= (.: f2)
      >>= pure . Result
      where
        f1 = T.pack $ symbolVal (Proxy :: Proxy f1)
        f2 = T.pack $ symbolVal (Proxy :: Proxy f2)
    parseJSON _ = fail "Object expected"
