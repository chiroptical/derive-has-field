{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module DeriveHasFieldSpec where

import Data.Data (Proxy (..))
import DeriveHasField
import GHC.TypeLits (Symbol)
import Import
import Test.Hspec

data SomeType = SomeType
  { someTypeSomeField :: String
  , someTypeSomeOtherField :: Int
  , someTypeSomeMaybeField :: Maybe Int
  , someTypeSomeEitherField :: Either String Int
  }

deriveHasFieldWith (dropPrefix "someType") ''SomeType

someType :: SomeType
someType =
  SomeType
    { someTypeSomeField = "hello"
    , someTypeSomeOtherField = 0
    , someTypeSomeMaybeField = Just 0
    , someTypeSomeEitherField = Right 0
    }

data OtherType a b = OtherType
  { otherTypeField :: Maybe a
  , otherTypeOtherField :: Either a b
  }

deriveHasFieldWith (dropPrefix "otherType") ''OtherType

otherType :: OtherType Int String
otherType =
  OtherType
    { otherTypeField = Just 0
    , otherTypeOtherField = Right "hello"
    }

data KindedType (kind :: * -> *) (sym :: Symbol) = KindedType
  { kindedTypeWithKind :: kind ()
  , kindedTypeWithSymbol :: Proxy sym
  }

deriveHasFieldWith (dropPrefix "kindedType") ''KindedType

kindedType :: KindedType Maybe "hello"
kindedType =
  KindedType
    { kindedTypeWithKind = Just ()
    , kindedTypeWithSymbol = Proxy @"hello"
    }

spec :: Spec
spec = do
  describe "deriveHasField" $ do
    it "compiles and gets the right field" $ do
      someType.someField `shouldBe` "hello"
      someType.someOtherField `shouldBe` 0
      someType.someMaybeField `shouldBe` Just 0
      someType.someEitherField `shouldBe` Right 0
    it "compiles and gets the right field" $ do
      otherType.field `shouldBe` Just 0
      otherType.otherField `shouldBe` Right "hello"
    it "compiles and gets the right field" $ do
      kindedType.withKind `shouldBe` Just ()
      kindedType.withSymbol `shouldBe` Proxy @"hello"
