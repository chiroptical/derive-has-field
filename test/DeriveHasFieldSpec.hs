{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module DeriveHasFieldSpec where

import DeriveHasField
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

data OtherType a = OtherType
  { otherTypeField :: Maybe a
  , otherTypeOtherField :: Maybe a
  }

deriveHasFieldWith (dropPrefix "otherType") ''OtherType

otherType :: OtherType Int
otherType =
  OtherType
    { otherTypeField = Just 0
    , otherTypeOtherField = Nothing
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
      otherType.otherField `shouldBe` Nothing
