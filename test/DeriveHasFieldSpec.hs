{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module DeriveHasFieldSpec where

import Data.Data (Proxy (..))
import DeriveHasField qualified
import GHC.TypeLits (Symbol)
import Import
import Test.Hspec

data SomeType = SomeType
  { someTypeSomeField :: String
  , someTypeSomeOtherField :: Int
  , someTypeSomeMaybeField :: Maybe Int
  , someTypeSomeEitherField :: Either String Int
  }

DeriveHasField.deriveHasField ''SomeType

someType :: SomeType
someType =
  SomeType
    { someTypeSomeField = "hello"
    , someTypeSomeOtherField = 0
    , someTypeSomeMaybeField = Just 0
    , someTypeSomeEitherField = Right 0
    }

data SomeTypePrefix = SomeTypePrefix
  { someTypePrefixSomeField :: String
  , someTypePrefixSomeOtherField :: Int
  , someTypePrefixSomeMaybeField :: Maybe Int
  , someTypePrefixSomeEitherField :: Either String Int
  }

DeriveHasField.deriveHasFieldWith (dropPrefix "someTypePrefix") ''SomeTypePrefix

someTypePrefix :: SomeTypePrefix
someTypePrefix =
  SomeTypePrefix
    { someTypePrefixSomeField = "hello"
    , someTypePrefixSomeOtherField = 0
    , someTypePrefixSomeMaybeField = Just 0
    , someTypePrefixSomeEitherField = Right 0
    }

data OtherType a b = OtherType
  { otherTypeField :: a
  , otherTypeMaybeField :: Maybe a
  , otherTypeEitherField :: Either a b
  }

DeriveHasField.deriveHasField ''OtherType

otherType :: OtherType Int String
otherType =
  OtherType
    { otherTypeField = 0
    , otherTypeMaybeField = Just 0
    , otherTypeEitherField = Right "hello"
    }

data OtherTypePrefix a b = OtherTypePrefix
  { otherTypePrefixField :: Maybe a
  , otherTypePrefixOtherField :: Either a b
  }

DeriveHasField.deriveHasFieldWith (dropPrefix "otherTypePrefix") ''OtherTypePrefix

otherTypePrefix :: OtherTypePrefix Int String
otherTypePrefix =
  OtherTypePrefix
    { otherTypePrefixField = Just 0
    , otherTypePrefixOtherField = Right "hello"
    }

data KindedType (kind :: * -> *) (sym :: Symbol) = KindedType
  { kindedTypeWithKind :: kind ()
  , kindedTypeWithSymbol :: Proxy sym
  }

DeriveHasField.deriveHasField ''KindedType

kindedType :: KindedType Maybe "hello"
kindedType =
  KindedType
    { kindedTypeWithKind = Just ()
    , kindedTypeWithSymbol = Proxy @"hello"
    }

data KindedTypePrefix (kind :: * -> *) (sym :: Symbol) = KindedTypePrefix
  { kindedTypePrefixWithKind :: kind ()
  , kindedTypePrefixWithSymbol :: Proxy sym
  }

DeriveHasField.deriveHasFieldWith (dropPrefix "kindedTypePrefix") ''KindedTypePrefix

kindedTypePrefix :: KindedTypePrefix Maybe "hello"
kindedTypePrefix =
  KindedTypePrefix
    { kindedTypePrefixWithKind = Just ()
    , kindedTypePrefixWithSymbol = Proxy @"hello"
    }

data ExampleWithPrefix = ExampleWithPrefix
  { ewpHello :: String
  , ewpWorld :: String
  }

DeriveHasField.deriveHasFieldWithPrefix "ewp" ''ExampleWithPrefix

someExampleWithPrefix :: ExampleWithPrefix
someExampleWithPrefix =
  ExampleWithPrefix
    { ewpHello = "hello"
    , ewpWorld = "world"
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
      otherType.field `shouldBe` 0
      otherType.maybeField `shouldBe` Just 0
      otherType.eitherField `shouldBe` Right "hello"
    it "compiles and gets the right field" $ do
      kindedType.withKind `shouldBe` Just ()
      kindedType.withSymbol `shouldBe` Proxy @"hello"

  describe "deriveHasFieldWith" $ do
    it "compiles and gets the right field" $ do
      someTypePrefix.someField `shouldBe` "hello"
      someTypePrefix.someOtherField `shouldBe` 0
      someTypePrefix.someMaybeField `shouldBe` Just 0
      someTypePrefix.someEitherField `shouldBe` Right 0
    it "compiles and gets the right field" $ do
      otherTypePrefix.field `shouldBe` Just 0
      otherTypePrefix.otherField `shouldBe` Right "hello"
    it "compiles and gets the right field" $ do
      kindedTypePrefix.withKind `shouldBe` Just ()
      kindedTypePrefix.withSymbol `shouldBe` Proxy @"hello"

  describe "deriveHasFieldWithPrefix" $ do
    it "compiles and gets the right field" $ do
      someExampleWithPrefix.hello `shouldBe` "hello"
      someExampleWithPrefix.world `shouldBe` "world"
