{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module DeriveHasFieldCompilerErrorsSpec (spec) where

import DeriveHasField qualified
import Import.Types
import Language.Haskell.TH (Dec)
import Language.Haskell.TH.TestUtils
import Test.Hspec

spec :: Spec
spec = do
  describe "deriveHasFieldWithPrefix" $ do
    it "must modify fields when a prefix is requested" $ do
      result :: Either String [Dec] <-
        $( do
            let state =
                  QState
                    { mode = MockQ
                    , knownNames = [("F", ''F)]
                    , reifyInfo = $(loadNames [''F])
                    }
            let result =
                  tryTestQ
                    state
                    (DeriveHasField.deriveHasFieldWithPrefix "world" ''F)
            [|pure result|]
         )
      result
        `shouldBe` Left "deriveHasField: the given prefix `world` doesn't match the data constructor names"

  describe "deriveHasField" $ do
    it "must be a record with fields" $ do
      result :: Either String [Dec] <-
        $( do
            let state =
                  QState
                    { mode = MockQ
                    , knownNames = [("C", ''C)]
                    , reifyInfo = $(loadNames [''C])
                    }
            let result =
                  tryTestQ
                    state
                    (DeriveHasField.deriveHasField ''C)
            [|pure result|]
         )
      result
        `shouldBe` Left "deriveHasField: only supports data constructors with field names"

    it "should not be a sum of products" $ do
      result :: Either String [Dec] <-
        $( do
            let state =
                  QState
                    { mode = MockQ
                    , knownNames = [("D", ''D)]
                    , reifyInfo = $(loadNames [''D])
                    }
            let result =
                  tryTestQ
                    state
                    (DeriveHasField.deriveHasField ''D)
            [|pure result|]
         )
      result
        `shouldBe` Left "deriveHasField: only supports product types with a single data constructor"

    it "must share the same string representation when using deriveHasField" $ do
      result :: Either String [Dec] <-
        $( do
            let state =
                  QState
                    { mode = MockQ
                    , knownNames = [("A", ''A)]
                    , reifyInfo = $(loadNames [''A])
                    }
            let result =
                  tryTestQ
                    state
                    (DeriveHasField.deriveHasField ''A)
            [|pure result|]
         )
      result
        `shouldBe` Left "deriveHasField: type and data constructor must have the same string representation"

    it "must match record field prefix when using deriveHasField" $ do
      result :: Either String [Dec] <-
        $( do
            let state =
                  QState
                    { mode = MockQ
                    , knownNames = [("X", ''X)]
                    , reifyInfo = $(loadNames [''X])
                    }
            let result =
                  tryTestQ
                    state
                    (DeriveHasField.deriveHasField ''X)
            [|pure result|]
         )
      result
        `shouldBe` Left "deriveHasField: the assumed prefix `x` doesn't match the data constructor names"
