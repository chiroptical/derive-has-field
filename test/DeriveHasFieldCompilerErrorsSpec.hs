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
  describe "deriveHasField" $ do
    it "compiles and gets the right field" $ do
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
