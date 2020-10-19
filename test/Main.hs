module Main where

import Prelude hiding (choose)
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Test.QuickCheck as QuickCheck
import qualified Test.QuickCheck.Property as QuickCheck
import qualified Focus


main =
  defaultMain $
  testGroup "" $
  [
    testCase "Monadically composed lookup and insert (https://github.com/nikita-volkov/stm-containers/issues/25)" $ let
      Focus.Focus absent present = do
        prev <- Focus.lookup
        Focus.insert "one"
        pure (isJust prev)
      in do
        assertEqual "" (True, Focus.Set "one") $ runIdentity (present "zero")
        assertEqual "" (False, Focus.Set "one") $ runIdentity absent
    ,
    testCase "Applicatively composed lookup and insert" $ let
      Focus.Focus absent present = (isJust <$> Focus.lookup) <* Focus.insert "one"
      in do
        assertEqual "" (True, Focus.Set "one") $ runIdentity (present "zero")
        assertEqual "" (False, Focus.Set "one") $ runIdentity absent
    ,
    testCase "Applicatively composed pure and insert" $ let
      Focus.Focus absent present = pure () <* Focus.insert "one"
      in do
        assertEqual "" ((), Focus.Set "one") (runIdentity (present "zero"))
        assertEqual "" ((), Focus.Set "one") (runIdentity absent)
    ,
    testCase "insert" $ let
      Focus.Focus absent present = Focus.insert "one"
      in do
        assertEqual "" ((), Focus.Set "one") (runIdentity (present "zero"))
        assertEqual "" ((), Focus.Set "one") (runIdentity absent)
    ,
    testCase "update" $ let
      f :: String -> Maybe String
      f = const Nothing
      Focus.Focus absent present = Focus.update f
      in do
        assertEqual "" ((), Focus.Remove) (runIdentity (present "zero"))
        assertEqual "" ((), Focus.Leave) (runIdentity absent)
  ]
