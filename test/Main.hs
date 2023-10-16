module Main where

import qualified Focus
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (choose)

main :: IO ()
main =
  defaultMain
    $ testGroup ""
    $ [ testCase "Monadically composed lookup and insert (https://github.com/nikita-volkov/stm-containers/issues/25)"
          $ let Focus.Focus absent present = do
                  prev <- Focus.lookup
                  Focus.insert "one"
                  pure (isJust prev)
             in do
                  assertEqual "" (True, Focus.Set "one") $ runIdentity (present "zero")
                  assertEqual "" (False, Focus.Set "one") $ runIdentity absent,
        testCase "Applicatively composed lookup and insert"
          $ let Focus.Focus absent present = (isJust <$> Focus.lookup) <* Focus.insert "one"
             in do
                  assertEqual "" (True, Focus.Set "one") $ runIdentity (present "zero")
                  assertEqual "" (False, Focus.Set "one") $ runIdentity absent,
        testCase "Applicatively composed pure and insert"
          $ let Focus.Focus absent present = pure () <* Focus.insert "one"
             in do
                  assertEqual "" ((), Focus.Set "one") (runIdentity (present "zero"))
                  assertEqual "" ((), Focus.Set "one") (runIdentity absent),
        testCase "insert"
          $ let Focus.Focus absent present = Focus.insert "one"
             in do
                  assertEqual "" ((), Focus.Set "one") (runIdentity (present "zero"))
                  assertEqual "" ((), Focus.Set "one") (runIdentity absent),
        testCase "alter"
          $ let f :: Maybe String -> Maybe String
                f = const Nothing
                Focus.Focus absent present = Focus.alter f
             in do
                  assertEqual "" ((), Focus.Remove) (runIdentity (present "zero"))
                  assertEqual "" ((), Focus.Leave) (runIdentity absent),
        testCase "update"
          $ let f :: String -> Maybe String
                f = const Nothing
                Focus.Focus absent present = Focus.update f
             in do
                  assertEqual "" ((), Focus.Remove) (runIdentity (present "zero"))
                  assertEqual "" ((), Focus.Leave) (runIdentity absent),
        testCase "updateM"
          $ let f :: String -> Identity (Maybe String)
                f = const (pure Nothing)
                Focus.Focus absent present = Focus.updateM f
             in do
                  assertEqual "" ((), Focus.Remove) (runIdentity (present "zero"))
                  assertEqual "" ((), Focus.Leave) (runIdentity absent),
        testCase "delete"
          $ let Focus.Focus absent present = Focus.delete
             in do
                  assertEqual "" ((), Focus.Remove) (runIdentity (present "zero"))
                  assertEqual "" ((), Focus.Leave @()) (runIdentity absent),
        testCase "Monadically composed lookup and delete (https://github.com/nikita-volkov/focus/issues/7)"
          $ let Focus.Focus absent present = do
                  a <- Focus.lookup
                  Focus.delete
                  pure a
             in do
                  assertEqual "" (Just (), Focus.Remove) (runIdentity (present ()))
                  assertEqual "" (Nothing @(), Focus.Leave) (runIdentity absent)
      ]
