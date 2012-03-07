module Spec (main, spec) where

import           Test.Hspec.Monadic
import           Test.Hspec.HUnit()
import           Test.HUnit

import           Network.MPD
import           Network.MPD.Util
import           System.Posix.Env hiding (getEnvDefault)

shouldBe :: (Eq a, Show a) => a -> a -> Assertion
shouldBe = (@?=)

main :: IO ()
main = hspecX spec

spec :: Specs
spec = do

  -- * Network.MPD.Util
  describe "splitGroups" $ do
    it "breaks an association list into sublists" $ do
      splitGroups ["1", "5"]
        [("1","a"),("2","b"),
         ("5","c"),("6","d"),
         ("1","z"),("2","y"),("3","x")]
         `shouldBe`
         [[("1","a"),("2","b")],
         [("5","c"),("6","d")],
         [("1","z"),("2","y"),("3","x")]]

  -- * Network.MPD
  describe "getEnvDefault" $ do
    it "returns the value of an environment variable" $ do
      setEnv "FOO" "foo" True
      r <- getEnvDefault "FOO" "bar"
      r `shouldBe` "foo"

    it "returns a given default value if that environment variable is not set" $ do
      unsetEnv "FOO"
      r <- getEnvDefault "FOO" "bar"
      r `shouldBe` "bar"

  describe "getConnectionSettings" $ do
    it "takes an optional argument, that overrides MPD_HOST" $ do
      setEnv "MPD_HOST" "user@example.com" True
      Right (host, _, pw) <- getConnectionSettings (Just "foo@bar") Nothing
      pw `shouldBe` "foo"
      host `shouldBe` "bar"

    it "takes an optional argument, that overrides MPD_PORT" $ do
      setEnv "MPD_PORT" "8080" True
      Right (_, port, _) <- getConnectionSettings Nothing (Just "23")
      port `shouldBe` 23

    it "returns an error message, if MPD_PORT is not an int" $ do
      setEnv "MPD_PORT" "foo" True
      r <- getConnectionSettings Nothing Nothing
      r `shouldBe` Left "\"foo\" is not a valid port!"
      unsetEnv "MPD_PORT"

    describe "host" $ do
      it "is taken from MPD_HOST" $ do
        setEnv "MPD_HOST" "example.com" True
        Right (host, _, _) <- getConnectionSettings Nothing Nothing
        host `shouldBe` "example.com"

      it "is 'localhost' if MPD_HOST is not set" $ do
        unsetEnv "MPD_HOST"
        Right (host, _, _) <- getConnectionSettings Nothing Nothing
        host `shouldBe` "localhost"

    describe "port" $ do
      it "is taken from MPD_PORT" $ do
        setEnv "MPD_PORT" "8080" True
        Right (_, port, _) <- getConnectionSettings Nothing Nothing
        port `shouldBe` 8080

      it "is 6600 if MPD_PORT is not set" $ do
        unsetEnv "MPD_PORT"
        Right (_, port, _) <- getConnectionSettings Nothing Nothing
        port `shouldBe` 6600

    describe "password" $ do
      it "is taken from MPD_HOST if MPD_HOST is of the form password@host" $ do
        setEnv "MPD_HOST" "password@host" True
        Right (host, _, pw) <- getConnectionSettings Nothing Nothing
        host `shouldBe` "host"
        pw `shouldBe` "password"

      it "is '' if MPD_HOST is not of the form password@host" $ do
        setEnv "MPD_HOST" "example.com" True
        Right (_, _, pw) <- getConnectionSettings Nothing Nothing
        pw `shouldBe` ""

      it "is '' if MPD_HOST is not set" $ do
        unsetEnv "MPD_HOST"
        Right (_, _, pw) <- getConnectionSettings Nothing Nothing
        pw `shouldBe` ""
