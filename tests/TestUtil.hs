module TestUtil (
  with
, withPassword
, module StringConn
, module Test.Hspec
) where

import           Network.MPD.Core
import           Network.MPD.Applicative

import           Test.Hspec
import           StringConn

with :: Eq a => Command a -> [(Expect, Response String)] -> Response a
with = flip testMPD . runCommand

withPassword :: Eq a => Password
             -> [(Expect, Response String)]
             -> Command a
             -> Response a
withPassword pwd ps m = testMPDWithPassword ps pwd (runCommand m)
