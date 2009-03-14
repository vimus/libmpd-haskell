{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

-- | Module    : StringConn
-- Copyright   : (c) Ben Sinclair 2005-2008
-- License     : LGPL (see LICENSE)
-- Maintainer  : bsinclai@turing.une.edu.au
-- Stability   : alpha
-- Portability : Haskell 98
--
-- A testing scaffold for MPD commands

module StringConn (StringMPD(..), Expect, Result(..), testMPD) where

import Prelude hiding (exp)
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Network.MPD.Core

newtype StringMPD a =
    SMPD { runSMPD :: ErrorT MPDError
                      (StateT [(Expect, Response String)]
                       (ReaderT Password Identity)) a
         } deriving (Functor, Monad, MonadError MPDError)

instance MonadMPD StringMPD where
    open  = return ()
    close = return ()
    getPassword = SMPD ask
    send request =
        SMPD $ do
            ~pairs@((expected_request,response):rest) <- get
            when (null pairs)
                 (throwError $ strMsg "MPD action made too many calls")
            when (expected_request /= request)
                 (throwError $ strMsg "unexpected MPD request")
            put rest
            either throwError return response

-- | An expected request.
type Expect = String

data Result a = Ok | Failure (Response a) [(Expect,String)]
                deriving Show

-- | Run an action against a set of expected requests and responses,
-- and an expected result. The result is Nothing if everything matched
-- what was expected. If anything differed the result of the
-- computation is returned along with pairs of expected and received
-- requests.
testMPD :: (Eq a)
        => [(Expect, Response String)] -- ^ The expected requests and their
                                       -- ^ corresponding responses.
        -> Response a                  -- ^ The expected result.
        -> Password                    -- ^ A password to be supplied.
        -> StringMPD a                 -- ^ The MPD action to run.
        -> Result a
testMPD pairs expected pw m
    | result == expected = Ok
    | otherwise          = Failure result (error "mismatches")
    where
        result = runIdentity $ runReaderT
                     (evalStateT (runErrorT $ runSMPD m) pairs) pw
