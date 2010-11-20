{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wwarn #-}

-- | Module    : StringConn
-- Copyright   : (c) Ben Sinclair 2005-2009
-- License     : LGPL (see LICENSE)
-- Maintainer  : bsinclai@turing.une.edu.au
-- Stability   : alpha
--
-- A testing scaffold for MPD commands

module StringConn where

import Prelude hiding (exp)
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Network.MPD.Core

-- | An expected request.
type Expect = String

data StringMPDError
    = TooManyRequests
    | UnexpectedRequest Expect String
      deriving (Show, Eq)

data Result a
    = Ok
    | BadResult (Response a) (Response a)  -- expected, then actual
    | BadRequest StringMPDError
      deriving (Show, Eq)

newtype MatchError = MErr (Either StringMPDError MPDError)
instance Error MatchError where
    strMsg = MErr . Right . strMsg

newtype StringMPD a =
    SMPD { runSMPD :: ErrorT MatchError
                      (StateT [(Expect, Response String)]
                       (ReaderT Password Identity)) a
         } deriving (Functor, Monad)

instance MonadError MPDError StringMPD where
    throwError = SMPD . throwError . MErr . Right
    catchError m f = SMPD $ catchError (runSMPD m) handler
        where
            handler err@(MErr (Left _)) = throwError err
            handler (MErr (Right err))  = runSMPD (f err)

instance MonadMPD StringMPD where
    open  = return ()
    close = return ()
    getPassword = SMPD ask
    send request =
        SMPD $ do
            ~pairs@((expected_request,response):rest) <- get
            when (null pairs)
                 (throwError . MErr $ Left TooManyRequests)
            when (expected_request /= request)
                 (throwError . MErr . Left
                             $ UnexpectedRequest expected_request request)
            put rest
            either (throwError . MErr . Right) return response

-- | Run an action against a set of expected requests and responses,
-- and an expected result. The result is Nothing if everything matched
-- what was expected. If anything differed the result of the
-- computation is returned along with pairs of expected and received
-- requests.
testMPD :: (Eq a)
        => [(Expect, Response String)] -- ^ The expected requests and their
                                       -- ^ corresponding responses.
        -> Response a                  -- ^ The expected final result.
        -> Password                    -- ^ A password to be supplied.
        -> StringMPD a                 -- ^ The MPD action to run.
        -> Result a
testMPD pairs expected passwd m =
    let result = runIdentity
               $ runReaderT (evalStateT (runErrorT $ runSMPD m) pairs) passwd
    in case result of
           Right r
               | Right r == expected -> Ok
               | otherwise           -> BadResult expected (Right r)
           Left (MErr (Right r))
               | Left r == expected -> Ok
               | otherwise          -> BadResult expected (Left r)
           Left (MErr (Left e)) -> BadRequest e
