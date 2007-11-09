{-
    libmpd for Haskell, an MPD client library.
    Copyright (C) 2005-2007  Ben Sinclair <bsinclai@turing.une.edu.au>

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
-}

-- | Module    : Network.MPD.StringConn
-- Copyright   : (c) Ben Sinclair 2005-2007
-- License     : LGPL
-- Maintainer  : bsinclai@turing.une.edu.au
-- Stability   : alpha
-- Portability : Haskell 98
--
-- Connection over a network socket.

module Network.MPD.StringConn (Expect, testMPD) where

import Control.Monad (liftM)
import Prelude hiding (exp)
import Network.MPD.Prim
import Data.IORef

-- | An expected request.
type Expect = String

-- | Run an action against a set of expected requests and responses,
-- and an expected result. The result is Nothing if everything matched
-- what was expected. If anything differed the result of the
-- computation is returned along with pairs of expected and received
-- requests.
testMPD :: (Eq a)
        => [(Expect, Response String)] -- ^ The expected requests and their
                                       -- ^ corresponding responses.
        -> Response a                  -- ^ The expected result.
        -> IO (Maybe String)           -- ^ An action that supplies passwords.
        -> MPD a                       -- ^ The MPD action to run.
        -> IO (Maybe (Response a, [(Expect,String)]))
testMPD pairs expected getpw m = do
    expectsRef    <- newIORef pairs
    mismatchesRef <- newIORef ([] :: [(Expect, String)])
    let open'  = return ()
        close' = return ()
        send'  = send expectsRef mismatchesRef
    result <- runMPD m $ Conn open' close' send' getpw
    mismatches <- liftM reverse $ readIORef mismatchesRef
    return $ if null mismatches && result == expected
             then Nothing
             else Just (result, mismatches)

send :: IORef [(Expect, Response String)] -- Expected requests and their
                                          -- responses.
     -> IORef [(Expect, String)]          -- An initially empty list of
                                          -- mismatches between expected and
                                          -- actual requests.
     -> String
     -> IO (Response String)
send expsR mmsR str = do
    xs <- readIORef expsR
    case xs of
        ((exp,resp):_) | exp == str -> modifyIORef expsR (drop 1) >> return resp
                       | otherwise  -> addMismatch exp
        [] -> addMismatch ""
    where
        addMismatch exp = modifyIORef mmsR ((exp,str):) >> return (Left NoMPD)
