#!/usr/bin/env runhaskell

Compile and run tests.
Usage: runhaskell run-tests.lhs [arguments]

\begin{code}
import System.Exit (ExitCode(..), exitWith)
import System.IO (hGetContents)
import System.Process (runCommand, system, waitForProcess)

main :: IO ()
main = do
    -- Build the test binary
    run "runhaskell Setup configure -f test --disable-optimization --user"
    run "runhaskell Setup build"

    -- Generate library haddocks
    run "runhaskell Setup configure --user"
    run "runhaskell Setup haddock --hyperlink"

    -- Run test suite and report outcome to caller
    ph <- runCommand "dist/build/test/test"
    exitWith =<< waitForProcess =<< runCommand "dist/build/test/test"

-- A wrapper for 'system' that halts the program if the command fails.
run :: String -> IO ()
run x = system x >>= catchFailure
    where
        catchFailure (ExitFailure _) = exitWith (ExitFailure 1)
        catchFailure              _  = return ()

\end{code}
