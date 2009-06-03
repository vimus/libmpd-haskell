#!/usr/bin/env runhaskell

Compile and run tests.
Usage: runhaskell run-tests.lhs [arguments]

\begin{code}
import Control.Monad (when)
import System.Directory (removeFile)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.IO (hGetContents)
import System.Process


main = do
    -- Build the test binary
    run "runhaskell Setup configure -f test --disable-optimization --user"
    run "runhaskell Setup build"

    -- Generate library haddocks
    run "runhaskell Setup configure --user"
    run "runhaskell Setup haddock --hyperlink"

    -- Run test suite and save output to test.log
    args <- getArgs
    (_, oh, _, ph) <- runInteractiveProcess "dist/build/test/test" args
                                            Nothing Nothing
    waitForProcess ph
    result <- hGetContents oh
    putStr result
    writeFile "test.log" result

    -- Detect failure
    let ws = concatMap words (lines result)
        failStrings = ["Falsifiable", "*** FAILURE ***"]
    when (any (`elem` ws) failStrings) $ do
        putStrLn "Failure detected: see test.log"
        exitWith (ExitFailure 1)

    -- Cleanup
    removeFile "test.log"

-- A wrapper for 'system' that halts the program if the command fails.
run x = system x >>= catchFailure
    where
        catchFailure (ExitFailure _) = exitWith (ExitFailure 1)
        catchFailure              _  = return ()

\end{code}
