#!/usr/bin/env runhaskell

Generate coverage reports with HPC.
Usage: runhaskell coverage.lhs

\begin{code}
import Control.Monad (when)
import System.Cmd (system)
import System.Exit (ExitCode(..), exitWith)
import System.Directory (doesDirectoryExist, removeDirectoryRecursive,
                         removeFile)

excludeModules =
    ["Main", "Arbitrary", "Properties", "Displayable", "Commands", "StringConn"]

main = do
    -- Cleanup from previous runs
    hpcExists <- doesDirectoryExist ".hpc"
    when hpcExists $ do
        removeDirectoryRecursive ".hpc"
        removeFile "test.tix"

    -- Build and generate coverage report
    run "runhaskell Setup clean"
    run "runhaskell Setup configure -f test -f coverage --disable-optimization --user"
    run "runhaskell Setup build"
    run "dist/build/test/test"
    run ("hpc markup test.tix --destdir=report " ++ exclude)
    run ("hpc report test.tix " ++ exclude)
    where
        exclude = unwords (map ("--exclude=" ++) excludeModules)

run x = system x >>= catchFailure
    where
        catchFailure (ExitFailure _) = exitWith (ExitFailure 1)
        catchFailure              _  = return ()
\end{code}
