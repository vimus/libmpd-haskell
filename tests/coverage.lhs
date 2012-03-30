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
    [ "Main"
    , "Arbitrary"
    , "Properties"
    , "Commands"
    , "StringConn"
    , "Unparse"
    , "Defaults"
    ]

main = do
    -- Cleanup from previous runs
    hpcExists <- doesDirectoryExist ".hpc"
    when hpcExists $ do
        removeDirectoryRecursive ".hpc"
        removeFile "specs.tix"

    -- Build and generate coverage report
    run "runhaskell Setup clean"
    run "runhaskell Setup configure --enable-tests --enable-library-coverage --disable-optimization --user"
    run "runhaskell Setup build"
    run "dist/build/specs/specs"
    run ("hpc markup specs.tix --destdir=coverage-report " ++ exclude)
    run ("hpc report specs.tix " ++ exclude)
    where
        exclude = unwords (map ("--exclude=" ++) excludeModules)

run x = system x >>= catchFailure
    where
        catchFailure (ExitFailure _) = exitWith (ExitFailure 1)
        catchFailure              _  = return ()
\end{code}
