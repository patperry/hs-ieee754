#!/usr/bin/env runhaskell
> import Distribution.Simple
> import System.Cmd
>
> testing _ _ _ _ = system "runhaskell tests/Properties.hs" >> return ()
>
> main = defaultMainWithHooks defaultUserHooks
>        {runTests=testing}
