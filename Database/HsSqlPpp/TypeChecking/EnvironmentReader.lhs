Copyright 2009 Jake Wheat

This module contains the code to read a set of environment updates
from a database.

> {-# OPTIONS_HADDOCK hide  #-}

> module Database.HsSqlPpp.TypeChecking.EnvironmentReader
>     (readEnvironmentFromDatabase) where

> import Database.HsSqlPpp.TypeChecking.EnvironmentInternal

> -- | Creates an 'EnvironmentUpdate' list by reading the database given.
> -- To create an Environment value from this, use
> --
> -- @
> -- env <- readEnvironmentFromDatabase 'something'
> -- let newEnv = updateEnvironment defaultEnvironment env
> -- @
> readEnvironmentFromDatabase :: String -- ^ name of the database to read
>                             -> IO [EnvironmentUpdate]
> readEnvironmentFromDatabase = undefined
