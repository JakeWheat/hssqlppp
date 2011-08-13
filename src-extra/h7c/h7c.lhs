
compiler library and command line access/ build tool

change the typecheckdb example to use this library
use the cmd to generate defaultTemplate1Catalog

milestones:

command line: parses sql, outputs parse errors or optionally ast if
successful

run typecheck on sql, display type errors

typecheck options: against saved schema, pg dump, pg database, sql
  source

load sql into database with optional type check

display catalog

use sql extensions - how should this work?:

  create a build file: this contains
    a list of sql filenames to use
    a list of extension names (fully qualified function names)
    a search path for the implementation of these modules

then can have the above options with sql extensions
plus ability out output the transformed sql as text

generate documentation



design:

what are the command line options

how do extensions work: want to support custom extensions as haskell
  source, and as dlls.



1st goal:
chaos loader
tpch conversion util
-> parse sql
   extensionise sql
   output transformed sql
   load sql into database
   typecheck
   clear database
   load into database



> import System.Environment
> import System.Exit

> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.Ast


> import ChaosExtensions


------------------

These are the options which will eventually be given on the command
line or in a config file

want to try to include the extensions by (possibly) compiling and
dlopening them when you run the compiler, so extensions are used in a
project either by having the haskell source of the extensions and/or
one or more precompiled dlls

> data Options = Options
>     {connectionString :: String
>     ,sourceFiles :: [String]
>     ,extensions :: [[Statement] -> [Statement]]}

> options :: Options
> options = Options
>           {connectionString = "dbname=chaos"
>           ,sourceFiles =
>            map ("examples/chaos/sql/" ++)
>                    ["chaos/server/Metadata.sql"
>                    ,"chaos/server/PiecePrototypes.sql"
>                    ,"chaos/server/Spells.sql"
>                    ,"chaos/server/GlobalData.sql"
>                    ,"chaos/server/Wizards.sql"
>                    ,"chaos/server/Pieces.sql"
>                    ,"chaos/server/TurnSequence.sql"
>                    ,"chaos/server/actions/TestSupport.sql"
>                    ,"chaos/server/actions/SquaresValid.sql"
>                    ,"chaos/server/actions/NextPhase.sql"
>                    ,"chaos/server/actions/SpellChoice.sql"
>                    ,"chaos/server/actions/SpellCast.sql"
>                    ,"chaos/server/actions/MovePhase.sql"
>                    ,"chaos/server/actions/Autonomous.sql"
>                    ,"chaos/server/actions/PieceChanges.sql"
>                    ,"chaos/server/actions/History.sql"
>                    ,"chaos/server/NewGame.sql"
>                    ,"chaos/server/TestBoards.sql"
>                    ,"chaos/server/AI.sql"
>                    ,"chaos/client/Sprites.sql"
>                    ,"chaos/client/WizardDisplayInfo.sql"
>                    ,"chaos/client/BoardWidget.sql"
>                    ,"chaos/client/SpellBookWidget.sql"
>                    ,"chaos/client/NewGameWidget.sql"
>                    ,"chaos/client/ClientActions.sql"
>                    ,"chaos/client/ClientNewGame.sql"]
>           ,extensions = chaosExtensions}



-----------------------

> main :: IO ()
> main = do
>   files <- getArgs
>   eas <- mapM parseStatementsFromFile files
>   let east :: Either ParseErrorExtra [Statement]
>       east = do
>              as <- sequence eas
>              return $ concat as
>   either (\e -> print e >> exitFailure)
>          (const $ return ())
>          east