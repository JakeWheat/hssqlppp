
> module Database.HsSqlPpp.Chaos.ChaosFiles where


The reference list of sql files for chaos2010 example db, in order


> chaosFiles :: [String]
> chaosFiles = map ("examples/chaos/sql/" ++)
>         ["chaos/server/Metadata.sql"
>         ,"chaos/server/PiecePrototypes.sql"
>         ,"chaos/server/Spells.sql"
>         ,"chaos/server/GlobalData.sql"
>         ,"chaos/server/Wizards.sql"
>         ,"chaos/server/Pieces.sql"
>         ,"chaos/server/TurnSequence.sql"
>         ,"chaos/server/actions/TestSupport.sql"
>         ,"chaos/server/actions/SquaresValid.sql"
>         ,"chaos/server/actions/NextPhase.sql"
>         ,"chaos/server/actions/SpellChoice.sql"
>         ,"chaos/server/actions/SpellCast.sql"
>         ,"chaos/server/actions/MovePhase.sql"
>         ,"chaos/server/actions/Autonomous.sql"
>         ,"chaos/server/actions/PieceChanges.sql"
>         ,"chaos/server/actions/History.sql"
>         ,"chaos/server/NewGame.sql"
>         ,"chaos/server/TestBoards.sql"
>         ,"chaos/server/AI.sql"
>         ,"chaos/client/Sprites.sql"
>         ,"chaos/client/WizardDisplayInfo.sql"
>         ,"chaos/client/BoardWidget.sql"
>         ,"chaos/client/SpellBookWidget.sql"
>         ,"chaos/client/NewGameWidget.sql"
>         ,"chaos/client/ClientActions.sql"
>         ,"chaos/client/ClientNewGame.sql"
>         ]
