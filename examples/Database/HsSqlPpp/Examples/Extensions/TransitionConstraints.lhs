Copyright 2010 Jake Wheat

transition constraints
======================

quickly hacked together. at the moment only supports constraints involving
a single tuple at a time from a single table. Separate functions
to create a constraint for updates, inserts and deletes.

I'm still not sure transition constraints like this are
useful. Something better might be regular constraints on
temporal relations.

> {-# LANGUAGE ViewPatterns, QuasiQuotes, ScopedTypeVariables, TemplateHaskell #-}
>
> module Database.HsSqlPpp.Examples.Extensions.TransitionConstraints
>     where
>
> import Data.Generics.Uniplate.Data
>
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Examples.Extensions.ExtensionsUtils
> import Database.HsSqlPpp.SqlQuote

examples
--------

> transitionConstraintExamples :: [ExtensionTest]
> transitionConstraintExamples = [
>    ExtensionTest "TransitionConstraint insert"
>     transitionConstraint
>     [$sqlStmts|
>
>      select create_insert_transition_tuple_constraint(
>         'relvar', 'relvar_insert', 'NEW.x>10');
>
>      |]
>     [$sqlStmts|
>
>      create function check_relvar_insert() returns trigger as $a$
>      begin
>        if not (NEW.x>10) then
>            raise exception 'insert on relvar violates transition constraint relvar_insert';
>        end if;
>        return OLD;
>      end;
>      $a$ language plpgsql volatile;
>
>      create trigger relvar_insert_transition_trigger
>        after insert on relvar
>        for each row
>        execute procedure check_relvar_insert();
>
>      |]
>   ,ExtensionTest "TransitionConstraint update"
>     transitionConstraint
>     [$sqlStmts|
>
>      select create_update_transition_tuple_constraint(
>         'relvar', 'relvar_update', 'NEW.x=OLD.y');
>
>      |]
>     [$sqlStmts|
>
>      create function check_relvar_update() returns trigger as $a$
>      begin
>        if not (NEW.x=OLD.y) then
>            raise exception 'update on relvar violates transition constraint relvar_update';
>        end if;
>        return OLD;
>      end;
>      $a$ language plpgsql volatile;
>
>      create trigger relvar_update_transition_trigger
>        after update on relvar
>        for each row
>        execute procedure check_relvar_update();
>
>      |]
>   ,ExtensionTest "TransitionConstraint delete"
>     transitionConstraint
>     [$sqlStmts|
>
>      select create_delete_transition_tuple_constraint(
>         'relvar', 'relvar_delete', 'OLD.y > 0');
>
>      |]
>     [$sqlStmts|
>
>      create function check_relvar_delete() returns trigger as $a$
>      begin
>        if not (OLD.y > 0) then
>            raise exception 'delete on relvar violates transition constraint relvar_delete';
>        end if;
>        return null;
>      end;
>      $a$ language plpgsql volatile;
>
>      create trigger relvar_delete_transition_trigger
>        after delete on relvar
>        for each row
>        execute procedure check_relvar_delete();
>
>      |]
>   ]

implementation
--------------

>
> transitionConstraint :: [Statement] -> [Statement]
> transitionConstraint =
>     transformBi $ \x ->
>       case x of
>         (funCallView -> FunCallView _
>                                     fn
>                                     [StringLit _ _ tableName
>                                     ,StringLit _ _ constraintName
>                                     ,StringLit _ _ expressionText]):tl
>             | fn == "create_insert_transition_tuple_constraint" ->
>                  gen TInsert tableName constraintName expressionText ++ tl
>             | fn == "create_update_transition_tuple_constraint" ->
>                  gen TUpdate tableName constraintName expressionText ++ tl
>             | fn == "create_delete_transition_tuple_constraint" ->
>                  gen TDelete tableName constraintName expressionText ++ tl
>         x1 -> x1
>     where
>       gen :: TriggerEvent -> String -> String -> String -> [Statement]
>       gen tct tableName constraintName expressionText =
>           let spliceFnName = "check_" ++ constraintName
>               ttname = case tct of
>                           TInsert -> "insert"
>                           TUpdate -> "update"
>                           TDelete -> "delete"
>               spliceErrMsg = ttname ++ " on " ++ tableName ++
>                        " violates transition constraint " ++ constraintName
>               spliceTriggerName = tableName ++ "_" ++ ttname ++ "_transition_trigger"
>               ret = if tct == TDelete
>                     then NullLit []
>                     else Identifier [] "OLD"
>               expr = either (error . show) id
>                             $ parseExpression "" expressionText
>           in [$sqlStmts|

\begin{code}

      create function $(spliceFnName)() returns trigger as $a$
      begin
        if not ( $(expr)) then
            raise exception '$(spliceErrMsg)';
        end if;
        return $(ret);
      end;
      $a$ language plpgsql volatile;

      create trigger $(spliceTriggerName)
        after $(tct) on relvar
        for each row
        execute procedure $(spliceFnName)();

\end{code}

>                            |]
