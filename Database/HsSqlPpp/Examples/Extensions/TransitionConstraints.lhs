Copyright 2010 Jake Wheat

=== transition constraints

quickly hacked together. at the moment only supports constraints involving
a single tuple at a time from a single table. Separate functions
to create a constraint for updates, inserts and deletes.

I'm still not sure transition constraints like this are
useful. Something better might be non transition constraints on
temporal relations.


> {-# LANGUAGE ViewPatterns, QuasiQuotes, ScopedTypeVariables, TemplateHaskell #-}
>
> module Database.HsSqlPpp.Examples.Extensions.TransitionConstraints
>     where
>
> import Data.Generics.Uniplate.Data
> --import Language.Haskell.TH
> --import Debug.Trace
>
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Utils.Here
> --import Database.HsSqlPpp.Utils.Utils
> import Database.HsSqlPpp.Examples.Extensions.ExtensionsUtils
> import Database.HsSqlPpp.SqlQuote

> transitionConstraintExamples :: [ExtensionTest]
> transitionConstraintExamples = [
>    ExtensionTest "TransitionConstraint insert"
>     transitionConstraint
>     [$here|
>
>      select create_insert_transition_tuple_constraint(
>         'relvar', 'relvar_insert', 'NEW.x>10');
>
>      |]
>     [$here|
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
>     [$here|
>
>      select create_update_transition_tuple_constraint(
>         'relvar', 'relvar_update', 'NEW.x=OLD.y');
>
>      |]
>     [$here|
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
>     [$here|
>
>      select create_delete_transition_tuple_constraint(
>         'relvar', 'relvar_delete', 'OLD.y > 0');
>
>      |]
>     [$here|
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

> {-transitionConstraintTemplate :: [Statement]
> transitionConstraintTemplate =
>     [$sqlQuote|
>
>      create function transition_constraint_check_function() returns trigger as $a$
>      begin
>        if not (transition_constraint_expression) then
>            --'transition on relvar violates transition constraint constraint_name';
>            raise exception 'transition_constraint_error_message';
>        end if;
>        return 'transition_constraint_return';
>      end;
>      $a$ language plpgsql volatile;
>
>      create trigger transition_constraint_trigger_name
>        after insert on relvar
>        for each row
>        execute procedure transition_constraint_check_function();
>
>      |]-}

 > --makeTemplate :: [Statement]
 > makeTemplate :: ExpQ -> [Statement]

> {-makeTemplate :: String -> Expression -> Expression -> [Statement]
> makeTemplate spliceFnName expr ret =
>     [$sqlQuote|
>
>      create function spliceFnName() returns trigger as $a$
>      begin
>        if not ( $(expr)) then
>            --'transition on relvar violates transition constraint constraint_name';
>            raise exception 'transition_constraint_error_message';
>        end if;
>        return $(ret); -- 'transition_constraint_return';
>      end;
>      $a$ language plpgsql volatile;
>
>      create trigger transition_constraint_trigger_name
>        after insert on relvar
>        for each row
>        execute procedure spliceFnName();
>
>      |]-}

transition_constraint_check_function - identifier
transition_constraint_expression - expression
transition_constraint_error_message - string
transition_constraint_return - expression either OLD or null
transition_constraint_trigger_name - identifier
after insert on relvar - replace insert with update or delete if neccessary

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
>               template0 = [$sqlQuote|
\begin{code}

      create function $(spliceFnName)() returns trigger as $a$
      begin
        if not ( $(expr)) then
            --'transition on relvar violates transition constraint constraint_name';
            raise exception '$(spliceErrMsg)';
        end if;
        return $(ret); -- 'transition_constraint_return';
      end;
      $a$ language plpgsql volatile;

      create trigger $(spliceTriggerName)
        after insert on relvar
        for each row
        execute procedure $(spliceFnName)();

\end{code}
>                            |]
>               {-template0 = makeTemplate ("check_" ++ constraintName) expression retPart
>               --template0 = transitionConstraintTemplate
>               ttname = case tct of
>                           TInsert -> "insert"
>                           TUpdate -> "update"
>                           TDelete -> "delete"
>               errMsg = ttname ++ " on " ++ tableName ++
>                        " violates transition constraint " ++ constraintName;
>               --replace the strings: function name, error message, trigger name
>               template1 = mapStrings [
>                        --("transition_constraint_check_function"
>                        --,"check_" ++ constraintName)
>                        ("transition_constraint_error_message",errMsg)
>                       ,("transition_constraint_trigger_name"
>                        ,tableName ++ "_" ++ ttname ++ "_transition_trigger")]
>                     template0-}
>               -- replace the expression
>               {-expression = case parseExpression "" expressionText of
>                              Left e -> error $ show e
>                              Right ex -> ex-}
>               -- replace the return type of the trigger function
>               {-ret = if tct == TDelete
>                     then NullLit []
>                     else Identifier [] "OLD"-}
>               -- and replace the trigger event
>               {-template2 = flip transformBi template1 $ \x ->
>                             case x of
>                               (Identifier _ "transition_constraint_expression") -> expression
>                               --(StringLit _ "'" "transition_constraint_return") -> ret
>                               x1 -> x1-}
>               -- replace the trigger event
>               template3 = flip transformBi template0 $ \x ->
>                             case x of
>                               TInsert -> tct
>                               x1 -> x1
>           in template3
