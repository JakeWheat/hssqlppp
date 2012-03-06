
transition constraints
======================

quickly hacked together. at the moment only supports constraints involving
a single tuple at a time from a single table. Separate functions
to create a constraint for updates, inserts and deletes.

I'm still not sure transition constraints like this are
useful. Something better might be regular constraints on
temporal relations.

> {-# LANGUAGE QuasiQuotes, ScopedTypeVariables,OverloadedStrings #-}
>
> module Database.HsSqlPpp.Extensions.TransitionConstraints
>     where
>
> import Data.Generics
> import Data.Generics.Uniplate.Data
>
> import Database.HsSqlPpp.Ast
> --import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Extensions.ExtensionsUtils
> import Database.HsSqlPpp.Quote
> import Database.HsSqlPpp.Extensions.AstUtils


examples
--------

> transitionConstraintExamples :: [ExtensionTest]
> transitionConstraintExamples = [
>    ExtensionTest "TransitionConstraint insert"
>     transitionConstraints
>     [sqlStmts|
>
>      select create_insert_transition_tuple_constraint(
>         'relvar', 'relvar_insert', 'NEW.x>10');
>
>      |]
>     [sqlStmts|
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
>     transitionConstraints
>     [sqlStmts|
>
>      select create_update_transition_tuple_constraint(
>         'relvar', 'relvar_update', 'NEW.x=OLD.y');
>
>      |]
>     [sqlStmts|
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
>     transitionConstraints
>     [sqlStmts|
>
>      select create_delete_transition_tuple_constraint(
>         'relvar', 'relvar_delete', 'OLD.y > 0');
>
>      |]
>     [sqlStmts|
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
> transitionConstraints :: Data a => a -> a
> transitionConstraints =
>     transformBi $ \x ->
>       case x of
>         s@[sqlStmt| select $m(fn)($e(tablename)
>                                  ,$e(constraintname)
>                                  ,$e(expressiontext));|] : tl
>             | fn == Nmc "create_insert_transition_tuple_constraint" ->
>                  replaceSourcePos s (
>                  gen TInsert tablename constraintname expressiontext) ++ tl
>             | fn == Nmc "create_update_transition_tuple_constraint" ->
>                  replaceSourcePos s (
>                  gen TUpdate tablename constraintname expressiontext) ++ tl
>             | fn == Nmc "create_delete_transition_tuple_constraint" ->
>                  replaceSourcePos s (
>                  gen TDelete tablename constraintname expressiontext) ++ tl
>         x1 -> x1
>     where
>       gen :: TriggerEvent -> ScalarExpr -> ScalarExpr -> ScalarExpr -> [Statement]
>       gen tct tablename constraintName expr =
>           let un (StringLit _ x) = x
>               un y = error $ "bad stringlit: " ++ show y
>               tblname = Nmc $ un tablename
>               n x = Name emptyAnnotation [Nmc x]
>               spliceFnName = n $ "check_" ++ un constraintName
>               ttname = case tct of
>                           TInsert -> "insert"
>                           TUpdate -> "update"
>                           TDelete -> "delete"
>                           AntiTriggerEvent s -> "$(" ++ s ++ ")"
>               spliceErrMsg = ttname ++ " on " ++ un tablename ++
>                        " violates transition constraint " ++ un constraintName
>               spliceTriggerName = Nmc $ un tablename ++ "_" ++ ttname ++ "_transition_trigger"
>               ret = if tct == TDelete
>                     then [sqlExpr| null |]
>                     else [sqlExpr| OLD |]
>               --expr = either (error . show) id
>               --              $ parseScalarExpr defaultParseFlags "" Nothing expressionText
>           in [sqlStmts|

\begin{code}

      create function $n(spliceFnName)() returns trigger as $a$
      begin
        if not ( $e(expr)) then
            raise exception $s(spliceErrMsg);
        end if;
        return $e(ret);
      end;
      $a$ language plpgsql volatile;

      create trigger $m(spliceTriggerName)
        after $t(tct) on $m(tblname)
        for each row
        execute procedure $n(spliceFnName)();

\end{code}

>                            |]
