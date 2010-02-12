Copyright 2010 Jake Wheat


> {-# LANGUAGE ViewPatterns, QuasiQuotes, ScopedTypeVariables #-}
> module Database.HsSqlPpp.Examples.Extensions.Denormalized6nfExamples
>     (denormalized6nfExamples) where
>
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Examples.Extensions.ExtensionsUtils
> import Database.HsSqlPpp.SqlQuote
> import Database.HsSqlPpp.Examples.Extensions.Denormalized6nf
>
> denormalized6nfExamples :: [ExtensionTest]
> denormalized6nfExamples  = [denormalized6nfExample1
>                            ,denormalized6nfExample2
>                            ,denormalized6nfExample3
>                            ,denormalized6nfExample4
>                            ,denormalized6nfExample5
>                            ,denormalized6nfExample6
>                            ,denormalized6nfExample7
>                            ]

simple example with one 'subclass'

> denormalized6nfExample1 :: ExtensionTest
> denormalized6nfExample1 =
>   ExtensionTest
>     "denormalized6nfExample1"
>     denormalized6nf
>     [$sqlStmts|
>       select create6nf($$
>         pieces (
>           ptype text primary key,
>           ptype2 text
>         );
>         creatures : piece (
>           speed int,
>           agility int
>         );
>       $$);
>      |]
>     [$sqlStmts|
>       create table piece (
>         ptype text primary key,
>         ptype2 text not null,
>         speed int null,
>         agility int null,
>         constraint creatures_fields
>           check ((speed is null and agility is null)
>                  or (speed is not null and agility is not null))
>       );
>       create view pieces_base as
>         select ptype, ptype2 from pieces;
>       create view creatures as
>         select ptype,ptype2,speed,agility from pieces
>                where speed is not null
>                      and agility is not null;
>      |]

two unconnected 'subclasses'

> denormalized6nfExample2 :: ExtensionTest
> denormalized6nfExample2 =
>   ExtensionTest
>     "denormalized6nfExample2"
>     denormalized6nf
>     [$sqlStmts|
>       select create6nf ($$
>         pieces (
>           ptype text primary key
>         );
>         creatures : pieces (
>           speed int,
>           agility int
>         );
>         attackers : pieces (
>           attack_strength int,
>           attack_style text
>         );
>         -- this provides a name for the view, which we don't generate otherwise
>         attacking_creatures : pieces,attackers;
>       $$);
>      |]
>     [$sqlStmts|
>       create table pieces
>        (ptype text primary key
>        ,speed int null
>        ,agility int null
>        ,attack_strength int null
>        ,attack_style text null
>        ,constraint creatures_fields
>           check ((speed is null and agility is null)
>                  or (speed is not null and agility is not null))
>        ,constraint attackers_fields
>           check ((attack_strength is null and attack_style is null)
>                  or (attack_strength is not null and attack_style is not null))
>        );
>       create view pieces_base as
>         select ptype
>           from pieces;
>       create view creatures as
>         select ptype,speed,agility
>           from piece_prototypes
>           where speed is not null
>                 and agility is not null;
>       create view attackers as
>         select ptype,attack_strength,attack_style
>           from pieces
>           where attack_strength is not null
>                 and attack_style is not null;
>       create view attacking_creatures as
>         select ptype,speed,agility,attack_strength,attack_style
>           from pieces
>           where speed is not null
>                 and agility is not null
>                 and attack_strength is not null
>                 and attack_style is not null;
>      |]

one field only check

> denormalized6nfExample3 :: ExtensionTest
> denormalized6nfExample3 =
>   ExtensionTest
>     "denormalized6nfExample3"
>     denormalized6nf
>     [$sqlStmts|
>       select create6nf ($$
>         pieces (
>           ptype text primary key
>         );
>         creatures : pieces (
>           speed int
>         );
>       $$);
>      |]
>     [$sqlStmts|
>       create table pieces
>        (ptype text primary key
>        ,speed int null
>        );
>       create view pieces as
>         select ptype from pieces;
>       create view creatures as
>         select ptype,speed from pieces
>           where speed is not null;
>      |]

no combo view check

> denormalized6nfExample4 :: ExtensionTest
> denormalized6nfExample4 =
>   ExtensionTest
>     "denormalized6nfExample4"
>     denormalized6nf
>     [$sqlStmts|
>       select create6nf ($$
>         pieces (
>           ptype text primary key
>         );
>         creatures : pieces  (
>           speed int,
>           agility int
>         );
>         attackers : pieces (
>           attack_strength int,
>           attack_style text
>         );
>       $$);
>      |]
>     [$sqlStmts|
>       create table pieces
>        (ptype text primary key
>        ,speed int null
>        ,agility int null
>        ,attack_strength int null
>        ,attack_style text null
>        ,constraint creatures_fields
>           check ((speed is null and agility is null)
>                  or (speed is not null and agility is not null))
>        ,constraint attackers_fields
>           check ((attack_strength is null and attack_style is null)
>                  or (attack_strength is not null and attack_style is not null))
>        );
>       create view pieces_base as
>         select ptype
>           from pieces;
>       create view creatures as
>         select ptype,speed,agility
>           from pieces
>           where speed is not null
>                 and agility is not null;
>       create view attackers as
>         select ptype,attack_strength,attack_style
>           from pieces
>           where attack_strength is not null
>                 and attack_style is not null;
>      |]

two chained 'subclasses'

> denormalized6nfExample5 :: ExtensionTest
> denormalized6nfExample5 =
>   ExtensionTest
>     "denormalized6nfExample5"
>     denormalized6nf
>     [$sqlStmts|
>       select create6nf ($$
>         pieces (
>           ptype text primary key
>         );
>         creatures : pieces (
>           speed int,
>           agility int
>         );
>         attackers : creatures (
>           attack_strength int,
>           attack_style text
>         );
>       $$);
>      |]
>     [$sqlStmts|
>       create table pieces
>        (ptype text primary key
>        ,speed int null
>        ,agility int null
>        ,attack_strength int null
>        ,attack_style text null
>        ,constraint creatures_fields
>           check ((speed is null and agility is null)
>                  or (speed is not null and agility is not null))
>        ,constraint attackers_fields
>           check ((speed is null and agility is null
>                   and attack_strength is null and attack_style is null)
>                  or (speed is not null and attack_strength is not null
>                      and attack_strength is not null and attack_style is not null))
>        );
>       create view pieces_base as
>         select ptype
>           from pieces;
>       create view creatures as
>         select ptype,speed,agility
>           from pieces
>           where speed is not null
>                 and agility is not null;
>       create view attackers as
>         select ptype,speed,agility,
>                attack_strength,attack_style
>           from pieces
>           where speed is not null
>                 and agility is not null
>                 and attack_strength is not null
>                 and attack_style is not null;
>      |]

diamond inheritance

> denormalized6nfExample6 :: ExtensionTest
> denormalized6nfExample6 =
>   ExtensionTest
>     "denormalized6nfExample6"
>     denormalized6nf
>     [$sqlStmts|
>       select create6nf ($$
>         pieces (
>           ptype text primary key
>         );
>         creatures : pieces (
>           speed int,
>           agility int
>         );
>         attackers : pieces (
>           attack_strength int,
>           attack_style text
>         );
>         monsters : creatures, attackers (
>           resistance int,
>           armour int
>         );
>       $$);
>      |]
>     [$sqlStmts|
>       create table pieces
>        (ptype text primary key
>        ,speed int null
>        ,agility int null
>        ,attack_strength int null
>        ,attack_style text null
>        ,resistance int null
>        ,armour int null
>        ,constraint creatures_fields
>           check ((speed is null and agility is null)
>               or (speed is not null and agility is not null))
>        ,constraint attackers_fields
>           check ((attack_strength is null and attack_style is null)
>               or (attack_strength is not null and attack_style is not null))
>        ,constraint monsters_fields
>           check ((speed is null and agility is null
>                   and attack_strength is null and attack_style is null
>                   and resistance is null and armour is null)
>               or (speed is not null and attack_strength is not null
>                   and attack_strength is not null and attack_style is not null
>                   and resistance is not null and armour is not null))
>        );
>       create view pieces_base as
>         select ptype
>           from pieces;
>       create view creatures as
>         select ptype,speed,agility
>           from pieces
>           where speed is not null
>                 and agility is not null;
>       create view attackers as
>         select ptype,attack_strength,attack_style
>           from pieces
>           where attack_strength is not null
>                 and attack_style is not null;
>       create view monsters as
>         select ptype,speed,agility,attack_strength,attack_style,
>                resistance,armour
>           from pieces
>           where speed is not null
>                 and agility is not null
>                 and attack_strength is not null
>                 and attack_style is not null
>                 and resistance is not null
>                 and armour is not null;
>      |]

fdk

> denormalized6nfExample7 :: ExtensionTest
> denormalized6nfExample7 =
>   ExtensionTest
>     "denormalized6nfExample7"
>     denormalized6nf
>     [$sqlStmts|
>       select create6nf ($$
>         pieces (
>           ptype text primary key
>         );
>         creatures : pieces (
>           speed int,
>           agility int
>         );
>         attackers (
>           attack_strength int,
>           attack_style text
>         );
>         mutually_exclusive(attackers,creatures);
>       $$);
>      |]
>     [$sqlStmts|
>       create table pieces
>        (ptype text primary key
>        ,speed int null
>        ,agility int null
>        ,attack_strength int null
>        ,attack_style text null
>        ,constraint creatures_fields
>           check ((speed is null and agility is null)
>                  or (speed is not null and agility is not null))
>        ,constraint attackers_fields
>           check ((attack_strength is null and attack_style is null)
>                  or (attack_strength is not null and attack_style is not null))
>        ,constraint me_pieces_attackers
>           -- doesn't scale to three mutual exclusives
>           check (speed is not null <> attack_strength is not null)
>        );
>       create view pieces_base as
>         select ptype
>           from pieces;
>       create view creatures as
>         select ptype,speed,agility
>           from pieces
>           where speed is not null
>                 and agility is not null;
>       create view attackers as
>         select ptype,attack_strength,attack_style
>           from pieces
>           where attack_strength is not null
>                 and attack_style is not null;
>      |]

todo: no base class, distributed key
