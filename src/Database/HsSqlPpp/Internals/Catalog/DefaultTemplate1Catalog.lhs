


This file is auto generated, to regenerate use the
regenDefaultTemplate1catalog.sh script. You will need postgresql
installed to do this.

> module Database.HsSqlPpp.Internals.Catalog.DefaultTemplate1Catalog
>      (defaultTemplate1Catalog) where
>
> import Database.HsSqlPpp.Internals.Catalog.CatalogInternal
> --import Database.HsSqlPpp.Internals.TypesInternal
> -- | The catalog from a default template1 database in roughly the
> -- latest postgres. 'select version()' from the dbms this catalog
> -- was generated from: PostgreSQL 9.1.1 on x86_64-unknown-linux-gnu, compiled by gcc-4.6.real (Debian 4.6.1-15) 4.6.1, 64-bit
> defaultTemplate1Catalog :: Catalog
> defaultTemplate1Catalog =
>     (\l -> case l of
>              Left x -> error $ show x
>              Right e -> e) $
>      updateCatalog defaultCatalog

    
>        [CatCreateScalarType "abstime", CatCreateScalarType "aclitem",
>         CatCreateScalarType "bit", CatCreateScalarType "bool",
>         CatCreateScalarType "box", CatCreateScalarType "bpchar",
>         CatCreateScalarType "bytea", CatCreateScalarType "char",
>         CatCreateScalarType "cid", CatCreateScalarType "cidr",
>         CatCreateScalarType "circle", CatCreateScalarType "date",
>         CatCreateScalarType "float4", CatCreateScalarType "float8",
>         CatCreateScalarType "gtsvector", CatCreateScalarType "inet",
>         CatCreateScalarType "int2", CatCreateScalarType "int2vector",
>         CatCreateScalarType "int4", CatCreateScalarType "int8",
>         CatCreateScalarType "interval", CatCreateScalarType "line",
>         CatCreateScalarType "lseg", CatCreateScalarType "macaddr",
>         CatCreateScalarType "money", CatCreateScalarType "name",
>         CatCreateScalarType "numeric", CatCreateScalarType "oid",
>         CatCreateScalarType "oidvector", CatCreateScalarType "path",
>         CatCreateScalarType "pg_node_tree", CatCreateScalarType "point",
>         CatCreateScalarType "polygon", CatCreateScalarType "refcursor",
>         CatCreateScalarType "regclass", CatCreateScalarType "regconfig",
>         CatCreateScalarType "regdictionary", CatCreateScalarType "regoper",
>         CatCreateScalarType "regoperator", CatCreateScalarType "regproc",
>         CatCreateScalarType "regprocedure", CatCreateScalarType "regtype",
>         CatCreateScalarType "reltime", CatCreateScalarType "smgr",
>         CatCreateScalarType "text", CatCreateScalarType "tid",
>         CatCreateScalarType "time", CatCreateScalarType "timestamp",
>         CatCreateScalarType "timestamptz", CatCreateScalarType "timetz",
>         CatCreateScalarType "tinterval", CatCreateScalarType "tsquery",
>         CatCreateScalarType "tsvector",
>         CatCreateScalarType "txid_snapshot", CatCreateScalarType "unknown",
>         CatCreateScalarType "uuid", CatCreateScalarType "varbit",
>         CatCreateScalarType "varchar", CatCreateScalarType "xid",
>         CatCreateScalarType "xml",
>         CatCreateDomainType "information_schema.cardinal_number" "int4",
>         CatCreateDomainType "information_schema.character_data" "varchar",
>         CatCreateDomainType "information_schema.sql_identifier" "varchar",
>         CatCreateDomainType "information_schema.time_stamp" "timestamptz",
>         CatCreateDomainType "information_schema.yes_or_no" "varchar",
>         CatCreateArrayType "_abstime" "abstime",
>         CatCreateArrayType "_aclitem" "aclitem",
>         CatCreateArrayType "_bit" "bit", CatCreateArrayType "_bool" "bool",
>         CatCreateArrayType "_box" "box",
>         CatCreateArrayType "_bpchar" "bpchar",
>         CatCreateArrayType "_bytea" "bytea",
>         CatCreateArrayType "_char" "char", CatCreateArrayType "_cid" "cid",
>         CatCreateArrayType "_cidr" "cidr",
>         CatCreateArrayType "_circle" "circle",
>         CatCreateArrayType "_cstring" "cstring",
>         CatCreateArrayType "_date" "date",
>         CatCreateArrayType "_float4" "float4",
>         CatCreateArrayType "_float8" "float8",
>         CatCreateArrayType "_gtsvector" "gtsvector",
>         CatCreateArrayType "_inet" "inet",
>         CatCreateArrayType "_int2" "int2",
>         CatCreateArrayType "_int2vector" "int2vector",
>         CatCreateArrayType "_int4" "int4",
>         CatCreateArrayType "_int8" "int8",
>         CatCreateArrayType "_interval" "interval",
>         CatCreateArrayType "_line" "line",
>         CatCreateArrayType "_lseg" "lseg",
>         CatCreateArrayType "_macaddr" "macaddr",
>         CatCreateArrayType "_money" "money",
>         CatCreateArrayType "_name" "name",
>         CatCreateArrayType "_numeric" "numeric",
>         CatCreateArrayType "_oid" "oid",
>         CatCreateArrayType "_oidvector" "oidvector",
>         CatCreateArrayType "_path" "path",
>         CatCreateArrayType "_point" "point",
>         CatCreateArrayType "_polygon" "polygon",
>         CatCreateArrayType "_record" "record",
>         CatCreateArrayType "_refcursor" "refcursor",
>         CatCreateArrayType "_regclass" "regclass",
>         CatCreateArrayType "_regconfig" "regconfig",
>         CatCreateArrayType "_regdictionary" "regdictionary",
>         CatCreateArrayType "_regoper" "regoper",
>         CatCreateArrayType "_regoperator" "regoperator",
>         CatCreateArrayType "_regproc" "regproc",
>         CatCreateArrayType "_regprocedure" "regprocedure",
>         CatCreateArrayType "_regtype" "regtype",
>         CatCreateArrayType "_reltime" "reltime",
>         CatCreateArrayType "_text" "text", CatCreateArrayType "_tid" "tid",
>         CatCreateArrayType "_time" "time",
>         CatCreateArrayType "_timestamp" "timestamp",
>         CatCreateArrayType "_timestamptz" "timestamptz",
>         CatCreateArrayType "_timetz" "timetz",
>         CatCreateArrayType "_tinterval" "tinterval",
>         CatCreateArrayType "_tsquery" "tsquery",
>         CatCreateArrayType "_tsvector" "tsvector",
>         CatCreateArrayType "_txid_snapshot" "txid_snapshot",
>         CatCreateArrayType "_uuid" "uuid",
>         CatCreateArrayType "_varbit" "varbit",
>         CatCreateArrayType "_varchar" "varchar",
>         CatCreateArrayType "_xid" "xid", CatCreateArrayType "_xml" "xml",
>         CatCreatePrefixOp "!!" "int8" "numeric",
>         CatCreatePrefixOp "!!" "tsquery" "tsquery",
>         CatCreatePrefixOp "!~" "text" "bool",
>         CatCreatePrefixOp "!~" "text" "bool",
>         CatCreatePrefixOp "!~" "text" "bool",
>         CatCreatePrefixOp "!~*" "text" "bool",
>         CatCreatePrefixOp "!~*" "text" "bool",
>         CatCreatePrefixOp "!~*" "text" "bool",
>         CatCreatePrefixOp "!~~" "text" "bool",
>         CatCreatePrefixOp "!~~" "bytea" "bool",
>         CatCreatePrefixOp "!~~" "text" "bool",
>         CatCreatePrefixOp "!~~" "text" "bool",
>         CatCreatePrefixOp "!~~*" "text" "bool",
>         CatCreatePrefixOp "!~~*" "text" "bool",
>         CatCreatePrefixOp "!~~*" "text" "bool",
>         CatCreatePrefixOp "#" "int8" "int8",
>         CatCreatePrefixOp "#" "polygon" "int4",
>         CatCreatePrefixOp "#" "lseg" "point",
>         CatCreatePrefixOp "#" "line" "point",
>         CatCreatePrefixOp "#" "path" "int4",
>         CatCreatePrefixOp "#" "bit" "bit",
>         CatCreatePrefixOp "#" "box" "box",
>         CatCreatePrefixOp "#" "int2" "int2",
>         CatCreatePrefixOp "#" "int4" "int4",
>         CatCreatePrefixOp "##" "lseg" "point",
>         CatCreatePrefixOp "##" "line" "point",
>         CatCreatePrefixOp "##" "box" "point",
>         CatCreatePrefixOp "##" "box" "point",
>         CatCreatePrefixOp "##" "lseg" "point",
>         CatCreatePrefixOp "##" "line" "point",
>         CatCreatePrefixOp "##" "box" "point",
>         CatCreatePrefixOp "##" "lseg" "point",
>         CatCreatePrefixOp "#<" "reltime" "bool",
>         CatCreatePrefixOp "#<=" "reltime" "bool",
>         CatCreatePrefixOp "#<>" "reltime" "bool",
>         CatCreatePrefixOp "#=" "reltime" "bool",
>         CatCreatePrefixOp "#>" "reltime" "bool",
>         CatCreatePrefixOp "#>=" "reltime" "bool",
>         CatCreatePrefixOp "%" "int2" "int2",
>         CatCreatePrefixOp "%" "int8" "int8",
>         CatCreatePrefixOp "%" "numeric" "numeric",
>         CatCreatePrefixOp "%" "int4" "int4",
>         CatCreatePrefixOp "&" "inet" "inet",
>         CatCreatePrefixOp "&" "int8" "int8",
>         CatCreatePrefixOp "&" "int4" "int4",
>         CatCreatePrefixOp "&" "int2" "int2",
>         CatCreatePrefixOp "&" "bit" "bit",
>         CatCreatePrefixOp "&&" "tsquery" "tsquery",
>         CatCreatePrefixOp "&&" "tinterval" "bool",
>         CatCreatePrefixOp "&&" "box" "bool",
>         CatCreatePrefixOp "&&" "circle" "bool",
>         CatCreatePrefixOp "&&" "anyarray" "bool",
>         CatCreatePrefixOp "&&" "polygon" "bool",
>         CatCreatePrefixOp "&<" "polygon" "bool",
>         CatCreatePrefixOp "&<" "circle" "bool",
>         CatCreatePrefixOp "&<" "box" "bool",
>         CatCreatePrefixOp "&<|" "polygon" "bool",
>         CatCreatePrefixOp "&<|" "circle" "bool",
>         CatCreatePrefixOp "&<|" "box" "bool",
>         CatCreatePrefixOp "&>" "polygon" "bool",
>         CatCreatePrefixOp "&>" "circle" "bool",
>         CatCreatePrefixOp "&>" "box" "bool",
>         CatCreatePrefixOp "*" "money" "money",
>         CatCreatePrefixOp "*" "money" "money",
>         CatCreatePrefixOp "*" "int2" "int2",
>         CatCreatePrefixOp "*" "float4" "float4",
>         CatCreatePrefixOp "*" "point" "path",
>         CatCreatePrefixOp "*" "int8" "int8",
>         CatCreatePrefixOp "*" "money" "money",
>         CatCreatePrefixOp "*" "numeric" "numeric",
>         CatCreatePrefixOp "*" "point" "circle",
>         CatCreatePrefixOp "*" "money" "money",
>         CatCreatePrefixOp "*" "float4" "money",
>         CatCreatePrefixOp "*" "point" "point",
>         CatCreatePrefixOp "*" "int2" "int8",
>         CatCreatePrefixOp "*" "float8" "money",
>         CatCreatePrefixOp "*" "int8" "int8",
>         CatCreatePrefixOp "*" "float8" "float8",
>         CatCreatePrefixOp "*" "interval" "interval",
>         CatCreatePrefixOp "*" "float8" "interval",
>         CatCreatePrefixOp "*" "int8" "int8",
>         CatCreatePrefixOp "*" "int4" "int4",
>         CatCreatePrefixOp "*" "float8" "float8",
>         CatCreatePrefixOp "*" "int4" "int4",
>         CatCreatePrefixOp "*" "int2" "int4",
>         CatCreatePrefixOp "*" "float4" "float8",
>         CatCreatePrefixOp "*" "point" "box",
>         CatCreatePrefixOp "*" "int4" "money",
>         CatCreatePrefixOp "*" "int4" "int8",
>         CatCreatePrefixOp "*" "int2" "money",
>         CatCreatePrefixOp "+" "date" "timestamp",
>         CatCreatePrefixOp "+" "point" "circle",
>         CatCreatePrefixOp "+" "int8" "int8",
>         CatCreatePrefixOp "+" "timetz" "timetz",
>         CatCreatePrefixOp "+" "int2" "int2",
>         CatCreatePrefixOp "+" "int4" "int4",
>         CatCreatePrefixOp "+" "timestamp" "timestamp",
>         CatCreatePrefixOp "+" "int2" "int8",
>         CatCreatePrefixOp "+" "time" "time",
>         CatCreatePrefixOp "+" "int8" "int8",
>         CatCreatePrefixOp "+" "interval" "timetz",
>         CatCreatePrefixOp "+" "float4" "float4",
>         CatCreatePrefixOp "+" "interval" "time",
>         CatCreatePrefixOp "+" "float8" "float8",
>         CatCreatePrefixOp "+" "reltime" "abstime",
>         CatCreatePrefixOp "+" "int4" "int8",
>         CatCreatePrefixOp "+" "numeric" "numeric",
>         CatCreatePrefixOp "+" "point" "box",
>         CatCreatePrefixOp "+" "numeric" "numeric",
>         CatCreatePrefixOp "+" "int8" "int8",
>         CatCreatePrefixOp "+" "money" "money",
>         CatCreatePrefixOp "+" "int8" "int8",
>         CatCreatePrefixOp "+" "aclitem" "_aclitem",
>         CatCreatePrefixOp "+" "inet" "inet",
>         CatCreatePrefixOp "+" "int8" "inet",
>         CatCreatePrefixOp "+" "interval" "timestamp",
>         CatCreatePrefixOp "+" "interval" "timestamp",
>         CatCreatePrefixOp "+" "int4" "date",
>         CatCreatePrefixOp "+" "float8" "float8",
>         CatCreatePrefixOp "+" "point" "point",
>         CatCreatePrefixOp "+" "interval" "timestamptz",
>         CatCreatePrefixOp "+" "path" "path",
>         CatCreatePrefixOp "+" "float4" "float8",
>         CatCreatePrefixOp "+" "interval" "interval",
>         CatCreatePrefixOp "+" "time" "timestamp",
>         CatCreatePrefixOp "+" "timetz" "timestamptz",
>         CatCreatePrefixOp "+" "date" "timestamp",
>         CatCreatePrefixOp "+" "point" "path",
>         CatCreatePrefixOp "+" "float8" "float8",
>         CatCreatePrefixOp "+" "int2" "int2",
>         CatCreatePrefixOp "+" "int4" "int4",
>         CatCreatePrefixOp "+" "int4" "int4",
>         CatCreatePrefixOp "+" "int2" "int4",
>         CatCreatePrefixOp "+" "date" "date",
>         CatCreatePrefixOp "+" "timestamptz" "timestamptz",
>         CatCreatePrefixOp "+" "date" "timestamptz",
>         CatCreatePrefixOp "+" "float4" "float4",
>         CatCreatePrefixOp "-" "money" "money",
>         CatCreatePrefixOp "-" "point" "box",
>         CatCreatePrefixOp "-" "interval" "timestamp",
>         CatCreatePrefixOp "-" "timestamp" "interval",
>         CatCreatePrefixOp "-" "int8" "int8",
>         CatCreatePrefixOp "-" "interval" "timetz",
>         CatCreatePrefixOp "-" "interval" "time",
>         CatCreatePrefixOp "-" "numeric" "numeric",
>         CatCreatePrefixOp "-" "numeric" "numeric",
>         CatCreatePrefixOp "-" "inet" "int8",
>         CatCreatePrefixOp "-" "int8" "inet",
>         CatCreatePrefixOp "-" "int2" "int2",
>         CatCreatePrefixOp "-" "int4" "int4",
>         CatCreatePrefixOp "-" "int4" "int4",
>         CatCreatePrefixOp "-" "int2" "int4",
>         CatCreatePrefixOp "-" "int4" "int4",
>         CatCreatePrefixOp "-" "int2" "int2",
>         CatCreatePrefixOp "-" "point" "circle",
>         CatCreatePrefixOp "-" "reltime" "abstime",
>         CatCreatePrefixOp "-" "time" "interval",
>         CatCreatePrefixOp "-" "float4" "float4",
>         CatCreatePrefixOp "-" "float8" "float8",
>         CatCreatePrefixOp "-" "float4" "float4",
>         CatCreatePrefixOp "-" "float8" "float8",
>         CatCreatePrefixOp "-" "interval" "interval",
>         CatCreatePrefixOp "-" "interval" "interval",
>         CatCreatePrefixOp "-" "interval" "timestamptz",
>         CatCreatePrefixOp "-" "timestamptz" "interval",
>         CatCreatePrefixOp "-" "float4" "float8",
>         CatCreatePrefixOp "-" "float8" "float8",
>         CatCreatePrefixOp "-" "int4" "date",
>         CatCreatePrefixOp "-" "date" "int4",
>         CatCreatePrefixOp "-" "interval" "timestamp",
>         CatCreatePrefixOp "-" "aclitem" "_aclitem",
>         CatCreatePrefixOp "-" "int8" "int8",
>         CatCreatePrefixOp "-" "int4" "int8",
>         CatCreatePrefixOp "-" "int8" "int8",
>         CatCreatePrefixOp "-" "int2" "int8",
>         CatCreatePrefixOp "-" "int8" "int8",
>         CatCreatePrefixOp "-" "point" "point",
>         CatCreatePrefixOp "-" "point" "path",
>         CatCreatePrefixOp "/" "numeric" "numeric",
>         CatCreatePrefixOp "/" "int8" "int8",
>         CatCreatePrefixOp "/" "int4" "int4",
>         CatCreatePrefixOp "/" "int4" "money",
>         CatCreatePrefixOp "/" "float8" "float8",
>         CatCreatePrefixOp "/" "money" "float8",
>         CatCreatePrefixOp "/" "int4" "int8",
>         CatCreatePrefixOp "/" "float4" "money",
>         CatCreatePrefixOp "/" "float8" "interval",
>         CatCreatePrefixOp "/" "point" "point",
>         CatCreatePrefixOp "/" "point" "box",
>         CatCreatePrefixOp "/" "int2" "int2",
>         CatCreatePrefixOp "/" "int8" "int8",
>         CatCreatePrefixOp "/" "point" "path",
>         CatCreatePrefixOp "/" "int2" "money",
>         CatCreatePrefixOp "/" "float4" "float8",
>         CatCreatePrefixOp "/" "int2" "int8",
>         CatCreatePrefixOp "/" "point" "circle",
>         CatCreatePrefixOp "/" "float8" "float8",
>         CatCreatePrefixOp "/" "float4" "float4",
>         CatCreatePrefixOp "/" "int4" "int4",
>         CatCreatePrefixOp "/" "float8" "money",
>         CatCreatePrefixOp "/" "int2" "int4",
>         CatCreatePrefixOp "/" "int8" "int8",
>         CatCreatePrefixOp "<" "tinterval" "bool",
>         CatCreatePrefixOp "<" "bit" "bool",
>         CatCreatePrefixOp "<" "box" "bool",
>         CatCreatePrefixOp "<" "varbit" "bool",
>         CatCreatePrefixOp "<" "int8" "bool",
>         CatCreatePrefixOp "<" "int2" "bool",
>         CatCreatePrefixOp "<" "path" "bool",
>         CatCreatePrefixOp "<" "int4" "bool",
>         CatCreatePrefixOp "<" "int8" "bool",
>         CatCreatePrefixOp "<" "bytea" "bool",
>         CatCreatePrefixOp "<" "tsvector" "bool",
>         CatCreatePrefixOp "<" "anyenum" "bool",
>         CatCreatePrefixOp "<" "uuid" "bool",
>         CatCreatePrefixOp "<" "timestamp" "bool",
>         CatCreatePrefixOp "<" "tid" "bool",
>         CatCreatePrefixOp "<" "timestamp" "bool",
>         CatCreatePrefixOp "<" "timestamptz" "bool",
>         CatCreatePrefixOp "<" "date" "bool",
>         CatCreatePrefixOp "<" "interval" "bool",
>         CatCreatePrefixOp "<" "date" "bool",
>         CatCreatePrefixOp "<" "money" "bool",
>         CatCreatePrefixOp "<" "oid" "bool",
>         CatCreatePrefixOp "<" "oidvector" "bool",
>         CatCreatePrefixOp "<" "timestamptz" "bool",
>         CatCreatePrefixOp "<" "int8" "bool",
>         CatCreatePrefixOp "<" "circle" "bool",
>         CatCreatePrefixOp "<" "reltime" "bool",
>         CatCreatePrefixOp "<" "tsquery" "bool",
>         CatCreatePrefixOp "<" "bool" "bool",
>         CatCreatePrefixOp "<" "float4" "bool",
>         CatCreatePrefixOp "<" "float4" "bool",
>         CatCreatePrefixOp "<" "timestamptz" "bool",
>         CatCreatePrefixOp "<" "char" "bool",
>         CatCreatePrefixOp "<" "int2" "bool",
>         CatCreatePrefixOp "<" "float8" "bool",
>         CatCreatePrefixOp "<" "int4" "bool",
>         CatCreatePrefixOp "<" "lseg" "bool",
>         CatCreatePrefixOp "<" "timetz" "bool",
>         CatCreatePrefixOp "<" "name" "bool",
>         CatCreatePrefixOp "<" "time" "bool",
>         CatCreatePrefixOp "<" "timestamp" "bool",
>         CatCreatePrefixOp "<" "text" "bool",
>         CatCreatePrefixOp "<" "int4" "bool",
>         CatCreatePrefixOp "<" "date" "bool",
>         CatCreatePrefixOp "<" "int2" "bool",
>         CatCreatePrefixOp "<" "macaddr" "bool",
>         CatCreatePrefixOp "<" "anyarray" "bool",
>         CatCreatePrefixOp "<" "float8" "bool",
>         CatCreatePrefixOp "<" "bpchar" "bool",
>         CatCreatePrefixOp "<" "inet" "bool",
>         CatCreatePrefixOp "<" "record" "bool",
>         CatCreatePrefixOp "<" "numeric" "bool",
>         CatCreatePrefixOp "<" "abstime" "bool",
>         CatCreatePrefixOp "<#>" "abstime" "tinterval",
>         CatCreatePrefixOp "<->" "box" "float8",
>         CatCreatePrefixOp "<->" "point" "float8",
>         CatCreatePrefixOp "<->" "line" "float8",
>         CatCreatePrefixOp "<->" "lseg" "float8",
>         CatCreatePrefixOp "<->" "box" "float8",
>         CatCreatePrefixOp "<->" "line" "float8",
>         CatCreatePrefixOp "<->" "path" "float8",
>         CatCreatePrefixOp "<->" "box" "float8",
>         CatCreatePrefixOp "<->" "path" "float8",
>         CatCreatePrefixOp "<->" "line" "float8",
>         CatCreatePrefixOp "<->" "lseg" "float8",
>         CatCreatePrefixOp "<->" "polygon" "float8",
>         CatCreatePrefixOp "<->" "circle" "float8",
>         CatCreatePrefixOp "<->" "circle" "float8",
>         CatCreatePrefixOp "<->" "polygon" "float8",
>         CatCreatePrefixOp "<->" "box" "float8",
>         CatCreatePrefixOp "<<" "point" "bool",
>         CatCreatePrefixOp "<<" "int4" "int4",
>         CatCreatePrefixOp "<<" "polygon" "bool",
>         CatCreatePrefixOp "<<" "int4" "int2",
>         CatCreatePrefixOp "<<" "box" "bool",
>         CatCreatePrefixOp "<<" "inet" "bool",
>         CatCreatePrefixOp "<<" "tinterval" "bool",
>         CatCreatePrefixOp "<<" "int4" "int8",
>         CatCreatePrefixOp "<<" "int4" "bit",
>         CatCreatePrefixOp "<<" "circle" "bool",
>         CatCreatePrefixOp "<<=" "inet" "bool",
>         CatCreatePrefixOp "<<|" "box" "bool",
>         CatCreatePrefixOp "<<|" "circle" "bool",
>         CatCreatePrefixOp "<<|" "polygon" "bool",
>         CatCreatePrefixOp "<=" "path" "bool",
>         CatCreatePrefixOp "<=" "float4" "bool",
>         CatCreatePrefixOp "<=" "int2" "bool",
>         CatCreatePrefixOp "<=" "interval" "bool",
>         CatCreatePrefixOp "<=" "int4" "bool",
>         CatCreatePrefixOp "<=" "timestamptz" "bool",
>         CatCreatePrefixOp "<=" "numeric" "bool",
>         CatCreatePrefixOp "<=" "uuid" "bool",
>         CatCreatePrefixOp "<=" "abstime" "bool",
>         CatCreatePrefixOp "<=" "bpchar" "bool",
>         CatCreatePrefixOp "<=" "date" "bool",
>         CatCreatePrefixOp "<=" "timestamp" "bool",
>         CatCreatePrefixOp "<=" "oidvector" "bool",
>         CatCreatePrefixOp "<=" "record" "bool",
>         CatCreatePrefixOp "<=" "float8" "bool",
>         CatCreatePrefixOp "<=" "tsvector" "bool",
>         CatCreatePrefixOp "<=" "timestamptz" "bool",
>         CatCreatePrefixOp "<=" "inet" "bool",
>         CatCreatePrefixOp "<=" "bool" "bool",
>         CatCreatePrefixOp "<=" "text" "bool",
>         CatCreatePrefixOp "<=" "lseg" "bool",
>         CatCreatePrefixOp "<=" "oid" "bool",
>         CatCreatePrefixOp "<=" "anyenum" "bool",
>         CatCreatePrefixOp "<=" "int4" "bool",
>         CatCreatePrefixOp "<=" "int2" "bool",
>         CatCreatePrefixOp "<=" "bytea" "bool",
>         CatCreatePrefixOp "<=" "int8" "bool",
>         CatCreatePrefixOp "<=" "timestamp" "bool",
>         CatCreatePrefixOp "<=" "int2" "bool",
>         CatCreatePrefixOp "<=" "int8" "bool",
>         CatCreatePrefixOp "<=" "reltime" "bool",
>         CatCreatePrefixOp "<=" "macaddr" "bool",
>         CatCreatePrefixOp "<=" "date" "bool",
>         CatCreatePrefixOp "<=" "name" "bool",
>         CatCreatePrefixOp "<=" "time" "bool",
>         CatCreatePrefixOp "<=" "tinterval" "bool",
>         CatCreatePrefixOp "<=" "anyarray" "bool",
>         CatCreatePrefixOp "<=" "bit" "bool",
>         CatCreatePrefixOp "<=" "money" "bool",
>         CatCreatePrefixOp "<=" "timetz" "bool",
>         CatCreatePrefixOp "<=" "box" "bool",
>         CatCreatePrefixOp "<=" "circle" "bool",
>         CatCreatePrefixOp "<=" "date" "bool",
>         CatCreatePrefixOp "<=" "varbit" "bool",
>         CatCreatePrefixOp "<=" "timestamptz" "bool",
>         CatCreatePrefixOp "<=" "tsquery" "bool",
>         CatCreatePrefixOp "<=" "int8" "bool",
>         CatCreatePrefixOp "<=" "float8" "bool",
>         CatCreatePrefixOp "<=" "char" "bool",
>         CatCreatePrefixOp "<=" "timestamp" "bool",
>         CatCreatePrefixOp "<=" "float4" "bool",
>         CatCreatePrefixOp "<=" "tid" "bool",
>         CatCreatePrefixOp "<=" "int4" "bool",
>         CatCreatePrefixOp "<>" "timestamptz" "bool",
>         CatCreatePrefixOp "<>" "timestamp" "bool",
>         CatCreatePrefixOp "<>" "tsvector" "bool",
>         CatCreatePrefixOp "<>" "reltime" "bool",
>         CatCreatePrefixOp "<>" "int8" "bool",
>         CatCreatePrefixOp "<>" "int8" "bool",
>         CatCreatePrefixOp "<>" "bytea" "bool",
>         CatCreatePrefixOp "<>" "date" "bool",
>         CatCreatePrefixOp "<>" "int4" "bool",
>         CatCreatePrefixOp "<>" "abstime" "bool",
>         CatCreatePrefixOp "<>" "money" "bool",
>         CatCreatePrefixOp "<>" "int2" "bool",
>         CatCreatePrefixOp "<>" "int4" "bool",
>         CatCreatePrefixOp "<>" "point" "bool",
>         CatCreatePrefixOp "<>" "lseg" "bool",
>         CatCreatePrefixOp "<>" "int2" "bool",
>         CatCreatePrefixOp "<>" "int2" "bool",
>         CatCreatePrefixOp "<>" "text" "bool",
>         CatCreatePrefixOp "<>" "anyenum" "bool",
>         CatCreatePrefixOp "<>" "macaddr" "bool",
>         CatCreatePrefixOp "<>" "int8" "bool",
>         CatCreatePrefixOp "<>" "bool" "bool",
>         CatCreatePrefixOp "<>" "time" "bool",
>         CatCreatePrefixOp "<>" "int4" "bool",
>         CatCreatePrefixOp "<>" "timetz" "bool",
>         CatCreatePrefixOp "<>" "name" "bool",
>         CatCreatePrefixOp "<>" "bit" "bool",
>         CatCreatePrefixOp "<>" "timestamp" "bool",
>         CatCreatePrefixOp "<>" "float8" "bool",
>         CatCreatePrefixOp "<>" "char" "bool",
>         CatCreatePrefixOp "<>" "numeric" "bool",
>         CatCreatePrefixOp "<>" "float4" "bool",
>         CatCreatePrefixOp "<>" "tsquery" "bool",
>         CatCreatePrefixOp "<>" "timestamptz" "bool",
>         CatCreatePrefixOp "<>" "float4" "bool",
>         CatCreatePrefixOp "<>" "date" "bool",
>         CatCreatePrefixOp "<>" "varbit" "bool",
>         CatCreatePrefixOp "<>" "uuid" "bool",
>         CatCreatePrefixOp "<>" "tinterval" "bool",
>         CatCreatePrefixOp "<>" "inet" "bool",
>         CatCreatePrefixOp "<>" "date" "bool",
>         CatCreatePrefixOp "<>" "oidvector" "bool",
>         CatCreatePrefixOp "<>" "bpchar" "bool",
>         CatCreatePrefixOp "<>" "interval" "bool",
>         CatCreatePrefixOp "<>" "oid" "bool",
>         CatCreatePrefixOp "<>" "timestamptz" "bool",
>         CatCreatePrefixOp "<>" "anyarray" "bool",
>         CatCreatePrefixOp "<>" "timestamp" "bool",
>         CatCreatePrefixOp "<>" "float8" "bool",
>         CatCreatePrefixOp "<>" "circle" "bool",
>         CatCreatePrefixOp "<>" "record" "bool",
>         CatCreatePrefixOp "<>" "tid" "bool",
>         CatCreatePrefixOp "<?>" "tinterval" "bool",
>         CatCreatePrefixOp "<@" "polygon" "bool",
>         CatCreatePrefixOp "<@" "box" "bool",
>         CatCreatePrefixOp "<@" "lseg" "bool",
>         CatCreatePrefixOp "<@" "anyarray" "bool",
>         CatCreatePrefixOp "<@" "line" "bool",
>         CatCreatePrefixOp "<@" "circle" "bool",
>         CatCreatePrefixOp "<@" "polygon" "bool",
>         CatCreatePrefixOp "<@" "box" "bool",
>         CatCreatePrefixOp "<@" "line" "bool",
>         CatCreatePrefixOp "<@" "circle" "bool",
>         CatCreatePrefixOp "<@" "box" "bool",
>         CatCreatePrefixOp "<@" "tsquery" "bool",
>         CatCreatePrefixOp "<@" "path" "bool",
>         CatCreatePrefixOp "<^" "point" "bool",
>         CatCreatePrefixOp "<^" "box" "bool",
>         CatCreatePrefixOp "=" "date" "bool",
>         CatCreatePrefixOp "=" "varbit" "bool",
>         CatCreatePrefixOp "=" "aclitem" "bool",
>         CatCreatePrefixOp "=" "anyenum" "bool",
>         CatCreatePrefixOp "=" "box" "bool",
>         CatCreatePrefixOp "=" "bit" "bool",
>         CatCreatePrefixOp "=" "numeric" "bool",
>         CatCreatePrefixOp "=" "bool" "bool",
>         CatCreatePrefixOp "=" "bpchar" "bool",
>         CatCreatePrefixOp "=" "tinterval" "bool",
>         CatCreatePrefixOp "=" "uuid" "bool",
>         CatCreatePrefixOp "=" "anyarray" "bool",
>         CatCreatePrefixOp "=" "float8" "bool",
>         CatCreatePrefixOp "=" "date" "bool",
>         CatCreatePrefixOp "=" "money" "bool",
>         CatCreatePrefixOp "=" "char" "bool",
>         CatCreatePrefixOp "=" "record" "bool",
>         CatCreatePrefixOp "=" "time" "bool",
>         CatCreatePrefixOp "=" "name" "bool",
>         CatCreatePrefixOp "=" "timetz" "bool",
>         CatCreatePrefixOp "=" "int2" "bool",
>         CatCreatePrefixOp "=" "int4" "bool",
>         CatCreatePrefixOp "=" "text" "bool",
>         CatCreatePrefixOp "=" "float8" "bool",
>         CatCreatePrefixOp "=" "timestamp" "bool",
>         CatCreatePrefixOp "=" "float4" "bool",
>         CatCreatePrefixOp "=" "timestamptz" "bool",
>         CatCreatePrefixOp "=" "float4" "bool",
>         CatCreatePrefixOp "=" "xid" "bool",
>         CatCreatePrefixOp "=" "int8" "bool",
>         CatCreatePrefixOp "=" "oidvector" "bool",
>         CatCreatePrefixOp "=" "inet" "bool",
>         CatCreatePrefixOp "=" "int4" "bool",
>         CatCreatePrefixOp "=" "timestamptz" "bool",
>         CatCreatePrefixOp "=" "path" "bool",
>         CatCreatePrefixOp "=" "date" "bool",
>         CatCreatePrefixOp "=" "interval" "bool",
>         CatCreatePrefixOp "=" "tsquery" "bool",
>         CatCreatePrefixOp "=" "oid" "bool",
>         CatCreatePrefixOp "=" "cid" "bool",
>         CatCreatePrefixOp "=" "timestamptz" "bool",
>         CatCreatePrefixOp "=" "int2vector" "bool",
>         CatCreatePrefixOp "=" "circle" "bool",
>         CatCreatePrefixOp "=" "timestamp" "bool",
>         CatCreatePrefixOp "=" "tid" "bool",
>         CatCreatePrefixOp "=" "reltime" "bool",
>         CatCreatePrefixOp "=" "timestamp" "bool",
>         CatCreatePrefixOp "=" "int8" "bool",
>         CatCreatePrefixOp "=" "bytea" "bool",
>         CatCreatePrefixOp "=" "lseg" "bool",
>         CatCreatePrefixOp "=" "tsvector" "bool",
>         CatCreatePrefixOp "=" "int4" "bool",
>         CatCreatePrefixOp "=" "abstime" "bool",
>         CatCreatePrefixOp "=" "int2" "bool",
>         CatCreatePrefixOp "=" "int4" "bool",
>         CatCreatePrefixOp "=" "int2" "bool",
>         CatCreatePrefixOp "=" "line" "bool",
>         CatCreatePrefixOp "=" "macaddr" "bool",
>         CatCreatePrefixOp "=" "int8" "bool",
>         CatCreatePrefixOp ">" "tinterval" "bool",
>         CatCreatePrefixOp ">" "money" "bool",
>         CatCreatePrefixOp ">" "path" "bool",
>         CatCreatePrefixOp ">" "float8" "bool",
>         CatCreatePrefixOp ">" "bpchar" "bool",
>         CatCreatePrefixOp ">" "anyarray" "bool",
>         CatCreatePrefixOp ">" "text" "bool",
>         CatCreatePrefixOp ">" "date" "bool",
>         CatCreatePrefixOp ">" "name" "bool",
>         CatCreatePrefixOp ">" "time" "bool",
>         CatCreatePrefixOp ">" "timetz" "bool",
>         CatCreatePrefixOp ">" "char" "bool",
>         CatCreatePrefixOp ">" "float8" "bool",
>         CatCreatePrefixOp ">" "float4" "bool",
>         CatCreatePrefixOp ">" "float4" "bool",
>         CatCreatePrefixOp ">" "oidvector" "bool",
>         CatCreatePrefixOp ">" "timestamptz" "bool",
>         CatCreatePrefixOp ">" "oid" "bool",
>         CatCreatePrefixOp ">" "interval" "bool",
>         CatCreatePrefixOp ">" "circle" "bool",
>         CatCreatePrefixOp ">" "reltime" "bool",
>         CatCreatePrefixOp ">" "record" "bool",
>         CatCreatePrefixOp ">" "abstime" "bool",
>         CatCreatePrefixOp ">" "int2" "bool",
>         CatCreatePrefixOp ">" "int4" "bool",
>         CatCreatePrefixOp ">" "lseg" "bool",
>         CatCreatePrefixOp ">" "macaddr" "bool",
>         CatCreatePrefixOp ">" "inet" "bool",
>         CatCreatePrefixOp ">" "int4" "bool",
>         CatCreatePrefixOp ">" "int2" "bool",
>         CatCreatePrefixOp ">" "numeric" "bool",
>         CatCreatePrefixOp ">" "bit" "bool",
>         CatCreatePrefixOp ">" "box" "bool",
>         CatCreatePrefixOp ">" "varbit" "bool",
>         CatCreatePrefixOp ">" "int8" "bool",
>         CatCreatePrefixOp ">" "int2" "bool",
>         CatCreatePrefixOp ">" "int4" "bool",
>         CatCreatePrefixOp ">" "int8" "bool",
>         CatCreatePrefixOp ">" "bytea" "bool",
>         CatCreatePrefixOp ">" "timestamp" "bool",
>         CatCreatePrefixOp ">" "tid" "bool",
>         CatCreatePrefixOp ">" "timestamp" "bool",
>         CatCreatePrefixOp ">" "timestamptz" "bool",
>         CatCreatePrefixOp ">" "date" "bool",
>         CatCreatePrefixOp ">" "date" "bool",
>         CatCreatePrefixOp ">" "timestamptz" "bool",
>         CatCreatePrefixOp ">" "timestamp" "bool",
>         CatCreatePrefixOp ">" "uuid" "bool",
>         CatCreatePrefixOp ">" "anyenum" "bool",
>         CatCreatePrefixOp ">" "bool" "bool",
>         CatCreatePrefixOp ">" "tsvector" "bool",
>         CatCreatePrefixOp ">" "tsquery" "bool",
>         CatCreatePrefixOp ">" "int8" "bool",
>         CatCreatePrefixOp ">=" "oid" "bool",
>         CatCreatePrefixOp ">=" "bytea" "bool",
>         CatCreatePrefixOp ">=" "tsvector" "bool",
>         CatCreatePrefixOp ">=" "record" "bool",
>         CatCreatePrefixOp ">=" "timestamp" "bool",
>         CatCreatePrefixOp ">=" "abstime" "bool",
>         CatCreatePrefixOp ">=" "anyenum" "bool",
>         CatCreatePrefixOp ">=" "tid" "bool",
>         CatCreatePrefixOp ">=" "text" "bool",
>         CatCreatePrefixOp ">=" "tinterval" "bool",
>         CatCreatePrefixOp ">=" "timestamp" "bool",
>         CatCreatePrefixOp ">=" "float8" "bool",
>         CatCreatePrefixOp ">=" "uuid" "bool",
>         CatCreatePrefixOp ">=" "anyarray" "bool",
>         CatCreatePrefixOp ">=" "bpchar" "bool",
>         CatCreatePrefixOp ">=" "money" "bool",
>         CatCreatePrefixOp ">=" "date" "bool",
>         CatCreatePrefixOp ">=" "oidvector" "bool",
>         CatCreatePrefixOp ">=" "reltime" "bool",
>         CatCreatePrefixOp ">=" "timestamptz" "bool",
>         CatCreatePrefixOp ">=" "circle" "bool",
>         CatCreatePrefixOp ">=" "timestamp" "bool",
>         CatCreatePrefixOp ">=" "box" "bool",
>         CatCreatePrefixOp ">=" "date" "bool",
>         CatCreatePrefixOp ">=" "interval" "bool",
>         CatCreatePrefixOp ">=" "timestamptz" "bool",
>         CatCreatePrefixOp ">=" "timetz" "bool",
>         CatCreatePrefixOp ">=" "char" "bool",
>         CatCreatePrefixOp ">=" "inet" "bool",
>         CatCreatePrefixOp ">=" "varbit" "bool",
>         CatCreatePrefixOp ">=" "int2" "bool",
>         CatCreatePrefixOp ">=" "macaddr" "bool",
>         CatCreatePrefixOp ">=" "int4" "bool",
>         CatCreatePrefixOp ">=" "int8" "bool",
>         CatCreatePrefixOp ">=" "float4" "bool",
>         CatCreatePrefixOp ">=" "int8" "bool",
>         CatCreatePrefixOp ">=" "time" "bool",
>         CatCreatePrefixOp ">=" "int2" "bool",
>         CatCreatePrefixOp ">=" "bit" "bool",
>         CatCreatePrefixOp ">=" "float4" "bool",
>         CatCreatePrefixOp ">=" "name" "bool",
>         CatCreatePrefixOp ">=" "lseg" "bool",
>         CatCreatePrefixOp ">=" "timestamptz" "bool",
>         CatCreatePrefixOp ">=" "numeric" "bool",
>         CatCreatePrefixOp ">=" "int4" "bool",
>         CatCreatePrefixOp ">=" "int4" "bool",
>         CatCreatePrefixOp ">=" "int2" "bool",
>         CatCreatePrefixOp ">=" "date" "bool",
>         CatCreatePrefixOp ">=" "int8" "bool",
>         CatCreatePrefixOp ">=" "float8" "bool",
>         CatCreatePrefixOp ">=" "path" "bool",
>         CatCreatePrefixOp ">=" "bool" "bool",
>         CatCreatePrefixOp ">=" "tsquery" "bool",
>         CatCreatePrefixOp ">>" "box" "bool",
>         CatCreatePrefixOp ">>" "inet" "bool",
>         CatCreatePrefixOp ">>" "int4" "bit",
>         CatCreatePrefixOp ">>" "point" "bool",
>         CatCreatePrefixOp ">>" "int4" "int2",
>         CatCreatePrefixOp ">>" "circle" "bool",
>         CatCreatePrefixOp ">>" "polygon" "bool",
>         CatCreatePrefixOp ">>" "int4" "int4",
>         CatCreatePrefixOp ">>" "int4" "int8",
>         CatCreatePrefixOp ">>=" "inet" "bool",
>         CatCreatePrefixOp ">^" "point" "bool",
>         CatCreatePrefixOp ">^" "box" "bool",
>         CatCreatePrefixOp "?#" "line" "bool",
>         CatCreatePrefixOp "?#" "path" "bool",
>         CatCreatePrefixOp "?#" "box" "bool",
>         CatCreatePrefixOp "?#" "box" "bool",
>         CatCreatePrefixOp "?#" "line" "bool",
>         CatCreatePrefixOp "?#" "lseg" "bool",
>         CatCreatePrefixOp "?#" "box" "bool",
>         CatCreatePrefixOp "?-" "lseg" "bool",
>         CatCreatePrefixOp "?-" "line" "bool",
>         CatCreatePrefixOp "?-" "point" "bool",
>         CatCreatePrefixOp "?-|" "line" "bool",
>         CatCreatePrefixOp "?-|" "lseg" "bool",
>         CatCreatePrefixOp "?|" "line" "bool",
>         CatCreatePrefixOp "?|" "lseg" "bool",
>         CatCreatePrefixOp "?|" "point" "bool",
>         CatCreatePrefixOp "?||" "line" "bool",
>         CatCreatePrefixOp "?||" "lseg" "bool",
>         CatCreatePrefixOp "@" "circle" "bool",
>         CatCreatePrefixOp "@" "float8" "float8",
>         CatCreatePrefixOp "@" "float4" "float4",
>         CatCreatePrefixOp "@" "polygon" "bool",
>         CatCreatePrefixOp "@" "circle" "bool",
>         CatCreatePrefixOp "@" "line" "bool",
>         CatCreatePrefixOp "@" "box" "bool",
>         CatCreatePrefixOp "@" "lseg" "bool",
>         CatCreatePrefixOp "@" "path" "bool",
>         CatCreatePrefixOp "@" "line" "bool",
>         CatCreatePrefixOp "@" "box" "bool",
>         CatCreatePrefixOp "@" "int2" "int2",
>         CatCreatePrefixOp "@" "int4" "int4",
>         CatCreatePrefixOp "@" "int8" "int8",
>         CatCreatePrefixOp "@" "polygon" "bool",
>         CatCreatePrefixOp "@" "box" "bool",
>         CatCreatePrefixOp "@" "numeric" "numeric",
>         CatCreatePrefixOp "@-@" "path" "float8",
>         CatCreatePrefixOp "@-@" "lseg" "float8",
>         CatCreatePrefixOp "@>" "box" "bool",
>         CatCreatePrefixOp "@>" "polygon" "bool",
>         CatCreatePrefixOp "@>" "point" "bool",
>         CatCreatePrefixOp "@>" "circle" "bool",
>         CatCreatePrefixOp "@>" "tsquery" "bool",
>         CatCreatePrefixOp "@>" "anyarray" "bool",
>         CatCreatePrefixOp "@>" "aclitem" "bool",
>         CatCreatePrefixOp "@>" "point" "bool",
>         CatCreatePrefixOp "@>" "point" "bool",
>         CatCreatePrefixOp "@>" "point" "bool",
>         CatCreatePrefixOp "@@" "circle" "point",
>         CatCreatePrefixOp "@@" "tsquery" "bool",
>         CatCreatePrefixOp "@@" "tsquery" "bool",
>         CatCreatePrefixOp "@@" "lseg" "point",
>         CatCreatePrefixOp "@@" "path" "point",
>         CatCreatePrefixOp "@@" "polygon" "point",
>         CatCreatePrefixOp "@@" "tsvector" "bool",
>         CatCreatePrefixOp "@@" "text" "bool",
>         CatCreatePrefixOp "@@" "box" "point",
>         CatCreatePrefixOp "@@@" "tsquery" "bool",
>         CatCreatePrefixOp "@@@" "tsvector" "bool",
>         CatCreatePrefixOp "^" "numeric" "numeric",
>         CatCreatePrefixOp "^" "float8" "float8",
>         CatCreatePrefixOp "|" "int4" "int4",
>         CatCreatePrefixOp "|" "tinterval" "abstime",
>         CatCreatePrefixOp "|" "int8" "int8",
>         CatCreatePrefixOp "|" "int2" "int2",
>         CatCreatePrefixOp "|" "bit" "bit",
>         CatCreatePrefixOp "|" "inet" "inet",
>         CatCreatePrefixOp "|&>" "polygon" "bool",
>         CatCreatePrefixOp "|&>" "circle" "bool",
>         CatCreatePrefixOp "|&>" "box" "bool",
>         CatCreatePrefixOp "|/" "float8" "float8",
>         CatCreatePrefixOp "|>>" "box" "bool",
>         CatCreatePrefixOp "|>>" "circle" "bool",
>         CatCreatePrefixOp "|>>" "polygon" "bool",
>         CatCreatePrefixOp "||" "anyarray" "anyarray",
>         CatCreatePrefixOp "||" "anynonarray" "text",
>         CatCreatePrefixOp "||" "tsquery" "tsquery",
>         CatCreatePrefixOp "||" "varbit" "varbit",
>         CatCreatePrefixOp "||" "bytea" "bytea",
>         CatCreatePrefixOp "||" "tsvector" "tsvector",
>         CatCreatePrefixOp "||" "anyelement" "anyarray",
>         CatCreatePrefixOp "||" "text" "text",
>         CatCreatePrefixOp "||" "anyarray" "anyarray",
>         CatCreatePrefixOp "||" "text" "text",
>         CatCreatePrefixOp "||/" "float8" "float8",
>         CatCreatePrefixOp "~" "int4" "int4",
>         CatCreatePrefixOp "~" "aclitem" "bool",
>         CatCreatePrefixOp "~" "circle" "bool",
>         CatCreatePrefixOp "~" "bit" "bit",
>         CatCreatePrefixOp "~" "text" "bool",
>         CatCreatePrefixOp "~" "text" "bool",
>         CatCreatePrefixOp "~" "inet" "inet",
>         CatCreatePrefixOp "~" "text" "bool",
>         CatCreatePrefixOp "~" "point" "bool",
>         CatCreatePrefixOp "~" "int2" "int2",
>         CatCreatePrefixOp "~" "point" "bool",
>         CatCreatePrefixOp "~" "polygon" "bool",
>         CatCreatePrefixOp "~" "int8" "int8",
>         CatCreatePrefixOp "~" "point" "bool",
>         CatCreatePrefixOp "~" "box" "bool",
>         CatCreatePrefixOp "~*" "text" "bool",
>         CatCreatePrefixOp "~*" "text" "bool",
>         CatCreatePrefixOp "~*" "text" "bool",
>         CatCreatePrefixOp "~<=~" "text" "bool",
>         CatCreatePrefixOp "~<=~" "bpchar" "bool",
>         CatCreatePrefixOp "~<~" "bpchar" "bool",
>         CatCreatePrefixOp "~<~" "text" "bool",
>         CatCreatePrefixOp "~=" "circle" "bool",
>         CatCreatePrefixOp "~=" "box" "bool",
>         CatCreatePrefixOp "~=" "point" "bool",
>         CatCreatePrefixOp "~=" "polygon" "bool",
>         CatCreatePrefixOp "~=" "tinterval" "bool",
>         CatCreatePrefixOp "~>=~" "text" "bool",
>         CatCreatePrefixOp "~>=~" "bpchar" "bool",
>         CatCreatePrefixOp "~>~" "text" "bool",
>         CatCreatePrefixOp "~>~" "bpchar" "bool",
>         CatCreatePrefixOp "~~" "text" "bool",
>         CatCreatePrefixOp "~~" "bytea" "bool",
>         CatCreatePrefixOp "~~" "text" "bool",
>         CatCreatePrefixOp "~~" "text" "bool",
>         CatCreatePrefixOp "~~*" "text" "bool",
>         CatCreatePrefixOp "~~*" "text" "bool",
>         CatCreatePrefixOp "~~*" "text" "bool",
>         CatCreatePostfixOp "!" "int8" "numeric",
>         CatCreatePostfixOp "!~" "bpchar" "bool",
>         CatCreatePostfixOp "!~" "text" "bool",
>         CatCreatePostfixOp "!~" "name" "bool",
>         CatCreatePostfixOp "!~*" "text" "bool",
>         CatCreatePostfixOp "!~*" "bpchar" "bool",
>         CatCreatePostfixOp "!~*" "name" "bool",
>         CatCreatePostfixOp "!~~" "text" "bool",
>         CatCreatePostfixOp "!~~" "name" "bool",
>         CatCreatePostfixOp "!~~" "bpchar" "bool",
>         CatCreatePostfixOp "!~~" "bytea" "bool",
>         CatCreatePostfixOp "!~~*" "bpchar" "bool",
>         CatCreatePostfixOp "!~~*" "name" "bool",
>         CatCreatePostfixOp "!~~*" "text" "bool",
>         CatCreatePostfixOp "#" "line" "point",
>         CatCreatePostfixOp "#" "bit" "bit",
>         CatCreatePostfixOp "#" "int2" "int2",
>         CatCreatePostfixOp "#" "int4" "int4",
>         CatCreatePostfixOp "#" "int8" "int8",
>         CatCreatePostfixOp "#" "box" "box",
>         CatCreatePostfixOp "#" "lseg" "point",
>         CatCreatePostfixOp "##" "line" "point",
>         CatCreatePostfixOp "##" "point" "point",
>         CatCreatePostfixOp "##" "point" "point",
>         CatCreatePostfixOp "##" "point" "point",
>         CatCreatePostfixOp "##" "lseg" "point",
>         CatCreatePostfixOp "##" "lseg" "point",
>         CatCreatePostfixOp "##" "line" "point",
>         CatCreatePostfixOp "##" "lseg" "point",
>         CatCreatePostfixOp "#<" "tinterval" "bool",
>         CatCreatePostfixOp "#<=" "tinterval" "bool",
>         CatCreatePostfixOp "#<>" "tinterval" "bool",
>         CatCreatePostfixOp "#=" "tinterval" "bool",
>         CatCreatePostfixOp "#>" "tinterval" "bool",
>         CatCreatePostfixOp "#>=" "tinterval" "bool",
>         CatCreatePostfixOp "%" "int4" "int4",
>         CatCreatePostfixOp "%" "int8" "int8",
>         CatCreatePostfixOp "%" "int2" "int2",
>         CatCreatePostfixOp "%" "numeric" "numeric",
>         CatCreatePostfixOp "&" "int8" "int8",
>         CatCreatePostfixOp "&" "int2" "int2",
>         CatCreatePostfixOp "&" "bit" "bit",
>         CatCreatePostfixOp "&" "inet" "inet",
>         CatCreatePostfixOp "&" "int4" "int4",
>         CatCreatePostfixOp "&&" "box" "bool",
>         CatCreatePostfixOp "&&" "polygon" "bool",
>         CatCreatePostfixOp "&&" "circle" "bool",
>         CatCreatePostfixOp "&&" "tsquery" "tsquery",
>         CatCreatePostfixOp "&&" "anyarray" "bool",
>         CatCreatePostfixOp "&&" "tinterval" "bool",
>         CatCreatePostfixOp "&<" "polygon" "bool",
>         CatCreatePostfixOp "&<" "circle" "bool",
>         CatCreatePostfixOp "&<" "box" "bool",
>         CatCreatePostfixOp "&<|" "circle" "bool",
>         CatCreatePostfixOp "&<|" "box" "bool",
>         CatCreatePostfixOp "&<|" "polygon" "bool",
>         CatCreatePostfixOp "&>" "polygon" "bool",
>         CatCreatePostfixOp "&>" "box" "bool",
>         CatCreatePostfixOp "&>" "circle" "bool",
>         CatCreatePostfixOp "*" "box" "box",
>         CatCreatePostfixOp "*" "int8" "int8",
>         CatCreatePostfixOp "*" "float8" "float8",
>         CatCreatePostfixOp "*" "float4" "float4",
>         CatCreatePostfixOp "*" "money" "money",
>         CatCreatePostfixOp "*" "int4" "int4",
>         CatCreatePostfixOp "*" "int4" "int8",
>         CatCreatePostfixOp "*" "int2" "money",
>         CatCreatePostfixOp "*" "int4" "money",
>         CatCreatePostfixOp "*" "int8" "int8",
>         CatCreatePostfixOp "*" "money" "money",
>         CatCreatePostfixOp "*" "int8" "int8",
>         CatCreatePostfixOp "*" "money" "money",
>         CatCreatePostfixOp "*" "int4" "int4",
>         CatCreatePostfixOp "*" "int2" "int4",
>         CatCreatePostfixOp "*" "int2" "int8",
>         CatCreatePostfixOp "*" "interval" "interval",
>         CatCreatePostfixOp "*" "float8" "interval",
>         CatCreatePostfixOp "*" "float8" "float8",
>         CatCreatePostfixOp "*" "path" "path",
>         CatCreatePostfixOp "*" "numeric" "numeric",
>         CatCreatePostfixOp "*" "money" "money",
>         CatCreatePostfixOp "*" "float8" "money",
>         CatCreatePostfixOp "*" "int2" "int2",
>         CatCreatePostfixOp "*" "point" "point",
>         CatCreatePostfixOp "*" "float4" "float8",
>         CatCreatePostfixOp "*" "circle" "circle",
>         CatCreatePostfixOp "*" "float4" "money",
>         CatCreatePostfixOp "+" "timetz" "timetz",
>         CatCreatePostfixOp "+" "path" "path",
>         CatCreatePostfixOp "+" "path" "path",
>         CatCreatePostfixOp "+" "interval" "interval",
>         CatCreatePostfixOp "+" "point" "point",
>         CatCreatePostfixOp "+" "float8" "float8",
>         CatCreatePostfixOp "+" "date" "timestamp",
>         CatCreatePostfixOp "+" "circle" "circle",
>         CatCreatePostfixOp "+" "date" "timestamptz",
>         CatCreatePostfixOp "+" "time" "timestamp",
>         CatCreatePostfixOp "+" "timestamp" "timestamp",
>         CatCreatePostfixOp "+" "date" "timestamp",
>         CatCreatePostfixOp "+" "int4" "int8",
>         CatCreatePostfixOp "+" "timetz" "timestamptz",
>         CatCreatePostfixOp "+" "_aclitem" "_aclitem",
>         CatCreatePostfixOp "+" "int8" "int8",
>         CatCreatePostfixOp "+" "timestamptz" "timestamptz",
>         CatCreatePostfixOp "+" "int2" "int2",
>         CatCreatePostfixOp "+" "int4" "int4",
>         CatCreatePostfixOp "+" "int2" "int4",
>         CatCreatePostfixOp "+" "int4" "int4",
>         CatCreatePostfixOp "+" "int4" "date",
>         CatCreatePostfixOp "+" "interval" "time",
>         CatCreatePostfixOp "+" "money" "money",
>         CatCreatePostfixOp "+" "int8" "int8",
>         CatCreatePostfixOp "+" "int8" "inet",
>         CatCreatePostfixOp "+" "abstime" "abstime",
>         CatCreatePostfixOp "+" "time" "time",
>         CatCreatePostfixOp "+" "float4" "float4",
>         CatCreatePostfixOp "+" "float8" "float8",
>         CatCreatePostfixOp "+" "float4" "float8",
>         CatCreatePostfixOp "+" "interval" "timestamp",
>         CatCreatePostfixOp "+" "interval" "timestamptz",
>         CatCreatePostfixOp "+" "date" "date",
>         CatCreatePostfixOp "+" "interval" "timestamp",
>         CatCreatePostfixOp "+" "interval" "timetz",
>         CatCreatePostfixOp "+" "box" "box",
>         CatCreatePostfixOp "+" "numeric" "numeric",
>         CatCreatePostfixOp "+" "inet" "inet",
>         CatCreatePostfixOp "+" "int8" "int8",
>         CatCreatePostfixOp "+" "int2" "int8",
>         CatCreatePostfixOp "-" "int8" "int8",
>         CatCreatePostfixOp "-" "float8" "float8",
>         CatCreatePostfixOp "-" "timestamptz" "timestamptz",
>         CatCreatePostfixOp "-" "inet" "int8",
>         CatCreatePostfixOp "-" "date" "int4",
>         CatCreatePostfixOp "-" "timestamptz" "interval",
>         CatCreatePostfixOp "-" "inet" "inet",
>         CatCreatePostfixOp "-" "int2" "int2",
>         CatCreatePostfixOp "-" "int4" "int4",
>         CatCreatePostfixOp "-" "int2" "int4",
>         CatCreatePostfixOp "-" "int4" "int4",
>         CatCreatePostfixOp "-" "time" "interval",
>         CatCreatePostfixOp "-" "int4" "int8",
>         CatCreatePostfixOp "-" "timestamp" "interval",
>         CatCreatePostfixOp "-" "point" "point",
>         CatCreatePostfixOp "-" "money" "money",
>         CatCreatePostfixOp "-" "numeric" "numeric",
>         CatCreatePostfixOp "-" "float4" "float8",
>         CatCreatePostfixOp "-" "int8" "int8",
>         CatCreatePostfixOp "-" "path" "path",
>         CatCreatePostfixOp "-" "timetz" "timetz",
>         CatCreatePostfixOp "-" "box" "box",
>         CatCreatePostfixOp "-" "abstime" "abstime",
>         CatCreatePostfixOp "-" "time" "time",
>         CatCreatePostfixOp "-" "int2" "int8",
>         CatCreatePostfixOp "-" "timestamp" "timestamp",
>         CatCreatePostfixOp "-" "circle" "circle",
>         CatCreatePostfixOp "-" "date" "date",
>         CatCreatePostfixOp "-" "float4" "float4",
>         CatCreatePostfixOp "-" "int8" "int8",
>         CatCreatePostfixOp "-" "interval" "interval",
>         CatCreatePostfixOp "-" "date" "timestamp",
>         CatCreatePostfixOp "-" "float8" "float8",
>         CatCreatePostfixOp "-" "_aclitem" "_aclitem",
>         CatCreatePostfixOp "/" "int4" "int4",
>         CatCreatePostfixOp "/" "int4" "int4",
>         CatCreatePostfixOp "/" "int2" "int2",
>         CatCreatePostfixOp "/" "money" "float8",
>         CatCreatePostfixOp "/" "int8" "int8",
>         CatCreatePostfixOp "/" "float4" "float4",
>         CatCreatePostfixOp "/" "float8" "float8",
>         CatCreatePostfixOp "/" "circle" "circle",
>         CatCreatePostfixOp "/" "interval" "interval",
>         CatCreatePostfixOp "/" "int2" "int8",
>         CatCreatePostfixOp "/" "money" "money",
>         CatCreatePostfixOp "/" "float4" "float8",
>         CatCreatePostfixOp "/" "path" "path",
>         CatCreatePostfixOp "/" "float8" "float8",
>         CatCreatePostfixOp "/" "int8" "int8",
>         CatCreatePostfixOp "/" "int8" "int8",
>         CatCreatePostfixOp "/" "int2" "int4",
>         CatCreatePostfixOp "/" "money" "money",
>         CatCreatePostfixOp "/" "box" "box",
>         CatCreatePostfixOp "/" "point" "point",
>         CatCreatePostfixOp "/" "money" "money",
>         CatCreatePostfixOp "/" "numeric" "numeric",
>         CatCreatePostfixOp "/" "int4" "int8",
>         CatCreatePostfixOp "/" "money" "money",
>         CatCreatePostfixOp "<" "varbit" "bool",
>         CatCreatePostfixOp "<" "float4" "bool",
>         CatCreatePostfixOp "<" "float8" "bool",
>         CatCreatePostfixOp "<" "interval" "bool",
>         CatCreatePostfixOp "<" "timestamptz" "bool",
>         CatCreatePostfixOp "<" "tid" "bool",
>         CatCreatePostfixOp "<" "int8" "bool",
>         CatCreatePostfixOp "<" "timestamptz" "bool",
>         CatCreatePostfixOp "<" "record" "bool",
>         CatCreatePostfixOp "<" "int8" "bool",
>         CatCreatePostfixOp "<" "timestamp" "bool",
>         CatCreatePostfixOp "<" "timestamptz" "bool",
>         CatCreatePostfixOp "<" "timestamp" "bool",
>         CatCreatePostfixOp "<" "date" "bool",
>         CatCreatePostfixOp "<" "date" "bool",
>         CatCreatePostfixOp "<" "box" "bool",
>         CatCreatePostfixOp "<" "timestamp" "bool",
>         CatCreatePostfixOp "<" "bytea" "bool",
>         CatCreatePostfixOp "<" "bool" "bool",
>         CatCreatePostfixOp "<" "int4" "bool",
>         CatCreatePostfixOp "<" "int2" "bool",
>         CatCreatePostfixOp "<" "int4" "bool",
>         CatCreatePostfixOp "<" "int8" "bool",
>         CatCreatePostfixOp "<" "int2" "bool",
>         CatCreatePostfixOp "<" "tsquery" "bool",
>         CatCreatePostfixOp "<" "abstime" "bool",
>         CatCreatePostfixOp "<" "reltime" "bool",
>         CatCreatePostfixOp "<" "tsvector" "bool",
>         CatCreatePostfixOp "<" "oid" "bool",
>         CatCreatePostfixOp "<" "oidvector" "bool",
>         CatCreatePostfixOp "<" "bit" "bool",
>         CatCreatePostfixOp "<" "int2" "bool",
>         CatCreatePostfixOp "<" "float4" "bool",
>         CatCreatePostfixOp "<" "anyenum" "bool",
>         CatCreatePostfixOp "<" "char" "bool",
>         CatCreatePostfixOp "<" "numeric" "bool",
>         CatCreatePostfixOp "<" "int4" "bool",
>         CatCreatePostfixOp "<" "name" "bool",
>         CatCreatePostfixOp "<" "text" "bool",
>         CatCreatePostfixOp "<" "float8" "bool",
>         CatCreatePostfixOp "<" "uuid" "bool",
>         CatCreatePostfixOp "<" "inet" "bool",
>         CatCreatePostfixOp "<" "macaddr" "bool",
>         CatCreatePostfixOp "<" "path" "bool",
>         CatCreatePostfixOp "<" "lseg" "bool",
>         CatCreatePostfixOp "<" "tinterval" "bool",
>         CatCreatePostfixOp "<" "money" "bool",
>         CatCreatePostfixOp "<" "bpchar" "bool",
>         CatCreatePostfixOp "<" "anyarray" "bool",
>         CatCreatePostfixOp "<" "date" "bool",
>         CatCreatePostfixOp "<" "time" "bool",
>         CatCreatePostfixOp "<" "circle" "bool",
>         CatCreatePostfixOp "<" "timetz" "bool",
>         CatCreatePostfixOp "<#>" "abstime" "tinterval",
>         CatCreatePostfixOp "<->" "lseg" "float8",
>         CatCreatePostfixOp "<->" "lseg" "float8",
>         CatCreatePostfixOp "<->" "point" "float8",
>         CatCreatePostfixOp "<->" "point" "float8",
>         CatCreatePostfixOp "<->" "point" "float8",
>         CatCreatePostfixOp "<->" "line" "float8",
>         CatCreatePostfixOp "<->" "point" "float8",
>         CatCreatePostfixOp "<->" "circle" "float8",
>         CatCreatePostfixOp "<->" "point" "float8",
>         CatCreatePostfixOp "<->" "circle" "float8",
>         CatCreatePostfixOp "<->" "point" "float8",
>         CatCreatePostfixOp "<->" "polygon" "float8",
>         CatCreatePostfixOp "<->" "lseg" "float8",
>         CatCreatePostfixOp "<->" "line" "float8",
>         CatCreatePostfixOp "<->" "path" "float8",
>         CatCreatePostfixOp "<->" "box" "float8",
>         CatCreatePostfixOp "<<" "tinterval" "bool",
>         CatCreatePostfixOp "<<" "int4" "int4",
>         CatCreatePostfixOp "<<" "int8" "int8",
>         CatCreatePostfixOp "<<" "circle" "bool",
>         CatCreatePostfixOp "<<" "inet" "bool",
>         CatCreatePostfixOp "<<" "int2" "int2",
>         CatCreatePostfixOp "<<" "box" "bool",
>         CatCreatePostfixOp "<<" "point" "bool",
>         CatCreatePostfixOp "<<" "bit" "bit",
>         CatCreatePostfixOp "<<" "polygon" "bool",
>         CatCreatePostfixOp "<<=" "inet" "bool",
>         CatCreatePostfixOp "<<|" "circle" "bool",
>         CatCreatePostfixOp "<<|" "box" "bool",
>         CatCreatePostfixOp "<<|" "polygon" "bool",
>         CatCreatePostfixOp "<=" "timestamp" "bool",
>         CatCreatePostfixOp "<=" "bpchar" "bool",
>         CatCreatePostfixOp "<=" "box" "bool",
>         CatCreatePostfixOp "<=" "date" "bool",
>         CatCreatePostfixOp "<=" "date" "bool",
>         CatCreatePostfixOp "<=" "float4" "bool",
>         CatCreatePostfixOp "<=" "anyarray" "bool",
>         CatCreatePostfixOp "<=" "timestamp" "bool",
>         CatCreatePostfixOp "<=" "timestamptz" "bool",
>         CatCreatePostfixOp "<=" "int8" "bool",
>         CatCreatePostfixOp "<=" "timetz" "bool",
>         CatCreatePostfixOp "<=" "date" "bool",
>         CatCreatePostfixOp "<=" "timestamp" "bool",
>         CatCreatePostfixOp "<=" "timestamptz" "bool",
>         CatCreatePostfixOp "<=" "int8" "bool",
>         CatCreatePostfixOp "<=" "int4" "bool",
>         CatCreatePostfixOp "<=" "circle" "bool",
>         CatCreatePostfixOp "<=" "inet" "bool",
>         CatCreatePostfixOp "<=" "tid" "bool",
>         CatCreatePostfixOp "<=" "macaddr" "bool",
>         CatCreatePostfixOp "<=" "record" "bool",
>         CatCreatePostfixOp "<=" "lseg" "bool",
>         CatCreatePostfixOp "<=" "time" "bool",
>         CatCreatePostfixOp "<=" "path" "bool",
>         CatCreatePostfixOp "<=" "interval" "bool",
>         CatCreatePostfixOp "<=" "float8" "bool",
>         CatCreatePostfixOp "<=" "uuid" "bool",
>         CatCreatePostfixOp "<=" "text" "bool",
>         CatCreatePostfixOp "<=" "name" "bool",
>         CatCreatePostfixOp "<=" "numeric" "bool",
>         CatCreatePostfixOp "<=" "char" "bool",
>         CatCreatePostfixOp "<=" "float4" "bool",
>         CatCreatePostfixOp "<=" "timestamptz" "bool",
>         CatCreatePostfixOp "<=" "oidvector" "bool",
>         CatCreatePostfixOp "<=" "tinterval" "bool",
>         CatCreatePostfixOp "<=" "bit" "bool",
>         CatCreatePostfixOp "<=" "anyenum" "bool",
>         CatCreatePostfixOp "<=" "oid" "bool",
>         CatCreatePostfixOp "<=" "float8" "bool",
>         CatCreatePostfixOp "<=" "reltime" "bool",
>         CatCreatePostfixOp "<=" "money" "bool",
>         CatCreatePostfixOp "<=" "tsvector" "bool",
>         CatCreatePostfixOp "<=" "varbit" "bool",
>         CatCreatePostfixOp "<=" "abstime" "bool",
>         CatCreatePostfixOp "<=" "int2" "bool",
>         CatCreatePostfixOp "<=" "int4" "bool",
>         CatCreatePostfixOp "<=" "int2" "bool",
>         CatCreatePostfixOp "<=" "bool" "bool",
>         CatCreatePostfixOp "<=" "tsquery" "bool",
>         CatCreatePostfixOp "<=" "int8" "bool",
>         CatCreatePostfixOp "<=" "int4" "bool",
>         CatCreatePostfixOp "<=" "int2" "bool",
>         CatCreatePostfixOp "<=" "bytea" "bool",
>         CatCreatePostfixOp "<>" "int2" "bool",
>         CatCreatePostfixOp "<>" "int4" "bool",
>         CatCreatePostfixOp "<>" "bool" "bool",
>         CatCreatePostfixOp "<>" "tid" "bool",
>         CatCreatePostfixOp "<>" "int8" "bool",
>         CatCreatePostfixOp "<>" "int8" "bool",
>         CatCreatePostfixOp "<>" "int4" "bool",
>         CatCreatePostfixOp "<>" "int2" "bool",
>         CatCreatePostfixOp "<>" "text" "bool",
>         CatCreatePostfixOp "<>" "int2" "bool",
>         CatCreatePostfixOp "<>" "int4" "bool",
>         CatCreatePostfixOp "<>" "abstime" "bool",
>         CatCreatePostfixOp "<>" "reltime" "bool",
>         CatCreatePostfixOp "<>" "oid" "bool",
>         CatCreatePostfixOp "<>" "oidvector" "bool",
>         CatCreatePostfixOp "<>" "float4" "bool",
>         CatCreatePostfixOp "<>" "char" "bool",
>         CatCreatePostfixOp "<>" "name" "bool",
>         CatCreatePostfixOp "<>" "float8" "bool",
>         CatCreatePostfixOp "<>" "point" "bool",
>         CatCreatePostfixOp "<>" "tinterval" "bool",
>         CatCreatePostfixOp "<>" "money" "bool",
>         CatCreatePostfixOp "<>" "bpchar" "bool",
>         CatCreatePostfixOp "<>" "anyarray" "bool",
>         CatCreatePostfixOp "<>" "date" "bool",
>         CatCreatePostfixOp "<>" "time" "bool",
>         CatCreatePostfixOp "<>" "timetz" "bool",
>         CatCreatePostfixOp "<>" "float4" "bool",
>         CatCreatePostfixOp "<>" "float8" "bool",
>         CatCreatePostfixOp "<>" "timestamptz" "bool",
>         CatCreatePostfixOp "<>" "interval" "bool",
>         CatCreatePostfixOp "<>" "circle" "bool",
>         CatCreatePostfixOp "<>" "lseg" "bool",
>         CatCreatePostfixOp "<>" "macaddr" "bool",
>         CatCreatePostfixOp "<>" "inet" "bool",
>         CatCreatePostfixOp "<>" "numeric" "bool",
>         CatCreatePostfixOp "<>" "bit" "bool",
>         CatCreatePostfixOp "<>" "varbit" "bool",
>         CatCreatePostfixOp "<>" "int8" "bool",
>         CatCreatePostfixOp "<>" "bytea" "bool",
>         CatCreatePostfixOp "<>" "timestamp" "bool",
>         CatCreatePostfixOp "<>" "date" "bool",
>         CatCreatePostfixOp "<>" "date" "bool",
>         CatCreatePostfixOp "<>" "timestamp" "bool",
>         CatCreatePostfixOp "<>" "timestamptz" "bool",
>         CatCreatePostfixOp "<>" "timestamp" "bool",
>         CatCreatePostfixOp "<>" "timestamptz" "bool",
>         CatCreatePostfixOp "<>" "uuid" "bool",
>         CatCreatePostfixOp "<>" "anyenum" "bool",
>         CatCreatePostfixOp "<>" "tsvector" "bool",
>         CatCreatePostfixOp "<>" "tsquery" "bool",
>         CatCreatePostfixOp "<>" "record" "bool",
>         CatCreatePostfixOp "<?>" "abstime" "bool",
>         CatCreatePostfixOp "<@" "point" "bool",
>         CatCreatePostfixOp "<@" "box" "bool",
>         CatCreatePostfixOp "<@" "point" "bool",
>         CatCreatePostfixOp "<@" "point" "bool",
>         CatCreatePostfixOp "<@" "point" "bool",
>         CatCreatePostfixOp "<@" "circle" "bool",
>         CatCreatePostfixOp "<@" "tsquery" "bool",
>         CatCreatePostfixOp "<@" "lseg" "bool",
>         CatCreatePostfixOp "<@" "lseg" "bool",
>         CatCreatePostfixOp "<@" "point" "bool",
>         CatCreatePostfixOp "<@" "polygon" "bool",
>         CatCreatePostfixOp "<@" "point" "bool",
>         CatCreatePostfixOp "<@" "anyarray" "bool",
>         CatCreatePostfixOp "<^" "point" "bool",
>         CatCreatePostfixOp "<^" "box" "bool",
>         CatCreatePostfixOp "=" "int8" "bool",
>         CatCreatePostfixOp "=" "lseg" "bool",
>         CatCreatePostfixOp "=" "int4" "bool",
>         CatCreatePostfixOp "=" "int2" "bool",
>         CatCreatePostfixOp "=" "time" "bool",
>         CatCreatePostfixOp "=" "timestamp" "bool",
>         CatCreatePostfixOp "=" "date" "bool",
>         CatCreatePostfixOp "=" "bytea" "bool",
>         CatCreatePostfixOp "=" "date" "bool",
>         CatCreatePostfixOp "=" "tsvector" "bool",
>         CatCreatePostfixOp "=" "box" "bool",
>         CatCreatePostfixOp "=" "timestamp" "bool",
>         CatCreatePostfixOp "=" "char" "bool",
>         CatCreatePostfixOp "=" "money" "bool",
>         CatCreatePostfixOp "=" "circle" "bool",
>         CatCreatePostfixOp "=" "tinterval" "bool",
>         CatCreatePostfixOp "=" "name" "bool",
>         CatCreatePostfixOp "=" "int2" "bool",
>         CatCreatePostfixOp "=" "int4" "bool",
>         CatCreatePostfixOp "=" "anyenum" "bool",
>         CatCreatePostfixOp "=" "text" "bool",
>         CatCreatePostfixOp "=" "timetz" "bool",
>         CatCreatePostfixOp "=" "uuid" "bool",
>         CatCreatePostfixOp "=" "line" "bool",
>         CatCreatePostfixOp "=" "path" "bool",
>         CatCreatePostfixOp "=" "macaddr" "bool",
>         CatCreatePostfixOp "=" "xid" "bool",
>         CatCreatePostfixOp "=" "xid" "bool",
>         CatCreatePostfixOp "=" "date" "bool",
>         CatCreatePostfixOp "=" "inet" "bool",
>         CatCreatePostfixOp "=" "cid" "bool",
>         CatCreatePostfixOp "=" "int2vector" "bool",
>         CatCreatePostfixOp "=" "float4" "bool",
>         CatCreatePostfixOp "=" "tid" "bool",
>         CatCreatePostfixOp "=" "int8" "bool",
>         CatCreatePostfixOp "=" "float8" "bool",
>         CatCreatePostfixOp "=" "record" "bool",
>         CatCreatePostfixOp "=" "timestamptz" "bool",
>         CatCreatePostfixOp "=" "int8" "bool",
>         CatCreatePostfixOp "=" "numeric" "bool",
>         CatCreatePostfixOp "=" "bool" "bool",
>         CatCreatePostfixOp "=" "anyarray" "bool",
>         CatCreatePostfixOp "=" "float4" "bool",
>         CatCreatePostfixOp "=" "oidvector" "bool",
>         CatCreatePostfixOp "=" "timestamp" "bool",
>         CatCreatePostfixOp "=" "bit" "bool",
>         CatCreatePostfixOp "=" "tsquery" "bool",
>         CatCreatePostfixOp "=" "float8" "bool",
>         CatCreatePostfixOp "=" "oid" "bool",
>         CatCreatePostfixOp "=" "timestamptz" "bool",
>         CatCreatePostfixOp "=" "bpchar" "bool",
>         CatCreatePostfixOp "=" "varbit" "bool",
>         CatCreatePostfixOp "=" "reltime" "bool",
>         CatCreatePostfixOp "=" "aclitem" "bool",
>         CatCreatePostfixOp "=" "int2" "bool",
>         CatCreatePostfixOp "=" "int4" "bool",
>         CatCreatePostfixOp "=" "abstime" "bool",
>         CatCreatePostfixOp "=" "interval" "bool",
>         CatCreatePostfixOp "=" "timestamptz" "bool",
>         CatCreatePostfixOp ">" "bpchar" "bool",
>         CatCreatePostfixOp ">" "timestamptz" "bool",
>         CatCreatePostfixOp ">" "float4" "bool",
>         CatCreatePostfixOp ">" "text" "bool",
>         CatCreatePostfixOp ">" "box" "bool",
>         CatCreatePostfixOp ">" "bytea" "bool",
>         CatCreatePostfixOp ">" "time" "bool",
>         CatCreatePostfixOp ">" "int8" "bool",
>         CatCreatePostfixOp ">" "bool" "bool",
>         CatCreatePostfixOp ">" "timestamp" "bool",
>         CatCreatePostfixOp ">" "name" "bool",
>         CatCreatePostfixOp ">" "tsquery" "bool",
>         CatCreatePostfixOp ">" "date" "bool",
>         CatCreatePostfixOp ">" "int2" "bool",
>         CatCreatePostfixOp ">" "date" "bool",
>         CatCreatePostfixOp ">" "money" "bool",
>         CatCreatePostfixOp ">" "int8" "bool",
>         CatCreatePostfixOp ">" "timestamptz" "bool",
>         CatCreatePostfixOp ">" "tinterval" "bool",
>         CatCreatePostfixOp ">" "oid" "bool",
>         CatCreatePostfixOp ">" "varbit" "bool",
>         CatCreatePostfixOp ">" "timestamptz" "bool",
>         CatCreatePostfixOp ">" "anyenum" "bool",
>         CatCreatePostfixOp ">" "int4" "bool",
>         CatCreatePostfixOp ">" "int2" "bool",
>         CatCreatePostfixOp ">" "timestamp" "bool",
>         CatCreatePostfixOp ">" "interval" "bool",
>         CatCreatePostfixOp ">" "numeric" "bool",
>         CatCreatePostfixOp ">" "path" "bool",
>         CatCreatePostfixOp ">" "lseg" "bool",
>         CatCreatePostfixOp ">" "date" "bool",
>         CatCreatePostfixOp ">" "uuid" "bool",
>         CatCreatePostfixOp ">" "anyarray" "bool",
>         CatCreatePostfixOp ">" "record" "bool",
>         CatCreatePostfixOp ">" "char" "bool",
>         CatCreatePostfixOp ">" "int8" "bool",
>         CatCreatePostfixOp ">" "float4" "bool",
>         CatCreatePostfixOp ">" "tsvector" "bool",
>         CatCreatePostfixOp ">" "float8" "bool",
>         CatCreatePostfixOp ">" "timestamp" "bool",
>         CatCreatePostfixOp ">" "macaddr" "bool",
>         CatCreatePostfixOp ">" "reltime" "bool",
>         CatCreatePostfixOp ">" "abstime" "bool",
>         CatCreatePostfixOp ">" "timetz" "bool",
>         CatCreatePostfixOp ">" "int4" "bool",
>         CatCreatePostfixOp ">" "int2" "bool",
>         CatCreatePostfixOp ">" "inet" "bool",
>         CatCreatePostfixOp ">" "int4" "bool",
>         CatCreatePostfixOp ">" "circle" "bool",
>         CatCreatePostfixOp ">" "bit" "bool",
>         CatCreatePostfixOp ">" "oidvector" "bool",
>         CatCreatePostfixOp ">" "tid" "bool",
>         CatCreatePostfixOp ">" "float8" "bool",
>         CatCreatePostfixOp ">=" "time" "bool",
>         CatCreatePostfixOp ">=" "circle" "bool",
>         CatCreatePostfixOp ">=" "date" "bool",
>         CatCreatePostfixOp ">=" "anyarray" "bool",
>         CatCreatePostfixOp ">=" "bpchar" "bool",
>         CatCreatePostfixOp ">=" "money" "bool",
>         CatCreatePostfixOp ">=" "tinterval" "bool",
>         CatCreatePostfixOp ">=" "path" "bool",
>         CatCreatePostfixOp ">=" "lseg" "bool",
>         CatCreatePostfixOp ">=" "macaddr" "bool",
>         CatCreatePostfixOp ">=" "bool" "bool",
>         CatCreatePostfixOp ">=" "inet" "bool",
>         CatCreatePostfixOp ">=" "float8" "bool",
>         CatCreatePostfixOp ">=" "text" "bool",
>         CatCreatePostfixOp ">=" "name" "bool",
>         CatCreatePostfixOp ">=" "char" "bool",
>         CatCreatePostfixOp ">=" "numeric" "bool",
>         CatCreatePostfixOp ">=" "float4" "bool",
>         CatCreatePostfixOp ">=" "oidvector" "bool",
>         CatCreatePostfixOp ">=" "bit" "bool",
>         CatCreatePostfixOp ">=" "oid" "bool",
>         CatCreatePostfixOp ">=" "reltime" "bool",
>         CatCreatePostfixOp ">=" "abstime" "bool",
>         CatCreatePostfixOp ">=" "varbit" "bool",
>         CatCreatePostfixOp ">=" "int4" "bool",
>         CatCreatePostfixOp ">=" "int2" "bool",
>         CatCreatePostfixOp ">=" "tsquery" "bool",
>         CatCreatePostfixOp ">=" "int2" "bool",
>         CatCreatePostfixOp ">=" "int8" "bool",
>         CatCreatePostfixOp ">=" "int4" "bool",
>         CatCreatePostfixOp ">=" "int2" "bool",
>         CatCreatePostfixOp ">=" "bytea" "bool",
>         CatCreatePostfixOp ">=" "timestamp" "bool",
>         CatCreatePostfixOp ">=" "box" "bool",
>         CatCreatePostfixOp ">=" "date" "bool",
>         CatCreatePostfixOp ">=" "date" "bool",
>         CatCreatePostfixOp ">=" "timestamp" "bool",
>         CatCreatePostfixOp ">=" "timestamptz" "bool",
>         CatCreatePostfixOp ">=" "int8" "bool",
>         CatCreatePostfixOp ">=" "timestamp" "bool",
>         CatCreatePostfixOp ">=" "int8" "bool",
>         CatCreatePostfixOp ">=" "timestamptz" "bool",
>         CatCreatePostfixOp ">=" "tid" "bool",
>         CatCreatePostfixOp ">=" "record" "bool",
>         CatCreatePostfixOp ">=" "uuid" "bool",
>         CatCreatePostfixOp ">=" "int4" "bool",
>         CatCreatePostfixOp ">=" "anyenum" "bool",
>         CatCreatePostfixOp ">=" "tsvector" "bool",
>         CatCreatePostfixOp ">=" "timestamptz" "bool",
>         CatCreatePostfixOp ">=" "float8" "bool",
>         CatCreatePostfixOp ">=" "interval" "bool",
>         CatCreatePostfixOp ">=" "float4" "bool",
>         CatCreatePostfixOp ">=" "timetz" "bool",
>         CatCreatePostfixOp ">>" "polygon" "bool",
>         CatCreatePostfixOp ">>" "bit" "bit",
>         CatCreatePostfixOp ">>" "circle" "bool",
>         CatCreatePostfixOp ">>" "int4" "int4",
>         CatCreatePostfixOp ">>" "int8" "int8",
>         CatCreatePostfixOp ">>" "int2" "int2",
>         CatCreatePostfixOp ">>" "point" "bool",
>         CatCreatePostfixOp ">>" "box" "bool",
>         CatCreatePostfixOp ">>" "inet" "bool",
>         CatCreatePostfixOp ">>=" "inet" "bool",
>         CatCreatePostfixOp ">^" "box" "bool",
>         CatCreatePostfixOp ">^" "point" "bool",
>         CatCreatePostfixOp "?#" "box" "bool",
>         CatCreatePostfixOp "?#" "line" "bool",
>         CatCreatePostfixOp "?#" "lseg" "bool",
>         CatCreatePostfixOp "?#" "lseg" "bool",
>         CatCreatePostfixOp "?#" "lseg" "bool",
>         CatCreatePostfixOp "?#" "line" "bool",
>         CatCreatePostfixOp "?#" "path" "bool",
>         CatCreatePostfixOp "?-" "point" "bool",
>         CatCreatePostfixOp "?-|" "line" "bool",
>         CatCreatePostfixOp "?-|" "lseg" "bool",
>         CatCreatePostfixOp "?|" "point" "bool",
>         CatCreatePostfixOp "?||" "line" "bool",
>         CatCreatePostfixOp "?||" "lseg" "bool",
>         CatCreatePostfixOp "@" "point" "bool",
>         CatCreatePostfixOp "@" "circle" "bool",
>         CatCreatePostfixOp "@" "lseg" "bool",
>         CatCreatePostfixOp "@" "lseg" "bool",
>         CatCreatePostfixOp "@" "point" "bool",
>         CatCreatePostfixOp "@" "point" "bool",
>         CatCreatePostfixOp "@" "point" "bool",
>         CatCreatePostfixOp "@" "point" "bool",
>         CatCreatePostfixOp "@" "polygon" "bool",
>         CatCreatePostfixOp "@" "point" "bool",
>         CatCreatePostfixOp "@" "box" "bool",
>         CatCreatePostfixOp "@>" "polygon" "bool",
>         CatCreatePostfixOp "@>" "_aclitem" "bool",
>         CatCreatePostfixOp "@>" "circle" "bool",
>         CatCreatePostfixOp "@>" "anyarray" "bool",
>         CatCreatePostfixOp "@>" "tsquery" "bool",
>         CatCreatePostfixOp "@>" "circle" "bool",
>         CatCreatePostfixOp "@>" "box" "bool",
>         CatCreatePostfixOp "@>" "path" "bool",
>         CatCreatePostfixOp "@>" "polygon" "bool",
>         CatCreatePostfixOp "@>" "box" "bool",
>         CatCreatePostfixOp "@@" "text" "bool",
>         CatCreatePostfixOp "@@" "tsvector" "bool",
>         CatCreatePostfixOp "@@" "text" "bool",
>         CatCreatePostfixOp "@@" "tsquery" "bool",
>         CatCreatePostfixOp "@@@" "tsquery" "bool",
>         CatCreatePostfixOp "@@@" "tsvector" "bool",
>         CatCreatePostfixOp "^" "float8" "float8",
>         CatCreatePostfixOp "^" "numeric" "numeric",
>         CatCreatePostfixOp "|" "int8" "int8",
>         CatCreatePostfixOp "|" "int2" "int2",
>         CatCreatePostfixOp "|" "inet" "inet",
>         CatCreatePostfixOp "|" "int4" "int4",
>         CatCreatePostfixOp "|" "bit" "bit",
>         CatCreatePostfixOp "|&>" "box" "bool",
>         CatCreatePostfixOp "|&>" "polygon" "bool",
>         CatCreatePostfixOp "|&>" "circle" "bool",
>         CatCreatePostfixOp "|>>" "circle" "bool",
>         CatCreatePostfixOp "|>>" "box" "bool",
>         CatCreatePostfixOp "|>>" "polygon" "bool",
>         CatCreatePostfixOp "||" "tsvector" "tsvector",
>         CatCreatePostfixOp "||" "anynonarray" "text",
>         CatCreatePostfixOp "||" "text" "text",
>         CatCreatePostfixOp "||" "anyelement" "anyarray",
>         CatCreatePostfixOp "||" "anyarray" "anyarray",
>         CatCreatePostfixOp "||" "anyarray" "anyarray",
>         CatCreatePostfixOp "||" "tsquery" "tsquery",
>         CatCreatePostfixOp "||" "text" "text",
>         CatCreatePostfixOp "||" "varbit" "varbit",
>         CatCreatePostfixOp "||" "bytea" "bytea",
>         CatCreatePostfixOp "~" "polygon" "bool",
>         CatCreatePostfixOp "~" "circle" "bool",
>         CatCreatePostfixOp "~" "path" "bool",
>         CatCreatePostfixOp "~" "box" "bool",
>         CatCreatePostfixOp "~" "polygon" "bool",
>         CatCreatePostfixOp "~" "_aclitem" "bool",
>         CatCreatePostfixOp "~" "bpchar" "bool",
>         CatCreatePostfixOp "~" "circle" "bool",
>         CatCreatePostfixOp "~" "text" "bool",
>         CatCreatePostfixOp "~" "name" "bool",
>         CatCreatePostfixOp "~*" "text" "bool",
>         CatCreatePostfixOp "~*" "name" "bool",
>         CatCreatePostfixOp "~*" "bpchar" "bool",
>         CatCreatePostfixOp "~<=~" "bpchar" "bool",
>         CatCreatePostfixOp "~<=~" "text" "bool",
>         CatCreatePostfixOp "~<~" "bpchar" "bool",
>         CatCreatePostfixOp "~<~" "text" "bool",
>         CatCreatePostfixOp "~=" "box" "bool",
>         CatCreatePostfixOp "~=" "circle" "bool",
>         CatCreatePostfixOp "~=" "tinterval" "bool",
>         CatCreatePostfixOp "~=" "polygon" "bool",
>         CatCreatePostfixOp "~=" "point" "bool",
>         CatCreatePostfixOp "~>=~" "bpchar" "bool",
>         CatCreatePostfixOp "~>=~" "text" "bool",
>         CatCreatePostfixOp "~>~" "text" "bool",
>         CatCreatePostfixOp "~>~" "bpchar" "bool",
>         CatCreatePostfixOp "~~" "bytea" "bool",
>         CatCreatePostfixOp "~~" "text" "bool",
>         CatCreatePostfixOp "~~" "name" "bool",
>         CatCreatePostfixOp "~~" "bpchar" "bool",
>         CatCreatePostfixOp "~~*" "text" "bool",
>         CatCreatePostfixOp "~~*" "name" "bool",
>         CatCreatePostfixOp "~~*" "bpchar" "bool",
>         CatCreateBinaryOp "!~" "name" "text" "bool",
>         CatCreateBinaryOp "!~" "text" "text" "bool",
>         CatCreateBinaryOp "!~" "bpchar" "text" "bool",
>         CatCreateBinaryOp "!~*" "name" "text" "bool",
>         CatCreateBinaryOp "!~*" "text" "text" "bool",
>         CatCreateBinaryOp "!~*" "bpchar" "text" "bool",
>         CatCreateBinaryOp "!~~" "text" "text" "bool",
>         CatCreateBinaryOp "!~~" "name" "text" "bool",
>         CatCreateBinaryOp "!~~" "bpchar" "text" "bool",
>         CatCreateBinaryOp "!~~" "bytea" "bytea" "bool",
>         CatCreateBinaryOp "!~~*" "name" "text" "bool",
>         CatCreateBinaryOp "!~~*" "bpchar" "text" "bool",
>         CatCreateBinaryOp "!~~*" "text" "text" "bool",
>         CatCreateBinaryOp "#" "bit" "bit" "bit",
>         CatCreateBinaryOp "#" "line" "line" "point",
>         CatCreateBinaryOp "#" "int8" "int8" "int8",
>         CatCreateBinaryOp "#" "int4" "int4" "int4",
>         CatCreateBinaryOp "#" "lseg" "lseg" "point",
>         CatCreateBinaryOp "#" "box" "box" "box",
>         CatCreateBinaryOp "#" "int2" "int2" "int2",
>         CatCreateBinaryOp "##" "lseg" "line" "point",
>         CatCreateBinaryOp "##" "point" "lseg" "point",
>         CatCreateBinaryOp "##" "lseg" "lseg" "point",
>         CatCreateBinaryOp "##" "line" "lseg" "point",
>         CatCreateBinaryOp "##" "line" "box" "point",
>         CatCreateBinaryOp "##" "point" "line" "point",
>         CatCreateBinaryOp "##" "lseg" "box" "point",
>         CatCreateBinaryOp "##" "point" "box" "point",
>         CatCreateBinaryOp "#<" "tinterval" "reltime" "bool",
>         CatCreateBinaryOp "#<=" "tinterval" "reltime" "bool",
>         CatCreateBinaryOp "#<>" "tinterval" "reltime" "bool",
>         CatCreateBinaryOp "#=" "tinterval" "reltime" "bool",
>         CatCreateBinaryOp "#>" "tinterval" "reltime" "bool",
>         CatCreateBinaryOp "#>=" "tinterval" "reltime" "bool",
>         CatCreateBinaryOp "%" "int4" "int4" "int4",
>         CatCreateBinaryOp "%" "numeric" "numeric" "numeric",
>         CatCreateBinaryOp "%" "int2" "int2" "int2",
>         CatCreateBinaryOp "%" "int8" "int8" "int8",
>         CatCreateBinaryOp "&" "int8" "int8" "int8",
>         CatCreateBinaryOp "&" "inet" "inet" "inet",
>         CatCreateBinaryOp "&" "bit" "bit" "bit",
>         CatCreateBinaryOp "&" "int2" "int2" "int2",
>         CatCreateBinaryOp "&" "int4" "int4" "int4",
>         CatCreateBinaryOp "&&" "tsquery" "tsquery" "tsquery",
>         CatCreateBinaryOp "&&" "circle" "circle" "bool",
>         CatCreateBinaryOp "&&" "anyarray" "anyarray" "bool",
>         CatCreateBinaryOp "&&" "tinterval" "tinterval" "bool",
>         CatCreateBinaryOp "&&" "box" "box" "bool",
>         CatCreateBinaryOp "&&" "polygon" "polygon" "bool",
>         CatCreateBinaryOp "&<" "polygon" "polygon" "bool",
>         CatCreateBinaryOp "&<" "box" "box" "bool",
>         CatCreateBinaryOp "&<" "circle" "circle" "bool",
>         CatCreateBinaryOp "&<|" "box" "box" "bool",
>         CatCreateBinaryOp "&<|" "polygon" "polygon" "bool",
>         CatCreateBinaryOp "&<|" "circle" "circle" "bool",
>         CatCreateBinaryOp "&>" "polygon" "polygon" "bool",
>         CatCreateBinaryOp "&>" "circle" "circle" "bool",
>         CatCreateBinaryOp "&>" "box" "box" "bool",
>         CatCreateBinaryOp "*" "interval" "float8" "interval",
>         CatCreateBinaryOp "*" "float8" "money" "money",
>         CatCreateBinaryOp "*" "money" "int2" "money",
>         CatCreateBinaryOp "*" "money" "int4" "money",
>         CatCreateBinaryOp "*" "int4" "int8" "int8",
>         CatCreateBinaryOp "*" "money" "float8" "money",
>         CatCreateBinaryOp "*" "int2" "int4" "int4",
>         CatCreateBinaryOp "*" "int4" "int2" "int4",
>         CatCreateBinaryOp "*" "numeric" "numeric" "numeric",
>         CatCreateBinaryOp "*" "box" "point" "box",
>         CatCreateBinaryOp "*" "int8" "int2" "int8",
>         CatCreateBinaryOp "*" "int2" "int8" "int8",
>         CatCreateBinaryOp "*" "path" "point" "path",
>         CatCreateBinaryOp "*" "point" "point" "point",
>         CatCreateBinaryOp "*" "money" "float4" "money",
>         CatCreateBinaryOp "*" "int8" "int8" "int8",
>         CatCreateBinaryOp "*" "float8" "interval" "interval",
>         CatCreateBinaryOp "*" "float8" "float4" "float8",
>         CatCreateBinaryOp "*" "float4" "money" "money",
>         CatCreateBinaryOp "*" "float4" "float8" "float8",
>         CatCreateBinaryOp "*" "float8" "float8" "float8",
>         CatCreateBinaryOp "*" "int8" "int4" "int8",
>         CatCreateBinaryOp "*" "circle" "point" "circle",
>         CatCreateBinaryOp "*" "float4" "float4" "float4",
>         CatCreateBinaryOp "*" "int4" "int4" "int4",
>         CatCreateBinaryOp "*" "int2" "money" "money",
>         CatCreateBinaryOp "*" "int4" "money" "money",
>         CatCreateBinaryOp "*" "int2" "int2" "int2",
>         CatCreateBinaryOp "+" "timestamptz" "interval" "timestamptz",
>         CatCreateBinaryOp "+" "float4" "float8" "float8",
>         CatCreateBinaryOp "+" "int8" "inet" "inet",
>         CatCreateBinaryOp "+" "inet" "int8" "inet",
>         CatCreateBinaryOp "+" "date" "timetz" "timestamptz",
>         CatCreateBinaryOp "+" "int8" "int8" "int8",
>         CatCreateBinaryOp "+" "int8" "int4" "int8",
>         CatCreateBinaryOp "+" "int4" "int8" "int8",
>         CatCreateBinaryOp "+" "int8" "int2" "int8",
>         CatCreateBinaryOp "+" "int2" "int8" "int8",
>         CatCreateBinaryOp "+" "path" "point" "path",
>         CatCreateBinaryOp "+" "path" "path" "path",
>         CatCreateBinaryOp "+" "point" "point" "point",
>         CatCreateBinaryOp "+" "float8" "float4" "float8",
>         CatCreateBinaryOp "+" "date" "interval" "timestamp",
>         CatCreateBinaryOp "+" "circle" "point" "circle",
>         CatCreateBinaryOp "+" "timestamp" "interval" "timestamp",
>         CatCreateBinaryOp "+" "time" "date" "timestamp",
>         CatCreateBinaryOp "+" "date" "time" "timestamp",
>         CatCreateBinaryOp "+" "_aclitem" "aclitem" "_aclitem",
>         CatCreateBinaryOp "+" "int4" "date" "date",
>         CatCreateBinaryOp "+" "timetz" "date" "timestamptz",
>         CatCreateBinaryOp "+" "int2" "int2" "int2",
>         CatCreateBinaryOp "+" "int4" "int4" "int4",
>         CatCreateBinaryOp "+" "int2" "int4" "int4",
>         CatCreateBinaryOp "+" "int4" "int2" "int4",
>         CatCreateBinaryOp "+" "interval" "time" "time",
>         CatCreateBinaryOp "+" "money" "money" "money",
>         CatCreateBinaryOp "+" "timetz" "interval" "timetz",
>         CatCreateBinaryOp "+" "abstime" "reltime" "abstime",
>         CatCreateBinaryOp "+" "time" "interval" "time",
>         CatCreateBinaryOp "+" "float4" "float4" "float4",
>         CatCreateBinaryOp "+" "float8" "float8" "float8",
>         CatCreateBinaryOp "+" "interval" "timestamptz" "timestamptz",
>         CatCreateBinaryOp "+" "interval" "timestamp" "timestamp",
>         CatCreateBinaryOp "+" "interval" "timetz" "timetz",
>         CatCreateBinaryOp "+" "interval" "interval" "interval",
>         CatCreateBinaryOp "+" "box" "point" "box",
>         CatCreateBinaryOp "+" "numeric" "numeric" "numeric",
>         CatCreateBinaryOp "+" "interval" "date" "timestamp",
>         CatCreateBinaryOp "+" "date" "int4" "date",
>         CatCreateBinaryOp "-" "int2" "int8" "int8",
>         CatCreateBinaryOp "-" "_aclitem" "aclitem" "_aclitem",
>         CatCreateBinaryOp "-" "float8" "float8" "float8",
>         CatCreateBinaryOp "-" "int8" "int2" "int8",
>         CatCreateBinaryOp "-" "timestamptz" "interval" "timestamptz",
>         CatCreateBinaryOp "-" "int8" "int4" "int8",
>         CatCreateBinaryOp "-" "timestamp" "timestamp" "interval",
>         CatCreateBinaryOp "-" "timestamp" "interval" "timestamp",
>         CatCreateBinaryOp "-" "box" "point" "box",
>         CatCreateBinaryOp "-" "int2" "int4" "int4",
>         CatCreateBinaryOp "-" "int4" "int2" "int4",
>         CatCreateBinaryOp "-" "int4" "int4" "int4",
>         CatCreateBinaryOp "-" "interval" "interval" "interval",
>         CatCreateBinaryOp "-" "int2" "int2" "int2",
>         CatCreateBinaryOp "-" "numeric" "numeric" "numeric",
>         CatCreateBinaryOp "-" "money" "money" "money",
>         CatCreateBinaryOp "-" "date" "int4" "date",
>         CatCreateBinaryOp "-" "circle" "point" "circle",
>         CatCreateBinaryOp "-" "int8" "int8" "int8",
>         CatCreateBinaryOp "-" "abstime" "reltime" "abstime",
>         CatCreateBinaryOp "-" "timetz" "interval" "timetz",
>         CatCreateBinaryOp "-" "float8" "float4" "float8",
>         CatCreateBinaryOp "-" "inet" "int8" "inet",
>         CatCreateBinaryOp "-" "time" "interval" "time",
>         CatCreateBinaryOp "-" "date" "date" "int4",
>         CatCreateBinaryOp "-" "date" "interval" "timestamp",
>         CatCreateBinaryOp "-" "int4" "int8" "int8",
>         CatCreateBinaryOp "-" "float4" "float4" "float4",
>         CatCreateBinaryOp "-" "point" "point" "point",
>         CatCreateBinaryOp "-" "float4" "float8" "float8",
>         CatCreateBinaryOp "-" "time" "time" "interval",
>         CatCreateBinaryOp "-" "path" "point" "path",
>         CatCreateBinaryOp "-" "timestamptz" "timestamptz" "interval",
>         CatCreateBinaryOp "-" "inet" "inet" "int8",
>         CatCreateBinaryOp "/" "box" "point" "box",
>         CatCreateBinaryOp "/" "int4" "int4" "int4",
>         CatCreateBinaryOp "/" "int2" "int2" "int2",
>         CatCreateBinaryOp "/" "money" "money" "float8",
>         CatCreateBinaryOp "/" "float4" "float4" "float4",
>         CatCreateBinaryOp "/" "circle" "point" "circle",
>         CatCreateBinaryOp "/" "interval" "float8" "interval",
>         CatCreateBinaryOp "/" "float8" "float8" "float8",
>         CatCreateBinaryOp "/" "float8" "float4" "float8",
>         CatCreateBinaryOp "/" "float4" "float8" "float8",
>         CatCreateBinaryOp "/" "money" "float4" "money",
>         CatCreateBinaryOp "/" "point" "point" "point",
>         CatCreateBinaryOp "/" "int8" "int4" "int8",
>         CatCreateBinaryOp "/" "path" "point" "path",
>         CatCreateBinaryOp "/" "int2" "int8" "int8",
>         CatCreateBinaryOp "/" "int8" "int2" "int8",
>         CatCreateBinaryOp "/" "money" "int2" "money",
>         CatCreateBinaryOp "/" "numeric" "numeric" "numeric",
>         CatCreateBinaryOp "/" "int8" "int8" "int8",
>         CatCreateBinaryOp "/" "int4" "int8" "int8",
>         CatCreateBinaryOp "/" "int4" "int2" "int4",
>         CatCreateBinaryOp "/" "int2" "int4" "int4",
>         CatCreateBinaryOp "/" "money" "float8" "money",
>         CatCreateBinaryOp "/" "money" "int4" "money",
>         CatCreateBinaryOp "<" "int8" "int4" "bool",
>         CatCreateBinaryOp "<" "float4" "float8" "bool",
>         CatCreateBinaryOp "<" "interval" "interval" "bool",
>         CatCreateBinaryOp "<" "float8" "float4" "bool",
>         CatCreateBinaryOp "<" "timestamptz" "timestamptz" "bool",
>         CatCreateBinaryOp "<" "tid" "tid" "bool",
>         CatCreateBinaryOp "<" "int8" "int8" "bool",
>         CatCreateBinaryOp "<" "timestamptz" "timestamp" "bool",
>         CatCreateBinaryOp "<" "record" "record" "bool",
>         CatCreateBinaryOp "<" "timestamp" "timestamptz" "bool",
>         CatCreateBinaryOp "<" "timestamptz" "date" "bool",
>         CatCreateBinaryOp "<" "timestamp" "date" "bool",
>         CatCreateBinaryOp "<" "date" "timestamptz" "bool",
>         CatCreateBinaryOp "<" "date" "timestamp" "bool",
>         CatCreateBinaryOp "<" "box" "box" "bool",
>         CatCreateBinaryOp "<" "timestamp" "timestamp" "bool",
>         CatCreateBinaryOp "<" "bytea" "bytea" "bool",
>         CatCreateBinaryOp "<" "bool" "bool" "bool",
>         CatCreateBinaryOp "<" "int4" "int8" "bool",
>         CatCreateBinaryOp "<" "int2" "int4" "bool",
>         CatCreateBinaryOp "<" "int4" "int2" "bool",
>         CatCreateBinaryOp "<" "int8" "int2" "bool",
>         CatCreateBinaryOp "<" "int2" "int8" "bool",
>         CatCreateBinaryOp "<" "tsquery" "tsquery" "bool",
>         CatCreateBinaryOp "<" "abstime" "abstime" "bool",
>         CatCreateBinaryOp "<" "varbit" "varbit" "bool",
>         CatCreateBinaryOp "<" "reltime" "reltime" "bool",
>         CatCreateBinaryOp "<" "tsvector" "tsvector" "bool",
>         CatCreateBinaryOp "<" "oid" "oid" "bool",
>         CatCreateBinaryOp "<" "oidvector" "oidvector" "bool",
>         CatCreateBinaryOp "<" "bit" "bit" "bool",
>         CatCreateBinaryOp "<" "int2" "int2" "bool",
>         CatCreateBinaryOp "<" "float4" "float4" "bool",
>         CatCreateBinaryOp "<" "anyenum" "anyenum" "bool",
>         CatCreateBinaryOp "<" "char" "char" "bool",
>         CatCreateBinaryOp "<" "numeric" "numeric" "bool",
>         CatCreateBinaryOp "<" "int4" "int4" "bool",
>         CatCreateBinaryOp "<" "name" "name" "bool",
>         CatCreateBinaryOp "<" "text" "text" "bool",
>         CatCreateBinaryOp "<" "float8" "float8" "bool",
>         CatCreateBinaryOp "<" "uuid" "uuid" "bool",
>         CatCreateBinaryOp "<" "inet" "inet" "bool",
>         CatCreateBinaryOp "<" "macaddr" "macaddr" "bool",
>         CatCreateBinaryOp "<" "path" "path" "bool",
>         CatCreateBinaryOp "<" "lseg" "lseg" "bool",
>         CatCreateBinaryOp "<" "tinterval" "tinterval" "bool",
>         CatCreateBinaryOp "<" "money" "money" "bool",
>         CatCreateBinaryOp "<" "bpchar" "bpchar" "bool",
>         CatCreateBinaryOp "<" "anyarray" "anyarray" "bool",
>         CatCreateBinaryOp "<" "date" "date" "bool",
>         CatCreateBinaryOp "<" "time" "time" "bool",
>         CatCreateBinaryOp "<" "circle" "circle" "bool",
>         CatCreateBinaryOp "<" "timetz" "timetz" "bool",
>         CatCreateBinaryOp "<#>" "abstime" "abstime" "tinterval",
>         CatCreateBinaryOp "<->" "lseg" "line" "float8",
>         CatCreateBinaryOp "<->" "point" "path" "float8",
>         CatCreateBinaryOp "<->" "point" "box" "float8",
>         CatCreateBinaryOp "<->" "point" "lseg" "float8",
>         CatCreateBinaryOp "<->" "line" "box" "float8",
>         CatCreateBinaryOp "<->" "point" "point" "float8",
>         CatCreateBinaryOp "<->" "circle" "polygon" "float8",
>         CatCreateBinaryOp "<->" "point" "circle" "float8",
>         CatCreateBinaryOp "<->" "circle" "circle" "float8",
>         CatCreateBinaryOp "<->" "point" "line" "float8",
>         CatCreateBinaryOp "<->" "polygon" "polygon" "float8",
>         CatCreateBinaryOp "<->" "lseg" "lseg" "float8",
>         CatCreateBinaryOp "<->" "line" "line" "float8",
>         CatCreateBinaryOp "<->" "path" "path" "float8",
>         CatCreateBinaryOp "<->" "box" "box" "float8",
>         CatCreateBinaryOp "<->" "lseg" "box" "float8",
>         CatCreateBinaryOp "<<" "int2" "int4" "int2",
>         CatCreateBinaryOp "<<" "bit" "int4" "bit",
>         CatCreateBinaryOp "<<" "int8" "int4" "int8",
>         CatCreateBinaryOp "<<" "circle" "circle" "bool",
>         CatCreateBinaryOp "<<" "tinterval" "tinterval" "bool",
>         CatCreateBinaryOp "<<" "inet" "inet" "bool",
>         CatCreateBinaryOp "<<" "box" "box" "bool",
>         CatCreateBinaryOp "<<" "point" "point" "bool",
>         CatCreateBinaryOp "<<" "int4" "int4" "int4",
>         CatCreateBinaryOp "<<" "polygon" "polygon" "bool",
>         CatCreateBinaryOp "<<=" "inet" "inet" "bool",
>         CatCreateBinaryOp "<<|" "circle" "circle" "bool",
>         CatCreateBinaryOp "<<|" "box" "box" "bool",
>         CatCreateBinaryOp "<<|" "polygon" "polygon" "bool",
>         CatCreateBinaryOp "<=" "timestamp" "timestamp" "bool",
>         CatCreateBinaryOp "<=" "float8" "float4" "bool",
>         CatCreateBinaryOp "<=" "bpchar" "bpchar" "bool",
>         CatCreateBinaryOp "<=" "box" "box" "bool",
>         CatCreateBinaryOp "<=" "date" "timestamp" "bool",
>         CatCreateBinaryOp "<=" "date" "timestamptz" "bool",
>         CatCreateBinaryOp "<=" "float4" "float8" "bool",
>         CatCreateBinaryOp "<=" "anyarray" "anyarray" "bool",
>         CatCreateBinaryOp "<=" "timestamp" "date" "bool",
>         CatCreateBinaryOp "<=" "timestamptz" "date" "bool",
>         CatCreateBinaryOp "<=" "int8" "int4" "bool",
>         CatCreateBinaryOp "<=" "timetz" "timetz" "bool",
>         CatCreateBinaryOp "<=" "date" "date" "bool",
>         CatCreateBinaryOp "<=" "timestamp" "timestamptz" "bool",
>         CatCreateBinaryOp "<=" "timestamptz" "timestamp" "bool",
>         CatCreateBinaryOp "<=" "int8" "int8" "bool",
>         CatCreateBinaryOp "<=" "int4" "int8" "bool",
>         CatCreateBinaryOp "<=" "circle" "circle" "bool",
>         CatCreateBinaryOp "<=" "inet" "inet" "bool",
>         CatCreateBinaryOp "<=" "macaddr" "macaddr" "bool",
>         CatCreateBinaryOp "<=" "record" "record" "bool",
>         CatCreateBinaryOp "<=" "lseg" "lseg" "bool",
>         CatCreateBinaryOp "<=" "time" "time" "bool",
>         CatCreateBinaryOp "<=" "path" "path" "bool",
>         CatCreateBinaryOp "<=" "interval" "interval" "bool",
>         CatCreateBinaryOp "<=" "float8" "float8" "bool",
>         CatCreateBinaryOp "<=" "uuid" "uuid" "bool",
>         CatCreateBinaryOp "<=" "text" "text" "bool",
>         CatCreateBinaryOp "<=" "name" "name" "bool",
>         CatCreateBinaryOp "<=" "numeric" "numeric" "bool",
>         CatCreateBinaryOp "<=" "char" "char" "bool",
>         CatCreateBinaryOp "<=" "float4" "float4" "bool",
>         CatCreateBinaryOp "<=" "tid" "tid" "bool",
>         CatCreateBinaryOp "<=" "oidvector" "oidvector" "bool",
>         CatCreateBinaryOp "<=" "tinterval" "tinterval" "bool",
>         CatCreateBinaryOp "<=" "bit" "bit" "bool",
>         CatCreateBinaryOp "<=" "anyenum" "anyenum" "bool",
>         CatCreateBinaryOp "<=" "oid" "oid" "bool",
>         CatCreateBinaryOp "<=" "timestamptz" "timestamptz" "bool",
>         CatCreateBinaryOp "<=" "reltime" "reltime" "bool",
>         CatCreateBinaryOp "<=" "money" "money" "bool",
>         CatCreateBinaryOp "<=" "tsvector" "tsvector" "bool",
>         CatCreateBinaryOp "<=" "varbit" "varbit" "bool",
>         CatCreateBinaryOp "<=" "abstime" "abstime" "bool",
>         CatCreateBinaryOp "<=" "int2" "int8" "bool",
>         CatCreateBinaryOp "<=" "int4" "int2" "bool",
>         CatCreateBinaryOp "<=" "int2" "int4" "bool",
>         CatCreateBinaryOp "<=" "bool" "bool" "bool",
>         CatCreateBinaryOp "<=" "tsquery" "tsquery" "bool",
>         CatCreateBinaryOp "<=" "int8" "int2" "bool",
>         CatCreateBinaryOp "<=" "int4" "int4" "bool",
>         CatCreateBinaryOp "<=" "int2" "int2" "bool",
>         CatCreateBinaryOp "<=" "bytea" "bytea" "bool",
>         CatCreateBinaryOp "<>" "float4" "float8" "bool",
>         CatCreateBinaryOp "<>" "int4" "int8" "bool",
>         CatCreateBinaryOp "<>" "bool" "bool" "bool",
>         CatCreateBinaryOp "<>" "tid" "tid" "bool",
>         CatCreateBinaryOp "<>" "int8" "int8" "bool",
>         CatCreateBinaryOp "<>" "int8" "int4" "bool",
>         CatCreateBinaryOp "<>" "int4" "int4" "bool",
>         CatCreateBinaryOp "<>" "int2" "int2" "bool",
>         CatCreateBinaryOp "<>" "text" "text" "bool",
>         CatCreateBinaryOp "<>" "int2" "int4" "bool",
>         CatCreateBinaryOp "<>" "int4" "int2" "bool",
>         CatCreateBinaryOp "<>" "abstime" "abstime" "bool",
>         CatCreateBinaryOp "<>" "reltime" "reltime" "bool",
>         CatCreateBinaryOp "<>" "oid" "oid" "bool",
>         CatCreateBinaryOp "<>" "oidvector" "oidvector" "bool",
>         CatCreateBinaryOp "<>" "float4" "float4" "bool",
>         CatCreateBinaryOp "<>" "char" "char" "bool",
>         CatCreateBinaryOp "<>" "name" "name" "bool",
>         CatCreateBinaryOp "<>" "float8" "float8" "bool",
>         CatCreateBinaryOp "<>" "point" "point" "bool",
>         CatCreateBinaryOp "<>" "tinterval" "tinterval" "bool",
>         CatCreateBinaryOp "<>" "money" "money" "bool",
>         CatCreateBinaryOp "<>" "bpchar" "bpchar" "bool",
>         CatCreateBinaryOp "<>" "anyarray" "anyarray" "bool",
>         CatCreateBinaryOp "<>" "date" "date" "bool",
>         CatCreateBinaryOp "<>" "time" "time" "bool",
>         CatCreateBinaryOp "<>" "timetz" "timetz" "bool",
>         CatCreateBinaryOp "<>" "float8" "float4" "bool",
>         CatCreateBinaryOp "<>" "timestamptz" "timestamptz" "bool",
>         CatCreateBinaryOp "<>" "interval" "interval" "bool",
>         CatCreateBinaryOp "<>" "circle" "circle" "bool",
>         CatCreateBinaryOp "<>" "lseg" "lseg" "bool",
>         CatCreateBinaryOp "<>" "macaddr" "macaddr" "bool",
>         CatCreateBinaryOp "<>" "inet" "inet" "bool",
>         CatCreateBinaryOp "<>" "numeric" "numeric" "bool",
>         CatCreateBinaryOp "<>" "bit" "bit" "bool",
>         CatCreateBinaryOp "<>" "varbit" "varbit" "bool",
>         CatCreateBinaryOp "<>" "int2" "int8" "bool",
>         CatCreateBinaryOp "<>" "int8" "int2" "bool",
>         CatCreateBinaryOp "<>" "bytea" "bytea" "bool",
>         CatCreateBinaryOp "<>" "timestamp" "timestamp" "bool",
>         CatCreateBinaryOp "<>" "date" "timestamp" "bool",
>         CatCreateBinaryOp "<>" "date" "timestamptz" "bool",
>         CatCreateBinaryOp "<>" "timestamp" "date" "bool",
>         CatCreateBinaryOp "<>" "timestamptz" "date" "bool",
>         CatCreateBinaryOp "<>" "timestamp" "timestamptz" "bool",
>         CatCreateBinaryOp "<>" "timestamptz" "timestamp" "bool",
>         CatCreateBinaryOp "<>" "uuid" "uuid" "bool",
>         CatCreateBinaryOp "<>" "anyenum" "anyenum" "bool",
>         CatCreateBinaryOp "<>" "tsvector" "tsvector" "bool",
>         CatCreateBinaryOp "<>" "tsquery" "tsquery" "bool",
>         CatCreateBinaryOp "<>" "record" "record" "bool",
>         CatCreateBinaryOp "<?>" "abstime" "tinterval" "bool",
>         CatCreateBinaryOp "<@" "lseg" "box" "bool",
>         CatCreateBinaryOp "<@" "point" "circle" "bool",
>         CatCreateBinaryOp "<@" "anyarray" "anyarray" "bool",
>         CatCreateBinaryOp "<@" "tsquery" "tsquery" "bool",
>         CatCreateBinaryOp "<@" "circle" "circle" "bool",
>         CatCreateBinaryOp "<@" "lseg" "line" "bool",
>         CatCreateBinaryOp "<@" "polygon" "polygon" "bool",
>         CatCreateBinaryOp "<@" "point" "box" "bool",
>         CatCreateBinaryOp "<@" "point" "polygon" "bool",
>         CatCreateBinaryOp "<@" "point" "path" "bool",
>         CatCreateBinaryOp "<@" "box" "box" "bool",
>         CatCreateBinaryOp "<@" "point" "line" "bool",
>         CatCreateBinaryOp "<@" "point" "lseg" "bool",
>         CatCreateBinaryOp "<^" "point" "point" "bool",
>         CatCreateBinaryOp "<^" "box" "box" "bool",
>         CatCreateBinaryOp "=" "anyarray" "anyarray" "bool",
>         CatCreateBinaryOp "=" "name" "name" "bool",
>         CatCreateBinaryOp "=" "time" "time" "bool",
>         CatCreateBinaryOp "=" "int2" "int2" "bool",
>         CatCreateBinaryOp "=" "bpchar" "bpchar" "bool",
>         CatCreateBinaryOp "=" "aclitem" "aclitem" "bool",
>         CatCreateBinaryOp "=" "int4" "int4" "bool",
>         CatCreateBinaryOp "=" "anyenum" "anyenum" "bool",
>         CatCreateBinaryOp "=" "text" "text" "bool",
>         CatCreateBinaryOp "=" "uuid" "uuid" "bool",
>         CatCreateBinaryOp "=" "lseg" "lseg" "bool",
>         CatCreateBinaryOp "=" "xid" "xid" "bool",
>         CatCreateBinaryOp "=" "xid" "int4" "bool",
>         CatCreateBinaryOp "=" "cid" "cid" "bool",
>         CatCreateBinaryOp "=" "int2vector" "int2vector" "bool",
>         CatCreateBinaryOp "=" "float4" "float8" "bool",
>         CatCreateBinaryOp "=" "money" "money" "bool",
>         CatCreateBinaryOp "=" "tid" "tid" "bool",
>         CatCreateBinaryOp "=" "int4" "int8" "bool",
>         CatCreateBinaryOp "=" "tinterval" "tinterval" "bool",
>         CatCreateBinaryOp "=" "int8" "int8" "bool",
>         CatCreateBinaryOp "=" "record" "record" "bool",
>         CatCreateBinaryOp "=" "timestamptz" "timestamp" "bool",
>         CatCreateBinaryOp "=" "int2" "int8" "bool",
>         CatCreateBinaryOp "=" "timestamp" "timestamptz" "bool",
>         CatCreateBinaryOp "=" "timestamptz" "date" "bool",
>         CatCreateBinaryOp "=" "circle" "circle" "bool",
>         CatCreateBinaryOp "=" "date" "date" "bool",
>         CatCreateBinaryOp "=" "timestamp" "date" "bool",
>         CatCreateBinaryOp "=" "date" "timestamptz" "bool",
>         CatCreateBinaryOp "=" "line" "line" "bool",
>         CatCreateBinaryOp "=" "path" "path" "bool",
>         CatCreateBinaryOp "=" "macaddr" "macaddr" "bool",
>         CatCreateBinaryOp "=" "date" "timestamp" "bool",
>         CatCreateBinaryOp "=" "box" "box" "bool",
>         CatCreateBinaryOp "=" "float8" "float4" "bool",
>         CatCreateBinaryOp "=" "timestamp" "timestamp" "bool",
>         CatCreateBinaryOp "=" "inet" "inet" "bool",
>         CatCreateBinaryOp "=" "bytea" "bytea" "bool",
>         CatCreateBinaryOp "=" "int2" "int4" "bool",
>         CatCreateBinaryOp "=" "int4" "int2" "bool",
>         CatCreateBinaryOp "=" "int8" "int2" "bool",
>         CatCreateBinaryOp "=" "abstime" "abstime" "bool",
>         CatCreateBinaryOp "=" "int8" "int4" "bool",
>         CatCreateBinaryOp "=" "float8" "float8" "bool",
>         CatCreateBinaryOp "=" "reltime" "reltime" "bool",
>         CatCreateBinaryOp "=" "interval" "interval" "bool",
>         CatCreateBinaryOp "=" "tsquery" "tsquery" "bool",
>         CatCreateBinaryOp "=" "varbit" "varbit" "bool",
>         CatCreateBinaryOp "=" "timestamptz" "timestamptz" "bool",
>         CatCreateBinaryOp "=" "oid" "oid" "bool",
>         CatCreateBinaryOp "=" "bool" "bool" "bool",
>         CatCreateBinaryOp "=" "bit" "bit" "bool",
>         CatCreateBinaryOp "=" "numeric" "numeric" "bool",
>         CatCreateBinaryOp "=" "oidvector" "oidvector" "bool",
>         CatCreateBinaryOp "=" "float4" "float4" "bool",
>         CatCreateBinaryOp "=" "tsvector" "tsvector" "bool",
>         CatCreateBinaryOp "=" "char" "char" "bool",
>         CatCreateBinaryOp "=" "timetz" "timetz" "bool",
>         CatCreateBinaryOp ">" "inet" "inet" "bool",
>         CatCreateBinaryOp ">" "time" "time" "bool",
>         CatCreateBinaryOp ">" "circle" "circle" "bool",
>         CatCreateBinaryOp ">" "date" "date" "bool",
>         CatCreateBinaryOp ">" "anyarray" "anyarray" "bool",
>         CatCreateBinaryOp ">" "bpchar" "bpchar" "bool",
>         CatCreateBinaryOp ">" "money" "money" "bool",
>         CatCreateBinaryOp ">" "tinterval" "tinterval" "bool",
>         CatCreateBinaryOp ">" "path" "path" "bool",
>         CatCreateBinaryOp ">" "lseg" "lseg" "bool",
>         CatCreateBinaryOp ">" "macaddr" "macaddr" "bool",
>         CatCreateBinaryOp ">" "float8" "float8" "bool",
>         CatCreateBinaryOp ">" "text" "text" "bool",
>         CatCreateBinaryOp ">" "name" "name" "bool",
>         CatCreateBinaryOp ">" "numeric" "numeric" "bool",
>         CatCreateBinaryOp ">" "char" "char" "bool",
>         CatCreateBinaryOp ">" "float4" "float4" "bool",
>         CatCreateBinaryOp ">" "bit" "bit" "bool",
>         CatCreateBinaryOp ">" "oidvector" "oidvector" "bool",
>         CatCreateBinaryOp ">" "oid" "oid" "bool",
>         CatCreateBinaryOp ">" "reltime" "reltime" "bool",
>         CatCreateBinaryOp ">" "varbit" "varbit" "bool",
>         CatCreateBinaryOp ">" "abstime" "abstime" "bool",
>         CatCreateBinaryOp ">" "int4" "int8" "bool",
>         CatCreateBinaryOp ">" "int2" "int8" "bool",
>         CatCreateBinaryOp ">" "int8" "int2" "bool",
>         CatCreateBinaryOp ">" "int4" "int2" "bool",
>         CatCreateBinaryOp ">" "int2" "int4" "bool",
>         CatCreateBinaryOp ">" "int4" "int4" "bool",
>         CatCreateBinaryOp ">" "int2" "int2" "bool",
>         CatCreateBinaryOp ">" "tsquery" "tsquery" "bool",
>         CatCreateBinaryOp ">" "bytea" "bytea" "bool",
>         CatCreateBinaryOp ">" "bool" "bool" "bool",
>         CatCreateBinaryOp ">" "timestamp" "timestamp" "bool",
>         CatCreateBinaryOp ">" "box" "box" "bool",
>         CatCreateBinaryOp ">" "date" "timestamp" "bool",
>         CatCreateBinaryOp ">" "date" "timestamptz" "bool",
>         CatCreateBinaryOp ">" "timestamp" "date" "bool",
>         CatCreateBinaryOp ">" "timestamptz" "date" "bool",
>         CatCreateBinaryOp ">" "int8" "int4" "bool",
>         CatCreateBinaryOp ">" "timestamp" "timestamptz" "bool",
>         CatCreateBinaryOp ">" "timestamptz" "timestamp" "bool",
>         CatCreateBinaryOp ">" "int8" "int8" "bool",
>         CatCreateBinaryOp ">" "tid" "tid" "bool",
>         CatCreateBinaryOp ">" "uuid" "uuid" "bool",
>         CatCreateBinaryOp ">" "anyenum" "anyenum" "bool",
>         CatCreateBinaryOp ">" "record" "record" "bool",
>         CatCreateBinaryOp ">" "tsvector" "tsvector" "bool",
>         CatCreateBinaryOp ">" "timestamptz" "timestamptz" "bool",
>         CatCreateBinaryOp ">" "float8" "float4" "bool",
>         CatCreateBinaryOp ">" "interval" "interval" "bool",
>         CatCreateBinaryOp ">" "float4" "float8" "bool",
>         CatCreateBinaryOp ">" "timetz" "timetz" "bool",
>         CatCreateBinaryOp ">=" "int2" "int8" "bool",
>         CatCreateBinaryOp ">=" "timestamptz" "timestamp" "bool",
>         CatCreateBinaryOp ">=" "float8" "float8" "bool",
>         CatCreateBinaryOp ">=" "bit" "bit" "bool",
>         CatCreateBinaryOp ">=" "uuid" "uuid" "bool",
>         CatCreateBinaryOp ">=" "oidvector" "oidvector" "bool",
>         CatCreateBinaryOp ">=" "bool" "bool" "bool",
>         CatCreateBinaryOp ">=" "float4" "float4" "bool",
>         CatCreateBinaryOp ">=" "float4" "float8" "bool",
>         CatCreateBinaryOp ">=" "numeric" "numeric" "bool",
>         CatCreateBinaryOp ">=" "int8" "int2" "bool",
>         CatCreateBinaryOp ">=" "char" "char" "bool",
>         CatCreateBinaryOp ">=" "date" "timestamp" "bool",
>         CatCreateBinaryOp ">=" "record" "record" "bool",
>         CatCreateBinaryOp ">=" "date" "timestamptz" "bool",
>         CatCreateBinaryOp ">=" "date" "date" "bool",
>         CatCreateBinaryOp ">=" "interval" "interval" "bool",
>         CatCreateBinaryOp ">=" "timestamp" "date" "bool",
>         CatCreateBinaryOp ">=" "timestamptz" "timestamptz" "bool",
>         CatCreateBinaryOp ">=" "lseg" "lseg" "bool",
>         CatCreateBinaryOp ">=" "path" "path" "bool",
>         CatCreateBinaryOp ">=" "money" "money" "bool",
>         CatCreateBinaryOp ">=" "timestamptz" "date" "bool",
>         CatCreateBinaryOp ">=" "int8" "int4" "bool",
>         CatCreateBinaryOp ">=" "float8" "float4" "bool",
>         CatCreateBinaryOp ">=" "timestamp" "timestamptz" "bool",
>         CatCreateBinaryOp ">=" "circle" "circle" "bool",
>         CatCreateBinaryOp ">=" "int8" "int8" "bool",
>         CatCreateBinaryOp ">=" "tinterval" "tinterval" "bool",
>         CatCreateBinaryOp ">=" "tid" "tid" "bool",
>         CatCreateBinaryOp ">=" "int2" "int4" "bool",
>         CatCreateBinaryOp ">=" "int4" "int4" "bool",
>         CatCreateBinaryOp ">=" "int4" "int2" "bool",
>         CatCreateBinaryOp ">=" "int2" "int2" "bool",
>         CatCreateBinaryOp ">=" "tsvector" "tsvector" "bool",
>         CatCreateBinaryOp ">=" "bpchar" "bpchar" "bool",
>         CatCreateBinaryOp ">=" "inet" "inet" "bool",
>         CatCreateBinaryOp ">=" "tsquery" "tsquery" "bool",
>         CatCreateBinaryOp ">=" "time" "time" "bool",
>         CatCreateBinaryOp ">=" "anyenum" "anyenum" "bool",
>         CatCreateBinaryOp ">=" "varbit" "varbit" "bool",
>         CatCreateBinaryOp ">=" "abstime" "abstime" "bool",
>         CatCreateBinaryOp ">=" "bytea" "bytea" "bool",
>         CatCreateBinaryOp ">=" "timetz" "timetz" "bool",
>         CatCreateBinaryOp ">=" "text" "text" "bool",
>         CatCreateBinaryOp ">=" "name" "name" "bool",
>         CatCreateBinaryOp ">=" "macaddr" "macaddr" "bool",
>         CatCreateBinaryOp ">=" "reltime" "reltime" "bool",
>         CatCreateBinaryOp ">=" "timestamp" "timestamp" "bool",
>         CatCreateBinaryOp ">=" "anyarray" "anyarray" "bool",
>         CatCreateBinaryOp ">=" "int4" "int8" "bool",
>         CatCreateBinaryOp ">=" "box" "box" "bool",
>         CatCreateBinaryOp ">=" "oid" "oid" "bool",
>         CatCreateBinaryOp ">>" "int4" "int4" "int4",
>         CatCreateBinaryOp ">>" "point" "point" "bool",
>         CatCreateBinaryOp ">>" "bit" "int4" "bit",
>         CatCreateBinaryOp ">>" "inet" "inet" "bool",
>         CatCreateBinaryOp ">>" "circle" "circle" "bool",
>         CatCreateBinaryOp ">>" "box" "box" "bool",
>         CatCreateBinaryOp ">>" "int8" "int4" "int8",
>         CatCreateBinaryOp ">>" "polygon" "polygon" "bool",
>         CatCreateBinaryOp ">>" "int2" "int4" "int2",
>         CatCreateBinaryOp ">>=" "inet" "inet" "bool",
>         CatCreateBinaryOp ">^" "point" "point" "bool",
>         CatCreateBinaryOp ">^" "box" "box" "bool",
>         CatCreateBinaryOp "?#" "lseg" "line" "bool",
>         CatCreateBinaryOp "?#" "box" "box" "bool",
>         CatCreateBinaryOp "?#" "line" "line" "bool",
>         CatCreateBinaryOp "?#" "path" "path" "bool",
>         CatCreateBinaryOp "?#" "lseg" "box" "bool",
>         CatCreateBinaryOp "?#" "line" "box" "bool",
>         CatCreateBinaryOp "?#" "lseg" "lseg" "bool",
>         CatCreateBinaryOp "?-" "point" "point" "bool",
>         CatCreateBinaryOp "?-|" "lseg" "lseg" "bool",
>         CatCreateBinaryOp "?-|" "line" "line" "bool",
>         CatCreateBinaryOp "?|" "point" "point" "bool",
>         CatCreateBinaryOp "?||" "line" "line" "bool",
>         CatCreateBinaryOp "?||" "lseg" "lseg" "bool",
>         CatCreateBinaryOp "@>" "box" "point" "bool",
>         CatCreateBinaryOp "@>" "path" "point" "bool",
>         CatCreateBinaryOp "@>" "polygon" "point" "bool",
>         CatCreateBinaryOp "@>" "circle" "point" "bool",
>         CatCreateBinaryOp "@>" "box" "box" "bool",
>         CatCreateBinaryOp "@>" "tsquery" "tsquery" "bool",
>         CatCreateBinaryOp "@>" "polygon" "polygon" "bool",
>         CatCreateBinaryOp "@>" "circle" "circle" "bool",
>         CatCreateBinaryOp "@>" "anyarray" "anyarray" "bool",
>         CatCreateBinaryOp "@>" "_aclitem" "aclitem" "bool",
>         CatCreateBinaryOp "@@" "text" "tsquery" "bool",
>         CatCreateBinaryOp "@@" "tsvector" "tsquery" "bool",
>         CatCreateBinaryOp "@@" "text" "text" "bool",
>         CatCreateBinaryOp "@@" "tsquery" "tsvector" "bool",
>         CatCreateBinaryOp "@@@" "tsvector" "tsquery" "bool",
>         CatCreateBinaryOp "@@@" "tsquery" "tsvector" "bool",
>         CatCreateBinaryOp "^" "float8" "float8" "float8",
>         CatCreateBinaryOp "^" "numeric" "numeric" "numeric",
>         CatCreateBinaryOp "|" "bit" "bit" "bit",
>         CatCreateBinaryOp "|" "int2" "int2" "int2",
>         CatCreateBinaryOp "|" "int4" "int4" "int4",
>         CatCreateBinaryOp "|" "int8" "int8" "int8",
>         CatCreateBinaryOp "|" "inet" "inet" "inet",
>         CatCreateBinaryOp "|&>" "box" "box" "bool",
>         CatCreateBinaryOp "|&>" "polygon" "polygon" "bool",
>         CatCreateBinaryOp "|&>" "circle" "circle" "bool",
>         CatCreateBinaryOp "|>>" "circle" "circle" "bool",
>         CatCreateBinaryOp "|>>" "box" "box" "bool",
>         CatCreateBinaryOp "|>>" "polygon" "polygon" "bool",
>         CatCreateBinaryOp "||" "anyarray" "anyelement" "anyarray",
>         CatCreateBinaryOp "||" "anyarray" "anyarray" "anyarray",
>         CatCreateBinaryOp "||" "anyelement" "anyarray" "anyarray",
>         CatCreateBinaryOp "||" "text" "anynonarray" "text",
>         CatCreateBinaryOp "||" "anynonarray" "text" "text",
>         CatCreateBinaryOp "||" "tsquery" "tsquery" "tsquery",
>         CatCreateBinaryOp "||" "text" "text" "text",
>         CatCreateBinaryOp "||" "varbit" "varbit" "varbit",
>         CatCreateBinaryOp "||" "bytea" "bytea" "bytea",
>         CatCreateBinaryOp "||" "tsvector" "tsvector" "tsvector",
>         CatCreateBinaryOp "~" "polygon" "point" "bool",
>         CatCreateBinaryOp "~" "path" "point" "bool",
>         CatCreateBinaryOp "~" "polygon" "polygon" "bool",
>         CatCreateBinaryOp "~" "text" "text" "bool",
>         CatCreateBinaryOp "~" "name" "text" "bool",
>         CatCreateBinaryOp "~" "circle" "point" "bool",
>         CatCreateBinaryOp "~" "circle" "circle" "bool",
>         CatCreateBinaryOp "~" "box" "box" "bool",
>         CatCreateBinaryOp "~" "bpchar" "text" "bool",
>         CatCreateBinaryOp "~" "_aclitem" "aclitem" "bool",
>         CatCreateBinaryOp "~*" "bpchar" "text" "bool",
>         CatCreateBinaryOp "~*" "name" "text" "bool",
>         CatCreateBinaryOp "~*" "text" "text" "bool",
>         CatCreateBinaryOp "~<=~" "bpchar" "bpchar" "bool",
>         CatCreateBinaryOp "~<=~" "text" "text" "bool",
>         CatCreateBinaryOp "~<~" "bpchar" "bpchar" "bool",
>         CatCreateBinaryOp "~<~" "text" "text" "bool",
>         CatCreateBinaryOp "~=" "circle" "circle" "bool",
>         CatCreateBinaryOp "~=" "polygon" "polygon" "bool",
>         CatCreateBinaryOp "~=" "point" "point" "bool",
>         CatCreateBinaryOp "~=" "tinterval" "tinterval" "bool",
>         CatCreateBinaryOp "~=" "box" "box" "bool",
>         CatCreateBinaryOp "~>=~" "bpchar" "bpchar" "bool",
>         CatCreateBinaryOp "~>=~" "text" "text" "bool",
>         CatCreateBinaryOp "~>~" "text" "text" "bool",
>         CatCreateBinaryOp "~>~" "bpchar" "bpchar" "bool",
>         CatCreateBinaryOp "~~" "bytea" "bytea" "bool",
>         CatCreateBinaryOp "~~" "name" "text" "bool",
>         CatCreateBinaryOp "~~" "text" "text" "bool",
>         CatCreateBinaryOp "~~" "bpchar" "text" "bool",
>         CatCreateBinaryOp "~~*" "bpchar" "text" "bool",
>         CatCreateBinaryOp "~~*" "text" "text" "bool",
>         CatCreateBinaryOp "~~*" "name" "text" "bool",
>         CatCreateFunction "byteaout" ["bytea"] False "cstring",
>         CatCreateFunction "charout" ["char"] False "cstring",
>         CatCreateFunction "namein" ["cstring"] False "name",
>         CatCreateFunction "nameout" ["name"] False "cstring",
>         CatCreateFunction "int2in" ["cstring"] False "int2",
>         CatCreateFunction "int2out" ["int2"] False "cstring",
>         CatCreateFunction "int2vectorin" ["cstring"] False "int2vector",
>         CatCreateFunction "int2vectorout" ["int2vector"] False "cstring",
>         CatCreateFunction "int4in" ["cstring"] False "int4",
>         CatCreateFunction "int4out" ["int4"] False "cstring",
>         CatCreateFunction "regprocin" ["cstring"] False "regproc",
>         CatCreateFunction "regprocout" ["regproc"] False "cstring",
>         CatCreateFunction "textin" ["cstring"] False "text",
>         CatCreateFunction "textout" ["text"] False "cstring",
>         CatCreateFunction "tidin" ["cstring"] False "tid",
>         CatCreateFunction "tidout" ["tid"] False "cstring",
>         CatCreateFunction "xidin" ["cstring"] False "xid",
>         CatCreateFunction "xidout" ["xid"] False "cstring",
>         CatCreateFunction "cidin" ["cstring"] False "cid",
>         CatCreateFunction "cidout" ["cid"] False "cstring",
>         CatCreateFunction "oidvectorin" ["cstring"] False "oidvector",
>         CatCreateFunction "oidvectorout" ["oidvector"] False "cstring",
>         CatCreateFunction "boollt" ["bool", "bool"] False "bool",
>         CatCreateFunction "boolgt" ["bool", "bool"] False "bool",
>         CatCreateFunction "booleq" ["bool", "bool"] False "bool",
>         CatCreateFunction "chareq" ["char", "char"] False "bool",
>         CatCreateFunction "nameeq" ["name", "name"] False "bool",
>         CatCreateFunction "int2eq" ["int2", "int2"] False "bool",
>         CatCreateFunction "int2lt" ["int2", "int2"] False "bool",
>         CatCreateFunction "int4eq" ["int4", "int4"] False "bool",
>         CatCreateFunction "int4lt" ["int4", "int4"] False "bool",
>         CatCreateFunction "texteq" ["text", "text"] False "bool",
>         CatCreateFunction "xideq" ["xid", "xid"] False "bool",
>         CatCreateFunction "cideq" ["cid", "cid"] False "bool",
>         CatCreateFunction "charne" ["char", "char"] False "bool",
>         CatCreateFunction "charle" ["char", "char"] False "bool",
>         CatCreateFunction "chargt" ["char", "char"] False "bool",
>         CatCreateFunction "charge" ["char", "char"] False "bool",
>         CatCreateFunction "int4" ["char"] False "int4",
>         CatCreateFunction "char" ["int4"] False "char",
>         CatCreateFunction "nameregexeq" ["text", "name"] False "bool",
>         CatCreateFunction "boolne" ["bool", "bool"] False "bool",
>         CatCreateFunction "eqsel" ["internal", "oid", "internal", "int4"]
>           False
>           "float8",
>         CatCreateFunction "neqsel" ["internal", "oid", "internal", "int4"]
>           False
>           "float8",
>         CatCreateFunction "scalarltsel"
>           ["internal", "oid", "internal", "int4"]
>           False
>           "float8",
>         CatCreateFunction "scalargtsel"
>           ["internal", "oid", "internal", "int4"]
>           False
>           "float8",
>         CatCreateFunction "eqjoinsel"
>           ["internal", "oid", "internal", "int2", "internal"]
>           False
>           "float8",
>         CatCreateFunction "neqjoinsel"
>           ["internal", "oid", "internal", "int2", "internal"]
>           False
>           "float8",
>         CatCreateFunction "scalarltjoinsel"
>           ["internal", "oid", "internal", "int2", "internal"]
>           False
>           "float8",
>         CatCreateFunction "scalargtjoinsel"
>           ["internal", "oid", "internal", "int2", "internal"]
>           False
>           "float8",
>         CatCreateFunction "unknownin" ["cstring"] False "unknown",
>         CatCreateFunction "unknownout" ["unknown"] False "cstring",
>         CatCreateFunction "numeric_fac" ["int8"] False "numeric",
>         CatCreateFunction "box_above_eq" ["box", "box"] False "bool",
>         CatCreateFunction "box_below_eq" ["box", "box"] False "bool",
>         CatCreateFunction "point_in" ["cstring"] False "point",
>         CatCreateFunction "point_out" ["point"] False "cstring",
>         CatCreateFunction "lseg_in" ["cstring"] False "lseg",
>         CatCreateFunction "lseg_out" ["lseg"] False "cstring",
>         CatCreateFunction "path_in" ["cstring"] False "path",
>         CatCreateFunction "path_out" ["path"] False "cstring",
>         CatCreateFunction "box_in" ["cstring"] False "box",
>         CatCreateFunction "box_out" ["box"] False "cstring",
>         CatCreateFunction "box_overlap" ["box", "box"] False "bool",
>         CatCreateFunction "box_ge" ["box", "box"] False "bool",
>         CatCreateFunction "box_gt" ["box", "box"] False "bool",
>         CatCreateFunction "box_eq" ["box", "box"] False "bool",
>         CatCreateFunction "box_lt" ["box", "box"] False "bool",
>         CatCreateFunction "box_le" ["box", "box"] False "bool",
>         CatCreateFunction "point_above" ["point", "point"] False "bool",
>         CatCreateFunction "point_left" ["point", "point"] False "bool",
>         CatCreateFunction "point_right" ["point", "point"] False "bool",
>         CatCreateFunction "point_below" ["point", "point"] False "bool",
>         CatCreateFunction "point_eq" ["point", "point"] False "bool",
>         CatCreateFunction "on_pb" ["point", "box"] False "bool",
>         CatCreateFunction "on_ppath" ["point", "path"] False "bool",
>         CatCreateFunction "box_center" ["box"] False "point",
>         CatCreateFunction "areasel" ["internal", "oid", "internal", "int4"]
>           False
>           "float8",
>         CatCreateFunction "areajoinsel"
>           ["internal", "oid", "internal", "int2", "internal"]
>           False
>           "float8",
>         CatCreateFunction "int4mul" ["int4", "int4"] False "int4",
>         CatCreateFunction "int4ne" ["int4", "int4"] False "bool",
>         CatCreateFunction "int2ne" ["int2", "int2"] False "bool",
>         CatCreateFunction "int2gt" ["int2", "int2"] False "bool",
>         CatCreateFunction "int4gt" ["int4", "int4"] False "bool",
>         CatCreateFunction "int2le" ["int2", "int2"] False "bool",
>         CatCreateFunction "int4le" ["int4", "int4"] False "bool",
>         CatCreateFunction "int4ge" ["int4", "int4"] False "bool",
>         CatCreateFunction "int2ge" ["int2", "int2"] False "bool",
>         CatCreateFunction "int2mul" ["int2", "int2"] False "int2",
>         CatCreateFunction "int2div" ["int2", "int2"] False "int2",
>         CatCreateFunction "int4div" ["int4", "int4"] False "int4",
>         CatCreateFunction "int2mod" ["int2", "int2"] False "int2",
>         CatCreateFunction "int4mod" ["int4", "int4"] False "int4",
>         CatCreateFunction "textne" ["text", "text"] False "bool",
>         CatCreateFunction "int24eq" ["int2", "int4"] False "bool",
>         CatCreateFunction "int42eq" ["int4", "int2"] False "bool",
>         CatCreateFunction "int24lt" ["int2", "int4"] False "bool",
>         CatCreateFunction "int42lt" ["int4", "int2"] False "bool",
>         CatCreateFunction "int24gt" ["int2", "int4"] False "bool",
>         CatCreateFunction "int42gt" ["int4", "int2"] False "bool",
>         CatCreateFunction "int24ne" ["int2", "int4"] False "bool",
>         CatCreateFunction "int42ne" ["int4", "int2"] False "bool",
>         CatCreateFunction "int24le" ["int2", "int4"] False "bool",
>         CatCreateFunction "int42le" ["int4", "int2"] False "bool",
>         CatCreateFunction "int24ge" ["int2", "int4"] False "bool",
>         CatCreateFunction "int42ge" ["int4", "int2"] False "bool",
>         CatCreateFunction "int24mul" ["int2", "int4"] False "int4",
>         CatCreateFunction "int42mul" ["int4", "int2"] False "int4",
>         CatCreateFunction "int24div" ["int2", "int4"] False "int4",
>         CatCreateFunction "int42div" ["int4", "int2"] False "int4",
>         CatCreateFunction "int2pl" ["int2", "int2"] False "int2",
>         CatCreateFunction "int4pl" ["int4", "int4"] False "int4",
>         CatCreateFunction "int24pl" ["int2", "int4"] False "int4",
>         CatCreateFunction "int42pl" ["int4", "int2"] False "int4",
>         CatCreateFunction "int2mi" ["int2", "int2"] False "int2",
>         CatCreateFunction "int4mi" ["int4", "int4"] False "int4",
>         CatCreateFunction "int24mi" ["int2", "int4"] False "int4",
>         CatCreateFunction "int42mi" ["int4", "int2"] False "int4",
>         CatCreateFunction "oideq" ["oid", "oid"] False "bool",
>         CatCreateFunction "oidne" ["oid", "oid"] False "bool",
>         CatCreateFunction "box_same" ["box", "box"] False "bool",
>         CatCreateFunction "box_contain" ["box", "box"] False "bool",
>         CatCreateFunction "box_left" ["box", "box"] False "bool",
>         CatCreateFunction "box_overleft" ["box", "box"] False "bool",
>         CatCreateFunction "box_overright" ["box", "box"] False "bool",
>         CatCreateFunction "box_right" ["box", "box"] False "bool",
>         CatCreateFunction "box_contained" ["box", "box"] False "bool",
>         CatCreateFunction "box_contain_pt" ["box", "point"] False "bool",
>         CatCreateFunction "pg_node_tree_in" ["cstring"] False
>           "pg_node_tree",
>         CatCreateFunction "pg_node_tree_out" ["pg_node_tree"] False
>           "cstring",
>         CatCreateFunction "pg_node_tree_recv" ["internal"] False
>           "pg_node_tree",
>         CatCreateFunction "pg_node_tree_send" ["pg_node_tree"] False
>           "bytea",
>         CatCreateFunction "float4in" ["cstring"] False "float4",
>         CatCreateFunction "float4out" ["float4"] False "cstring",
>         CatCreateFunction "float4mul" ["float4", "float4"] False "float4",
>         CatCreateFunction "float4div" ["float4", "float4"] False "float4",
>         CatCreateFunction "float4pl" ["float4", "float4"] False "float4",
>         CatCreateFunction "float4mi" ["float4", "float4"] False "float4",
>         CatCreateFunction "float4um" ["float4"] False "float4",
>         CatCreateFunction "float4abs" ["float4"] False "float4",
>         CatCreateFunction "float4_accum" ["_float8", "float4"] False
>           "_float8",
>         CatCreateFunction "float4larger" ["float4", "float4"] False
>           "float4",
>         CatCreateFunction "float4smaller" ["float4", "float4"] False
>           "float4",
>         CatCreateFunction "int4um" ["int4"] False "int4",
>         CatCreateFunction "int2um" ["int2"] False "int2",
>         CatCreateFunction "float8in" ["cstring"] False "float8",
>         CatCreateFunction "float8out" ["float8"] False "cstring",
>         CatCreateFunction "float8mul" ["float8", "float8"] False "float8",
>         CatCreateFunction "float8div" ["float8", "float8"] False "float8",
>         CatCreateFunction "float8pl" ["float8", "float8"] False "float8",
>         CatCreateFunction "float8mi" ["float8", "float8"] False "float8",
>         CatCreateFunction "float8um" ["float8"] False "float8",
>         CatCreateFunction "float8abs" ["float8"] False "float8",
>         CatCreateFunction "float8_accum" ["_float8", "float8"] False
>           "_float8",
>         CatCreateFunction "float8larger" ["float8", "float8"] False
>           "float8",
>         CatCreateFunction "float8smaller" ["float8", "float8"] False
>           "float8",
>         CatCreateFunction "lseg_center" ["lseg"] False "point",
>         CatCreateFunction "path_center" ["path"] False "point",
>         CatCreateFunction "poly_center" ["polygon"] False "point",
>         CatCreateFunction "dround" ["float8"] False "float8",
>         CatCreateFunction "dtrunc" ["float8"] False "float8",
>         CatCreateFunction "dsqrt" ["float8"] False "float8",
>         CatCreateFunction "dcbrt" ["float8"] False "float8",
>         CatCreateFunction "dpow" ["float8", "float8"] False "float8",
>         CatCreateFunction "dexp" ["float8"] False "float8",
>         CatCreateFunction "dlog1" ["float8"] False "float8",
>         CatCreateFunction "float8" ["int2"] False "float8",
>         CatCreateFunction "float4" ["int2"] False "float4",
>         CatCreateFunction "int2" ["float8"] False "int2",
>         CatCreateFunction "int2" ["float4"] False "int2",
>         CatCreateFunction "line_distance" ["line", "line"] False "float8",
>         CatCreateFunction "abstimein" ["cstring"] False "abstime",
>         CatCreateFunction "abstimeout" ["abstime"] False "cstring",
>         CatCreateFunction "reltimein" ["cstring"] False "reltime",
>         CatCreateFunction "reltimeout" ["reltime"] False "cstring",
>         CatCreateFunction "timepl" ["abstime", "reltime"] False "abstime",
>         CatCreateFunction "timemi" ["abstime", "reltime"] False "abstime",
>         CatCreateFunction "tintervalin" ["cstring"] False "tinterval",
>         CatCreateFunction "tintervalout" ["tinterval"] False "cstring",
>         CatCreateFunction "intinterval" ["abstime", "tinterval"] False
>           "bool",
>         CatCreateFunction "tintervalrel" ["tinterval"] False "reltime",
>         CatCreateFunction "abstimeeq" ["abstime", "abstime"] False "bool",
>         CatCreateFunction "abstimene" ["abstime", "abstime"] False "bool",
>         CatCreateFunction "abstimelt" ["abstime", "abstime"] False "bool",
>         CatCreateFunction "abstimegt" ["abstime", "abstime"] False "bool",
>         CatCreateFunction "abstimele" ["abstime", "abstime"] False "bool",
>         CatCreateFunction "abstimege" ["abstime", "abstime"] False "bool",
>         CatCreateFunction "reltimeeq" ["reltime", "reltime"] False "bool",
>         CatCreateFunction "reltimene" ["reltime", "reltime"] False "bool",
>         CatCreateFunction "reltimelt" ["reltime", "reltime"] False "bool",
>         CatCreateFunction "reltimegt" ["reltime", "reltime"] False "bool",
>         CatCreateFunction "reltimele" ["reltime", "reltime"] False "bool",
>         CatCreateFunction "reltimege" ["reltime", "reltime"] False "bool",
>         CatCreateFunction "tintervalsame" ["tinterval", "tinterval"] False
>           "bool",
>         CatCreateFunction "tintervalct" ["tinterval", "tinterval"] False
>           "bool",
>         CatCreateFunction "tintervalov" ["tinterval", "tinterval"] False
>           "bool",
>         CatCreateFunction "tintervalleneq" ["tinterval", "reltime"] False
>           "bool",
>         CatCreateFunction "tintervallenne" ["tinterval", "reltime"] False
>           "bool",
>         CatCreateFunction "tintervallenlt" ["tinterval", "reltime"] False
>           "bool",
>         CatCreateFunction "tintervallengt" ["tinterval", "reltime"] False
>           "bool",
>         CatCreateFunction "tintervallenle" ["tinterval", "reltime"] False
>           "bool",
>         CatCreateFunction "tintervallenge" ["tinterval", "reltime"] False
>           "bool",
>         CatCreateFunction "tintervalstart" ["tinterval"] False "abstime",
>         CatCreateFunction "tintervalend" ["tinterval"] False "abstime",
>         CatCreateFunction "isfinite" ["abstime"] False "bool",
>         CatCreateFunction "inter_sl" ["lseg", "line"] False "bool",
>         CatCreateFunction "inter_lb" ["line", "box"] False "bool",
>         CatCreateFunction "float48mul" ["float4", "float8"] False "float8",
>         CatCreateFunction "float48div" ["float4", "float8"] False "float8",
>         CatCreateFunction "float48pl" ["float4", "float8"] False "float8",
>         CatCreateFunction "float48mi" ["float4", "float8"] False "float8",
>         CatCreateFunction "float84mul" ["float8", "float4"] False "float8",
>         CatCreateFunction "float84div" ["float8", "float4"] False "float8",
>         CatCreateFunction "float84pl" ["float8", "float4"] False "float8",
>         CatCreateFunction "float84mi" ["float8", "float4"] False "float8",
>         CatCreateFunction "float4eq" ["float4", "float4"] False "bool",
>         CatCreateFunction "float4ne" ["float4", "float4"] False "bool",
>         CatCreateFunction "float4lt" ["float4", "float4"] False "bool",
>         CatCreateFunction "float4le" ["float4", "float4"] False "bool",
>         CatCreateFunction "float4gt" ["float4", "float4"] False "bool",
>         CatCreateFunction "float4ge" ["float4", "float4"] False "bool",
>         CatCreateFunction "float8eq" ["float8", "float8"] False "bool",
>         CatCreateFunction "float8ne" ["float8", "float8"] False "bool",
>         CatCreateFunction "float8lt" ["float8", "float8"] False "bool",
>         CatCreateFunction "float8le" ["float8", "float8"] False "bool",
>         CatCreateFunction "float8gt" ["float8", "float8"] False "bool",
>         CatCreateFunction "float8ge" ["float8", "float8"] False "bool",
>         CatCreateFunction "float48eq" ["float4", "float8"] False "bool",
>         CatCreateFunction "float48ne" ["float4", "float8"] False "bool",
>         CatCreateFunction "float48lt" ["float4", "float8"] False "bool",
>         CatCreateFunction "float48le" ["float4", "float8"] False "bool",
>         CatCreateFunction "float48gt" ["float4", "float8"] False "bool",
>         CatCreateFunction "float48ge" ["float4", "float8"] False "bool",
>         CatCreateFunction "float84eq" ["float8", "float4"] False "bool",
>         CatCreateFunction "float84ne" ["float8", "float4"] False "bool",
>         CatCreateFunction "float84lt" ["float8", "float4"] False "bool",
>         CatCreateFunction "float84le" ["float8", "float4"] False "bool",
>         CatCreateFunction "float84gt" ["float8", "float4"] False "bool",
>         CatCreateFunction "float84ge" ["float8", "float4"] False "bool",
>         CatCreateFunction "float8" ["float4"] False "float8",
>         CatCreateFunction "float4" ["float8"] False "float4",
>         CatCreateFunction "int4" ["int2"] False "int4",
>         CatCreateFunction "int2" ["int4"] False "int2",
>         CatCreateFunction "int2vectoreq" ["int2vector", "int2vector"] False
>           "bool",
>         CatCreateFunction "float8" ["int4"] False "float8",
>         CatCreateFunction "int4" ["float8"] False "int4",
>         CatCreateFunction "float4" ["int4"] False "float4",
>         CatCreateFunction "int4" ["float4"] False "int4",
>         CatCreateFunction "width_bucket"
>           ["float8", "float8", "float8", "int4"]
>           False
>           "int4",
>         CatCreateFunction "ginbuildempty" ["internal"] False "void",
>         CatCreateFunction "gistbuildempty" ["internal"] False "void",
>         CatCreateFunction "hashbuildempty" ["internal"] False "void",
>         CatCreateFunction "btbuildempty" ["internal"] False "void",
>         CatCreateFunction "hash_aclitem" ["aclitem"] False "int4",
>         CatCreateFunction "btgettuple" ["internal", "internal"] False
>           "bool",
>         CatCreateFunction "btinsert"
>           ["internal", "internal", "internal", "internal", "internal",
>            "internal"]
>           False
>           "bool",
>         CatCreateFunction "btbulkdelete"
>           ["internal", "internal", "internal", "internal"]
>           False
>           "internal",
>         CatCreateFunction "btbeginscan"
>           ["internal", "internal", "internal"]
>           False
>           "internal",
>         CatCreateFunction "btrescan"
>           ["internal", "internal", "internal", "internal", "internal"]
>           False
>           "void",
>         CatCreateFunction "btendscan" ["internal"] False "void",
>         CatCreateFunction "btmarkpos" ["internal"] False "void",
>         CatCreateFunction "btrestrpos" ["internal"] False "void",
>         CatCreateFunction "btbuild" ["internal", "internal", "internal"]
>           False
>           "internal",
>         CatCreateFunction "poly_same" ["polygon", "polygon"] False "bool",
>         CatCreateFunction "poly_contain" ["polygon", "polygon"] False
>           "bool",
>         CatCreateFunction "poly_left" ["polygon", "polygon"] False "bool",
>         CatCreateFunction "poly_overleft" ["polygon", "polygon"] False
>           "bool",
>         CatCreateFunction "poly_overright" ["polygon", "polygon"] False
>           "bool",
>         CatCreateFunction "poly_right" ["polygon", "polygon"] False "bool",
>         CatCreateFunction "poly_contained" ["polygon", "polygon"] False
>           "bool",
>         CatCreateFunction "poly_overlap" ["polygon", "polygon"] False
>           "bool",
>         CatCreateFunction "poly_in" ["cstring"] False "polygon",
>         CatCreateFunction "poly_out" ["polygon"] False "cstring",
>         CatCreateFunction "btint2cmp" ["int2", "int2"] False "int4",
>         CatCreateFunction "btint4cmp" ["int4", "int4"] False "int4",
>         CatCreateFunction "btfloat4cmp" ["float4", "float4"] False "int4",
>         CatCreateFunction "btfloat8cmp" ["float8", "float8"] False "int4",
>         CatCreateFunction "btoidcmp" ["oid", "oid"] False "int4",
>         CatCreateFunction "btabstimecmp" ["abstime", "abstime"] False
>           "int4",
>         CatCreateFunction "btcharcmp" ["char", "char"] False "int4",
>         CatCreateFunction "btnamecmp" ["name", "name"] False "int4",
>         CatCreateFunction "bttextcmp" ["text", "text"] False "int4",
>         CatCreateFunction "lseg_distance" ["lseg", "lseg"] False "float8",
>         CatCreateFunction "lseg_interpt" ["lseg", "lseg"] False "point",
>         CatCreateFunction "dist_ps" ["point", "lseg"] False "float8",
>         CatCreateFunction "dist_pb" ["point", "box"] False "float8",
>         CatCreateFunction "dist_sb" ["lseg", "box"] False "float8",
>         CatCreateFunction "close_ps" ["point", "lseg"] False "point",
>         CatCreateFunction "close_pb" ["point", "box"] False "point",
>         CatCreateFunction "close_sb" ["lseg", "box"] False "point",
>         CatCreateFunction "on_ps" ["point", "lseg"] False "bool",
>         CatCreateFunction "path_distance" ["path", "path"] False "float8",
>         CatCreateFunction "dist_ppath" ["point", "path"] False "float8",
>         CatCreateFunction "on_sb" ["lseg", "box"] False "bool",
>         CatCreateFunction "inter_sb" ["lseg", "box"] False "bool",
>         CatCreateFunction "string_to_array" ["text", "text", "text"] False
>           "_text",
>         CatCreateFunction "cash_cmp" ["money", "money"] False "int4",
>         CatCreateFunction "array_append" ["anyarray", "anyelement"] False
>           "anyarray",
>         CatCreateFunction "array_prepend" ["anyelement", "anyarray"] False
>           "anyarray",
>         CatCreateFunction "btreltimecmp" ["reltime", "reltime"] False
>           "int4",
>         CatCreateFunction "bttintervalcmp" ["tinterval", "tinterval"] False
>           "int4",
>         CatCreateFunction "btarraycmp" ["anyarray", "anyarray"] False
>           "int4",
>         CatCreateFunction "array_cat" ["anyarray", "anyarray"] False
>           "anyarray",
>         CatCreateFunction "array_to_string" ["anyarray", "text", "text"]
>           False
>           "text",
>         CatCreateFunction "array_ne" ["anyarray", "anyarray"] False "bool",
>         CatCreateFunction "array_lt" ["anyarray", "anyarray"] False "bool",
>         CatCreateFunction "array_gt" ["anyarray", "anyarray"] False "bool",
>         CatCreateFunction "array_le" ["anyarray", "anyarray"] False "bool",
>         CatCreateFunction "string_to_array" ["text", "text"] False "_text",
>         CatCreateFunction "array_to_string" ["anyarray", "text"] False
>           "text",
>         CatCreateFunction "array_ge" ["anyarray", "anyarray"] False "bool",
>         CatCreateFunction "hashint2vector" ["int2vector"] False "int4",
>         CatCreateFunction "hashmacaddr" ["macaddr"] False "int4",
>         CatCreateFunction "hashtext" ["text"] False "int4",
>         CatCreateFunction "text" ["bpchar"] False "text",
>         CatCreateFunction "btoidvectorcmp" ["oidvector", "oidvector"] False
>           "int4",
>         CatCreateFunction "text" ["name"] False "text",
>         CatCreateFunction "name" ["text"] False "name",
>         CatCreateFunction "bpchar" ["name"] False "bpchar",
>         CatCreateFunction "name" ["bpchar"] False "name",
>         CatCreateFunction "hashinet" ["inet"] False "int4",
>         CatCreateFunction "hashvacuumcleanup" ["internal", "internal"]
>           False
>           "internal",
>         CatCreateFunction "hash_numeric" ["numeric"] False "int4",
>         CatCreateFunction "macaddr_in" ["cstring"] False "macaddr",
>         CatCreateFunction "macaddr_out" ["macaddr"] False "cstring",
>         CatCreateFunction "hashcostestimate"
>           ["internal", "internal", "internal", "internal", "internal",
>            "internal", "internal", "internal", "internal"]
>           False
>           "void",
>         CatCreateFunction "hashgettuple" ["internal", "internal"] False
>           "bool",
>         CatCreateFunction "hashinsert"
>           ["internal", "internal", "internal", "internal", "internal",
>            "internal"]
>           False
>           "bool",
>         CatCreateFunction "hashbulkdelete"
>           ["internal", "internal", "internal", "internal"]
>           False
>           "internal",
>         CatCreateFunction "hashbeginscan"
>           ["internal", "internal", "internal"]
>           False
>           "internal",
>         CatCreateFunction "hashrescan"
>           ["internal", "internal", "internal", "internal", "internal"]
>           False
>           "void",
>         CatCreateFunction "hashendscan" ["internal"] False "void",
>         CatCreateFunction "hashmarkpos" ["internal"] False "void",
>         CatCreateFunction "hashrestrpos" ["internal"] False "void",
>         CatCreateFunction "hashbuild" ["internal", "internal", "internal"]
>           False
>           "internal",
>         CatCreateFunction "hashint2" ["int2"] False "int4",
>         CatCreateFunction "hashint4" ["int4"] False "int4",
>         CatCreateFunction "hashfloat4" ["float4"] False "int4",
>         CatCreateFunction "hashfloat8" ["float8"] False "int4",
>         CatCreateFunction "hashoid" ["oid"] False "int4",
>         CatCreateFunction "hashchar" ["char"] False "int4",
>         CatCreateFunction "hashname" ["name"] False "int4",
>         CatCreateFunction "hashvarlena" ["internal"] False "int4",
>         CatCreateFunction "hashoidvector" ["oidvector"] False "int4",
>         CatCreateFunction "text_larger" ["text", "text"] False "text",
>         CatCreateFunction "text_smaller" ["text", "text"] False "text",
>         CatCreateFunction "int8in" ["cstring"] False "int8",
>         CatCreateFunction "int8out" ["int8"] False "cstring",
>         CatCreateFunction "int8um" ["int8"] False "int8",
>         CatCreateFunction "int8pl" ["int8", "int8"] False "int8",
>         CatCreateFunction "int8mi" ["int8", "int8"] False "int8",
>         CatCreateFunction "int8mul" ["int8", "int8"] False "int8",
>         CatCreateFunction "int8div" ["int8", "int8"] False "int8",
>         CatCreateFunction "int8eq" ["int8", "int8"] False "bool",
>         CatCreateFunction "int8ne" ["int8", "int8"] False "bool",
>         CatCreateFunction "int8lt" ["int8", "int8"] False "bool",
>         CatCreateFunction "int8gt" ["int8", "int8"] False "bool",
>         CatCreateFunction "int8le" ["int8", "int8"] False "bool",
>         CatCreateFunction "int8ge" ["int8", "int8"] False "bool",
>         CatCreateFunction "int84eq" ["int8", "int4"] False "bool",
>         CatCreateFunction "int84ne" ["int8", "int4"] False "bool",
>         CatCreateFunction "int84lt" ["int8", "int4"] False "bool",
>         CatCreateFunction "int84gt" ["int8", "int4"] False "bool",
>         CatCreateFunction "int84le" ["int8", "int4"] False "bool",
>         CatCreateFunction "int84ge" ["int8", "int4"] False "bool",
>         CatCreateFunction "int4" ["int8"] False "int4",
>         CatCreateFunction "int8" ["int4"] False "int8",
>         CatCreateFunction "float8" ["int8"] False "float8",
>         CatCreateFunction "int8" ["float8"] False "int8",
>         CatCreateFunction "array_larger" ["anyarray", "anyarray"] False
>           "anyarray",
>         CatCreateFunction "array_smaller" ["anyarray", "anyarray"] False
>           "anyarray",
>         CatCreateFunction "abbrev" ["inet"] False "text",
>         CatCreateFunction "abbrev" ["cidr"] False "text",
>         CatCreateFunction "set_masklen" ["inet", "int4"] False "inet",
>         CatCreateFunction "oidvectorne" ["oidvector", "oidvector"] False
>           "bool",
>         CatCreateFunction "hash_array" ["anyarray"] False "int4",
>         CatCreateFunction "set_masklen" ["cidr", "int4"] False "cidr",
>         CatCreateFunction "btgetbitmap" ["internal", "internal"] False
>           "int8",
>         CatCreateFunction "hashgetbitmap" ["internal", "internal"] False
>           "int8",
>         CatCreateFunction "gistgetbitmap" ["internal", "internal"] False
>           "int8",
>         CatCreateFunction "float4" ["int8"] False "float4",
>         CatCreateFunction "int8" ["float4"] False "int8",
>         CatCreateFunction "namelt" ["name", "name"] False "bool",
>         CatCreateFunction "namele" ["name", "name"] False "bool",
>         CatCreateFunction "namegt" ["name", "name"] False "bool",
>         CatCreateFunction "namege" ["name", "name"] False "bool",
>         CatCreateFunction "namene" ["name", "name"] False "bool",
>         CatCreateFunction "bpchar" ["bpchar", "int4", "bool"] False
>           "bpchar",
>         CatCreateFunction "varchar" ["varchar", "int4", "bool"] False
>           "varchar",
>         CatCreateFunction "mktinterval" ["abstime", "abstime"] False
>           "tinterval",
>         CatCreateFunction "oidvectorlt" ["oidvector", "oidvector"] False
>           "bool",
>         CatCreateFunction "oidvectorle" ["oidvector", "oidvector"] False
>           "bool",
>         CatCreateFunction "oidvectoreq" ["oidvector", "oidvector"] False
>           "bool",
>         CatCreateFunction "oidvectorge" ["oidvector", "oidvector"] False
>           "bool",
>         CatCreateFunction "oidvectorgt" ["oidvector", "oidvector"] False
>           "bool",
>         CatCreateFunction "network" ["inet"] False "cidr",
>         CatCreateFunction "netmask" ["inet"] False "inet",
>         CatCreateFunction "masklen" ["inet"] False "int4",
>         CatCreateFunction "broadcast" ["inet"] False "inet",
>         CatCreateFunction "host" ["inet"] False "text",
>         CatCreateFunction "family" ["inet"] False "int4",
>         CatCreateFunction "int2" ["int8"] False "int2",
>         CatCreateFunction "lo_create" ["oid"] False "oid",
>         CatCreateFunction "oidlt" ["oid", "oid"] False "bool",
>         CatCreateFunction "oidle" ["oid", "oid"] False "bool",
>         CatCreateFunction "octet_length" ["bytea"] False "int4",
>         CatCreateFunction "get_byte" ["bytea", "int4"] False "int4",
>         CatCreateFunction "set_byte" ["bytea", "int4", "int4"] False
>           "bytea",
>         CatCreateFunction "get_bit" ["bytea", "int4"] False "int4",
>         CatCreateFunction "set_bit" ["bytea", "int4", "int4"] False
>           "bytea",
>         CatCreateFunction "dist_pl" ["point", "line"] False "float8",
>         CatCreateFunction "dist_lb" ["line", "box"] False "float8",
>         CatCreateFunction "dist_sl" ["lseg", "line"] False "float8",
>         CatCreateFunction "dist_cpoly" ["circle", "polygon"] False
>           "float8",
>         CatCreateFunction "poly_distance" ["polygon", "polygon"] False
>           "float8",
>         CatCreateFunction "text" ["inet"] False "text",
>         CatCreateFunction "text_lt" ["text", "text"] False "bool",
>         CatCreateFunction "text_le" ["text", "text"] False "bool",
>         CatCreateFunction "text_gt" ["text", "text"] False "bool",
>         CatCreateFunction "text_ge" ["text", "text"] False "bool",
>         CatCreateFunction "array_eq" ["anyarray", "anyarray"] False "bool",
>         CatCreateFunction "array_dims" ["anyarray"] False "text",
>         CatCreateFunction "array_ndims" ["anyarray"] False "int4",
>         CatCreateFunction "overlay" ["bytea", "bytea", "int4", "int4"]
>           False
>           "bytea",
>         CatCreateFunction "array_in" ["cstring", "oid", "int4"] False
>           "anyarray",
>         CatCreateFunction "array_out" ["anyarray"] False "cstring",
>         CatCreateFunction "overlay" ["bytea", "bytea", "int4"] False
>           "bytea",
>         CatCreateFunction "trunc" ["macaddr"] False "macaddr",
>         CatCreateFunction "int8" ["int2"] False "int8",
>         CatCreateFunction "smgrin" ["cstring"] False "smgr",
>         CatCreateFunction "smgrout" ["smgr"] False "cstring",
>         CatCreateFunction "smgreq" ["smgr", "smgr"] False "bool",
>         CatCreateFunction "smgrne" ["smgr", "smgr"] False "bool",
>         CatCreateFunction "lo_import" ["text"] False "oid",
>         CatCreateFunction "lo_export" ["oid", "text"] False "int4",
>         CatCreateFunction "int4inc" ["int4"] False "int4",
>         CatCreateFunction "lo_import" ["text", "oid"] False "oid",
>         CatCreateFunction "int4larger" ["int4", "int4"] False "int4",
>         CatCreateFunction "int4smaller" ["int4", "int4"] False "int4",
>         CatCreateFunction "int2larger" ["int2", "int2"] False "int2",
>         CatCreateFunction "int2smaller" ["int2", "int2"] False "int2",
>         CatCreateFunction "gistcostestimate"
>           ["internal", "internal", "internal", "internal", "internal",
>            "internal", "internal", "internal", "internal"]
>           False
>           "void",
>         CatCreateFunction "gistgettuple" ["internal", "internal"] False
>           "bool",
>         CatCreateFunction "gistinsert"
>           ["internal", "internal", "internal", "internal", "internal",
>            "internal"]
>           False
>           "bool",
>         CatCreateFunction "gistbulkdelete"
>           ["internal", "internal", "internal", "internal"]
>           False
>           "internal",
>         CatCreateFunction "gistbeginscan"
>           ["internal", "internal", "internal"]
>           False
>           "internal",
>         CatCreateFunction "gistrescan"
>           ["internal", "internal", "internal", "internal", "internal"]
>           False
>           "void",
>         CatCreateFunction "gistendscan" ["internal"] False "void",
>         CatCreateFunction "gistmarkpos" ["internal"] False "void",
>         CatCreateFunction "gistrestrpos" ["internal"] False "void",
>         CatCreateFunction "gistbuild" ["internal", "internal", "internal"]
>           False
>           "internal",
>         CatCreateFunction "tintervaleq" ["tinterval", "tinterval"] False
>           "bool",
>         CatCreateFunction "tintervalne" ["tinterval", "tinterval"] False
>           "bool",
>         CatCreateFunction "tintervallt" ["tinterval", "tinterval"] False
>           "bool",
>         CatCreateFunction "tintervalgt" ["tinterval", "tinterval"] False
>           "bool",
>         CatCreateFunction "tintervalle" ["tinterval", "tinterval"] False
>           "bool",
>         CatCreateFunction "tintervalge" ["tinterval", "tinterval"] False
>           "bool",
>         CatCreateFunction "macaddr_eq" ["macaddr", "macaddr"] False "bool",
>         CatCreateFunction "macaddr_lt" ["macaddr", "macaddr"] False "bool",
>         CatCreateFunction "macaddr_le" ["macaddr", "macaddr"] False "bool",
>         CatCreateFunction "macaddr_gt" ["macaddr", "macaddr"] False "bool",
>         CatCreateFunction "macaddr_ge" ["macaddr", "macaddr"] False "bool",
>         CatCreateFunction "macaddr_ne" ["macaddr", "macaddr"] False "bool",
>         CatCreateFunction "macaddr_cmp" ["macaddr", "macaddr"] False
>           "int4",
>         CatCreateFunction "int82pl" ["int8", "int2"] False "int8",
>         CatCreateFunction "int82mi" ["int8", "int2"] False "int8",
>         CatCreateFunction "int82mul" ["int8", "int2"] False "int8",
>         CatCreateFunction "int82div" ["int8", "int2"] False "int8",
>         CatCreateFunction "int28pl" ["int2", "int8"] False "int8",
>         CatCreateFunction "btint8cmp" ["int8", "int8"] False "int4",
>         CatCreateFunction "cash_mul_flt4" ["money", "float4"] False
>           "money",
>         CatCreateFunction "cash_div_flt4" ["money", "float4"] False
>           "money",
>         CatCreateFunction "flt4_mul_cash" ["float4", "money"] False
>           "money",
>         CatCreateFunction "position" ["text", "text"] False "int4",
>         CatCreateFunction "textlike" ["text", "text"] False "bool",
>         CatCreateFunction "textnlike" ["text", "text"] False "bool",
>         CatCreateFunction "int48eq" ["int4", "int8"] False "bool",
>         CatCreateFunction "int48ne" ["int4", "int8"] False "bool",
>         CatCreateFunction "int48lt" ["int4", "int8"] False "bool",
>         CatCreateFunction "int48gt" ["int4", "int8"] False "bool",
>         CatCreateFunction "int48le" ["int4", "int8"] False "bool",
>         CatCreateFunction "int48ge" ["int4", "int8"] False "bool",
>         CatCreateFunction "namelike" ["name", "text"] False "bool",
>         CatCreateFunction "namenlike" ["name", "text"] False "bool",
>         CatCreateFunction "bpchar" ["char"] False "bpchar",
>         CatCreateFunction "int4_mul_cash" ["int4", "money"] False "money",
>         CatCreateFunction "int2_mul_cash" ["int2", "money"] False "money",
>         CatCreateFunction "cash_mul_int4" ["money", "int4"] False "money",
>         CatCreateFunction "cash_div_int4" ["money", "int4"] False "money",
>         CatCreateFunction "cash_mul_int2" ["money", "int2"] False "money",
>         CatCreateFunction "cash_div_int2" ["money", "int2"] False "money",
>         CatCreateFunction "strpos" ["text", "text"] False "int4",
>         CatCreateFunction "lower" ["text"] False "text",
>         CatCreateFunction "upper" ["text"] False "text",
>         CatCreateFunction "initcap" ["text"] False "text",
>         CatCreateFunction "lpad" ["text", "int4", "text"] False "text",
>         CatCreateFunction "rpad" ["text", "int4", "text"] False "text",
>         CatCreateFunction "ltrim" ["text", "text"] False "text",
>         CatCreateFunction "rtrim" ["text", "text"] False "text",
>         CatCreateFunction "substr" ["text", "int4", "int4"] False "text",
>         CatCreateFunction "translate" ["text", "text", "text"] False
>           "text",
>         CatCreateFunction "lpad" ["text", "int4"] False "text",
>         CatCreateFunction "rpad" ["text", "int4"] False "text",
>         CatCreateFunction "ltrim" ["text"] False "text",
>         CatCreateFunction "rtrim" ["text"] False "text",
>         CatCreateFunction "substr" ["text", "int4"] False "text",
>         CatCreateFunction "btrim" ["text", "text"] False "text",
>         CatCreateFunction "btrim" ["text"] False "text",
>         CatCreateFunction "cash_in" ["cstring"] False "money",
>         CatCreateFunction "cash_out" ["money"] False "cstring",
>         CatCreateFunction "cash_eq" ["money", "money"] False "bool",
>         CatCreateFunction "cash_ne" ["money", "money"] False "bool",
>         CatCreateFunction "cash_lt" ["money", "money"] False "bool",
>         CatCreateFunction "cash_le" ["money", "money"] False "bool",
>         CatCreateFunction "cash_gt" ["money", "money"] False "bool",
>         CatCreateFunction "cash_ge" ["money", "money"] False "bool",
>         CatCreateFunction "cash_pl" ["money", "money"] False "money",
>         CatCreateFunction "cash_mi" ["money", "money"] False "money",
>         CatCreateFunction "cash_mul_flt8" ["money", "float8"] False
>           "money",
>         CatCreateFunction "cash_div_flt8" ["money", "float8"] False
>           "money",
>         CatCreateFunction "cashlarger" ["money", "money"] False "money",
>         CatCreateFunction "cashsmaller" ["money", "money"] False "money",
>         CatCreateFunction "inet_in" ["cstring"] False "inet",
>         CatCreateFunction "inet_out" ["inet"] False "cstring",
>         CatCreateFunction "flt8_mul_cash" ["float8", "money"] False
>           "money",
>         CatCreateFunction "network_eq" ["inet", "inet"] False "bool",
>         CatCreateFunction "network_lt" ["inet", "inet"] False "bool",
>         CatCreateFunction "network_le" ["inet", "inet"] False "bool",
>         CatCreateFunction "network_gt" ["inet", "inet"] False "bool",
>         CatCreateFunction "network_ge" ["inet", "inet"] False "bool",
>         CatCreateFunction "network_ne" ["inet", "inet"] False "bool",
>         CatCreateFunction "network_cmp" ["inet", "inet"] False "int4",
>         CatCreateFunction "network_sub" ["inet", "inet"] False "bool",
>         CatCreateFunction "network_subeq" ["inet", "inet"] False "bool",
>         CatCreateFunction "network_sup" ["inet", "inet"] False "bool",
>         CatCreateFunction "network_supeq" ["inet", "inet"] False "bool",
>         CatCreateFunction "cash_words" ["money"] False "text",
>         CatCreateFunction "substring" ["text", "int4", "int4"] False
>           "text",
>         CatCreateFunction "substring" ["text", "int4"] False "text",
>         CatCreateFunction "generate_series"
>           ["timestamp", "timestamp", "interval"]
>           False
>           "timestamp",
>         CatCreateFunction "generate_series"
>           ["timestamptz", "interval", "timestamptz"]
>           False
>           "timestamptz",
>         CatCreateFunction "mod" ["int2", "int2"] False "int2",
>         CatCreateFunction "mod" ["int4", "int4"] False "int4",
>         CatCreateFunction "int28mi" ["int2", "int8"] False "int8",
>         CatCreateFunction "int28mul" ["int2", "int8"] False "int8",
>         CatCreateFunction "char" ["text"] False "char",
>         CatCreateFunction "int8mod" ["int8", "int8"] False "int8",
>         CatCreateFunction "text" ["char"] False "text",
>         CatCreateFunction "mod" ["int8", "int8"] False "int8",
>         CatCreateFunction "int28div" ["int2", "int8"] False "int8",
>         CatCreateFunction "hashint8" ["int8"] False "int4",
>         CatCreateFunction "lo_open" ["oid", "int4"] False "int4",
>         CatCreateFunction "lo_close" ["int4"] False "int4",
>         CatCreateFunction "loread" ["int4", "int4"] False "bytea",
>         CatCreateFunction "lowrite" ["int4", "bytea"] False "int4",
>         CatCreateFunction "lo_lseek" ["int4", "int4", "int4"] False "int4",
>         CatCreateFunction "lo_creat" ["int4"] False "oid",
>         CatCreateFunction "lo_tell" ["int4"] False "int4",
>         CatCreateFunction "on_pl" ["point", "line"] False "bool",
>         CatCreateFunction "on_sl" ["lseg", "line"] False "bool",
>         CatCreateFunction "close_pl" ["point", "line"] False "point",
>         CatCreateFunction "close_sl" ["lseg", "line"] False "point",
>         CatCreateFunction "close_lb" ["line", "box"] False "point",
>         CatCreateFunction "lo_unlink" ["oid"] False "int4",
>         CatCreateFunction "btvacuumcleanup" ["internal", "internal"] False
>           "internal",
>         CatCreateFunction "path_inter" ["path", "path"] False "bool",
>         CatCreateFunction "area" ["box"] False "float8",
>         CatCreateFunction "width" ["box"] False "float8",
>         CatCreateFunction "height" ["box"] False "float8",
>         CatCreateFunction "box_distance" ["box", "box"] False "float8",
>         CatCreateFunction "area" ["path"] False "float8",
>         CatCreateFunction "box_intersect" ["box", "box"] False "box",
>         CatCreateFunction "diagonal" ["box"] False "lseg",
>         CatCreateFunction "path_n_lt" ["path", "path"] False "bool",
>         CatCreateFunction "path_n_gt" ["path", "path"] False "bool",
>         CatCreateFunction "path_n_eq" ["path", "path"] False "bool",
>         CatCreateFunction "path_n_le" ["path", "path"] False "bool",
>         CatCreateFunction "path_n_ge" ["path", "path"] False "bool",
>         CatCreateFunction "path_length" ["path"] False "float8",
>         CatCreateFunction "point_ne" ["point", "point"] False "bool",
>         CatCreateFunction "point_vert" ["point", "point"] False "bool",
>         CatCreateFunction "point_horiz" ["point", "point"] False "bool",
>         CatCreateFunction "point_distance" ["point", "point"] False
>           "float8",
>         CatCreateFunction "slope" ["point", "point"] False "float8",
>         CatCreateFunction "lseg" ["point", "point"] False "lseg",
>         CatCreateFunction "lseg_intersect" ["lseg", "lseg"] False "bool",
>         CatCreateFunction "lseg_parallel" ["lseg", "lseg"] False "bool",
>         CatCreateFunction "lseg_perp" ["lseg", "lseg"] False "bool",
>         CatCreateFunction "lseg_vertical" ["lseg"] False "bool",
>         CatCreateFunction "lseg_horizontal" ["lseg"] False "bool",
>         CatCreateFunction "lseg_eq" ["lseg", "lseg"] False "bool",
>         CatCreateFunction "lo_truncate" ["int4", "int4"] False "int4",
>         CatCreateFunction "timezone" ["interval", "timestamptz"] False
>           "timestamp",
>         CatCreateFunction "gist_point_compress" ["internal"] False
>           "internal",
>         CatCreateFunction "aclitemin" ["cstring"] False "aclitem",
>         CatCreateFunction "aclitemout" ["aclitem"] False "cstring",
>         CatCreateFunction "aclinsert" ["_aclitem", "aclitem"] False
>           "_aclitem",
>         CatCreateFunction "aclremove" ["_aclitem", "aclitem"] False
>           "_aclitem",
>         CatCreateFunction "aclcontains" ["_aclitem", "aclitem"] False
>           "bool",
>         CatCreateFunction "bpcharin" ["cstring", "oid", "int4"] False
>           "bpchar",
>         CatCreateFunction "bpcharout" ["bpchar"] False "cstring",
>         CatCreateFunction "varcharin" ["cstring", "oid", "int4"] False
>           "varchar",
>         CatCreateFunction "varcharout" ["varchar"] False "cstring",
>         CatCreateFunction "bpchareq" ["bpchar", "bpchar"] False "bool",
>         CatCreateFunction "bpcharlt" ["bpchar", "bpchar"] False "bool",
>         CatCreateFunction "bpcharle" ["bpchar", "bpchar"] False "bool",
>         CatCreateFunction "bpchargt" ["bpchar", "bpchar"] False "bool",
>         CatCreateFunction "bpcharge" ["bpchar", "bpchar"] False "bool",
>         CatCreateFunction "bpcharne" ["bpchar", "bpchar"] False "bool",
>         CatCreateFunction "aclitemeq" ["aclitem", "aclitem"] False "bool",
>         CatCreateFunction "bpchar_larger" ["bpchar", "bpchar"] False
>           "bpchar",
>         CatCreateFunction "bpchar_smaller" ["bpchar", "bpchar"] False
>           "bpchar",
>         CatCreateFunction "generate_series" ["int4", "int4", "int4"] False
>           "int4",
>         CatCreateFunction "generate_series" ["int4", "int4"] False "int4",
>         CatCreateFunction "generate_series" ["int8", "int8", "int8"] False
>           "int8",
>         CatCreateFunction "generate_series" ["int8", "int8"] False "int8",
>         CatCreateFunction "bpcharcmp" ["bpchar", "bpchar"] False "int4",
>         CatCreateFunction "regclass" ["text"] False "regclass",
>         CatCreateFunction "hashbpchar" ["bpchar"] False "int4",
>         CatCreateFunction "format_type" ["oid", "int4"] False "text",
>         CatCreateFunction "date_in" ["cstring"] False "date",
>         CatCreateFunction "date_out" ["date"] False "cstring",
>         CatCreateFunction "date_eq" ["date", "date"] False "bool",
>         CatCreateFunction "date_lt" ["date", "date"] False "bool",
>         CatCreateFunction "date_le" ["date", "date"] False "bool",
>         CatCreateFunction "date_gt" ["date", "date"] False "bool",
>         CatCreateFunction "date_ge" ["date", "date"] False "bool",
>         CatCreateFunction "date_ne" ["date", "date"] False "bool",
>         CatCreateFunction "date_cmp" ["date", "date"] False "int4",
>         CatCreateFunction "time_lt" ["time", "time"] False "bool",
>         CatCreateFunction "time_le" ["time", "time"] False "bool",
>         CatCreateFunction "time_gt" ["time", "time"] False "bool",
>         CatCreateFunction "time_ge" ["time", "time"] False "bool",
>         CatCreateFunction "time_ne" ["time", "time"] False "bool",
>         CatCreateFunction "time_cmp" ["time", "time"] False "int4",
>         CatCreateFunction "date_larger" ["date", "date"] False "date",
>         CatCreateFunction "date_smaller" ["date", "date"] False "date",
>         CatCreateFunction "date_mi" ["date", "date"] False "int4",
>         CatCreateFunction "date_pli" ["date", "int4"] False "date",
>         CatCreateFunction "date_mii" ["date", "int4"] False "date",
>         CatCreateFunction "time_in" ["cstring", "oid", "int4"] False
>           "time",
>         CatCreateFunction "time_out" ["time"] False "cstring",
>         CatCreateFunction "time_eq" ["time", "time"] False "bool",
>         CatCreateFunction "circle_add_pt" ["circle", "point"] False
>           "circle",
>         CatCreateFunction "circle_sub_pt" ["circle", "point"] False
>           "circle",
>         CatCreateFunction "circle_mul_pt" ["circle", "point"] False
>           "circle",
>         CatCreateFunction "circle_div_pt" ["circle", "point"] False
>           "circle",
>         CatCreateFunction "timestamptz_in" ["cstring", "oid", "int4"] False
>           "timestamptz",
>         CatCreateFunction "timestamptz_out" ["timestamptz"] False
>           "cstring",
>         CatCreateFunction "timestamptz_eq" ["timestamptz", "timestamptz"]
>           False
>           "bool",
>         CatCreateFunction "timestamptz_ne" ["timestamptz", "timestamptz"]
>           False
>           "bool",
>         CatCreateFunction "timestamptz_lt" ["timestamptz", "timestamptz"]
>           False
>           "bool",
>         CatCreateFunction "timestamptz_le" ["timestamptz", "timestamptz"]
>           False
>           "bool",
>         CatCreateFunction "timestamptz_ge" ["timestamptz", "timestamptz"]
>           False
>           "bool",
>         CatCreateFunction "timestamptz_gt" ["timestamptz", "timestamptz"]
>           False
>           "bool",
>         CatCreateFunction "to_timestamp" ["float8"] False "timestamptz",
>         CatCreateFunction "timezone" ["text", "timestamptz"] False
>           "timestamp",
>         CatCreateFunction "interval_in" ["cstring", "oid", "int4"] False
>           "interval",
>         CatCreateFunction "interval_out" ["interval"] False "cstring",
>         CatCreateFunction "interval_eq" ["interval", "interval"] False
>           "bool",
>         CatCreateFunction "interval_ne" ["interval", "interval"] False
>           "bool",
>         CatCreateFunction "interval_lt" ["interval", "interval"] False
>           "bool",
>         CatCreateFunction "interval_le" ["interval", "interval"] False
>           "bool",
>         CatCreateFunction "interval_ge" ["interval", "interval"] False
>           "bool",
>         CatCreateFunction "interval_gt" ["interval", "interval"] False
>           "bool",
>         CatCreateFunction "interval_um" ["interval"] False "interval",
>         CatCreateFunction "interval_pl" ["interval", "interval"] False
>           "interval",
>         CatCreateFunction "interval_mi" ["interval", "interval"] False
>           "interval",
>         CatCreateFunction "date_part" ["text", "timestamptz"] False
>           "float8",
>         CatCreateFunction "date_part" ["text", "interval"] False "float8",
>         CatCreateFunction "timestamptz" ["abstime"] False "timestamptz",
>         CatCreateFunction "timestamptz" ["date"] False "timestamptz",
>         CatCreateFunction "justify_hours" ["interval"] False "interval",
>         CatCreateFunction "timestamptz" ["date", "time"] False
>           "timestamptz",
>         CatCreateFunction "interval" ["reltime"] False "interval",
>         CatCreateFunction "date" ["timestamptz"] False "date",
>         CatCreateFunction "date" ["abstime"] False "date",
>         CatCreateFunction "abstime" ["timestamptz"] False "abstime",
>         CatCreateFunction "age" ["xid"] False "int4",
>         CatCreateFunction "timestamptz_mi" ["timestamptz", "timestamptz"]
>           False
>           "interval",
>         CatCreateFunction "timestamptz_pl_interval"
>           ["timestamptz", "interval"]
>           False
>           "timestamptz",
>         CatCreateFunction "timestamptz_mi_interval"
>           ["timestamptz", "interval"]
>           False
>           "timestamptz",
>         CatCreateFunction "generate_subscripts"
>           ["anyarray", "int4", "bool"]
>           False
>           "int4",
>         CatCreateFunction "generate_subscripts" ["anyarray", "int4"] False
>           "int4",
>         CatCreateFunction "array_fill" ["anyelement", "_int4"] False
>           "anyarray",
>         CatCreateFunction "reltime" ["interval"] False "reltime",
>         CatCreateFunction "timestamptz_smaller"
>           ["timestamptz", "timestamptz"]
>           False
>           "timestamptz",
>         CatCreateFunction "timestamptz_larger"
>           ["timestamptz", "timestamptz"]
>           False
>           "timestamptz",
>         CatCreateFunction "interval_smaller" ["interval", "interval"] False
>           "interval",
>         CatCreateFunction "interval_larger" ["interval", "interval"] False
>           "interval",
>         CatCreateFunction "age" ["timestamptz", "timestamptz"] False
>           "interval",
>         CatCreateFunction "interval" ["interval", "int4"] False "interval",
>         CatCreateFunction "obj_description" ["oid", "name"] False "text",
>         CatCreateFunction "col_description" ["oid", "int4"] False "text",
>         CatCreateFunction "date_trunc" ["text", "timestamptz"] False
>           "timestamptz",
>         CatCreateFunction "date_trunc" ["text", "interval"] False
>           "interval",
>         CatCreateFunction "int8inc" ["int8"] False "int8",
>         CatCreateFunction "int8abs" ["int8"] False "int8",
>         CatCreateFunction "int8larger" ["int8", "int8"] False "int8",
>         CatCreateFunction "int8smaller" ["int8", "int8"] False "int8",
>         CatCreateFunction "texticregexeq" ["text", "text"] False "bool",
>         CatCreateFunction "texticregexne" ["text", "text"] False "bool",
>         CatCreateFunction "nameicregexeq" ["name", "text"] False "bool",
>         CatCreateFunction "nameicregexne" ["name", "text"] False "bool",
>         CatCreateFunction "boolin" ["cstring"] False "bool",
>         CatCreateFunction "boolout" ["bool"] False "cstring",
>         CatCreateFunction "byteain" ["cstring"] False "bytea",
>         CatCreateFunction "charin" ["cstring"] False "char",
>         CatCreateFunction "charlt" ["char", "char"] False "bool",
>         CatCreateFunction "int4abs" ["int4"] False "int4",
>         CatCreateFunction "nameregexne" ["name", "text"] False "bool",
>         CatCreateFunction "int2abs" ["int2"] False "int2",
>         CatCreateFunction "textregexeq" ["text", "text"] False "bool",
>         CatCreateFunction "textregexne" ["text", "text"] False "bool",
>         CatCreateFunction "textlen" ["text"] False "int4",
>         CatCreateFunction "textcat" ["text", "text"] False "text",
>         CatCreateFunction "pg_char_to_encoding" ["name"] False "int4",
>         CatCreateFunction "tidne" ["tid", "tid"] False "bool",
>         CatCreateFunction "cidr_in" ["cstring"] False "cidr",
>         CatCreateFunction "btcostestimate"
>           ["internal", "internal", "internal", "internal", "internal",
>            "internal", "internal", "internal", "internal"]
>           False
>           "void",
>         CatCreateFunction "pg_column_size" ["any"] False "int4",
>         CatCreateFunction "overlaps"
>           ["timetz", "timetz", "timetz", "timetz"]
>           False
>           "bool",
>         CatCreateFunction "datetime_pl" ["date", "time"] False "timestamp",
>         CatCreateFunction "date_part" ["text", "timetz"] False "float8",
>         CatCreateFunction "int84pl" ["int8", "int4"] False "int8",
>         CatCreateFunction "int84mi" ["int8", "int4"] False "int8",
>         CatCreateFunction "int84mul" ["int8", "int4"] False "int8",
>         CatCreateFunction "int84div" ["int8", "int4"] False "int8",
>         CatCreateFunction "int48pl" ["int4", "int8"] False "int8",
>         CatCreateFunction "int48mi" ["int4", "int8"] False "int8",
>         CatCreateFunction "int48mul" ["int4", "int8"] False "int8",
>         CatCreateFunction "int48div" ["int4", "int8"] False "int8",
>         CatCreateFunction "quote_ident" ["text"] False "text",
>         CatCreateFunction "quote_literal" ["text"] False "text",
>         CatCreateFunction "quote_literal" ["anyelement"] False "text",
>         CatCreateFunction "array_fill" ["anyelement", "_int4", "_int4"]
>           False
>           "anyarray",
>         CatCreateFunction "oid" ["int8"] False "oid",
>         CatCreateFunction "int8" ["oid"] False "int8",
>         CatCreateFunction "quote_nullable" ["text"] False "text",
>         CatCreateFunction "quote_nullable" ["anyelement"] False "text",
>         CatCreateFunction "tideq" ["tid", "tid"] False "bool",
>         CatCreateFunction "currtid" ["oid", "tid"] False "tid",
>         CatCreateFunction "currtid2" ["text", "tid"] False "tid",
>         CatCreateFunction "justify_days" ["interval"] False "interval",
>         CatCreateFunction "timedate_pl" ["time", "date"] False "timestamp",
>         CatCreateFunction "datetimetz_pl" ["date", "timetz"] False
>           "timestamptz",
>         CatCreateFunction "timetzdate_pl" ["timetz", "date"] False
>           "timestamptz",
>         CatCreateFunction "positionsel"
>           ["internal", "oid", "internal", "int4"]
>           False
>           "float8",
>         CatCreateFunction "positionjoinsel"
>           ["internal", "oid", "internal", "int2", "internal"]
>           False
>           "float8",
>         CatCreateFunction "contsel" ["internal", "oid", "internal", "int4"]
>           False
>           "float8",
>         CatCreateFunction "contjoinsel"
>           ["internal", "oid", "internal", "int2", "internal"]
>           False
>           "float8",
>         CatCreateFunction "overlaps"
>           ["timestamptz", "timestamptz", "timestamptz", "timestamptz"]
>           False
>           "bool",
>         CatCreateFunction "overlaps"
>           ["timestamptz", "interval", "timestamptz", "interval"]
>           False
>           "bool",
>         CatCreateFunction "overlaps"
>           ["timestamptz", "timestamptz", "timestamptz", "interval"]
>           False
>           "bool",
>         CatCreateFunction "overlaps"
>           ["timestamptz", "interval", "timestamptz", "timestamptz"]
>           False
>           "bool",
>         CatCreateFunction "overlaps" ["time", "time", "time", "time"] False
>           "bool",
>         CatCreateFunction "overlaps"
>           ["time", "interval", "time", "interval"]
>           False
>           "bool",
>         CatCreateFunction "overlaps" ["time", "time", "time", "interval"]
>           False
>           "bool",
>         CatCreateFunction "overlaps" ["time", "interval", "time", "time"]
>           False
>           "bool",
>         CatCreateFunction "timestamp_in" ["cstring", "oid", "int4"] False
>           "timestamp",
>         CatCreateFunction "timestamp_out" ["timestamp"] False "cstring",
>         CatCreateFunction "timestamptz_cmp" ["timestamptz", "timestamptz"]
>           False
>           "int4",
>         CatCreateFunction "interval_cmp" ["interval", "interval"] False
>           "int4",
>         CatCreateFunction "time" ["timestamp"] False "time",
>         CatCreateFunction "length" ["text"] False "int4",
>         CatCreateFunction "length" ["bpchar"] False "int4",
>         CatCreateFunction "xideqint4" ["xid", "int4"] False "bool",
>         CatCreateFunction "interval_div" ["interval", "float8"] False
>           "interval",
>         CatCreateFunction "dlog10" ["float8"] False "float8",
>         CatCreateFunction "log" ["float8"] False "float8",
>         CatCreateFunction "ln" ["float8"] False "float8",
>         CatCreateFunction "round" ["float8"] False "float8",
>         CatCreateFunction "trunc" ["float8"] False "float8",
>         CatCreateFunction "sqrt" ["float8"] False "float8",
>         CatCreateFunction "cbrt" ["float8"] False "float8",
>         CatCreateFunction "pow" ["float8", "float8"] False "float8",
>         CatCreateFunction "exp" ["float8"] False "float8",
>         CatCreateFunction "obj_description" ["oid"] False "text",
>         CatCreateFunction "oidvectortypes" ["oidvector"] False "text",
>         CatCreateFunction "timetz_in" ["cstring", "oid", "int4"] False
>           "timetz",
>         CatCreateFunction "timetz_out" ["timetz"] False "cstring",
>         CatCreateFunction "timetz_eq" ["timetz", "timetz"] False "bool",
>         CatCreateFunction "timetz_ne" ["timetz", "timetz"] False "bool",
>         CatCreateFunction "timetz_lt" ["timetz", "timetz"] False "bool",
>         CatCreateFunction "timetz_le" ["timetz", "timetz"] False "bool",
>         CatCreateFunction "timetz_ge" ["timetz", "timetz"] False "bool",
>         CatCreateFunction "timetz_gt" ["timetz", "timetz"] False "bool",
>         CatCreateFunction "timetz_cmp" ["timetz", "timetz"] False "int4",
>         CatCreateFunction "timestamptz" ["date", "timetz"] False
>           "timestamptz",
>         CatCreateFunction "hostmask" ["inet"] False "inet",
>         CatCreateFunction "time" ["abstime"] False "time",
>         CatCreateFunction "makeaclitem" ["oid", "oid", "text", "bool"]
>           False
>           "aclitem",
>         CatCreateFunction "character_length" ["bpchar"] False "int4",
>         CatCreateFunction "power" ["float8", "float8"] False "float8",
>         CatCreateFunction "character_length" ["text"] False "int4",
>         CatCreateFunction "interval" ["time"] False "interval",
>         CatCreateFunction "char_length" ["bpchar"] False "int4",
>         CatCreateFunction "isfinite" ["date"] False "bool",
>         CatCreateFunction "octet_length" ["text"] False "int4",
>         CatCreateFunction "octet_length" ["bpchar"] False "int4",
>         CatCreateFunction "factorial" ["int8"] False "numeric",
>         CatCreateFunction "time_larger" ["time", "time"] False "time",
>         CatCreateFunction "time_smaller" ["time", "time"] False "time",
>         CatCreateFunction "timetz_larger" ["timetz", "timetz"] False
>           "timetz",
>         CatCreateFunction "timetz_smaller" ["timetz", "timetz"] False
>           "timetz",
>         CatCreateFunction "char_length" ["text"] False "int4",
>         CatCreateFunction "date_part" ["text", "abstime"] False "float8",
>         CatCreateFunction "date_part" ["text", "reltime"] False "float8",
>         CatCreateFunction "date_part" ["text", "date"] False "float8",
>         CatCreateFunction "date_part" ["text", "time"] False "float8",
>         CatCreateFunction "age" ["timestamptz"] False "interval",
>         CatCreateFunction "pg_get_constraintdef" ["oid"] False "text",
>         CatCreateFunction "timetz" ["timestamptz"] False "timetz",
>         CatCreateFunction "isfinite" ["timestamptz"] False "bool",
>         CatCreateFunction "isfinite" ["interval"] False "bool",
>         CatCreateFunction "pg_stat_get_backend_start" ["int4"] False
>           "timestamptz",
>         CatCreateFunction "pg_stat_get_backend_client_addr" ["int4"] False
>           "inet",
>         CatCreateFunction "pg_stat_get_backend_client_port" ["int4"] False
>           "int4",
>         CatCreateFunction "abs" ["float4"] False "float4",
>         CatCreateFunction "abs" ["float8"] False "float8",
>         CatCreateFunction "abs" ["int8"] False "int8",
>         CatCreateFunction "abs" ["int4"] False "int4",
>         CatCreateFunction "abs" ["int2"] False "int2",
>         CatCreateFunction "name" ["varchar"] False "name",
>         CatCreateFunction "varchar" ["name"] False "varchar",
>         CatCreateFunction "current_schemas" ["bool"] False "_name",
>         CatCreateFunction "overlay" ["text", "text", "int4", "int4"] False
>           "text",
>         CatCreateFunction "overlay" ["text", "text", "int4"] False "text",
>         CatCreateFunction "isvertical" ["point", "point"] False "bool",
>         CatCreateFunction "ishorizontal" ["point", "point"] False "bool",
>         CatCreateFunction "isparallel" ["lseg", "lseg"] False "bool",
>         CatCreateFunction "isperp" ["lseg", "lseg"] False "bool",
>         CatCreateFunction "isvertical" ["lseg"] False "bool",
>         CatCreateFunction "ishorizontal" ["lseg"] False "bool",
>         CatCreateFunction "isparallel" ["line", "line"] False "bool",
>         CatCreateFunction "isperp" ["line", "line"] False "bool",
>         CatCreateFunction "isvertical" ["line"] False "bool",
>         CatCreateFunction "ishorizontal" ["line"] False "bool",
>         CatCreateFunction "point" ["circle"] False "point",
>         CatCreateFunction "time" ["interval"] False "time",
>         CatCreateFunction "box" ["point", "point"] False "box",
>         CatCreateFunction "box_add" ["box", "point"] False "box",
>         CatCreateFunction "box_sub" ["box", "point"] False "box",
>         CatCreateFunction "box_mul" ["box", "point"] False "box",
>         CatCreateFunction "box_div" ["box", "point"] False "box",
>         CatCreateFunction "path_contain_pt" ["path", "point"] False "bool",
>         CatCreateFunction "cidr_out" ["cidr"] False "cstring",
>         CatCreateFunction "poly_contain_pt" ["polygon", "point"] False
>           "bool",
>         CatCreateFunction "pt_contained_poly" ["point", "polygon"] False
>           "bool",
>         CatCreateFunction "isclosed" ["path"] False "bool",
>         CatCreateFunction "isopen" ["path"] False "bool",
>         CatCreateFunction "path_npoints" ["path"] False "int4",
>         CatCreateFunction "pclose" ["path"] False "path",
>         CatCreateFunction "popen" ["path"] False "path",
>         CatCreateFunction "path_add" ["path", "path"] False "path",
>         CatCreateFunction "path_add_pt" ["path", "point"] False "path",
>         CatCreateFunction "path_sub_pt" ["path", "point"] False "path",
>         CatCreateFunction "path_mul_pt" ["path", "point"] False "path",
>         CatCreateFunction "path_div_pt" ["path", "point"] False "path",
>         CatCreateFunction "point" ["float8", "float8"] False "point",
>         CatCreateFunction "point_add" ["point", "point"] False "point",
>         CatCreateFunction "point_sub" ["point", "point"] False "point",
>         CatCreateFunction "point_mul" ["point", "point"] False "point",
>         CatCreateFunction "point_div" ["point", "point"] False "point",
>         CatCreateFunction "poly_npoints" ["polygon"] False "int4",
>         CatCreateFunction "box" ["polygon"] False "box",
>         CatCreateFunction "path" ["polygon"] False "path",
>         CatCreateFunction "polygon" ["box"] False "polygon",
>         CatCreateFunction "polygon" ["path"] False "polygon",
>         CatCreateFunction "circle_in" ["cstring"] False "circle",
>         CatCreateFunction "circle_out" ["circle"] False "cstring",
>         CatCreateFunction "circle_same" ["circle", "circle"] False "bool",
>         CatCreateFunction "circle_contain" ["circle", "circle"] False
>           "bool",
>         CatCreateFunction "circle_left" ["circle", "circle"] False "bool",
>         CatCreateFunction "circle_overleft" ["circle", "circle"] False
>           "bool",
>         CatCreateFunction "circle_overright" ["circle", "circle"] False
>           "bool",
>         CatCreateFunction "circle_right" ["circle", "circle"] False "bool",
>         CatCreateFunction "circle_contained" ["circle", "circle"] False
>           "bool",
>         CatCreateFunction "circle_overlap" ["circle", "circle"] False
>           "bool",
>         CatCreateFunction "circle_below" ["circle", "circle"] False "bool",
>         CatCreateFunction "circle_above" ["circle", "circle"] False "bool",
>         CatCreateFunction "circle_eq" ["circle", "circle"] False "bool",
>         CatCreateFunction "circle_ne" ["circle", "circle"] False "bool",
>         CatCreateFunction "circle_lt" ["circle", "circle"] False "bool",
>         CatCreateFunction "circle_gt" ["circle", "circle"] False "bool",
>         CatCreateFunction "circle_le" ["circle", "circle"] False "bool",
>         CatCreateFunction "circle_ge" ["circle", "circle"] False "bool",
>         CatCreateFunction "area" ["circle"] False "float8",
>         CatCreateFunction "diameter" ["circle"] False "float8",
>         CatCreateFunction "radius" ["circle"] False "float8",
>         CatCreateFunction "circle_distance" ["circle", "circle"] False
>           "float8",
>         CatCreateFunction "circle_center" ["circle"] False "point",
>         CatCreateFunction "circle" ["point", "float8"] False "circle",
>         CatCreateFunction "circle" ["polygon"] False "circle",
>         CatCreateFunction "polygon" ["int4", "circle"] False "polygon",
>         CatCreateFunction "dist_pc" ["point", "circle"] False "float8",
>         CatCreateFunction "circle_contain_pt" ["circle", "point"] False
>           "bool",
>         CatCreateFunction "pt_contained_circle" ["point", "circle"] False
>           "bool",
>         CatCreateFunction "circle" ["box"] False "circle",
>         CatCreateFunction "box" ["circle"] False "box",
>         CatCreateFunction "tinterval" ["abstime", "abstime"] False
>           "tinterval",
>         CatCreateFunction "lseg_ne" ["lseg", "lseg"] False "bool",
>         CatCreateFunction "lseg_lt" ["lseg", "lseg"] False "bool",
>         CatCreateFunction "lseg_le" ["lseg", "lseg"] False "bool",
>         CatCreateFunction "lseg_gt" ["lseg", "lseg"] False "bool",
>         CatCreateFunction "lseg_ge" ["lseg", "lseg"] False "bool",
>         CatCreateFunction "lseg_length" ["lseg"] False "float8",
>         CatCreateFunction "close_ls" ["line", "lseg"] False "point",
>         CatCreateFunction "close_lseg" ["lseg", "lseg"] False "point",
>         CatCreateFunction "line_in" ["cstring"] False "line",
>         CatCreateFunction "line_out" ["line"] False "cstring",
>         CatCreateFunction "line_eq" ["line", "line"] False "bool",
>         CatCreateFunction "line" ["point", "point"] False "line",
>         CatCreateFunction "line_interpt" ["line", "line"] False "point",
>         CatCreateFunction "line_intersect" ["line", "line"] False "bool",
>         CatCreateFunction "line_parallel" ["line", "line"] False "bool",
>         CatCreateFunction "line_perp" ["line", "line"] False "bool",
>         CatCreateFunction "line_vertical" ["line"] False "bool",
>         CatCreateFunction "line_horizontal" ["line"] False "bool",
>         CatCreateFunction "length" ["lseg"] False "float8",
>         CatCreateFunction "length" ["path"] False "float8",
>         CatCreateFunction "point" ["lseg"] False "point",
>         CatCreateFunction "point" ["path"] False "point",
>         CatCreateFunction "point" ["box"] False "point",
>         CatCreateFunction "point" ["polygon"] False "point",
>         CatCreateFunction "lseg" ["box"] False "lseg",
>         CatCreateFunction "center" ["box"] False "point",
>         CatCreateFunction "center" ["circle"] False "point",
>         CatCreateFunction "polygon" ["circle"] False "polygon",
>         CatCreateFunction "npoints" ["path"] False "int4",
>         CatCreateFunction "npoints" ["polygon"] False "int4",
>         CatCreateFunction "bit_in" ["cstring", "oid", "int4"] False "bit",
>         CatCreateFunction "bit_out" ["bit"] False "cstring",
>         CatCreateFunction "like" ["text", "text"] False "bool",
>         CatCreateFunction "notlike" ["text", "text"] False "bool",
>         CatCreateFunction "like" ["name", "text"] False "bool",
>         CatCreateFunction "notlike" ["name", "text"] False "bool",
>         CatCreateFunction "pg_get_ruledef" ["oid"] False "text",
>         CatCreateFunction "nextval" ["regclass"] False "int8",
>         CatCreateFunction "currval" ["regclass"] False "int8",
>         CatCreateFunction "setval" ["regclass", "int8"] False "int8",
>         CatCreateFunction "varbit_in" ["cstring", "oid", "int4"] False
>           "varbit",
>         CatCreateFunction "varbit_out" ["varbit"] False "cstring",
>         CatCreateFunction "biteq" ["bit", "bit"] False "bool",
>         CatCreateFunction "bitne" ["bit", "bit"] False "bool",
>         CatCreateFunction "bitge" ["bit", "bit"] False "bool",
>         CatCreateFunction "bitgt" ["bit", "bit"] False "bool",
>         CatCreateFunction "bitle" ["bit", "bit"] False "bool",
>         CatCreateFunction "bitlt" ["bit", "bit"] False "bool",
>         CatCreateFunction "bitcmp" ["bit", "bit"] False "int4",
>         CatCreateFunction "pg_encoding_to_char" ["int4"] False "name",
>         CatCreateFunction "setseed" ["float8"] False "void",
>         CatCreateFunction "asin" ["float8"] False "float8",
>         CatCreateFunction "acos" ["float8"] False "float8",
>         CatCreateFunction "atan" ["float8"] False "float8",
>         CatCreateFunction "atan2" ["float8", "float8"] False "float8",
>         CatCreateFunction "sin" ["float8"] False "float8",
>         CatCreateFunction "cos" ["float8"] False "float8",
>         CatCreateFunction "tan" ["float8"] False "float8",
>         CatCreateFunction "cot" ["float8"] False "float8",
>         CatCreateFunction "degrees" ["float8"] False "float8",
>         CatCreateFunction "radians" ["float8"] False "float8",
>         CatCreateFunction "interval_mul" ["interval", "float8"] False
>           "interval",
>         CatCreateFunction "pg_typeof" ["any"] False "regtype",
>         CatCreateFunction "ascii" ["text"] False "int4",
>         CatCreateFunction "chr" ["int4"] False "text",
>         CatCreateFunction "repeat" ["text", "int4"] False "text",
>         CatCreateFunction "similar_escape" ["text", "text"] False "text",
>         CatCreateFunction "mul_d_interval" ["float8", "interval"] False
>           "interval",
>         CatCreateFunction "bpcharlike" ["bpchar", "text"] False "bool",
>         CatCreateFunction "bpcharnlike" ["bpchar", "text"] False "bool",
>         CatCreateFunction "texticlike" ["text", "text"] False "bool",
>         CatCreateFunction "texticnlike" ["text", "text"] False "bool",
>         CatCreateFunction "nameiclike" ["name", "text"] False "bool",
>         CatCreateFunction "nameicnlike" ["name", "text"] False "bool",
>         CatCreateFunction "like_escape" ["text", "text"] False "text",
>         CatCreateFunction "oidgt" ["oid", "oid"] False "bool",
>         CatCreateFunction "oidge" ["oid", "oid"] False "bool",
>         CatCreateFunction "pg_get_viewdef" ["text"] False "text",
>         CatCreateFunction "pg_get_viewdef" ["oid"] False "text",
>         CatCreateFunction "pg_get_userbyid" ["oid"] False "name",
>         CatCreateFunction "pg_get_indexdef" ["oid"] False "text",
>         CatCreateFunction "bpcharicregexeq" ["bpchar", "text"] False
>           "bool",
>         CatCreateFunction "bpcharicregexne" ["bpchar", "text"] False
>           "bool",
>         CatCreateFunction "bpcharregexeq" ["bpchar", "text"] False "bool",
>         CatCreateFunction "bpcharregexne" ["bpchar", "text"] False "bool",
>         CatCreateFunction "bpchariclike" ["bpchar", "text"] False "bool",
>         CatCreateFunction "bpcharicnlike" ["bpchar", "text"] False "bool",
>         CatCreateFunction "pg_get_triggerdef" ["oid"] False "text",
>         CatCreateFunction "pg_get_serial_sequence" ["text", "text"] False
>           "text",
>         CatCreateFunction "varbiteq" ["varbit", "varbit"] False "bool",
>         CatCreateFunction "varbitne" ["varbit", "varbit"] False "bool",
>         CatCreateFunction "varbitge" ["varbit", "varbit"] False "bool",
>         CatCreateFunction "varbitgt" ["varbit", "varbit"] False "bool",
>         CatCreateFunction "varbitle" ["varbit", "varbit"] False "bool",
>         CatCreateFunction "varbitlt" ["varbit", "varbit"] False "bool",
>         CatCreateFunction "varbitcmp" ["varbit", "varbit"] False "int4",
>         CatCreateFunction "bitand" ["bit", "bit"] False "bit",
>         CatCreateFunction "bitor" ["bit", "bit"] False "bit",
>         CatCreateFunction "bitxor" ["bit", "bit"] False "bit",
>         CatCreateFunction "bitnot" ["bit"] False "bit",
>         CatCreateFunction "bitshiftleft" ["bit", "int4"] False "bit",
>         CatCreateFunction "bitshiftright" ["bit", "int4"] False "bit",
>         CatCreateFunction "bitcat" ["varbit", "varbit"] False "varbit",
>         CatCreateFunction "substring" ["bit", "int4", "int4"] False "bit",
>         CatCreateFunction "length" ["bit"] False "int4",
>         CatCreateFunction "octet_length" ["bit"] False "int4",
>         CatCreateFunction "bit" ["int4", "int4"] False "bit",
>         CatCreateFunction "int4" ["bit"] False "int4",
>         CatCreateFunction "bit" ["bit", "int4", "bool"] False "bit",
>         CatCreateFunction "varbit" ["varbit", "int4", "bool"] False
>           "varbit",
>         CatCreateFunction "time_hash" ["time"] False "int4",
>         CatCreateFunction "aclexplode" ["_aclitem"] False "record",
>         CatCreateFunction "time_mi_time" ["time", "time"] False "interval",
>         CatCreateFunction "boolle" ["bool", "bool"] False "bool",
>         CatCreateFunction "boolge" ["bool", "bool"] False "bool",
>         CatCreateFunction "btboolcmp" ["bool", "bool"] False "int4",
>         CatCreateFunction "timetz_hash" ["timetz"] False "int4",
>         CatCreateFunction "interval_hash" ["interval"] False "int4",
>         CatCreateFunction "position" ["bit", "bit"] False "int4",
>         CatCreateFunction "substring" ["bit", "int4"] False "bit",
>         CatCreateFunction "numeric_in" ["cstring", "oid", "int4"] False
>           "numeric",
>         CatCreateFunction "numeric_out" ["numeric"] False "cstring",
>         CatCreateFunction "numeric" ["numeric", "int4"] False "numeric",
>         CatCreateFunction "numeric_abs" ["numeric"] False "numeric",
>         CatCreateFunction "abs" ["numeric"] False "numeric",
>         CatCreateFunction "sign" ["numeric"] False "numeric",
>         CatCreateFunction "round" ["numeric", "int4"] False "numeric",
>         CatCreateFunction "round" ["numeric"] False "numeric",
>         CatCreateFunction "trunc" ["numeric", "int4"] False "numeric",
>         CatCreateFunction "trunc" ["numeric"] False "numeric",
>         CatCreateFunction "ceil" ["numeric"] False "numeric",
>         CatCreateFunction "floor" ["numeric"] False "numeric",
>         CatCreateFunction "length" ["bytea", "name"] False "int4",
>         CatCreateFunction "convert_from" ["bytea", "name"] False "text",
>         CatCreateFunction "cidr" ["inet"] False "cidr",
>         CatCreateFunction "pg_get_expr" ["pg_node_tree", "oid"] False
>           "text",
>         CatCreateFunction "convert_to" ["text", "name"] False "bytea",
>         CatCreateFunction "numeric_eq" ["numeric", "numeric"] False "bool",
>         CatCreateFunction "numeric_ne" ["numeric", "numeric"] False "bool",
>         CatCreateFunction "numeric_gt" ["numeric", "numeric"] False "bool",
>         CatCreateFunction "numeric_ge" ["numeric", "numeric"] False "bool",
>         CatCreateFunction "numeric_lt" ["numeric", "numeric"] False "bool",
>         CatCreateFunction "numeric_le" ["numeric", "numeric"] False "bool",
>         CatCreateFunction "numeric_add" ["numeric", "numeric"] False
>           "numeric",
>         CatCreateFunction "numeric_sub" ["numeric", "numeric"] False
>           "numeric",
>         CatCreateFunction "numeric_mul" ["numeric", "numeric"] False
>           "numeric",
>         CatCreateFunction "numeric_div" ["numeric", "numeric"] False
>           "numeric",
>         CatCreateFunction "mod" ["numeric", "numeric"] False "numeric",
>         CatCreateFunction "numeric_mod" ["numeric", "numeric"] False
>           "numeric",
>         CatCreateFunction "sqrt" ["numeric"] False "numeric",
>         CatCreateFunction "numeric_sqrt" ["numeric"] False "numeric",
>         CatCreateFunction "exp" ["numeric"] False "numeric",
>         CatCreateFunction "numeric_exp" ["numeric"] False "numeric",
>         CatCreateFunction "ln" ["numeric"] False "numeric",
>         CatCreateFunction "numeric_ln" ["numeric"] False "numeric",
>         CatCreateFunction "log" ["numeric", "numeric"] False "numeric",
>         CatCreateFunction "numeric_log" ["numeric", "numeric"] False
>           "numeric",
>         CatCreateFunction "pow" ["numeric", "numeric"] False "numeric",
>         CatCreateFunction "numeric_power" ["numeric", "numeric"] False
>           "numeric",
>         CatCreateFunction "numeric" ["int4"] False "numeric",
>         CatCreateFunction "log" ["numeric"] False "numeric",
>         CatCreateFunction "numeric" ["float4"] False "numeric",
>         CatCreateFunction "numeric" ["float8"] False "numeric",
>         CatCreateFunction "int4" ["numeric"] False "int4",
>         CatCreateFunction "float4" ["numeric"] False "float4",
>         CatCreateFunction "float8" ["numeric"] False "float8",
>         CatCreateFunction "time_pl_interval" ["time", "interval"] False
>           "time",
>         CatCreateFunction "time_mi_interval" ["time", "interval"] False
>           "time",
>         CatCreateFunction "timetz_pl_interval" ["timetz", "interval"] False
>           "timetz",
>         CatCreateFunction "timetz_mi_interval" ["timetz", "interval"] False
>           "timetz",
>         CatCreateFunction "numeric_inc" ["numeric"] False "numeric",
>         CatCreateFunction "setval" ["regclass", "int8", "bool"] False
>           "int8",
>         CatCreateFunction "numeric_smaller" ["numeric", "numeric"] False
>           "numeric",
>         CatCreateFunction "numeric_larger" ["numeric", "numeric"] False
>           "numeric",
>         CatCreateFunction "to_char" ["interval", "text"] False "text",
>         CatCreateFunction "numeric_cmp" ["numeric", "numeric"] False
>           "int4",
>         CatCreateFunction "to_char" ["timestamptz", "text"] False "text",
>         CatCreateFunction "numeric_uminus" ["numeric"] False "numeric",
>         CatCreateFunction "to_char" ["numeric", "text"] False "text",
>         CatCreateFunction "to_char" ["int4", "text"] False "text",
>         CatCreateFunction "to_char" ["int8", "text"] False "text",
>         CatCreateFunction "to_char" ["float4", "text"] False "text",
>         CatCreateFunction "to_char" ["float8", "text"] False "text",
>         CatCreateFunction "to_number" ["text", "text"] False "numeric",
>         CatCreateFunction "to_timestamp" ["text", "text"] False
>           "timestamptz",
>         CatCreateFunction "int8" ["numeric"] False "int8",
>         CatCreateFunction "to_date" ["text", "text"] False "date",
>         CatCreateFunction "numeric" ["int8"] False "numeric",
>         CatCreateFunction "numeric" ["int2"] False "numeric",
>         CatCreateFunction "int2" ["numeric"] False "int2",
>         CatCreateFunction "oidin" ["cstring"] False "oid",
>         CatCreateFunction "oidout" ["oid"] False "cstring",
>         CatCreateFunction "bit_length" ["bytea"] False "int4",
>         CatCreateFunction "bit_length" ["text"] False "int4",
>         CatCreateFunction "bit_length" ["bit"] False "int4",
>         CatCreateFunction "convert" ["bytea", "name", "name"] False
>           "bytea",
>         CatCreateFunction "iclikesel"
>           ["internal", "oid", "internal", "int4"]
>           False
>           "float8",
>         CatCreateFunction "icnlikesel"
>           ["internal", "oid", "internal", "int4"]
>           False
>           "float8",
>         CatCreateFunction "iclikejoinsel"
>           ["internal", "oid", "internal", "int2", "internal"]
>           False
>           "float8",
>         CatCreateFunction "icnlikejoinsel"
>           ["internal", "oid", "internal", "int2", "internal"]
>           False
>           "float8",
>         CatCreateFunction "regexeqsel"
>           ["internal", "oid", "internal", "int4"]
>           False
>           "float8",
>         CatCreateFunction "likesel" ["internal", "oid", "internal", "int4"]
>           False
>           "float8",
>         CatCreateFunction "icregexeqsel"
>           ["internal", "oid", "internal", "int4"]
>           False
>           "float8",
>         CatCreateFunction "regexnesel"
>           ["internal", "oid", "internal", "int4"]
>           False
>           "float8",
>         CatCreateFunction "nlikesel"
>           ["internal", "oid", "internal", "int4"]
>           False
>           "float8",
>         CatCreateFunction "icregexnesel"
>           ["oid", "internal", "internal", "int4"]
>           False
>           "float8",
>         CatCreateFunction "regexeqjoinsel"
>           ["oid", "internal", "internal", "int2", "internal"]
>           False
>           "float8",
>         CatCreateFunction "likejoinsel"
>           ["internal", "internal", "oid", "internal", "int2"]
>           False
>           "float8",
>         CatCreateFunction "icregexeqjoinsel"
>           ["int2", "internal", "oid", "internal", "internal"]
>           False
>           "float8",
>         CatCreateFunction "regexnejoinsel"
>           ["internal", "internal", "oid", "int2", "internal"]
>           False
>           "float8",
>         CatCreateFunction "nlikejoinsel"
>           ["internal", "oid", "internal", "int2", "internal"]
>           False
>           "float8",
>         CatCreateFunction "icregexnejoinsel"
>           ["internal", "internal", "oid", "internal", "int2"]
>           False
>           "float8",
>         CatCreateFunction "float8_avg" ["_float8"] False "float8",
>         CatCreateFunction "float8_var_samp" ["_float8"] False "float8",
>         CatCreateFunction "float8_stddev_samp" ["_float8"] False "float8",
>         CatCreateFunction "numeric_accum" ["_numeric", "numeric"] False
>           "_numeric",
>         CatCreateFunction "int2_accum" ["_numeric", "int2"] False
>           "_numeric",
>         CatCreateFunction "int4_accum" ["_numeric", "int4"] False
>           "_numeric",
>         CatCreateFunction "int8_accum" ["_numeric", "int8"] False
>           "_numeric",
>         CatCreateFunction "numeric_avg" ["_numeric"] False "numeric",
>         CatCreateFunction "numeric_var_samp" ["_numeric"] False "numeric",
>         CatCreateFunction "numeric_stddev_samp" ["_numeric"] False
>           "numeric",
>         CatCreateFunction "int2_sum" ["int8", "int2"] False "int8",
>         CatCreateFunction "int4_sum" ["int8", "int4"] False "int8",
>         CatCreateFunction "int8_sum" ["int8", "numeric"] False "numeric",
>         CatCreateFunction "interval_accum" ["_interval", "interval"] False
>           "_interval",
>         CatCreateFunction "interval_avg" ["_interval"] False "interval",
>         CatCreateFunction "to_ascii" ["text"] False "text",
>         CatCreateFunction "to_ascii" ["text", "int4"] False "text",
>         CatCreateFunction "to_ascii" ["text", "name"] False "text",
>         CatCreateFunction "interval_pl_time" ["interval", "time"] False
>           "time",
>         CatCreateFunction "int28eq" ["int2", "int8"] False "bool",
>         CatCreateFunction "int28ne" ["int2", "int8"] False "bool",
>         CatCreateFunction "int28lt" ["int2", "int8"] False "bool",
>         CatCreateFunction "int28gt" ["int2", "int8"] False "bool",
>         CatCreateFunction "int28le" ["int2", "int8"] False "bool",
>         CatCreateFunction "int28ge" ["int2", "int8"] False "bool",
>         CatCreateFunction "int82eq" ["int8", "int2"] False "bool",
>         CatCreateFunction "int82ne" ["int8", "int2"] False "bool",
>         CatCreateFunction "int82lt" ["int8", "int2"] False "bool",
>         CatCreateFunction "int82gt" ["int8", "int2"] False "bool",
>         CatCreateFunction "int82le" ["int8", "int2"] False "bool",
>         CatCreateFunction "int82ge" ["int8", "int2"] False "bool",
>         CatCreateFunction "int2and" ["int2", "int2"] False "int2",
>         CatCreateFunction "int2or" ["int2", "int2"] False "int2",
>         CatCreateFunction "int2xor" ["int2", "int2"] False "int2",
>         CatCreateFunction "int2not" ["int2"] False "int2",
>         CatCreateFunction "int2shl" ["int4", "int2"] False "int2",
>         CatCreateFunction "int2shr" ["int2", "int4"] False "int2",
>         CatCreateFunction "int4and" ["int4", "int4"] False "int4",
>         CatCreateFunction "int4or" ["int4", "int4"] False "int4",
>         CatCreateFunction "int4xor" ["int4", "int4"] False "int4",
>         CatCreateFunction "int4not" ["int4"] False "int4",
>         CatCreateFunction "int4shl" ["int4", "int4"] False "int4",
>         CatCreateFunction "int4shr" ["int4", "int4"] False "int4",
>         CatCreateFunction "int8and" ["int8", "int8"] False "int8",
>         CatCreateFunction "int8or" ["int8", "int8"] False "int8",
>         CatCreateFunction "int8xor" ["int8", "int8"] False "int8",
>         CatCreateFunction "int8not" ["int8"] False "int8",
>         CatCreateFunction "int8shl" ["int8", "int4"] False "int8",
>         CatCreateFunction "int8shr" ["int8", "int4"] False "int8",
>         CatCreateFunction "int8up" ["int8"] False "int8",
>         CatCreateFunction "int2up" ["int2"] False "int2",
>         CatCreateFunction "int4up" ["int4"] False "int4",
>         CatCreateFunction "float4up" ["float4"] False "float4",
>         CatCreateFunction "float8up" ["float8"] False "float8",
>         CatCreateFunction "numeric_uplus" ["numeric"] False "numeric",
>         CatCreateFunction "has_table_privilege" ["text", "text", "name"]
>           False
>           "bool",
>         CatCreateFunction "has_table_privilege" ["name", "text", "oid"]
>           False
>           "bool",
>         CatCreateFunction "has_table_privilege" ["oid", "text", "text"]
>           False
>           "bool",
>         CatCreateFunction "has_table_privilege" ["oid", "oid", "text"]
>           False
>           "bool",
>         CatCreateFunction "has_table_privilege" ["text", "text"] False
>           "bool",
>         CatCreateFunction "has_table_privilege" ["oid", "text"] False
>           "bool",
>         CatCreateFunction "pg_stat_get_numscans" ["oid"] False "int8",
>         CatCreateFunction "pg_stat_get_tuples_returned" ["oid"] False
>           "int8",
>         CatCreateFunction "pg_stat_get_tuples_fetched" ["oid"] False
>           "int8",
>         CatCreateFunction "pg_stat_get_tuples_inserted" ["oid"] False
>           "int8",
>         CatCreateFunction "pg_stat_get_tuples_updated" ["oid"] False
>           "int8",
>         CatCreateFunction "pg_stat_get_tuples_deleted" ["oid"] False
>           "int8",
>         CatCreateFunction "pg_stat_get_blocks_fetched" ["oid"] False
>           "int8",
>         CatCreateFunction "pg_stat_get_blocks_hit" ["oid"] False "int8",
>         CatCreateFunction "pg_stat_get_backend_pid" ["int4"] False "int4",
>         CatCreateFunction "pg_stat_get_backend_dbid" ["int4"] False "oid",
>         CatCreateFunction "pg_stat_get_backend_userid" ["int4"] False
>           "oid",
>         CatCreateFunction "pg_stat_get_backend_activity" ["int4"] False
>           "text",
>         CatCreateFunction "pg_stat_get_db_numbackends" ["oid"] False
>           "int4",
>         CatCreateFunction "pg_stat_get_db_xact_commit" ["oid"] False
>           "int8",
>         CatCreateFunction "pg_stat_get_db_xact_rollback" ["oid"] False
>           "int8",
>         CatCreateFunction "pg_stat_get_db_blocks_fetched" ["oid"] False
>           "int8",
>         CatCreateFunction "pg_stat_get_db_blocks_hit" ["oid"] False "int8",
>         CatCreateFunction "encode" ["bytea", "text"] False "text",
>         CatCreateFunction "decode" ["text", "text"] False "bytea",
>         CatCreateFunction "byteaeq" ["bytea", "bytea"] False "bool",
>         CatCreateFunction "bytealt" ["bytea", "bytea"] False "bool",
>         CatCreateFunction "byteale" ["bytea", "bytea"] False "bool",
>         CatCreateFunction "byteagt" ["bytea", "bytea"] False "bool",
>         CatCreateFunction "byteage" ["bytea", "bytea"] False "bool",
>         CatCreateFunction "byteane" ["bytea", "bytea"] False "bool",
>         CatCreateFunction "byteacmp" ["bytea", "bytea"] False "int4",
>         CatCreateFunction "timestamp" ["timestamp", "int4"] False
>           "timestamp",
>         CatCreateFunction "int2_avg_accum" ["int2", "_int8"] False "_int8",
>         CatCreateFunction "int4_avg_accum" ["_int8", "int4"] False "_int8",
>         CatCreateFunction "int8_avg" ["_int8"] False "numeric",
>         CatCreateFunction "oidlarger" ["oid", "oid"] False "oid",
>         CatCreateFunction "oidsmaller" ["oid", "oid"] False "oid",
>         CatCreateFunction "timestamptz" ["timestamptz", "int4"] False
>           "timestamptz",
>         CatCreateFunction "time" ["time", "int4"] False "time",
>         CatCreateFunction "timetz" ["timetz", "int4"] False "timetz",
>         CatCreateFunction "pg_stat_get_tuples_hot_updated" ["oid"] False
>           "int8",
>         CatCreateFunction "div" ["numeric", "numeric"] False "numeric",
>         CatCreateFunction "numeric_div_trunc" ["numeric", "numeric"] False
>           "numeric",
>         CatCreateFunction "shobj_description" ["oid", "name"] False "text",
>         CatCreateFunction "textanycat" ["anynonarray", "text"] False
>           "text",
>         CatCreateFunction "anytextcat" ["anynonarray", "text"] False
>           "text",
>         CatCreateFunction "bytealike" ["bytea", "bytea"] False "bool",
>         CatCreateFunction "byteanlike" ["bytea", "bytea"] False "bool",
>         CatCreateFunction "like" ["bytea", "bytea"] False "bool",
>         CatCreateFunction "notlike" ["bytea", "bytea"] False "bool",
>         CatCreateFunction "like_escape" ["bytea", "bytea"] False "bytea",
>         CatCreateFunction "length" ["bytea"] False "int4",
>         CatCreateFunction "byteacat" ["bytea", "bytea"] False "bytea",
>         CatCreateFunction "substring" ["int4", "int4", "bytea"] False
>           "bytea",
>         CatCreateFunction "substring" ["bytea", "int4"] False "bytea",
>         CatCreateFunction "position" ["bytea", "bytea"] False "int4",
>         CatCreateFunction "btrim" ["bytea", "bytea"] False "bytea",
>         CatCreateFunction "time" ["timestamptz"] False "time",
>         CatCreateFunction "date_trunc" ["text", "timestamp"] False
>           "timestamp",
>         CatCreateFunction "date_part" ["text", "timestamp"] False "float8",
>         CatCreateFunction "pg_stat_get_activity" ["int4"] False "record",
>         CatCreateFunction "timestamp" ["abstime"] False "timestamp",
>         CatCreateFunction "timestamp" ["date"] False "timestamp",
>         CatCreateFunction "timestamp" ["date", "time"] False "timestamp",
>         CatCreateFunction "timestamp" ["timestamptz"] False "timestamp",
>         CatCreateFunction "timestamptz" ["timestamp"] False "timestamptz",
>         CatCreateFunction "date" ["timestamp"] False "date",
>         CatCreateFunction "abstime" ["timestamp"] False "abstime",
>         CatCreateFunction "timestamp_mi" ["timestamp", "timestamp"] False
>           "interval",
>         CatCreateFunction "timestamp_pl_interval" ["timestamp", "interval"]
>           False
>           "timestamp",
>         CatCreateFunction "timestamp_mi_interval" ["timestamp", "interval"]
>           False
>           "timestamp",
>         CatCreateFunction "timestamp_smaller" ["timestamp", "timestamp"]
>           False
>           "timestamp",
>         CatCreateFunction "timestamp_larger" ["timestamp", "timestamp"]
>           False
>           "timestamp",
>         CatCreateFunction "timezone" ["text", "timetz"] False "timetz",
>         CatCreateFunction "timezone" ["interval", "timetz"] False "timetz",
>         CatCreateFunction "timestamp_hash" ["timestamp"] False "int4",
>         CatCreateFunction "overlaps"
>           ["timestamp", "timestamp", "timestamp", "timestamp"]
>           False
>           "bool",
>         CatCreateFunction "overlaps"
>           ["timestamp", "interval", "interval", "timestamp"]
>           False
>           "bool",
>         CatCreateFunction "overlaps"
>           ["timestamp", "timestamp", "interval", "timestamp"]
>           False
>           "bool",
>         CatCreateFunction "overlaps"
>           ["timestamp", "interval", "timestamp", "timestamp"]
>           False
>           "bool",
>         CatCreateFunction "timestamp_cmp" ["timestamp", "timestamp"] False
>           "int4",
>         CatCreateFunction "time" ["timetz"] False "time",
>         CatCreateFunction "timetz" ["time"] False "timetz",
>         CatCreateFunction "isfinite" ["timestamp"] False "bool",
>         CatCreateFunction "to_char" ["timestamp", "text"] False "text",
>         CatCreateFunction "timestamp_eq" ["timestamp", "timestamp"] False
>           "bool",
>         CatCreateFunction "timestamp_ne" ["timestamp", "timestamp"] False
>           "bool",
>         CatCreateFunction "timestamp_lt" ["timestamp", "timestamp"] False
>           "bool",
>         CatCreateFunction "timestamp_le" ["timestamp", "timestamp"] False
>           "bool",
>         CatCreateFunction "timestamp_ge" ["timestamp", "timestamp"] False
>           "bool",
>         CatCreateFunction "timestamp_gt" ["timestamp", "timestamp"] False
>           "bool",
>         CatCreateFunction "age" ["timestamp", "timestamp"] False
>           "interval",
>         CatCreateFunction "age" ["timestamp"] False "interval",
>         CatCreateFunction "timezone" ["timestamp", "text"] False
>           "timestamptz",
>         CatCreateFunction "timezone" ["interval", "timestamp"] False
>           "timestamptz",
>         CatCreateFunction "date_pl_interval" ["interval", "date"] False
>           "timestamp",
>         CatCreateFunction "date_mi_interval" ["date", "interval"] False
>           "timestamp",
>         CatCreateFunction "substring" ["text", "text"] False "text",
>         CatCreateFunction "substring" ["text", "text", "text"] False
>           "text",
>         CatCreateFunction "bit" ["int8", "int4"] False "bit",
>         CatCreateFunction "int8" ["bit"] False "int8",
>         CatCreateFunction "current_setting" ["text"] False "text",
>         CatCreateFunction "set_config" ["text", "text", "bool"] False
>           "text",
>         CatCreateFunction "pg_table_is_visible" ["oid"] False "bool",
>         CatCreateFunction "pg_type_is_visible" ["oid"] False "bool",
>         CatCreateFunction "pg_function_is_visible" ["oid"] False "bool",
>         CatCreateFunction "pg_operator_is_visible" ["oid"] False "bool",
>         CatCreateFunction "pg_opclass_is_visible" ["oid"] False "bool",
>         CatCreateFunction "substr" ["bytea", "int4", "int4"] False "bytea",
>         CatCreateFunction "substr" ["int4", "bytea"] False "bytea",
>         CatCreateFunction "replace" ["text", "text", "text"] False "text",
>         CatCreateFunction "split_part" ["text", "text", "int4"] False
>           "text",
>         CatCreateFunction "to_hex" ["int4"] False "text",
>         CatCreateFunction "to_hex" ["int8"] False "text",
>         CatCreateFunction "array_lower" ["anyarray", "int4"] False "int4",
>         CatCreateFunction "array_upper" ["anyarray", "int4"] False "int4",
>         CatCreateFunction "pg_conversion_is_visible" ["oid"] False "bool",
>         CatCreateFunction "pg_stat_get_backend_activity_start" ["int4"]
>           False
>           "timestamptz",
>         CatCreateFunction "pg_terminate_backend" ["int4"] False "bool",
>         CatCreateFunction "pg_get_functiondef" ["oid"] False "text",
>         CatCreateFunction "text_pattern_lt" ["text", "text"] False "bool",
>         CatCreateFunction "text_pattern_le" ["text", "text"] False "bool",
>         CatCreateFunction "pg_get_function_arguments" ["oid"] False "text",
>         CatCreateFunction "text_pattern_ge" ["text", "text"] False "bool",
>         CatCreateFunction "text_pattern_gt" ["text", "text"] False "bool",
>         CatCreateFunction "pg_get_function_result" ["oid"] False "text",
>         CatCreateFunction "bttext_pattern_cmp" ["text", "text"] False
>           "int4",
>         CatCreateFunction "ceiling" ["numeric"] False "numeric",
>         CatCreateFunction "pg_database_size" ["name"] False "int8",
>         CatCreateFunction "power" ["numeric", "numeric"] False "numeric",
>         CatCreateFunction "width_bucket"
>           ["int4", "numeric", "numeric", "numeric"]
>           False
>           "int4",
>         CatCreateFunction "pg_cancel_backend" ["int4"] False "bool",
>         CatCreateFunction "pg_start_backup" ["text", "bool"] False "text",
>         CatCreateFunction "bpchar_pattern_lt" ["bpchar", "bpchar"] False
>           "bool",
>         CatCreateFunction "bpchar_pattern_le" ["bpchar", "bpchar"] False
>           "bool",
>         CatCreateFunction "array_length" ["anyarray", "int4"] False "int4",
>         CatCreateFunction "bpchar_pattern_ge" ["bpchar", "bpchar"] False
>           "bool",
>         CatCreateFunction "bpchar_pattern_gt" ["bpchar", "bpchar"] False
>           "bool",
>         CatCreateFunction "gist_point_consistent"
>           ["point", "int4", "oid", "internal", "internal"]
>           False
>           "bool",
>         CatCreateFunction "btbpchar_pattern_cmp" ["bpchar", "bpchar"] False
>           "int4",
>         CatCreateFunction "has_sequence_privilege" ["text", "text", "name"]
>           False
>           "bool",
>         CatCreateFunction "has_sequence_privilege" ["name", "text", "oid"]
>           False
>           "bool",
>         CatCreateFunction "has_sequence_privilege" ["oid", "text", "text"]
>           False
>           "bool",
>         CatCreateFunction "has_sequence_privilege" ["oid", "oid", "text"]
>           False
>           "bool",
>         CatCreateFunction "has_sequence_privilege" ["text", "text"] False
>           "bool",
>         CatCreateFunction "has_sequence_privilege" ["oid", "text"] False
>           "bool",
>         CatCreateFunction "btint48cmp" ["int8", "int4"] False "int4",
>         CatCreateFunction "btint84cmp" ["int8", "int4"] False "int4",
>         CatCreateFunction "btint24cmp" ["int2", "int4"] False "int4",
>         CatCreateFunction "btint42cmp" ["int4", "int2"] False "int4",
>         CatCreateFunction "btint28cmp" ["int8", "int2"] False "int4",
>         CatCreateFunction "btint82cmp" ["int8", "int2"] False "int4",
>         CatCreateFunction "btfloat48cmp" ["float8", "float4"] False "int4",
>         CatCreateFunction "btfloat84cmp" ["float8", "float4"] False "int4",
>         CatCreateFunction "regprocedurein" ["cstring"] False
>           "regprocedure",
>         CatCreateFunction "regprocedureout" ["regprocedure"] False
>           "cstring",
>         CatCreateFunction "regoperin" ["cstring"] False "regoper",
>         CatCreateFunction "regoperout" ["regoper"] False "cstring",
>         CatCreateFunction "regoperatorin" ["cstring"] False "regoperator",
>         CatCreateFunction "regoperatorout" ["regoperator"] False "cstring",
>         CatCreateFunction "regclassin" ["cstring"] False "regclass",
>         CatCreateFunction "regclassout" ["regclass"] False "cstring",
>         CatCreateFunction "regtypein" ["cstring"] False "regtype",
>         CatCreateFunction "regtypeout" ["regtype"] False "cstring",
>         CatCreateFunction "pg_get_function_identity_arguments" ["oid"]
>           False
>           "text",
>         CatCreateFunction "fmgr_internal_validator" ["oid"] False "void",
>         CatCreateFunction "fmgr_c_validator" ["oid"] False "void",
>         CatCreateFunction "fmgr_sql_validator" ["oid"] False "void",
>         CatCreateFunction "has_database_privilege" ["name", "text", "text"]
>           False
>           "bool",
>         CatCreateFunction "has_database_privilege" ["name", "oid", "text"]
>           False
>           "bool",
>         CatCreateFunction "has_database_privilege" ["oid", "text", "text"]
>           False
>           "bool",
>         CatCreateFunction "has_database_privilege" ["oid", "text", "oid"]
>           False
>           "bool",
>         CatCreateFunction "has_database_privilege" ["text", "text"] False
>           "bool",
>         CatCreateFunction "has_database_privilege" ["text", "oid"] False
>           "bool",
>         CatCreateFunction "has_function_privilege" ["name", "text", "text"]
>           False
>           "bool",
>         CatCreateFunction "has_function_privilege" ["name", "oid", "text"]
>           False
>           "bool",
>         CatCreateFunction "has_function_privilege" ["text", "text", "oid"]
>           False
>           "bool",
>         CatCreateFunction "has_function_privilege" ["oid", "text", "oid"]
>           False
>           "bool",
>         CatCreateFunction "has_function_privilege" ["text", "text"] False
>           "bool",
>         CatCreateFunction "has_function_privilege" ["text", "oid"] False
>           "bool",
>         CatCreateFunction "has_language_privilege" ["name", "text", "text"]
>           False
>           "bool",
>         CatCreateFunction "has_language_privilege" ["name", "oid", "text"]
>           False
>           "bool",
>         CatCreateFunction "has_language_privilege" ["text", "text", "oid"]
>           False
>           "bool",
>         CatCreateFunction "has_language_privilege" ["oid", "oid", "text"]
>           False
>           "bool",
>         CatCreateFunction "has_language_privilege" ["text", "text"] False
>           "bool",
>         CatCreateFunction "has_language_privilege" ["text", "oid"] False
>           "bool",
>         CatCreateFunction "has_schema_privilege" ["name", "text", "text"]
>           False
>           "bool",
>         CatCreateFunction "has_schema_privilege" ["name", "oid", "text"]
>           False
>           "bool",
>         CatCreateFunction "has_schema_privilege" ["text", "text", "oid"]
>           False
>           "bool",
>         CatCreateFunction "has_schema_privilege" ["oid", "oid", "text"]
>           False
>           "bool",
>         CatCreateFunction "has_schema_privilege" ["text", "text"] False
>           "bool",
>         CatCreateFunction "has_schema_privilege" ["text", "oid"] False
>           "bool",
>         CatCreateFunction "regexp_replace" ["text", "text", "text"] False
>           "text",
>         CatCreateFunction "regexp_replace" ["text", "text", "text", "text"]
>           False
>           "text",
>         CatCreateFunction "pg_total_relation_size" ["regclass"] False
>           "int8",
>         CatCreateFunction "pg_size_pretty" ["int8"] False "text",
>         CatCreateFunction "pg_options_to_table" ["_text"] False "record",
>         CatCreateFunction "record_in" ["int4", "cstring", "oid"] False
>           "record",
>         CatCreateFunction "record_out" ["record"] False "cstring",
>         CatCreateFunction "cstring_in" ["cstring"] False "cstring",
>         CatCreateFunction "cstring_out" ["cstring"] False "cstring",
>         CatCreateFunction "any_in" ["cstring"] False "any",
>         CatCreateFunction "any_out" ["any"] False "cstring",
>         CatCreateFunction "anyarray_in" ["cstring"] False "anyarray",
>         CatCreateFunction "anyarray_out" ["anyarray"] False "cstring",
>         CatCreateFunction "void_in" ["cstring"] False "void",
>         CatCreateFunction "void_out" ["void"] False "cstring",
>         CatCreateFunction "trigger_in" ["cstring"] False "trigger",
>         CatCreateFunction "trigger_out" ["trigger"] False "cstring",
>         CatCreateFunction "language_handler_in" ["cstring"] False
>           "language_handler",
>         CatCreateFunction "language_handler_out" ["language_handler"] False
>           "cstring",
>         CatCreateFunction "internal_in" ["cstring"] False "internal",
>         CatCreateFunction "internal_out" ["internal"] False "cstring",
>         CatCreateFunction "opaque_in" ["cstring"] False "opaque",
>         CatCreateFunction "opaque_out" ["opaque"] False "cstring",
>         CatCreateFunction "ceil" ["float8"] False "float8",
>         CatCreateFunction "floor" ["float8"] False "float8",
>         CatCreateFunction "sign" ["float8"] False "float8",
>         CatCreateFunction "md5" ["text"] False "text",
>         CatCreateFunction "anyelement_in" ["cstring"] False "anyelement",
>         CatCreateFunction "anyelement_out" ["anyelement"] False "cstring",
>         CatCreateFunction "postgresql_fdw_validator" ["_text", "oid"] False
>           "bool",
>         CatCreateFunction "pg_encoding_max_length" ["int4"] False "int4",
>         CatCreateFunction "ceiling" ["float8"] False "float8",
>         CatCreateFunction "md5" ["bytea"] False "text",
>         CatCreateFunction "pg_tablespace_size" ["oid"] False "int8",
>         CatCreateFunction "pg_tablespace_size" ["name"] False "int8",
>         CatCreateFunction "pg_database_size" ["oid"] False "int8",
>         CatCreateFunction "pg_relation_size" ["regclass"] False "int8",
>         CatCreateFunction "unnest" ["anyarray"] False "anyelement",
>         CatCreateFunction "pg_relation_size" ["regclass", "text"] False
>           "int8",
>         CatCreateFunction "array_agg_transfn" ["internal", "anyelement"]
>           False
>           "internal",
>         CatCreateFunction "array_agg_finalfn" ["internal"] False
>           "anyarray",
>         CatCreateFunction "date_lt_timestamp" ["timestamp", "date"] False
>           "bool",
>         CatCreateFunction "date_le_timestamp" ["date", "timestamp"] False
>           "bool",
>         CatCreateFunction "date_eq_timestamp" ["timestamp", "date"] False
>           "bool",
>         CatCreateFunction "date_gt_timestamp" ["date", "timestamp"] False
>           "bool",
>         CatCreateFunction "date_ge_timestamp" ["date", "timestamp"] False
>           "bool",
>         CatCreateFunction "date_ne_timestamp" ["timestamp", "date"] False
>           "bool",
>         CatCreateFunction "date_cmp_timestamp" ["timestamp", "date"] False
>           "int4",
>         CatCreateFunction "date_lt_timestamptz" ["date", "timestamptz"]
>           False
>           "bool",
>         CatCreateFunction "date_le_timestamptz" ["date", "timestamptz"]
>           False
>           "bool",
>         CatCreateFunction "date_eq_timestamptz" ["date", "timestamptz"]
>           False
>           "bool",
>         CatCreateFunction "date_gt_timestamptz" ["date", "timestamptz"]
>           False
>           "bool",
>         CatCreateFunction "date_ge_timestamptz" ["timestamptz", "date"]
>           False
>           "bool",
>         CatCreateFunction "date_ne_timestamptz" ["date", "timestamptz"]
>           False
>           "bool",
>         CatCreateFunction "date_cmp_timestamptz" ["date", "timestamptz"]
>           False
>           "int4",
>         CatCreateFunction "timestamp_lt_date" ["timestamp", "date"] False
>           "bool",
>         CatCreateFunction "timestamp_le_date" ["date", "timestamp"] False
>           "bool",
>         CatCreateFunction "timestamp_eq_date" ["timestamp", "date"] False
>           "bool",
>         CatCreateFunction "timestamp_gt_date" ["timestamp", "date"] False
>           "bool",
>         CatCreateFunction "timestamp_ge_date" ["timestamp", "date"] False
>           "bool",
>         CatCreateFunction "timestamp_ne_date" ["timestamp", "date"] False
>           "bool",
>         CatCreateFunction "timestamp_cmp_date" ["timestamp", "date"] False
>           "int4",
>         CatCreateFunction "timestamptz_lt_date" ["date", "timestamptz"]
>           False
>           "bool",
>         CatCreateFunction "timestamptz_le_date" ["timestamptz", "date"]
>           False
>           "bool",
>         CatCreateFunction "timestamptz_eq_date" ["date", "timestamptz"]
>           False
>           "bool",
>         CatCreateFunction "timestamptz_gt_date" ["timestamptz", "date"]
>           False
>           "bool",
>         CatCreateFunction "timestamptz_ge_date" ["date", "timestamptz"]
>           False
>           "bool",
>         CatCreateFunction "timestamptz_ne_date" ["date", "timestamptz"]
>           False
>           "bool",
>         CatCreateFunction "timestamptz_cmp_date" ["timestamptz", "date"]
>           False
>           "int4",
>         CatCreateFunction "has_tablespace_privilege"
>           ["text", "name", "text"]
>           False
>           "bool",
>         CatCreateFunction "has_tablespace_privilege"
>           ["name", "oid", "text"]
>           False
>           "bool",
>         CatCreateFunction "has_tablespace_privilege"
>           ["text", "text", "oid"]
>           False
>           "bool",
>         CatCreateFunction "has_tablespace_privilege" ["oid", "text", "oid"]
>           False
>           "bool",
>         CatCreateFunction "has_tablespace_privilege" ["text", "text"] False
>           "bool",
>         CatCreateFunction "has_tablespace_privilege" ["oid", "text"] False
>           "bool",
>         CatCreateFunction "shell_in" ["cstring"] False "opaque",
>         CatCreateFunction "shell_out" ["opaque"] False "cstring",
>         CatCreateFunction "array_recv" ["internal", "oid", "int4"] False
>           "anyarray",
>         CatCreateFunction "array_send" ["anyarray"] False "bytea",
>         CatCreateFunction "record_recv" ["int4", "oid", "internal"] False
>           "record",
>         CatCreateFunction "record_send" ["record"] False "bytea",
>         CatCreateFunction "int2recv" ["internal"] False "int2",
>         CatCreateFunction "int2send" ["int2"] False "bytea",
>         CatCreateFunction "int4recv" ["internal"] False "int4",
>         CatCreateFunction "int4send" ["int4"] False "bytea",
>         CatCreateFunction "int8recv" ["internal"] False "int8",
>         CatCreateFunction "int8send" ["int8"] False "bytea",
>         CatCreateFunction "int2vectorrecv" ["internal"] False "int2vector",
>         CatCreateFunction "int2vectorsend" ["int2vector"] False "bytea",
>         CatCreateFunction "bytearecv" ["internal"] False "bytea",
>         CatCreateFunction "byteasend" ["bytea"] False "bytea",
>         CatCreateFunction "textrecv" ["internal"] False "text",
>         CatCreateFunction "textsend" ["text"] False "bytea",
>         CatCreateFunction "unknownrecv" ["internal"] False "unknown",
>         CatCreateFunction "unknownsend" ["unknown"] False "bytea",
>         CatCreateFunction "oidrecv" ["internal"] False "oid",
>         CatCreateFunction "oidsend" ["oid"] False "bytea",
>         CatCreateFunction "oidvectorrecv" ["internal"] False "oidvector",
>         CatCreateFunction "oidvectorsend" ["oidvector"] False "bytea",
>         CatCreateFunction "namerecv" ["internal"] False "name",
>         CatCreateFunction "namesend" ["name"] False "bytea",
>         CatCreateFunction "float4recv" ["internal"] False "float4",
>         CatCreateFunction "float4send" ["float4"] False "bytea",
>         CatCreateFunction "float8recv" ["internal"] False "float8",
>         CatCreateFunction "float8send" ["float8"] False "bytea",
>         CatCreateFunction "point_recv" ["internal"] False "point",
>         CatCreateFunction "point_send" ["point"] False "bytea",
>         CatCreateFunction "bpcharrecv" ["internal", "int4", "oid"] False
>           "bpchar",
>         CatCreateFunction "bpcharsend" ["bpchar"] False "bytea",
>         CatCreateFunction "varcharrecv" ["int4", "oid", "internal"] False
>           "varchar",
>         CatCreateFunction "varcharsend" ["varchar"] False "bytea",
>         CatCreateFunction "charrecv" ["internal"] False "char",
>         CatCreateFunction "charsend" ["char"] False "bytea",
>         CatCreateFunction "boolrecv" ["internal"] False "bool",
>         CatCreateFunction "boolsend" ["bool"] False "bytea",
>         CatCreateFunction "tidrecv" ["internal"] False "tid",
>         CatCreateFunction "tidsend" ["tid"] False "bytea",
>         CatCreateFunction "xidrecv" ["internal"] False "xid",
>         CatCreateFunction "xidsend" ["xid"] False "bytea",
>         CatCreateFunction "cidrecv" ["internal"] False "cid",
>         CatCreateFunction "cidsend" ["cid"] False "bytea",
>         CatCreateFunction "regprocrecv" ["internal"] False "regproc",
>         CatCreateFunction "regprocsend" ["regproc"] False "bytea",
>         CatCreateFunction "regprocedurerecv" ["internal"] False
>           "regprocedure",
>         CatCreateFunction "regproceduresend" ["regprocedure"] False
>           "bytea",
>         CatCreateFunction "regoperrecv" ["internal"] False "regoper",
>         CatCreateFunction "regopersend" ["regoper"] False "bytea",
>         CatCreateFunction "regoperatorrecv" ["internal"] False
>           "regoperator",
>         CatCreateFunction "regoperatorsend" ["regoperator"] False "bytea",
>         CatCreateFunction "regclassrecv" ["internal"] False "regclass",
>         CatCreateFunction "regclasssend" ["regclass"] False "bytea",
>         CatCreateFunction "regtyperecv" ["internal"] False "regtype",
>         CatCreateFunction "regtypesend" ["regtype"] False "bytea",
>         CatCreateFunction "bit_recv" ["internal", "oid", "int4"] False
>           "bit",
>         CatCreateFunction "bit_send" ["bit"] False "bytea",
>         CatCreateFunction "varbit_recv" ["internal", "oid", "int4"] False
>           "varbit",
>         CatCreateFunction "varbit_send" ["varbit"] False "bytea",
>         CatCreateFunction "numeric_recv" ["internal", "int4", "oid"] False
>           "numeric",
>         CatCreateFunction "numeric_send" ["numeric"] False "bytea",
>         CatCreateFunction "abstimerecv" ["internal"] False "abstime",
>         CatCreateFunction "abstimesend" ["abstime"] False "bytea",
>         CatCreateFunction "reltimerecv" ["internal"] False "reltime",
>         CatCreateFunction "reltimesend" ["reltime"] False "bytea",
>         CatCreateFunction "tintervalrecv" ["internal"] False "tinterval",
>         CatCreateFunction "tintervalsend" ["tinterval"] False "bytea",
>         CatCreateFunction "date_recv" ["internal"] False "date",
>         CatCreateFunction "date_send" ["date"] False "bytea",
>         CatCreateFunction "time_recv" ["internal", "oid", "int4"] False
>           "time",
>         CatCreateFunction "time_send" ["time"] False "bytea",
>         CatCreateFunction "timetz_recv" ["internal", "oid", "int4"] False
>           "timetz",
>         CatCreateFunction "timetz_send" ["timetz"] False "bytea",
>         CatCreateFunction "timestamp_recv" ["oid", "int4", "internal"]
>           False
>           "timestamp",
>         CatCreateFunction "timestamp_send" ["timestamp"] False "bytea",
>         CatCreateFunction "timestamptz_recv" ["int4", "internal", "oid"]
>           False
>           "timestamptz",
>         CatCreateFunction "timestamptz_send" ["timestamptz"] False "bytea",
>         CatCreateFunction "interval_recv" ["internal", "int4", "oid"] False
>           "interval",
>         CatCreateFunction "interval_send" ["interval"] False "bytea",
>         CatCreateFunction "lseg_recv" ["internal"] False "lseg",
>         CatCreateFunction "lseg_send" ["lseg"] False "bytea",
>         CatCreateFunction "path_recv" ["internal"] False "path",
>         CatCreateFunction "path_send" ["path"] False "bytea",
>         CatCreateFunction "box_recv" ["internal"] False "box",
>         CatCreateFunction "box_send" ["box"] False "bytea",
>         CatCreateFunction "poly_recv" ["internal"] False "polygon",
>         CatCreateFunction "poly_send" ["polygon"] False "bytea",
>         CatCreateFunction "line_recv" ["internal"] False "line",
>         CatCreateFunction "line_send" ["line"] False "bytea",
>         CatCreateFunction "circle_recv" ["internal"] False "circle",
>         CatCreateFunction "circle_send" ["circle"] False "bytea",
>         CatCreateFunction "cash_recv" ["internal"] False "money",
>         CatCreateFunction "cash_send" ["money"] False "bytea",
>         CatCreateFunction "macaddr_recv" ["internal"] False "macaddr",
>         CatCreateFunction "macaddr_send" ["macaddr"] False "bytea",
>         CatCreateFunction "inet_recv" ["internal"] False "inet",
>         CatCreateFunction "inet_send" ["inet"] False "bytea",
>         CatCreateFunction "cidr_recv" ["internal"] False "cidr",
>         CatCreateFunction "cidr_send" ["cidr"] False "bytea",
>         CatCreateFunction "cstring_recv" ["internal"] False "cstring",
>         CatCreateFunction "cstring_send" ["cstring"] False "bytea",
>         CatCreateFunction "anyarray_recv" ["internal"] False "anyarray",
>         CatCreateFunction "anyarray_send" ["anyarray"] False "bytea",
>         CatCreateFunction "pg_get_ruledef" ["bool", "oid"] False "text",
>         CatCreateFunction "pg_get_viewdef" ["text", "bool"] False "text",
>         CatCreateFunction "pg_get_viewdef" ["bool", "oid"] False "text",
>         CatCreateFunction "pg_get_indexdef" ["bool", "oid", "int4"] False
>           "text",
>         CatCreateFunction "pg_get_constraintdef" ["oid", "bool"] False
>           "text",
>         CatCreateFunction "pg_get_expr" ["bool", "pg_node_tree", "oid"]
>           False
>           "text",
>         CatCreateFunction "float8_var_pop" ["_float8"] False "float8",
>         CatCreateFunction "float8_stddev_pop" ["_float8"] False "float8",
>         CatCreateFunction "numeric_var_pop" ["_numeric"] False "numeric",
>         CatCreateFunction "booland_statefunc" ["bool", "bool"] False
>           "bool",
>         CatCreateFunction "boolor_statefunc" ["bool", "bool"] False "bool",
>         CatCreateFunction "timestamp_lt_timestamptz"
>           ["timestamp", "timestamptz"]
>           False
>           "bool",
>         CatCreateFunction "timestamp_le_timestamptz"
>           ["timestamptz", "timestamp"]
>           False
>           "bool",
>         CatCreateFunction "timestamp_eq_timestamptz"
>           ["timestamptz", "timestamp"]
>           False
>           "bool",
>         CatCreateFunction "timestamp_gt_timestamptz"
>           ["timestamptz", "timestamp"]
>           False
>           "bool",
>         CatCreateFunction "timestamp_ge_timestamptz"
>           ["timestamp", "timestamptz"]
>           False
>           "bool",
>         CatCreateFunction "timestamp_ne_timestamptz"
>           ["timestamptz", "timestamp"]
>           False
>           "bool",
>         CatCreateFunction "timestamp_cmp_timestamptz"
>           ["timestamptz", "timestamp"]
>           False
>           "int4",
>         CatCreateFunction "timestamptz_lt_timestamp"
>           ["timestamp", "timestamptz"]
>           False
>           "bool",
>         CatCreateFunction "timestamptz_le_timestamp"
>           ["timestamptz", "timestamp"]
>           False
>           "bool",
>         CatCreateFunction "timestamptz_eq_timestamp"
>           ["timestamptz", "timestamp"]
>           False
>           "bool",
>         CatCreateFunction "timestamptz_gt_timestamp"
>           ["timestamptz", "timestamp"]
>           False
>           "bool",
>         CatCreateFunction "timestamptz_ge_timestamp"
>           ["timestamp", "timestamptz"]
>           False
>           "bool",
>         CatCreateFunction "timestamptz_ne_timestamp"
>           ["timestamptz", "timestamp"]
>           False
>           "bool",
>         CatCreateFunction "timestamptz_cmp_timestamp"
>           ["timestamptz", "timestamp"]
>           False
>           "int4",
>         CatCreateFunction "interval_pl_date" ["interval", "date"] False
>           "timestamp",
>         CatCreateFunction "interval_pl_timetz" ["timetz", "interval"] False
>           "timetz",
>         CatCreateFunction "interval_pl_timestamp" ["interval", "timestamp"]
>           False
>           "timestamp",
>         CatCreateFunction "interval_pl_timestamptz"
>           ["timestamptz", "interval"]
>           False
>           "timestamptz",
>         CatCreateFunction "integer_pl_date" ["int4", "date"] False "date",
>         CatCreateFunction "pg_tablespace_databases" ["oid"] False "oid",
>         CatCreateFunction "bool" ["int4"] False "bool",
>         CatCreateFunction "int4" ["bool"] False "int4",
>         CatCreateFunction "gistvacuumcleanup" ["internal", "internal"]
>           False
>           "internal",
>         CatCreateFunction "box_below" ["box", "box"] False "bool",
>         CatCreateFunction "box_overbelow" ["box", "box"] False "bool",
>         CatCreateFunction "box_overabove" ["box", "box"] False "bool",
>         CatCreateFunction "box_above" ["box", "box"] False "bool",
>         CatCreateFunction "poly_below" ["polygon", "polygon"] False "bool",
>         CatCreateFunction "poly_overbelow" ["polygon", "polygon"] False
>           "bool",
>         CatCreateFunction "poly_overabove" ["polygon", "polygon"] False
>           "bool",
>         CatCreateFunction "poly_above" ["polygon", "polygon"] False "bool",
>         CatCreateFunction "gist_box_consistent"
>           ["box", "oid", "int4", "internal", "internal"]
>           False
>           "bool",
>         CatCreateFunction "gist_box_compress" ["internal"] False
>           "internal",
>         CatCreateFunction "gist_box_decompress" ["internal"] False
>           "internal",
>         CatCreateFunction "gist_box_penalty"
>           ["internal", "internal", "internal"]
>           False
>           "internal",
>         CatCreateFunction "gist_box_picksplit" ["internal", "internal"]
>           False
>           "internal",
>         CatCreateFunction "gist_box_union" ["internal", "internal"] False
>           "box",
>         CatCreateFunction "gist_box_same" ["internal", "box", "box"] False
>           "internal",
>         CatCreateFunction "gist_poly_consistent"
>           ["internal", "int4", "oid", "internal", "polygon"]
>           False
>           "bool",
>         CatCreateFunction "gist_poly_compress" ["internal"] False
>           "internal",
>         CatCreateFunction "circle_overbelow" ["circle", "circle"] False
>           "bool",
>         CatCreateFunction "circle_overabove" ["circle", "circle"] False
>           "bool",
>         CatCreateFunction "gist_circle_consistent"
>           ["internal", "internal", "circle", "oid", "int4"]
>           False
>           "bool",
>         CatCreateFunction "gist_circle_compress" ["internal"] False
>           "internal",
>         CatCreateFunction "numeric_stddev_pop" ["_numeric"] False
>           "numeric",
>         CatCreateFunction "domain_in" ["int4", "oid", "cstring"] False
>           "any",
>         CatCreateFunction "domain_recv" ["internal", "int4", "oid"] False
>           "any",
>         CatCreateFunction "xmlexists" ["text", "xml"] False "bool",
>         CatCreateFunction "pg_stat_file" ["text"] False "record",
>         CatCreateFunction "pg_read_file" ["text", "int8", "int8"] False
>           "text",
>         CatCreateFunction "pg_ls_dir" ["text"] False "text",
>         CatCreateFunction "pg_sleep" ["float8"] False "void",
>         CatCreateFunction "inetnot" ["inet"] False "inet",
>         CatCreateFunction "inetand" ["inet", "inet"] False "inet",
>         CatCreateFunction "inetor" ["inet", "inet"] False "inet",
>         CatCreateFunction "inetpl" ["int8", "inet"] False "inet",
>         CatCreateFunction "int8pl_inet" ["int8", "inet"] False "inet",
>         CatCreateFunction "inetmi_int8" ["int8", "inet"] False "inet",
>         CatCreateFunction "inetmi" ["inet", "inet"] False "int8",
>         CatCreateFunction "gin_cmp_prefix"
>           ["text", "text", "int2", "internal"]
>           False
>           "int4",
>         CatCreateFunction "pg_has_role" ["name", "name", "text"] False
>           "bool",
>         CatCreateFunction "pg_has_role" ["name", "oid", "text"] False
>           "bool",
>         CatCreateFunction "pg_has_role" ["oid", "text", "name"] False
>           "bool",
>         CatCreateFunction "pg_has_role" ["oid", "oid", "text"] False
>           "bool",
>         CatCreateFunction "pg_has_role" ["text", "name"] False "bool",
>         CatCreateFunction "pg_has_role" ["oid", "text"] False "bool",
>         CatCreateFunction "justify_interval" ["interval"] False "interval",
>         CatCreateFunction "pg_get_triggerdef" ["oid", "bool"] False "text",
>         CatCreateFunction "gingetbitmap" ["internal", "internal"] False
>           "int8",
>         CatCreateFunction "gininsert"
>           ["internal", "internal", "internal", "internal", "internal",
>            "internal"]
>           False
>           "bool",
>         CatCreateFunction "ginbeginscan"
>           ["internal", "internal", "internal"]
>           False
>           "internal",
>         CatCreateFunction "ginrescan"
>           ["internal", "internal", "internal", "internal", "internal"]
>           False
>           "void",
>         CatCreateFunction "ginendscan" ["internal"] False "void",
>         CatCreateFunction "ginmarkpos" ["internal"] False "void",
>         CatCreateFunction "ginrestrpos" ["internal"] False "void",
>         CatCreateFunction "ginbuild" ["internal", "internal", "internal"]
>           False
>           "internal",
>         CatCreateFunction "ginbulkdelete"
>           ["internal", "internal", "internal", "internal"]
>           False
>           "internal",
>         CatCreateFunction "ginvacuumcleanup" ["internal", "internal"] False
>           "internal",
>         CatCreateFunction "gincostestimate"
>           ["internal", "internal", "internal", "internal", "internal",
>            "internal", "internal", "internal", "internal"]
>           False
>           "void",
>         CatCreateFunction "ginarrayextract"
>           ["anyarray", "internal", "internal"]
>           False
>           "internal",
>         CatCreateFunction "ginarrayconsistent"
>           ["anyarray", "int2", "internal", "int4", "internal", "internal",
>            "internal", "internal"]
>           False
>           "bool",
>         CatCreateFunction "int8_avg_accum" ["_numeric", "int8"] False
>           "_numeric",
>         CatCreateFunction "arrayoverlap" ["anyarray", "anyarray"] False
>           "bool",
>         CatCreateFunction "arraycontains" ["anyarray", "anyarray"] False
>           "bool",
>         CatCreateFunction "arraycontained" ["anyarray", "anyarray"] False
>           "bool",
>         CatCreateFunction "pg_stat_get_db_tuples_returned" ["oid"] False
>           "int8",
>         CatCreateFunction "pg_stat_get_db_tuples_fetched" ["oid"] False
>           "int8",
>         CatCreateFunction "pg_stat_get_db_tuples_inserted" ["oid"] False
>           "int8",
>         CatCreateFunction "pg_stat_get_db_tuples_updated" ["oid"] False
>           "int8",
>         CatCreateFunction "pg_stat_get_db_tuples_deleted" ["oid"] False
>           "int8",
>         CatCreateFunction "regexp_matches" ["text", "text"] False "_text",
>         CatCreateFunction "regexp_matches" ["text", "text", "text"] False
>           "_text",
>         CatCreateFunction "regexp_split_to_table" ["text", "text"] False
>           "text",
>         CatCreateFunction "regexp_split_to_table" ["text", "text", "text"]
>           False
>           "text",
>         CatCreateFunction "regexp_split_to_array" ["text", "text"] False
>           "_text",
>         CatCreateFunction "regexp_split_to_array" ["text", "text", "text"]
>           False
>           "_text",
>         CatCreateFunction "ginqueryarrayextract"
>           ["internal", "internal", "internal", "internal", "internal",
>            "anyarray", "int2"]
>           False
>           "internal",
>         CatCreateFunction "anynonarray_in" ["cstring"] False "anynonarray",
>         CatCreateFunction "anynonarray_out" ["anynonarray"] False
>           "cstring",
>         CatCreateFunction "pg_stat_get_last_vacuum_time" ["oid"] False
>           "timestamptz",
>         CatCreateFunction "pg_stat_get_last_autovacuum_time" ["oid"] False
>           "timestamptz",
>         CatCreateFunction "pg_stat_get_last_analyze_time" ["oid"] False
>           "timestamptz",
>         CatCreateFunction "pg_stat_get_last_autoanalyze_time" ["oid"] False
>           "timestamptz",
>         CatCreateFunction "btoptions" ["_text", "bool"] False "bytea",
>         CatCreateFunction "hashoptions" ["_text", "bool"] False "bytea",
>         CatCreateFunction "gistoptions" ["_text", "bool"] False "bytea",
>         CatCreateFunction "ginoptions" ["_text", "bool"] False "bytea",
>         CatCreateFunction "tidgt" ["tid", "tid"] False "bool",
>         CatCreateFunction "tidlt" ["tid", "tid"] False "bool",
>         CatCreateFunction "tidge" ["tid", "tid"] False "bool",
>         CatCreateFunction "tidle" ["tid", "tid"] False "bool",
>         CatCreateFunction "bttidcmp" ["tid", "tid"] False "int4",
>         CatCreateFunction "tidlarger" ["tid", "tid"] False "tid",
>         CatCreateFunction "tidsmaller" ["tid", "tid"] False "tid",
>         CatCreateFunction "int8inc_any" ["int8", "any"] False "int8",
>         CatCreateFunction "int8inc_float8_float8"
>           ["int8", "float8", "float8"]
>           False
>           "int8",
>         CatCreateFunction "float8_regr_accum"
>           ["float8", "float8", "_float8"]
>           False
>           "_float8",
>         CatCreateFunction "float8_regr_sxx" ["_float8"] False "float8",
>         CatCreateFunction "float8_regr_syy" ["_float8"] False "float8",
>         CatCreateFunction "float8_regr_sxy" ["_float8"] False "float8",
>         CatCreateFunction "float8_regr_avgx" ["_float8"] False "float8",
>         CatCreateFunction "float8_regr_avgy" ["_float8"] False "float8",
>         CatCreateFunction "float8_regr_r2" ["_float8"] False "float8",
>         CatCreateFunction "float8_regr_slope" ["_float8"] False "float8",
>         CatCreateFunction "float8_regr_intercept" ["_float8"] False
>           "float8",
>         CatCreateFunction "float8_covar_pop" ["_float8"] False "float8",
>         CatCreateFunction "float8_covar_samp" ["_float8"] False "float8",
>         CatCreateFunction "float8_corr" ["_float8"] False "float8",
>         CatCreateFunction "pg_xlogfile_name_offset" ["text"] False
>           "record",
>         CatCreateFunction "pg_xlogfile_name" ["text"] False "text",
>         CatCreateFunction "pg_stat_get_backend_waiting" ["int4"] False
>           "bool",
>         CatCreateFunction "pg_is_other_temp_schema" ["oid"] False "bool",
>         CatCreateFunction "pg_stat_get_backend_xact_start" ["int4"] False
>           "timestamptz",
>         CatCreateFunction "numeric_avg_accum" ["_numeric", "numeric"] False
>           "_numeric",
>         CatCreateFunction "pg_stat_get_live_tuples" ["oid"] False "int8",
>         CatCreateFunction "pg_stat_get_dead_tuples" ["oid"] False "int8",
>         CatCreateFunction "pg_advisory_lock" ["int8"] False "void",
>         CatCreateFunction "pg_advisory_lock_shared" ["int8"] False "void",
>         CatCreateFunction "pg_try_advisory_lock" ["int8"] False "bool",
>         CatCreateFunction "pg_try_advisory_lock_shared" ["int8"] False
>           "bool",
>         CatCreateFunction "pg_advisory_unlock" ["int8"] False "bool",
>         CatCreateFunction "pg_advisory_unlock_shared" ["int8"] False
>           "bool",
>         CatCreateFunction "pg_advisory_lock" ["int4", "int4"] False "void",
>         CatCreateFunction "pg_advisory_lock_shared" ["int4", "int4"] False
>           "void",
>         CatCreateFunction "pg_try_advisory_lock" ["int4", "int4"] False
>           "bool",
>         CatCreateFunction "pg_try_advisory_lock_shared" ["int4", "int4"]
>           False
>           "bool",
>         CatCreateFunction "pg_advisory_unlock" ["int4", "int4"] False
>           "bool",
>         CatCreateFunction "pg_advisory_unlock_shared" ["int4", "int4"]
>           False
>           "bool",
>         CatCreateFunction "xml_in" ["cstring"] False "xml",
>         CatCreateFunction "xml_out" ["xml"] False "cstring",
>         CatCreateFunction "xmlcomment" ["text"] False "xml",
>         CatCreateFunction "xml" ["text"] False "xml",
>         CatCreateFunction "xmlvalidate" ["xml", "text"] False "bool",
>         CatCreateFunction "xml_recv" ["internal"] False "xml",
>         CatCreateFunction "xml_send" ["xml"] False "bytea",
>         CatCreateFunction "xmlconcat2" ["xml", "xml"] False "xml",
>         CatCreateFunction "varbittypmodin" ["_cstring"] False "int4",
>         CatCreateFunction "intervaltypmodin" ["_cstring"] False "int4",
>         CatCreateFunction "intervaltypmodout" ["int4"] False "cstring",
>         CatCreateFunction "timestamptypmodin" ["_cstring"] False "int4",
>         CatCreateFunction "timestamptypmodout" ["int4"] False "cstring",
>         CatCreateFunction "timestamptztypmodin" ["_cstring"] False "int4",
>         CatCreateFunction "timestamptztypmodout" ["int4"] False "cstring",
>         CatCreateFunction "timetypmodin" ["_cstring"] False "int4",
>         CatCreateFunction "timetypmodout" ["int4"] False "cstring",
>         CatCreateFunction "timetztypmodin" ["_cstring"] False "int4",
>         CatCreateFunction "timetztypmodout" ["int4"] False "cstring",
>         CatCreateFunction "bpchartypmodin" ["_cstring"] False "int4",
>         CatCreateFunction "bpchartypmodout" ["int4"] False "cstring",
>         CatCreateFunction "varchartypmodin" ["_cstring"] False "int4",
>         CatCreateFunction "varchartypmodout" ["int4"] False "cstring",
>         CatCreateFunction "numerictypmodin" ["_cstring"] False "int4",
>         CatCreateFunction "numerictypmodout" ["int4"] False "cstring",
>         CatCreateFunction "bittypmodin" ["_cstring"] False "int4",
>         CatCreateFunction "bittypmodout" ["int4"] False "cstring",
>         CatCreateFunction "varbittypmodout" ["int4"] False "cstring",
>         CatCreateFunction "text" ["xml"] False "text",
>         CatCreateFunction "table_to_xml"
>           ["bool", "bool", "text", "regclass"]
>           False
>           "xml",
>         CatCreateFunction "query_to_xml" ["bool", "text", "bool", "text"]
>           False
>           "xml",
>         CatCreateFunction "cursor_to_xml"
>           ["int4", "bool", "text", "refcursor", "bool"]
>           False
>           "xml",
>         CatCreateFunction "table_to_xmlschema"
>           ["bool", "text", "bool", "regclass"]
>           False
>           "xml",
>         CatCreateFunction "query_to_xmlschema"
>           ["text", "bool", "text", "bool"]
>           False
>           "xml",
>         CatCreateFunction "cursor_to_xmlschema"
>           ["bool", "refcursor", "bool", "text"]
>           False
>           "xml",
>         CatCreateFunction "table_to_xml_and_xmlschema"
>           ["bool", "bool", "text", "regclass"]
>           False
>           "xml",
>         CatCreateFunction "query_to_xml_and_xmlschema"
>           ["text", "bool", "text", "bool"]
>           False
>           "xml",
>         CatCreateFunction "xpath" ["xml", "text", "_text"] False "_xml",
>         CatCreateFunction "xpath" ["text", "xml"] False "_xml",
>         CatCreateFunction "schema_to_xml" ["bool", "name", "text", "bool"]
>           False
>           "xml",
>         CatCreateFunction "schema_to_xmlschema"
>           ["name", "bool", "text", "bool"]
>           False
>           "xml",
>         CatCreateFunction "schema_to_xml_and_xmlschema"
>           ["text", "name", "bool", "bool"]
>           False
>           "xml",
>         CatCreateFunction "database_to_xml" ["text", "bool", "bool"] False
>           "xml",
>         CatCreateFunction "database_to_xmlschema" ["bool", "bool", "text"]
>           False
>           "xml",
>         CatCreateFunction "database_to_xml_and_xmlschema"
>           ["bool", "bool", "text"]
>           False
>           "xml",
>         CatCreateFunction "txid_snapshot_in" ["cstring"] False
>           "txid_snapshot",
>         CatCreateFunction "txid_snapshot_out" ["txid_snapshot"] False
>           "cstring",
>         CatCreateFunction "txid_snapshot_recv" ["internal"] False
>           "txid_snapshot",
>         CatCreateFunction "txid_snapshot_send" ["txid_snapshot"] False
>           "bytea",
>         CatCreateFunction "txid_snapshot_xmin" ["txid_snapshot"] False
>           "int8",
>         CatCreateFunction "txid_snapshot_xmax" ["txid_snapshot"] False
>           "int8",
>         CatCreateFunction "txid_snapshot_xip" ["txid_snapshot"] False
>           "int8",
>         CatCreateFunction "txid_visible_in_snapshot"
>           ["txid_snapshot", "int8"]
>           False
>           "bool",
>         CatCreateFunction "uuid_in" ["cstring"] False "uuid",
>         CatCreateFunction "uuid_out" ["uuid"] False "cstring",
>         CatCreateFunction "uuid_lt" ["uuid", "uuid"] False "bool",
>         CatCreateFunction "uuid_le" ["uuid", "uuid"] False "bool",
>         CatCreateFunction "uuid_eq" ["uuid", "uuid"] False "bool",
>         CatCreateFunction "uuid_ge" ["uuid", "uuid"] False "bool",
>         CatCreateFunction "uuid_gt" ["uuid", "uuid"] False "bool",
>         CatCreateFunction "uuid_ne" ["uuid", "uuid"] False "bool",
>         CatCreateFunction "uuid_cmp" ["uuid", "uuid"] False "int4",
>         CatCreateFunction "uuid_recv" ["internal"] False "uuid",
>         CatCreateFunction "uuid_send" ["uuid"] False "bytea",
>         CatCreateFunction "uuid_hash" ["uuid"] False "int4",
>         CatCreateFunction "text" ["bool"] False "text",
>         CatCreateFunction "pg_stat_get_function_calls" ["oid"] False
>           "int8",
>         CatCreateFunction "pg_stat_get_function_time" ["oid"] False "int8",
>         CatCreateFunction "pg_stat_get_function_self_time" ["oid"] False
>           "int8",
>         CatCreateFunction "record_eq" ["record", "record"] False "bool",
>         CatCreateFunction "record_ne" ["record", "record"] False "bool",
>         CatCreateFunction "record_lt" ["record", "record"] False "bool",
>         CatCreateFunction "record_gt" ["record", "record"] False "bool",
>         CatCreateFunction "record_le" ["record", "record"] False "bool",
>         CatCreateFunction "record_ge" ["record", "record"] False "bool",
>         CatCreateFunction "btrecordcmp" ["record", "record"] False "int4",
>         CatCreateFunction "pg_table_size" ["regclass"] False "int8",
>         CatCreateFunction "pg_indexes_size" ["regclass"] False "int8",
>         CatCreateFunction "pg_relation_filenode" ["regclass"] False "oid",
>         CatCreateFunction "has_foreign_data_wrapper_privilege"
>           ["text", "name", "text"]
>           False
>           "bool",
>         CatCreateFunction "has_foreign_data_wrapper_privilege"
>           ["oid", "name", "text"]
>           False
>           "bool",
>         CatCreateFunction "has_foreign_data_wrapper_privilege"
>           ["text", "text", "oid"]
>           False
>           "bool",
>         CatCreateFunction "has_foreign_data_wrapper_privilege"
>           ["oid", "text", "oid"]
>           False
>           "bool",
>         CatCreateFunction "has_foreign_data_wrapper_privilege"
>           ["text", "text"]
>           False
>           "bool",
>         CatCreateFunction "has_foreign_data_wrapper_privilege"
>           ["oid", "text"]
>           False
>           "bool",
>         CatCreateFunction "has_server_privilege" ["name", "text", "text"]
>           False
>           "bool",
>         CatCreateFunction "has_server_privilege" ["name", "text", "oid"]
>           False
>           "bool",
>         CatCreateFunction "has_server_privilege" ["text", "text", "oid"]
>           False
>           "bool",
>         CatCreateFunction "has_server_privilege" ["oid", "oid", "text"]
>           False
>           "bool",
>         CatCreateFunction "has_server_privilege" ["text", "text"] False
>           "bool",
>         CatCreateFunction "has_server_privilege" ["oid", "text"] False
>           "bool",
>         CatCreateFunction "has_column_privilege"
>           ["text", "text", "text", "name"]
>           False
>           "bool",
>         CatCreateFunction "has_column_privilege"
>           ["text", "text", "name", "int2"]
>           False
>           "bool",
>         CatCreateFunction "has_column_privilege"
>           ["text", "oid", "text", "name"]
>           False
>           "bool",
>         CatCreateFunction "has_column_privilege"
>           ["text", "oid", "int2", "name"]
>           False
>           "bool",
>         CatCreateFunction "has_column_privilege"
>           ["text", "text", "oid", "text"]
>           False
>           "bool",
>         CatCreateFunction "has_column_privilege"
>           ["text", "text", "oid", "int2"]
>           False
>           "bool",
>         CatCreateFunction "has_column_privilege"
>           ["oid", "text", "oid", "text"]
>           False
>           "bool",
>         CatCreateFunction "has_column_privilege"
>           ["oid", "text", "oid", "int2"]
>           False
>           "bool",
>         CatCreateFunction "has_column_privilege" ["text", "text", "text"]
>           False
>           "bool",
>         CatCreateFunction "has_column_privilege" ["text", "text", "int2"]
>           False
>           "bool",
>         CatCreateFunction "has_column_privilege" ["text", "oid", "text"]
>           False
>           "bool",
>         CatCreateFunction "has_column_privilege" ["text", "oid", "int2"]
>           False
>           "bool",
>         CatCreateFunction "has_any_column_privilege"
>           ["text", "name", "text"]
>           False
>           "bool",
>         CatCreateFunction "has_any_column_privilege"
>           ["name", "text", "oid"]
>           False
>           "bool",
>         CatCreateFunction "has_any_column_privilege"
>           ["text", "text", "oid"]
>           False
>           "bool",
>         CatCreateFunction "has_any_column_privilege" ["oid", "oid", "text"]
>           False
>           "bool",
>         CatCreateFunction "has_any_column_privilege" ["text", "text"] False
>           "bool",
>         CatCreateFunction "has_any_column_privilege" ["text", "oid"] False
>           "bool",
>         CatCreateFunction "overlay" ["int4", "bit", "bit", "int4"] False
>           "bit",
>         CatCreateFunction "overlay" ["bit", "bit", "int4"] False "bit",
>         CatCreateFunction "get_bit" ["int4", "bit"] False "int4",
>         CatCreateFunction "set_bit" ["int4", "int4", "bit"] False "bit",
>         CatCreateFunction "pg_relation_filepath" ["regclass"] False "text",
>         CatCreateFunction "pg_notify" ["text", "text"] False "void",
>         CatCreateFunction "pg_stat_get_xact_numscans" ["oid"] False "int8",
>         CatCreateFunction "pg_stat_get_xact_tuples_returned" ["oid"] False
>           "int8",
>         CatCreateFunction "pg_stat_get_xact_tuples_fetched" ["oid"] False
>           "int8",
>         CatCreateFunction "pg_stat_get_xact_tuples_inserted" ["oid"] False
>           "int8",
>         CatCreateFunction "pg_stat_get_xact_tuples_updated" ["oid"] False
>           "int8",
>         CatCreateFunction "pg_stat_get_xact_tuples_deleted" ["oid"] False
>           "int8",
>         CatCreateFunction "pg_stat_get_xact_tuples_hot_updated" ["oid"]
>           False
>           "int8",
>         CatCreateFunction "pg_stat_get_xact_blocks_fetched" ["oid"] False
>           "int8",
>         CatCreateFunction "pg_stat_get_xact_blocks_hit" ["oid"] False
>           "int8",
>         CatCreateFunction "pg_stat_get_xact_function_calls" ["oid"] False
>           "int8",
>         CatCreateFunction "pg_stat_get_xact_function_time" ["oid"] False
>           "int8",
>         CatCreateFunction "pg_stat_get_xact_function_self_time" ["oid"]
>           False
>           "int8",
>         CatCreateFunction "xpath_exists" ["text", "xml", "_text"] False
>           "bool",
>         CatCreateFunction "xpath_exists" ["xml", "text"] False "bool",
>         CatCreateFunction "xml_is_well_formed" ["text"] False "bool",
>         CatCreateFunction "xml_is_well_formed_document" ["text"] False
>           "bool",
>         CatCreateFunction "xml_is_well_formed_content" ["text"] False
>           "bool",
>         CatCreateFunction "pg_stat_get_vacuum_count" ["oid"] False "int8",
>         CatCreateFunction "pg_stat_get_autovacuum_count" ["oid"] False
>           "int8",
>         CatCreateFunction "pg_stat_get_analyze_count" ["oid"] False "int8",
>         CatCreateFunction "pg_stat_get_autoanalyze_count" ["oid"] False
>           "int8",
>         CatCreateFunction "left" ["text", "int4"] False "text",
>         CatCreateFunction "right" ["int4", "text"] False "text",
>         CatCreateFunction "reverse" ["text"] False "text",
>         CatCreateFunction "gist_point_distance"
>           ["point", "oid", "internal", "int4"]
>           False
>           "float8",
>         CatCreateFunction "pg_stat_get_db_conflict_tablespace" ["oid"]
>           False
>           "int8",
>         CatCreateFunction "pg_stat_get_db_conflict_lock" ["oid"] False
>           "int8",
>         CatCreateFunction "pg_stat_get_db_conflict_snapshot" ["oid"] False
>           "int8",
>         CatCreateFunction "pg_stat_get_db_conflict_bufferpin" ["oid"] False
>           "int8",
>         CatCreateFunction "pg_stat_get_db_conflict_startup_deadlock"
>           ["oid"]
>           False
>           "int8",
>         CatCreateFunction "pg_stat_get_db_conflict_all" ["oid"] False
>           "int8",
>         CatCreateFunction "pg_stat_get_db_stat_reset_time" ["oid"] False
>           "timestamptz",
>         CatCreateFunction "ginarrayextract" ["internal", "anyarray"] False
>           "internal",
>         CatCreateFunction "gin_extract_tsvector" ["tsvector", "internal"]
>           False
>           "internal",
>         CatCreateFunction "pg_sequence_parameters" ["oid"] False "record",
>         CatCreateFunction "pg_extension_update_paths" ["name"] False
>           "record",
>         CatCreateFunction "pg_extension_config_dump" ["text", "regclass"]
>           False
>           "void",
>         CatCreateFunction "gin_extract_tsquery"
>           ["int2", "internal", "internal", "tsquery", "internal"]
>           False
>           "internal",
>         CatCreateFunction "gin_tsquery_consistent"
>           ["internal", "int4", "int2", "internal", "internal", "tsquery"]
>           False
>           "bool",
>         CatCreateFunction "pg_advisory_xact_lock" ["int8"] False "void",
>         CatCreateFunction "pg_advisory_xact_lock_shared" ["int8"] False
>           "void",
>         CatCreateFunction "pg_try_advisory_xact_lock" ["int8"] False
>           "bool",
>         CatCreateFunction "pg_try_advisory_xact_lock_shared" ["int8"] False
>           "bool",
>         CatCreateFunction "pg_advisory_xact_lock" ["int4", "int4"] False
>           "void",
>         CatCreateFunction "pg_advisory_xact_lock_shared" ["int4", "int4"]
>           False
>           "void",
>         CatCreateFunction "pg_try_advisory_xact_lock" ["int4", "int4"]
>           False
>           "bool",
>         CatCreateFunction "pg_try_advisory_xact_lock_shared"
>           ["int4", "int4"]
>           False
>           "bool",
>         CatCreateFunction "pg_create_restore_point" ["text"] False "text",
>         CatCreateFunction "fdw_handler_in" ["cstring"] False "fdw_handler",
>         CatCreateFunction "fdw_handler_out" ["fdw_handler"] False
>           "cstring",
>         CatCreateFunction "void_recv" ["internal"] False "void",
>         CatCreateFunction "void_send" ["void"] False "bytea",
>         CatCreateFunction "anyenum_in" ["cstring"] False "anyenum",
>         CatCreateFunction "anyenum_out" ["anyenum"] False "cstring",
>         CatCreateFunction "enum_in" ["cstring", "oid"] False "anyenum",
>         CatCreateFunction "enum_out" ["anyenum"] False "cstring",
>         CatCreateFunction "enum_eq" ["anyenum", "anyenum"] False "bool",
>         CatCreateFunction "enum_ne" ["anyenum", "anyenum"] False "bool",
>         CatCreateFunction "enum_lt" ["anyenum", "anyenum"] False "bool",
>         CatCreateFunction "enum_gt" ["anyenum", "anyenum"] False "bool",
>         CatCreateFunction "enum_le" ["anyenum", "anyenum"] False "bool",
>         CatCreateFunction "enum_ge" ["anyenum", "anyenum"] False "bool",
>         CatCreateFunction "enum_cmp" ["anyenum", "anyenum"] False "int4",
>         CatCreateFunction "hashenum" ["anyenum"] False "int4",
>         CatCreateFunction "enum_smaller" ["anyenum", "anyenum"] False
>           "anyenum",
>         CatCreateFunction "enum_larger" ["anyenum", "anyenum"] False
>           "anyenum",
>         CatCreateFunction "enum_first" ["anyenum"] False "anyenum",
>         CatCreateFunction "enum_last" ["anyenum"] False "anyenum",
>         CatCreateFunction "enum_range" ["anyenum", "anyenum"] False
>           "anyarray",
>         CatCreateFunction "enum_range" ["anyenum"] False "anyarray",
>         CatCreateFunction "enum_recv" ["oid", "cstring"] False "anyenum",
>         CatCreateFunction "enum_send" ["anyenum"] False "bytea",
>         CatCreateFunction "string_agg_transfn" ["text", "text", "internal"]
>           False
>           "internal",
>         CatCreateFunction "string_agg_finalfn" ["internal"] False "text",
>         CatCreateFunction "pg_describe_object" ["oid", "int4", "oid"] False
>           "text",
>         CatCreateFunction "format" ["text"] False "text",
>         CatCreateFunction "tsvectorin" ["cstring"] False "tsvector",
>         CatCreateFunction "tsvectorout" ["tsvector"] False "cstring",
>         CatCreateFunction "tsqueryin" ["cstring"] False "tsquery",
>         CatCreateFunction "tsqueryout" ["tsquery"] False "cstring",
>         CatCreateFunction "tsvector_lt" ["tsvector", "tsvector"] False
>           "bool",
>         CatCreateFunction "tsvector_le" ["tsvector", "tsvector"] False
>           "bool",
>         CatCreateFunction "tsvector_eq" ["tsvector", "tsvector"] False
>           "bool",
>         CatCreateFunction "tsvector_ne" ["tsvector", "tsvector"] False
>           "bool",
>         CatCreateFunction "tsvector_ge" ["tsvector", "tsvector"] False
>           "bool",
>         CatCreateFunction "tsvector_gt" ["tsvector", "tsvector"] False
>           "bool",
>         CatCreateFunction "tsvector_cmp" ["tsvector", "tsvector"] False
>           "int4",
>         CatCreateFunction "strip" ["tsvector"] False "tsvector",
>         CatCreateFunction "setweight" ["tsvector", "char"] False
>           "tsvector",
>         CatCreateFunction "tsvector_concat" ["tsvector", "tsvector"] False
>           "tsvector",
>         CatCreateFunction "ts_match_vq" ["tsvector", "tsquery"] False
>           "bool",
>         CatCreateFunction "ts_match_qv" ["tsvector", "tsquery"] False
>           "bool",
>         CatCreateFunction "tsvectorsend" ["tsvector"] False "bytea",
>         CatCreateFunction "tsvectorrecv" ["internal"] False "tsvector",
>         CatCreateFunction "tsquerysend" ["tsquery"] False "bytea",
>         CatCreateFunction "tsqueryrecv" ["internal"] False "tsquery",
>         CatCreateFunction "gtsvectorin" ["cstring"] False "gtsvector",
>         CatCreateFunction "gtsvectorout" ["gtsvector"] False "cstring",
>         CatCreateFunction "gtsvector_compress" ["internal"] False
>           "internal",
>         CatCreateFunction "gtsvector_decompress" ["internal"] False
>           "internal",
>         CatCreateFunction "gtsvector_picksplit" ["internal", "internal"]
>           False
>           "internal",
>         CatCreateFunction "gtsvector_union" ["internal", "internal"] False
>           "internal",
>         CatCreateFunction "gtsvector_same"
>           ["internal", "gtsvector", "gtsvector"]
>           False
>           "internal",
>         CatCreateFunction "gtsvector_penalty"
>           ["internal", "internal", "internal"]
>           False
>           "internal",
>         CatCreateFunction "gtsvector_consistent"
>           ["gtsvector", "oid", "int4", "internal", "internal"]
>           False
>           "bool",
>         CatCreateFunction "gin_extract_tsvector"
>           ["tsvector", "internal", "internal"]
>           False
>           "internal",
>         CatCreateFunction "gin_extract_tsquery"
>           ["internal", "tsquery", "internal", "internal", "internal", "int2",
>            "internal"]
>           False
>           "internal",
>         CatCreateFunction "gin_tsquery_consistent"
>           ["int2", "internal", "internal", "int4", "internal", "internal",
>            "tsquery", "internal"]
>           False
>           "bool",
>         CatCreateFunction "tsquery_lt" ["tsquery", "tsquery"] False "bool",
>         CatCreateFunction "tsquery_le" ["tsquery", "tsquery"] False "bool",
>         CatCreateFunction "tsquery_eq" ["tsquery", "tsquery"] False "bool",
>         CatCreateFunction "tsquery_ne" ["tsquery", "tsquery"] False "bool",
>         CatCreateFunction "tsquery_ge" ["tsquery", "tsquery"] False "bool",
>         CatCreateFunction "tsquery_gt" ["tsquery", "tsquery"] False "bool",
>         CatCreateFunction "tsquery_cmp" ["tsquery", "tsquery"] False
>           "int4",
>         CatCreateFunction "tsquery_and" ["tsquery", "tsquery"] False
>           "tsquery",
>         CatCreateFunction "tsquery_or" ["tsquery", "tsquery"] False
>           "tsquery",
>         CatCreateFunction "tsquery_not" ["tsquery"] False "tsquery",
>         CatCreateFunction "numnode" ["tsquery"] False "int4",
>         CatCreateFunction "querytree" ["tsquery"] False "text",
>         CatCreateFunction "ts_rewrite" ["tsquery", "tsquery", "tsquery"]
>           False
>           "tsquery",
>         CatCreateFunction "ts_rewrite" ["text", "tsquery"] False "tsquery",
>         CatCreateFunction "tsmatchsel"
>           ["oid", "internal", "internal", "int4"]
>           False
>           "float8",
>         CatCreateFunction "tsmatchjoinsel"
>           ["int2", "internal", "internal", "internal", "oid"]
>           False
>           "float8",
>         CatCreateFunction "ts_typanalyze" ["internal"] False "bool",
>         CatCreateFunction "ts_stat" ["text"] False "record",
>         CatCreateFunction "ts_stat" ["text", "text"] False "record",
>         CatCreateFunction "tsq_mcontains" ["tsquery", "tsquery"] False
>           "bool",
>         CatCreateFunction "tsq_mcontained" ["tsquery", "tsquery"] False
>           "bool",
>         CatCreateFunction "gtsquery_compress" ["internal"] False
>           "internal",
>         CatCreateFunction "gtsquery_decompress" ["internal"] False
>           "internal",
>         CatCreateFunction "gtsquery_picksplit" ["internal", "internal"]
>           False
>           "internal",
>         CatCreateFunction "gtsquery_union" ["internal", "internal"] False
>           "internal",
>         CatCreateFunction "gtsquery_same" ["int8", "int8", "internal"]
>           False
>           "internal",
>         CatCreateFunction "gtsquery_penalty"
>           ["internal", "internal", "internal"]
>           False
>           "internal",
>         CatCreateFunction "gtsquery_consistent"
>           ["oid", "internal", "internal", "int4", "internal"]
>           False
>           "bool",
>         CatCreateFunction "ts_rank"
>           ["_float4", "tsquery", "tsvector", "int4"]
>           False
>           "float4",
>         CatCreateFunction "ts_rank" ["tsvector", "_float4", "tsquery"]
>           False
>           "float4",
>         CatCreateFunction "ts_rank" ["tsquery", "int4", "tsvector"] False
>           "float4",
>         CatCreateFunction "ts_rank" ["tsvector", "tsquery"] False "float4",
>         CatCreateFunction "ts_rank_cd"
>           ["int4", "tsvector", "_float4", "tsquery"]
>           False
>           "float4",
>         CatCreateFunction "ts_rank_cd" ["tsquery", "_float4", "tsvector"]
>           False
>           "float4",
>         CatCreateFunction "ts_rank_cd" ["tsvector", "tsquery", "int4"]
>           False
>           "float4",
>         CatCreateFunction "ts_rank_cd" ["tsvector", "tsquery"] False
>           "float4",
>         CatCreateFunction "length" ["tsvector"] False "int4",
>         CatCreateFunction "ts_token_type" ["oid"] False "record",
>         CatCreateFunction "ts_token_type" ["text"] False "record",
>         CatCreateFunction "ts_parse" ["oid", "text"] False "record",
>         CatCreateFunction "ts_parse" ["text", "text"] False "record",
>         CatCreateFunction "prsd_start" ["int4", "internal"] False
>           "internal",
>         CatCreateFunction "prsd_nexttoken"
>           ["internal", "internal", "internal"]
>           False
>           "internal",
>         CatCreateFunction "prsd_end" ["internal"] False "void",
>         CatCreateFunction "prsd_headline"
>           ["internal", "internal", "tsquery"]
>           False
>           "internal",
>         CatCreateFunction "prsd_lextype" ["internal"] False "internal",
>         CatCreateFunction "ts_lexize" ["text", "regdictionary"] False
>           "_text",
>         CatCreateFunction "gin_cmp_tslexeme" ["text", "text"] False "int4",
>         CatCreateFunction "dsimple_init" ["internal"] False "internal",
>         CatCreateFunction "dsimple_lexize"
>           ["internal", "internal", "internal", "internal"]
>           False
>           "internal",
>         CatCreateFunction "dsynonym_init" ["internal"] False "internal",
>         CatCreateFunction "dsynonym_lexize"
>           ["internal", "internal", "internal", "internal"]
>           False
>           "internal",
>         CatCreateFunction "dispell_init" ["internal"] False "internal",
>         CatCreateFunction "dispell_lexize"
>           ["internal", "internal", "internal", "internal"]
>           False
>           "internal",
>         CatCreateFunction "regconfigin" ["cstring"] False "regconfig",
>         CatCreateFunction "regconfigout" ["regconfig"] False "cstring",
>         CatCreateFunction "regconfigrecv" ["internal"] False "regconfig",
>         CatCreateFunction "regconfigsend" ["regconfig"] False "bytea",
>         CatCreateFunction "thesaurus_init" ["internal"] False "internal",
>         CatCreateFunction "thesaurus_lexize"
>           ["internal", "internal", "internal", "internal"]
>           False
>           "internal",
>         CatCreateFunction "ts_headline"
>           ["text", "regconfig", "text", "tsquery"]
>           False
>           "text",
>         CatCreateFunction "ts_headline" ["tsquery", "regconfig", "text"]
>           False
>           "text",
>         CatCreateFunction "to_tsvector" ["text", "regconfig"] False
>           "tsvector",
>         CatCreateFunction "to_tsquery" ["regconfig", "text"] False
>           "tsquery",
>         CatCreateFunction "plainto_tsquery" ["text", "regconfig"] False
>           "tsquery",
>         CatCreateFunction "to_tsvector" ["text"] False "tsvector",
>         CatCreateFunction "to_tsquery" ["text"] False "tsquery",
>         CatCreateFunction "plainto_tsquery" ["text"] False "tsquery",
>         CatCreateFunction "ts_headline" ["tsquery", "text", "text"] False
>           "text",
>         CatCreateFunction "ts_headline" ["tsquery", "text"] False "text",
>         CatCreateFunction "pg_ts_parser_is_visible" ["oid"] False "bool",
>         CatCreateFunction "pg_ts_dict_is_visible" ["oid"] False "bool",
>         CatCreateFunction "pg_ts_config_is_visible" ["oid"] False "bool",
>         CatCreateFunction "ts_match_tt" ["text", "text"] False "bool",
>         CatCreateFunction "ts_match_tq" ["text", "tsquery"] False "bool",
>         CatCreateFunction "pg_ts_template_is_visible" ["oid"] False "bool",
>         CatCreateFunction "regdictionaryin" ["cstring"] False
>           "regdictionary",
>         CatCreateFunction "regdictionaryout" ["regdictionary"] False
>           "cstring",
>         CatCreateFunction "regdictionaryrecv" ["internal"] False
>           "regdictionary",
>         CatCreateFunction "regdictionarysend" ["regdictionary"] False
>           "bytea",
>         CatCreateFunction "pg_stat_reset_shared" ["text"] False "void",
>         CatCreateFunction "pg_stat_reset_single_table_counters" ["oid"]
>           False
>           "void",
>         CatCreateFunction "pg_stat_reset_single_function_counters" ["oid"]
>           False
>           "void",
>         CatCreateFunction "money" ["int4"] False "money",
>         CatCreateFunction "money" ["int8"] False "money",
>         CatCreateFunction "pg_collation_is_visible" ["oid"] False "bool",
>         CatCreateFunction "cash_div_cash" ["money", "money"] False
>           "float8",
>         CatCreateFunction "numeric" ["money"] False "numeric",
>         CatCreateFunction "money" ["numeric"] False "money",
>         CatCreateFunction "pg_read_file" ["text"] False "text",
>         CatCreateFunction "pg_read_binary_file" ["int8", "int8", "text"]
>           False
>           "bytea",
>         CatCreateFunction "pg_read_binary_file" ["text"] False "bytea",
>         CatCreateFunction "ts_debug" ["text", "regconfig"] False "record",
>         CatCreateFunction "ts_debug" ["text"] False "record",
>         CatCreateFunction "ascii_to_mic"
>           ["int4", "internal", "int4", "cstring", "int4"]
>           False
>           "void",
>         CatCreateFunction "mic_to_ascii"
>           ["internal", "int4", "int4", "int4", "cstring"]
>           False
>           "void",
>         CatCreateFunction "koi8r_to_mic"
>           ["int4", "int4", "internal", "int4", "cstring"]
>           False
>           "void",
>         CatCreateFunction "mic_to_koi8r"
>           ["internal", "cstring", "int4", "int4", "int4"]
>           False
>           "void",
>         CatCreateFunction "iso_to_mic"
>           ["int4", "int4", "cstring", "internal", "int4"]
>           False
>           "void",
>         CatCreateFunction "mic_to_iso"
>           ["int4", "int4", "int4", "internal", "cstring"]
>           False
>           "void",
>         CatCreateFunction "win1251_to_mic"
>           ["cstring", "internal", "int4", "int4", "int4"]
>           False
>           "void",
>         CatCreateFunction "mic_to_win1251"
>           ["int4", "int4", "int4", "internal", "cstring"]
>           False
>           "void",
>         CatCreateFunction "win866_to_mic"
>           ["internal", "int4", "int4", "cstring", "int4"]
>           False
>           "void",
>         CatCreateFunction "mic_to_win866"
>           ["int4", "int4", "int4", "cstring", "internal"]
>           False
>           "void",
>         CatCreateFunction "koi8r_to_win1251"
>           ["int4", "int4", "int4", "internal", "cstring"]
>           False
>           "void",
>         CatCreateFunction "win1251_to_koi8r"
>           ["int4", "int4", "int4", "internal", "cstring"]
>           False
>           "void",
>         CatCreateFunction "koi8r_to_win866"
>           ["internal", "int4", "int4", "cstring", "int4"]
>           False
>           "void",
>         CatCreateFunction "win866_to_koi8r"
>           ["internal", "int4", "int4", "cstring", "int4"]
>           False
>           "void",
>         CatCreateFunction "win866_to_win1251"
>           ["cstring", "int4", "internal", "int4", "int4"]
>           False
>           "void",
>         CatCreateFunction "win1251_to_win866"
>           ["int4", "internal", "int4", "cstring", "int4"]
>           False
>           "void",
>         CatCreateFunction "iso_to_koi8r"
>           ["int4", "cstring", "int4", "int4", "internal"]
>           False
>           "void",
>         CatCreateFunction "koi8r_to_iso"
>           ["int4", "internal", "int4", "cstring", "int4"]
>           False
>           "void",
>         CatCreateFunction "iso_to_win1251"
>           ["int4", "int4", "int4", "internal", "cstring"]
>           False
>           "void",
>         CatCreateFunction "win1251_to_iso"
>           ["int4", "cstring", "int4", "int4", "internal"]
>           False
>           "void",
>         CatCreateFunction "iso_to_win866"
>           ["int4", "cstring", "int4", "int4", "internal"]
>           False
>           "void",
>         CatCreateFunction "win866_to_iso"
>           ["int4", "int4", "cstring", "internal", "int4"]
>           False
>           "void",
>         CatCreateFunction "euc_cn_to_mic"
>           ["cstring", "int4", "int4", "internal", "int4"]
>           False
>           "void",
>         CatCreateFunction "mic_to_euc_cn"
>           ["internal", "int4", "int4", "cstring", "int4"]
>           False
>           "void",
>         CatCreateFunction "euc_jp_to_sjis"
>           ["internal", "int4", "int4", "cstring", "int4"]
>           False
>           "void",
>         CatCreateFunction "sjis_to_euc_jp"
>           ["int4", "internal", "int4", "int4", "cstring"]
>           False
>           "void",
>         CatCreateFunction "euc_jp_to_mic"
>           ["cstring", "int4", "int4", "int4", "internal"]
>           False
>           "void",
>         CatCreateFunction "sjis_to_mic"
>           ["int4", "int4", "cstring", "internal", "int4"]
>           False
>           "void",
>         CatCreateFunction "mic_to_euc_jp"
>           ["int4", "int4", "cstring", "int4", "internal"]
>           False
>           "void",
>         CatCreateFunction "mic_to_sjis"
>           ["int4", "int4", "int4", "internal", "cstring"]
>           False
>           "void",
>         CatCreateFunction "euc_kr_to_mic"
>           ["int4", "int4", "int4", "internal", "cstring"]
>           False
>           "void",
>         CatCreateFunction "mic_to_euc_kr"
>           ["int4", "cstring", "int4", "int4", "internal"]
>           False
>           "void",
>         CatCreateFunction "euc_tw_to_big5"
>           ["int4", "cstring", "int4", "internal", "int4"]
>           False
>           "void",
>         CatCreateFunction "big5_to_euc_tw"
>           ["int4", "int4", "int4", "internal", "cstring"]
>           False
>           "void",
>         CatCreateFunction "euc_tw_to_mic"
>           ["cstring", "int4", "internal", "int4", "int4"]
>           False
>           "void",
>         CatCreateFunction "big5_to_mic"
>           ["internal", "int4", "cstring", "int4", "int4"]
>           False
>           "void",
>         CatCreateFunction "mic_to_euc_tw"
>           ["int4", "int4", "internal", "int4", "cstring"]
>           False
>           "void",
>         CatCreateFunction "mic_to_big5"
>           ["int4", "int4", "internal", "cstring", "int4"]
>           False
>           "void",
>         CatCreateFunction "latin2_to_mic"
>           ["int4", "int4", "int4", "cstring", "internal"]
>           False
>           "void",
>         CatCreateFunction "mic_to_latin2"
>           ["int4", "internal", "int4", "cstring", "int4"]
>           False
>           "void",
>         CatCreateFunction "win1250_to_mic"
>           ["cstring", "int4", "int4", "internal", "int4"]
>           False
>           "void",
>         CatCreateFunction "mic_to_win1250"
>           ["cstring", "int4", "int4", "internal", "int4"]
>           False
>           "void",
>         CatCreateFunction "latin2_to_win1250"
>           ["internal", "int4", "cstring", "int4", "int4"]
>           False
>           "void",
>         CatCreateFunction "win1250_to_latin2"
>           ["internal", "int4", "int4", "int4", "cstring"]
>           False
>           "void",
>         CatCreateFunction "latin1_to_mic"
>           ["int4", "int4", "cstring", "internal", "int4"]
>           False
>           "void",
>         CatCreateFunction "mic_to_latin1"
>           ["internal", "int4", "cstring", "int4", "int4"]
>           False
>           "void",
>         CatCreateFunction "latin3_to_mic"
>           ["int4", "internal", "int4", "cstring", "int4"]
>           False
>           "void",
>         CatCreateFunction "mic_to_latin3"
>           ["internal", "int4", "cstring", "int4", "int4"]
>           False
>           "void",
>         CatCreateFunction "latin4_to_mic"
>           ["internal", "cstring", "int4", "int4", "int4"]
>           False
>           "void",
>         CatCreateFunction "mic_to_latin4"
>           ["internal", "int4", "int4", "int4", "cstring"]
>           False
>           "void",
>         CatCreateFunction "ascii_to_utf8"
>           ["int4", "int4", "int4", "cstring", "internal"]
>           False
>           "void",
>         CatCreateFunction "utf8_to_ascii"
>           ["int4", "internal", "int4", "cstring", "int4"]
>           False
>           "void",
>         CatCreateFunction "big5_to_utf8"
>           ["int4", "cstring", "int4", "internal", "int4"]
>           False
>           "void",
>         CatCreateFunction "utf8_to_big5"
>           ["internal", "cstring", "int4", "int4", "int4"]
>           False
>           "void",
>         CatCreateFunction "utf8_to_koi8r"
>           ["int4", "int4", "internal", "int4", "cstring"]
>           False
>           "void",
>         CatCreateFunction "koi8r_to_utf8"
>           ["int4", "cstring", "int4", "internal", "int4"]
>           False
>           "void",
>         CatCreateFunction "utf8_to_koi8u"
>           ["int4", "internal", "int4", "int4", "cstring"]
>           False
>           "void",
>         CatCreateFunction "koi8u_to_utf8"
>           ["cstring", "internal", "int4", "int4", "int4"]
>           False
>           "void",
>         CatCreateFunction "utf8_to_win"
>           ["cstring", "int4", "int4", "internal", "int4"]
>           False
>           "void",
>         CatCreateFunction "win_to_utf8"
>           ["cstring", "int4", "internal", "int4", "int4"]
>           False
>           "void",
>         CatCreateFunction "euc_cn_to_utf8"
>           ["int4", "int4", "int4", "internal", "cstring"]
>           False
>           "void",
>         CatCreateFunction "utf8_to_euc_cn"
>           ["cstring", "int4", "int4", "int4", "internal"]
>           False
>           "void",
>         CatCreateFunction "euc_jp_to_utf8"
>           ["int4", "cstring", "internal", "int4", "int4"]
>           False
>           "void",
>         CatCreateFunction "utf8_to_euc_jp"
>           ["cstring", "int4", "int4", "int4", "internal"]
>           False
>           "void",
>         CatCreateFunction "euc_kr_to_utf8"
>           ["int4", "int4", "internal", "cstring", "int4"]
>           False
>           "void",
>         CatCreateFunction "utf8_to_euc_kr"
>           ["int4", "int4", "int4", "cstring", "internal"]
>           False
>           "void",
>         CatCreateFunction "euc_tw_to_utf8"
>           ["int4", "cstring", "internal", "int4", "int4"]
>           False
>           "void",
>         CatCreateFunction "utf8_to_euc_tw"
>           ["int4", "internal", "int4", "int4", "cstring"]
>           False
>           "void",
>         CatCreateFunction "gb18030_to_utf8"
>           ["int4", "internal", "int4", "cstring", "int4"]
>           False
>           "void",
>         CatCreateFunction "utf8_to_gb18030"
>           ["internal", "cstring", "int4", "int4", "int4"]
>           False
>           "void",
>         CatCreateFunction "gbk_to_utf8"
>           ["internal", "int4", "cstring", "int4", "int4"]
>           False
>           "void",
>         CatCreateFunction "utf8_to_gbk"
>           ["int4", "internal", "int4", "cstring", "int4"]
>           False
>           "void",
>         CatCreateFunction "utf8_to_iso8859"
>           ["cstring", "int4", "int4", "internal", "int4"]
>           False
>           "void",
>         CatCreateFunction "iso8859_to_utf8"
>           ["internal", "int4", "int4", "int4", "cstring"]
>           False
>           "void",
>         CatCreateFunction "iso8859_1_to_utf8"
>           ["internal", "cstring", "int4", "int4", "int4"]
>           False
>           "void",
>         CatCreateFunction "utf8_to_iso8859_1"
>           ["int4", "cstring", "internal", "int4", "int4"]
>           False
>           "void",
>         CatCreateFunction "johab_to_utf8"
>           ["int4", "int4", "internal", "cstring", "int4"]
>           False
>           "void",
>         CatCreateFunction "utf8_to_johab"
>           ["cstring", "int4", "int4", "int4", "internal"]
>           False
>           "void",
>         CatCreateFunction "sjis_to_utf8"
>           ["int4", "int4", "cstring", "internal", "int4"]
>           False
>           "void",
>         CatCreateFunction "utf8_to_sjis"
>           ["int4", "int4", "cstring", "int4", "internal"]
>           False
>           "void",
>         CatCreateFunction "uhc_to_utf8"
>           ["int4", "int4", "int4", "internal", "cstring"]
>           False
>           "void",
>         CatCreateFunction "utf8_to_uhc"
>           ["cstring", "int4", "int4", "internal", "int4"]
>           False
>           "void",
>         CatCreateFunction "euc_jis_2004_to_utf8"
>           ["int4", "cstring", "internal", "int4", "int4"]
>           False
>           "void",
>         CatCreateFunction "utf8_to_euc_jis_2004"
>           ["int4", "int4", "internal", "cstring", "int4"]
>           False
>           "void",
>         CatCreateFunction "shift_jis_2004_to_utf8"
>           ["internal", "cstring", "int4", "int4", "int4"]
>           False
>           "void",
>         CatCreateFunction "utf8_to_shift_jis_2004"
>           ["int4", "int4", "int4", "cstring", "internal"]
>           False
>           "void",
>         CatCreateFunction "euc_jis_2004_to_shift_jis_2004"
>           ["int4", "int4", "cstring", "int4", "internal"]
>           False
>           "void",
>         CatCreateFunction "shift_jis_2004_to_euc_jis_2004"
>           ["int4", "cstring", "internal", "int4", "int4"]
>           False
>           "void",
>         CatCreateFunction "dsnowball_init" ["internal"] False "internal",
>         CatCreateFunction "dsnowball_lexize"
>           ["internal", "internal", "internal", "internal"]
>           False
>           "internal",
>         CatCreateFunction "plpgsql_inline_handler" ["internal"] False
>           "void",
>         CatCreateFunction "plpgsql_validator" ["oid"] False "void"]

