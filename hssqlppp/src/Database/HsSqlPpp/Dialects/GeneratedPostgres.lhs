


This file is auto generated, to regenerate run
make regenDefaultTemplate1Catalog. You will need postgresql
installed to do this.

> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Dialects.GeneratedPostgres
>      (generatedPostgresCatalogEntries) where
> import Database.HsSqlPpp.Internals.Catalog.CatalogTypes

> -- | The catalog from a default template1 database in roughly the
> -- latest postgres. 'select version()' from the dbms this catalog
> -- was generated from: 'PostgreSQL 9.4.5 on x86_64-unknown-linux-gnu, compiled by gcc (Debian 5.2.1-23) 5.2.1 20151028, 64-bit'.
> generatedPostgresCatalogEntries :: [CatalogUpdate]
> generatedPostgresCatalogEntries =

    
>        [ CatCreateScalarType "abstime"
>        , CatCreateScalarType "aclitem"
>        , CatCreateScalarType "bit"
>        , CatCreateScalarType "bool"
>        , CatCreateScalarType "box"
>        , CatCreateScalarType "bpchar"
>        , CatCreateScalarType "bytea"
>        , CatCreateScalarType "char"
>        , CatCreateScalarType "cid"
>        , CatCreateScalarType "cidr"
>        , CatCreateScalarType "circle"
>        , CatCreateScalarType "date"
>        , CatCreateScalarType "float4"
>        , CatCreateScalarType "float8"
>        , CatCreateScalarType "gtsvector"
>        , CatCreateScalarType "inet"
>        , CatCreateScalarType "int2"
>        , CatCreateScalarType "int2vector"
>        , CatCreateScalarType "int4"
>        , CatCreateScalarType "int8"
>        , CatCreateScalarType "interval"
>        , CatCreateScalarType "json"
>        , CatCreateScalarType "jsonb"
>        , CatCreateScalarType "line"
>        , CatCreateScalarType "lseg"
>        , CatCreateScalarType "macaddr"
>        , CatCreateScalarType "money"
>        , CatCreateScalarType "name"
>        , CatCreateScalarType "numeric"
>        , CatCreateScalarType "oid"
>        , CatCreateScalarType "oidvector"
>        , CatCreateScalarType "path"
>        , CatCreateScalarType "pg_lsn"
>        , CatCreateScalarType "pg_node_tree"
>        , CatCreateScalarType "point"
>        , CatCreateScalarType "polygon"
>        , CatCreateScalarType "refcursor"
>        , CatCreateScalarType "regclass"
>        , CatCreateScalarType "regconfig"
>        , CatCreateScalarType "regdictionary"
>        , CatCreateScalarType "regoper"
>        , CatCreateScalarType "regoperator"
>        , CatCreateScalarType "regproc"
>        , CatCreateScalarType "regprocedure"
>        , CatCreateScalarType "regtype"
>        , CatCreateScalarType "reltime"
>        , CatCreateScalarType "smgr"
>        , CatCreateScalarType "text"
>        , CatCreateScalarType "tid"
>        , CatCreateScalarType "time"
>        , CatCreateScalarType "timestamp"
>        , CatCreateScalarType "timestamptz"
>        , CatCreateScalarType "timetz"
>        , CatCreateScalarType "tinterval"
>        , CatCreateScalarType "tsquery"
>        , CatCreateScalarType "tsvector"
>        , CatCreateScalarType "txid_snapshot"
>        , CatCreateScalarType "unknown"
>        , CatCreateScalarType "uuid"
>        , CatCreateScalarType "varbit"
>        , CatCreateScalarType "varchar"
>        , CatCreateScalarType "xid"
>        , CatCreateScalarType "xml"
>        , CatCreateDomainType "information_schema.yes_or_no" "varchar"
>        , CatCreateCast
>            "information_schema.yes_or_no" "varchar" ImplicitCastContext
>        , CatCreateDomainType "information_schema.time_stamp" "timestamptz"
>        , CatCreateCast
>            "information_schema.time_stamp" "timestamptz" ImplicitCastContext
>        , CatCreateDomainType "information_schema.sql_identifier" "varchar"
>        , CatCreateCast
>            "information_schema.sql_identifier" "varchar" ImplicitCastContext
>        , CatCreateDomainType "information_schema.character_data" "varchar"
>        , CatCreateCast
>            "information_schema.character_data" "varchar" ImplicitCastContext
>        , CatCreateDomainType "information_schema.cardinal_number" "int4"
>        , CatCreateCast
>            "information_schema.cardinal_number" "int4" ImplicitCastContext
>        , CatCreateArrayType "_xml" "xml"
>        , CatCreateTypeCategoryEntry "_xml" ( "A" , False )
>        , CatCreateArrayType "_xid" "xid"
>        , CatCreateTypeCategoryEntry "_xid" ( "A" , False )
>        , CatCreateArrayType "_varchar" "varchar"
>        , CatCreateTypeCategoryEntry "_varchar" ( "A" , False )
>        , CatCreateArrayType "_varbit" "varbit"
>        , CatCreateTypeCategoryEntry "_varbit" ( "A" , False )
>        , CatCreateArrayType "_uuid" "uuid"
>        , CatCreateTypeCategoryEntry "_uuid" ( "A" , False )
>        , CatCreateArrayType "_txid_snapshot" "txid_snapshot"
>        , CatCreateTypeCategoryEntry "_txid_snapshot" ( "A" , False )
>        , CatCreateArrayType "_tsvector" "tsvector"
>        , CatCreateTypeCategoryEntry "_tsvector" ( "A" , False )
>        , CatCreateArrayType "_tstzrange" "tstzrange"
>        , CatCreateTypeCategoryEntry "_tstzrange" ( "A" , False )
>        , CatCreateArrayType "_tsrange" "tsrange"
>        , CatCreateTypeCategoryEntry "_tsrange" ( "A" , False )
>        , CatCreateArrayType "_tsquery" "tsquery"
>        , CatCreateTypeCategoryEntry "_tsquery" ( "A" , False )
>        , CatCreateArrayType "_tinterval" "tinterval"
>        , CatCreateTypeCategoryEntry "_tinterval" ( "A" , False )
>        , CatCreateArrayType "_timetz" "timetz"
>        , CatCreateTypeCategoryEntry "_timetz" ( "A" , False )
>        , CatCreateArrayType "_timestamptz" "timestamptz"
>        , CatCreateTypeCategoryEntry "_timestamptz" ( "A" , False )
>        , CatCreateArrayType "_timestamp" "timestamp"
>        , CatCreateTypeCategoryEntry "_timestamp" ( "A" , False )
>        , CatCreateArrayType "_time" "time"
>        , CatCreateTypeCategoryEntry "_time" ( "A" , False )
>        , CatCreateArrayType "_tid" "tid"
>        , CatCreateTypeCategoryEntry "_tid" ( "A" , False )
>        , CatCreateArrayType "_text" "text"
>        , CatCreateTypeCategoryEntry "_text" ( "A" , False )
>        , CatCreateArrayType "_reltime" "reltime"
>        , CatCreateTypeCategoryEntry "_reltime" ( "A" , False )
>        , CatCreateArrayType "_regtype" "regtype"
>        , CatCreateTypeCategoryEntry "_regtype" ( "A" , False )
>        , CatCreateArrayType "_regprocedure" "regprocedure"
>        , CatCreateTypeCategoryEntry "_regprocedure" ( "A" , False )
>        , CatCreateArrayType "_regproc" "regproc"
>        , CatCreateTypeCategoryEntry "_regproc" ( "A" , False )
>        , CatCreateArrayType "_regoperator" "regoperator"
>        , CatCreateTypeCategoryEntry "_regoperator" ( "A" , False )
>        , CatCreateArrayType "_regoper" "regoper"
>        , CatCreateTypeCategoryEntry "_regoper" ( "A" , False )
>        , CatCreateArrayType "_regdictionary" "regdictionary"
>        , CatCreateTypeCategoryEntry "_regdictionary" ( "A" , False )
>        , CatCreateArrayType "_regconfig" "regconfig"
>        , CatCreateTypeCategoryEntry "_regconfig" ( "A" , False )
>        , CatCreateArrayType "_regclass" "regclass"
>        , CatCreateTypeCategoryEntry "_regclass" ( "A" , False )
>        , CatCreateArrayType "_refcursor" "refcursor"
>        , CatCreateTypeCategoryEntry "_refcursor" ( "A" , False )
>        , CatCreateArrayType "_record" "record"
>        , CatCreateTypeCategoryEntry "_record" ( "A" , False )
>        , CatCreateArrayType "_polygon" "polygon"
>        , CatCreateTypeCategoryEntry "_polygon" ( "A" , False )
>        , CatCreateArrayType "_point" "point"
>        , CatCreateTypeCategoryEntry "_point" ( "A" , False )
>        , CatCreateArrayType "_pg_lsn" "pg_lsn"
>        , CatCreateTypeCategoryEntry "_pg_lsn" ( "A" , False )
>        , CatCreateArrayType "_path" "path"
>        , CatCreateTypeCategoryEntry "_path" ( "A" , False )
>        , CatCreateArrayType "_oidvector" "oidvector"
>        , CatCreateTypeCategoryEntry "_oidvector" ( "A" , False )
>        , CatCreateArrayType "_oid" "oid"
>        , CatCreateTypeCategoryEntry "_oid" ( "A" , False )
>        , CatCreateArrayType "_numrange" "numrange"
>        , CatCreateTypeCategoryEntry "_numrange" ( "A" , False )
>        , CatCreateArrayType "_numeric" "numeric"
>        , CatCreateTypeCategoryEntry "_numeric" ( "A" , False )
>        , CatCreateArrayType "_name" "name"
>        , CatCreateTypeCategoryEntry "_name" ( "A" , False )
>        , CatCreateArrayType "_money" "money"
>        , CatCreateTypeCategoryEntry "_money" ( "A" , False )
>        , CatCreateArrayType "_macaddr" "macaddr"
>        , CatCreateTypeCategoryEntry "_macaddr" ( "A" , False )
>        , CatCreateArrayType "_lseg" "lseg"
>        , CatCreateTypeCategoryEntry "_lseg" ( "A" , False )
>        , CatCreateArrayType "_line" "line"
>        , CatCreateTypeCategoryEntry "_line" ( "A" , False )
>        , CatCreateArrayType "_jsonb" "jsonb"
>        , CatCreateTypeCategoryEntry "_jsonb" ( "A" , False )
>        , CatCreateArrayType "_json" "json"
>        , CatCreateTypeCategoryEntry "_json" ( "A" , False )
>        , CatCreateArrayType "_interval" "interval"
>        , CatCreateTypeCategoryEntry "_interval" ( "A" , False )
>        , CatCreateArrayType "_int8range" "int8range"
>        , CatCreateTypeCategoryEntry "_int8range" ( "A" , False )
>        , CatCreateArrayType "_int8" "int8"
>        , CatCreateTypeCategoryEntry "_int8" ( "A" , False )
>        , CatCreateArrayType "_int4range" "int4range"
>        , CatCreateTypeCategoryEntry "_int4range" ( "A" , False )
>        , CatCreateArrayType "_int4" "int4"
>        , CatCreateTypeCategoryEntry "_int4" ( "A" , False )
>        , CatCreateArrayType "_int2vector" "int2vector"
>        , CatCreateTypeCategoryEntry "_int2vector" ( "A" , False )
>        , CatCreateArrayType "_int2" "int2"
>        , CatCreateTypeCategoryEntry "_int2" ( "A" , False )
>        , CatCreateArrayType "_inet" "inet"
>        , CatCreateTypeCategoryEntry "_inet" ( "A" , False )
>        , CatCreateArrayType "_gtsvector" "gtsvector"
>        , CatCreateTypeCategoryEntry "_gtsvector" ( "A" , False )
>        , CatCreateArrayType "_float8" "float8"
>        , CatCreateTypeCategoryEntry "_float8" ( "A" , False )
>        , CatCreateArrayType "_float4" "float4"
>        , CatCreateTypeCategoryEntry "_float4" ( "A" , False )
>        , CatCreateArrayType "_daterange" "daterange"
>        , CatCreateTypeCategoryEntry "_daterange" ( "A" , False )
>        , CatCreateArrayType "_date" "date"
>        , CatCreateTypeCategoryEntry "_date" ( "A" , False )
>        , CatCreateArrayType "_cstring" "cstring"
>        , CatCreateTypeCategoryEntry "_cstring" ( "A" , False )
>        , CatCreateArrayType "_circle" "circle"
>        , CatCreateTypeCategoryEntry "_circle" ( "A" , False )
>        , CatCreateArrayType "_cidr" "cidr"
>        , CatCreateTypeCategoryEntry "_cidr" ( "A" , False )
>        , CatCreateArrayType "_cid" "cid"
>        , CatCreateTypeCategoryEntry "_cid" ( "A" , False )
>        , CatCreateArrayType "_char" "char"
>        , CatCreateTypeCategoryEntry "_char" ( "A" , False )
>        , CatCreateArrayType "_bytea" "bytea"
>        , CatCreateTypeCategoryEntry "_bytea" ( "A" , False )
>        , CatCreateArrayType "_bpchar" "bpchar"
>        , CatCreateTypeCategoryEntry "_bpchar" ( "A" , False )
>        , CatCreateArrayType "_box" "box"
>        , CatCreateTypeCategoryEntry "_box" ( "A" , False )
>        , CatCreateArrayType "_bool" "bool"
>        , CatCreateTypeCategoryEntry "_bool" ( "A" , False )
>        , CatCreateArrayType "_bit" "bit"
>        , CatCreateTypeCategoryEntry "_bit" ( "A" , False )
>        , CatCreateArrayType "_aclitem" "aclitem"
>        , CatCreateTypeCategoryEntry "_aclitem" ( "A" , False )
>        , CatCreateArrayType "_abstime" "abstime"
>        , CatCreateTypeCategoryEntry "_abstime" ( "A" , False )
>        , CatCreatePrefixOp "!!" "int8" "numeric"
>        , CatCreatePrefixOp "!!" "tsquery" "tsquery"
>        , CatCreatePrefixOp "#" "path" "int4"
>        , CatCreatePrefixOp "#" "polygon" "int4"
>        , CatCreatePrefixOp "+" "int8" "int8"
>        , CatCreatePrefixOp "+" "int2" "int2"
>        , CatCreatePrefixOp "+" "int4" "int4"
>        , CatCreatePrefixOp "+" "float4" "float4"
>        , CatCreatePrefixOp "+" "float8" "float8"
>        , CatCreatePrefixOp "+" "numeric" "numeric"
>        , CatCreatePrefixOp "-" "int8" "int8"
>        , CatCreatePrefixOp "-" "int2" "int2"
>        , CatCreatePrefixOp "-" "int4" "int4"
>        , CatCreatePrefixOp "-" "float4" "float4"
>        , CatCreatePrefixOp "-" "float8" "float8"
>        , CatCreatePrefixOp "-" "interval" "interval"
>        , CatCreatePrefixOp "-" "numeric" "numeric"
>        , CatCreatePrefixOp "?-" "lseg" "bool"
>        , CatCreatePrefixOp "?-" "line" "bool"
>        , CatCreatePrefixOp "?|" "lseg" "bool"
>        , CatCreatePrefixOp "?|" "line" "bool"
>        , CatCreatePrefixOp "@" "int8" "int8"
>        , CatCreatePrefixOp "@" "int2" "int2"
>        , CatCreatePrefixOp "@" "int4" "int4"
>        , CatCreatePrefixOp "@" "float4" "float4"
>        , CatCreatePrefixOp "@" "float8" "float8"
>        , CatCreatePrefixOp "@" "numeric" "numeric"
>        , CatCreatePrefixOp "@-@" "lseg" "float8"
>        , CatCreatePrefixOp "@-@" "path" "float8"
>        , CatCreatePrefixOp "@@" "lseg" "point"
>        , CatCreatePrefixOp "@@" "path" "point"
>        , CatCreatePrefixOp "@@" "box" "point"
>        , CatCreatePrefixOp "@@" "polygon" "point"
>        , CatCreatePrefixOp "@@" "circle" "point"
>        , CatCreatePrefixOp "|" "tinterval" "abstime"
>        , CatCreatePrefixOp "|/" "float8" "float8"
>        , CatCreatePrefixOp "||/" "float8" "float8"
>        , CatCreatePrefixOp "~" "int8" "int8"
>        , CatCreatePrefixOp "~" "int2" "int2"
>        , CatCreatePrefixOp "~" "int4" "int4"
>        , CatCreatePrefixOp "~" "macaddr" "macaddr"
>        , CatCreatePrefixOp "~" "inet" "inet"
>        , CatCreatePrefixOp "~" "bit" "bit"
>        , CatCreatePostfixOp "!" "int8" "numeric"
>        , CatCreateBinaryOp "!~" "name" "text" "bool"
>        , CatCreateBinaryOp "!~" "text" "text" "bool"
>        , CatCreateBinaryOp "!~" "bpchar" "text" "bool"
>        , CatCreateBinaryOp "!~*" "name" "text" "bool"
>        , CatCreateBinaryOp "!~*" "text" "text" "bool"
>        , CatCreateBinaryOp "!~*" "bpchar" "text" "bool"
>        , CatCreateBinaryOp "!~~" "bytea" "bytea" "bool"
>        , CatCreateBinaryOp "!~~" "name" "text" "bool"
>        , CatCreateBinaryOp "!~~" "text" "text" "bool"
>        , CatCreateBinaryOp "!~~" "bpchar" "text" "bool"
>        , CatCreateBinaryOp "!~~*" "name" "text" "bool"
>        , CatCreateBinaryOp "!~~*" "text" "text" "bool"
>        , CatCreateBinaryOp "!~~*" "bpchar" "text" "bool"
>        , CatCreateBinaryOp "#" "int8" "int8" "int8"
>        , CatCreateBinaryOp "#" "int2" "int2" "int2"
>        , CatCreateBinaryOp "#" "int4" "int4" "int4"
>        , CatCreateBinaryOp "#" "lseg" "lseg" "point"
>        , CatCreateBinaryOp "#" "box" "box" "box"
>        , CatCreateBinaryOp "#" "line" "line" "point"
>        , CatCreateBinaryOp "#" "bit" "bit" "bit"
>        , CatCreateBinaryOp "##" "point" "lseg" "point"
>        , CatCreateBinaryOp "##" "point" "box" "point"
>        , CatCreateBinaryOp "##" "point" "line" "point"
>        , CatCreateBinaryOp "##" "lseg" "lseg" "point"
>        , CatCreateBinaryOp "##" "lseg" "box" "point"
>        , CatCreateBinaryOp "##" "lseg" "line" "point"
>        , CatCreateBinaryOp "##" "line" "lseg" "point"
>        , CatCreateBinaryOp "##" "line" "box" "point"
>        , CatCreateBinaryOp "#<" "tinterval" "reltime" "bool"
>        , CatCreateBinaryOp "#<=" "tinterval" "reltime" "bool"
>        , CatCreateBinaryOp "#<>" "tinterval" "reltime" "bool"
>        , CatCreateBinaryOp "#=" "tinterval" "reltime" "bool"
>        , CatCreateBinaryOp "#>" "json" "_text" "json"
>        , CatCreateBinaryOp "#>" "tinterval" "reltime" "bool"
>        , CatCreateBinaryOp "#>" "jsonb" "_text" "jsonb"
>        , CatCreateBinaryOp "#>=" "tinterval" "reltime" "bool"
>        , CatCreateBinaryOp "#>>" "json" "_text" "text"
>        , CatCreateBinaryOp "#>>" "jsonb" "_text" "text"
>        , CatCreateBinaryOp "%" "int8" "int8" "int8"
>        , CatCreateBinaryOp "%" "int2" "int2" "int2"
>        , CatCreateBinaryOp "%" "int4" "int4" "int4"
>        , CatCreateBinaryOp "%" "numeric" "numeric" "numeric"
>        , CatCreateBinaryOp "&" "int8" "int8" "int8"
>        , CatCreateBinaryOp "&" "int2" "int2" "int2"
>        , CatCreateBinaryOp "&" "int4" "int4" "int4"
>        , CatCreateBinaryOp "&" "macaddr" "macaddr" "macaddr"
>        , CatCreateBinaryOp "&" "inet" "inet" "inet"
>        , CatCreateBinaryOp "&" "bit" "bit" "bit"
>        , CatCreateBinaryOp "&&" "box" "box" "bool"
>        , CatCreateBinaryOp "&&" "polygon" "polygon" "bool"
>        , CatCreateBinaryOp "&&" "tinterval" "tinterval" "bool"
>        , CatCreateBinaryOp "&&" "circle" "circle" "bool"
>        , CatCreateBinaryOp "&&" "inet" "inet" "bool"
>        , CatCreateBinaryOp "&&" "anyarray" "anyarray" "bool"
>        , CatCreateBinaryOp "&&" "tsquery" "tsquery" "tsquery"
>        , CatCreateBinaryOp "&&" "anyrange" "anyrange" "bool"
>        , CatCreateBinaryOp "&<" "box" "box" "bool"
>        , CatCreateBinaryOp "&<" "polygon" "polygon" "bool"
>        , CatCreateBinaryOp "&<" "circle" "circle" "bool"
>        , CatCreateBinaryOp "&<" "anyrange" "anyrange" "bool"
>        , CatCreateBinaryOp "&<|" "box" "box" "bool"
>        , CatCreateBinaryOp "&<|" "polygon" "polygon" "bool"
>        , CatCreateBinaryOp "&<|" "circle" "circle" "bool"
>        , CatCreateBinaryOp "&>" "box" "box" "bool"
>        , CatCreateBinaryOp "&>" "polygon" "polygon" "bool"
>        , CatCreateBinaryOp "&>" "circle" "circle" "bool"
>        , CatCreateBinaryOp "&>" "anyrange" "anyrange" "bool"
>        , CatCreateBinaryOp "*" "int8" "int8" "int8"
>        , CatCreateBinaryOp "*" "int8" "int2" "int8"
>        , CatCreateBinaryOp "*" "int8" "int4" "int8"
>        , CatCreateBinaryOp "*" "int2" "int8" "int8"
>        , CatCreateBinaryOp "*" "int2" "int2" "int2"
>        , CatCreateBinaryOp "*" "int2" "int4" "int4"
>        , CatCreateBinaryOp "*" "int2" "money" "money"
>        , CatCreateBinaryOp "*" "int4" "int8" "int8"
>        , CatCreateBinaryOp "*" "int4" "int2" "int4"
>        , CatCreateBinaryOp "*" "int4" "int4" "int4"
>        , CatCreateBinaryOp "*" "int4" "money" "money"
>        , CatCreateBinaryOp "*" "point" "point" "point"
>        , CatCreateBinaryOp "*" "path" "point" "path"
>        , CatCreateBinaryOp "*" "box" "point" "box"
>        , CatCreateBinaryOp "*" "float4" "float4" "float4"
>        , CatCreateBinaryOp "*" "float4" "float8" "float8"
>        , CatCreateBinaryOp "*" "float4" "money" "money"
>        , CatCreateBinaryOp "*" "float8" "float4" "float8"
>        , CatCreateBinaryOp "*" "float8" "float8" "float8"
>        , CatCreateBinaryOp "*" "float8" "money" "money"
>        , CatCreateBinaryOp "*" "float8" "interval" "interval"
>        , CatCreateBinaryOp "*" "circle" "point" "circle"
>        , CatCreateBinaryOp "*" "money" "int2" "money"
>        , CatCreateBinaryOp "*" "money" "int4" "money"
>        , CatCreateBinaryOp "*" "money" "float4" "money"
>        , CatCreateBinaryOp "*" "money" "float8" "money"
>        , CatCreateBinaryOp "*" "interval" "float8" "interval"
>        , CatCreateBinaryOp "*" "numeric" "numeric" "numeric"
>        , CatCreateBinaryOp "*" "anyrange" "anyrange" "anyrange"
>        , CatCreateBinaryOp "*<" "record" "record" "bool"
>        , CatCreateBinaryOp "*<=" "record" "record" "bool"
>        , CatCreateBinaryOp "*<>" "record" "record" "bool"
>        , CatCreateBinaryOp "*=" "record" "record" "bool"
>        , CatCreateBinaryOp "*>" "record" "record" "bool"
>        , CatCreateBinaryOp "*>=" "record" "record" "bool"
>        , CatCreateBinaryOp "+" "int8" "int8" "int8"
>        , CatCreateBinaryOp "+" "int8" "int2" "int8"
>        , CatCreateBinaryOp "+" "int8" "int4" "int8"
>        , CatCreateBinaryOp "+" "int8" "inet" "inet"
>        , CatCreateBinaryOp "+" "int2" "int8" "int8"
>        , CatCreateBinaryOp "+" "int2" "int2" "int2"
>        , CatCreateBinaryOp "+" "int2" "int4" "int4"
>        , CatCreateBinaryOp "+" "int4" "int8" "int8"
>        , CatCreateBinaryOp "+" "int4" "int2" "int4"
>        , CatCreateBinaryOp "+" "int4" "int4" "int4"
>        , CatCreateBinaryOp "+" "int4" "date" "date"
>        , CatCreateBinaryOp "+" "point" "point" "point"
>        , CatCreateBinaryOp "+" "path" "point" "path"
>        , CatCreateBinaryOp "+" "path" "path" "path"
>        , CatCreateBinaryOp "+" "box" "point" "box"
>        , CatCreateBinaryOp "+" "float4" "float4" "float4"
>        , CatCreateBinaryOp "+" "float4" "float8" "float8"
>        , CatCreateBinaryOp "+" "float8" "float4" "float8"
>        , CatCreateBinaryOp "+" "float8" "float8" "float8"
>        , CatCreateBinaryOp "+" "abstime" "reltime" "abstime"
>        , CatCreateBinaryOp "+" "circle" "point" "circle"
>        , CatCreateBinaryOp "+" "money" "money" "money"
>        , CatCreateBinaryOp "+" "inet" "int8" "inet"
>        , CatCreateBinaryOp "+" "_aclitem" "aclitem" "_aclitem"
>        , CatCreateBinaryOp "+" "date" "int4" "date"
>        , CatCreateBinaryOp "+" "date" "time" "timestamp"
>        , CatCreateBinaryOp "+" "date" "interval" "timestamp"
>        , CatCreateBinaryOp "+" "date" "timetz" "timestamptz"
>        , CatCreateBinaryOp "+" "time" "date" "timestamp"
>        , CatCreateBinaryOp "+" "time" "interval" "time"
>        , CatCreateBinaryOp "+" "timestamp" "interval" "timestamp"
>        , CatCreateBinaryOp "+" "timestamptz" "interval" "timestamptz"
>        , CatCreateBinaryOp "+" "interval" "date" "timestamp"
>        , CatCreateBinaryOp "+" "interval" "time" "time"
>        , CatCreateBinaryOp "+" "interval" "timestamp" "timestamp"
>        , CatCreateBinaryOp "+" "interval" "timestamptz" "timestamptz"
>        , CatCreateBinaryOp "+" "interval" "interval" "interval"
>        , CatCreateBinaryOp "+" "interval" "timetz" "timetz"
>        , CatCreateBinaryOp "+" "timetz" "date" "timestamptz"
>        , CatCreateBinaryOp "+" "timetz" "interval" "timetz"
>        , CatCreateBinaryOp "+" "numeric" "numeric" "numeric"
>        , CatCreateBinaryOp "+" "anyrange" "anyrange" "anyrange"
>        , CatCreateBinaryOp "-" "int8" "int8" "int8"
>        , CatCreateBinaryOp "-" "int8" "int2" "int8"
>        , CatCreateBinaryOp "-" "int8" "int4" "int8"
>        , CatCreateBinaryOp "-" "int2" "int8" "int8"
>        , CatCreateBinaryOp "-" "int2" "int2" "int2"
>        , CatCreateBinaryOp "-" "int2" "int4" "int4"
>        , CatCreateBinaryOp "-" "int4" "int8" "int8"
>        , CatCreateBinaryOp "-" "int4" "int2" "int4"
>        , CatCreateBinaryOp "-" "int4" "int4" "int4"
>        , CatCreateBinaryOp "-" "point" "point" "point"
>        , CatCreateBinaryOp "-" "path" "point" "path"
>        , CatCreateBinaryOp "-" "box" "point" "box"
>        , CatCreateBinaryOp "-" "float4" "float4" "float4"
>        , CatCreateBinaryOp "-" "float4" "float8" "float8"
>        , CatCreateBinaryOp "-" "float8" "float4" "float8"
>        , CatCreateBinaryOp "-" "float8" "float8" "float8"
>        , CatCreateBinaryOp "-" "abstime" "reltime" "abstime"
>        , CatCreateBinaryOp "-" "circle" "point" "circle"
>        , CatCreateBinaryOp "-" "money" "money" "money"
>        , CatCreateBinaryOp "-" "inet" "int8" "inet"
>        , CatCreateBinaryOp "-" "inet" "inet" "int8"
>        , CatCreateBinaryOp "-" "_aclitem" "aclitem" "_aclitem"
>        , CatCreateBinaryOp "-" "date" "int4" "date"
>        , CatCreateBinaryOp "-" "date" "date" "int4"
>        , CatCreateBinaryOp "-" "date" "interval" "timestamp"
>        , CatCreateBinaryOp "-" "time" "time" "interval"
>        , CatCreateBinaryOp "-" "time" "interval" "time"
>        , CatCreateBinaryOp "-" "timestamp" "timestamp" "interval"
>        , CatCreateBinaryOp "-" "timestamp" "interval" "timestamp"
>        , CatCreateBinaryOp "-" "timestamptz" "timestamptz" "interval"
>        , CatCreateBinaryOp "-" "timestamptz" "interval" "timestamptz"
>        , CatCreateBinaryOp "-" "interval" "interval" "interval"
>        , CatCreateBinaryOp "-" "timetz" "interval" "timetz"
>        , CatCreateBinaryOp "-" "numeric" "numeric" "numeric"
>        , CatCreateBinaryOp "-" "pg_lsn" "pg_lsn" "numeric"
>        , CatCreateBinaryOp "-" "anyrange" "anyrange" "anyrange"
>        , CatCreateBinaryOp "->" "json" "int4" "json"
>        , CatCreateBinaryOp "->" "json" "text" "json"
>        , CatCreateBinaryOp "->" "jsonb" "int4" "jsonb"
>        , CatCreateBinaryOp "->" "jsonb" "text" "jsonb"
>        , CatCreateBinaryOp "->>" "json" "int4" "text"
>        , CatCreateBinaryOp "->>" "json" "text" "text"
>        , CatCreateBinaryOp "->>" "jsonb" "int4" "text"
>        , CatCreateBinaryOp "->>" "jsonb" "text" "text"
>        , CatCreateBinaryOp "-|-" "anyrange" "anyrange" "bool"
>        , CatCreateBinaryOp "/" "int8" "int8" "int8"
>        , CatCreateBinaryOp "/" "int8" "int2" "int8"
>        , CatCreateBinaryOp "/" "int8" "int4" "int8"
>        , CatCreateBinaryOp "/" "int2" "int8" "int8"
>        , CatCreateBinaryOp "/" "int2" "int2" "int2"
>        , CatCreateBinaryOp "/" "int2" "int4" "int4"
>        , CatCreateBinaryOp "/" "int4" "int8" "int8"
>        , CatCreateBinaryOp "/" "int4" "int2" "int4"
>        , CatCreateBinaryOp "/" "int4" "int4" "int4"
>        , CatCreateBinaryOp "/" "point" "point" "point"
>        , CatCreateBinaryOp "/" "path" "point" "path"
>        , CatCreateBinaryOp "/" "box" "point" "box"
>        , CatCreateBinaryOp "/" "float4" "float4" "float4"
>        , CatCreateBinaryOp "/" "float4" "float8" "float8"
>        , CatCreateBinaryOp "/" "float8" "float4" "float8"
>        , CatCreateBinaryOp "/" "float8" "float8" "float8"
>        , CatCreateBinaryOp "/" "circle" "point" "circle"
>        , CatCreateBinaryOp "/" "money" "int2" "money"
>        , CatCreateBinaryOp "/" "money" "int4" "money"
>        , CatCreateBinaryOp "/" "money" "float4" "money"
>        , CatCreateBinaryOp "/" "money" "float8" "money"
>        , CatCreateBinaryOp "/" "money" "money" "float8"
>        , CatCreateBinaryOp "/" "interval" "float8" "interval"
>        , CatCreateBinaryOp "/" "numeric" "numeric" "numeric"
>        , CatCreateBinaryOp "<" "bool" "bool" "bool"
>        , CatCreateBinaryOp "<" "bytea" "bytea" "bool"
>        , CatCreateBinaryOp "<" "char" "char" "bool"
>        , CatCreateBinaryOp "<" "name" "name" "bool"
>        , CatCreateBinaryOp "<" "int8" "int8" "bool"
>        , CatCreateBinaryOp "<" "int8" "int2" "bool"
>        , CatCreateBinaryOp "<" "int8" "int4" "bool"
>        , CatCreateBinaryOp "<" "int2" "int8" "bool"
>        , CatCreateBinaryOp "<" "int2" "int2" "bool"
>        , CatCreateBinaryOp "<" "int2" "int4" "bool"
>        , CatCreateBinaryOp "<" "int4" "int8" "bool"
>        , CatCreateBinaryOp "<" "int4" "int2" "bool"
>        , CatCreateBinaryOp "<" "int4" "int4" "bool"
>        , CatCreateBinaryOp "<" "text" "text" "bool"
>        , CatCreateBinaryOp "<" "oid" "oid" "bool"
>        , CatCreateBinaryOp "<" "tid" "tid" "bool"
>        , CatCreateBinaryOp "<" "oidvector" "oidvector" "bool"
>        , CatCreateBinaryOp "<" "lseg" "lseg" "bool"
>        , CatCreateBinaryOp "<" "path" "path" "bool"
>        , CatCreateBinaryOp "<" "box" "box" "bool"
>        , CatCreateBinaryOp "<" "float4" "float4" "bool"
>        , CatCreateBinaryOp "<" "float4" "float8" "bool"
>        , CatCreateBinaryOp "<" "float8" "float4" "bool"
>        , CatCreateBinaryOp "<" "float8" "float8" "bool"
>        , CatCreateBinaryOp "<" "abstime" "abstime" "bool"
>        , CatCreateBinaryOp "<" "reltime" "reltime" "bool"
>        , CatCreateBinaryOp "<" "tinterval" "tinterval" "bool"
>        , CatCreateBinaryOp "<" "circle" "circle" "bool"
>        , CatCreateBinaryOp "<" "money" "money" "bool"
>        , CatCreateBinaryOp "<" "macaddr" "macaddr" "bool"
>        , CatCreateBinaryOp "<" "inet" "inet" "bool"
>        , CatCreateBinaryOp "<" "bpchar" "bpchar" "bool"
>        , CatCreateBinaryOp "<" "date" "date" "bool"
>        , CatCreateBinaryOp "<" "date" "timestamp" "bool"
>        , CatCreateBinaryOp "<" "date" "timestamptz" "bool"
>        , CatCreateBinaryOp "<" "time" "time" "bool"
>        , CatCreateBinaryOp "<" "timestamp" "date" "bool"
>        , CatCreateBinaryOp "<" "timestamp" "timestamp" "bool"
>        , CatCreateBinaryOp "<" "timestamp" "timestamptz" "bool"
>        , CatCreateBinaryOp "<" "timestamptz" "date" "bool"
>        , CatCreateBinaryOp "<" "timestamptz" "timestamp" "bool"
>        , CatCreateBinaryOp "<" "timestamptz" "timestamptz" "bool"
>        , CatCreateBinaryOp "<" "interval" "interval" "bool"
>        , CatCreateBinaryOp "<" "timetz" "timetz" "bool"
>        , CatCreateBinaryOp "<" "bit" "bit" "bool"
>        , CatCreateBinaryOp "<" "varbit" "varbit" "bool"
>        , CatCreateBinaryOp "<" "numeric" "numeric" "bool"
>        , CatCreateBinaryOp "<" "record" "record" "bool"
>        , CatCreateBinaryOp "<" "anyarray" "anyarray" "bool"
>        , CatCreateBinaryOp "<" "uuid" "uuid" "bool"
>        , CatCreateBinaryOp "<" "pg_lsn" "pg_lsn" "bool"
>        , CatCreateBinaryOp "<" "anyenum" "anyenum" "bool"
>        , CatCreateBinaryOp "<" "tsvector" "tsvector" "bool"
>        , CatCreateBinaryOp "<" "tsquery" "tsquery" "bool"
>        , CatCreateBinaryOp "<" "jsonb" "jsonb" "bool"
>        , CatCreateBinaryOp "<" "anyrange" "anyrange" "bool"
>        , CatCreateBinaryOp "<#>" "abstime" "abstime" "tinterval"
>        , CatCreateBinaryOp "<->" "point" "point" "float8"
>        , CatCreateBinaryOp "<->" "point" "lseg" "float8"
>        , CatCreateBinaryOp "<->" "point" "path" "float8"
>        , CatCreateBinaryOp "<->" "point" "box" "float8"
>        , CatCreateBinaryOp "<->" "point" "line" "float8"
>        , CatCreateBinaryOp "<->" "point" "circle" "float8"
>        , CatCreateBinaryOp "<->" "lseg" "lseg" "float8"
>        , CatCreateBinaryOp "<->" "lseg" "box" "float8"
>        , CatCreateBinaryOp "<->" "lseg" "line" "float8"
>        , CatCreateBinaryOp "<->" "path" "path" "float8"
>        , CatCreateBinaryOp "<->" "box" "box" "float8"
>        , CatCreateBinaryOp "<->" "polygon" "polygon" "float8"
>        , CatCreateBinaryOp "<->" "line" "box" "float8"
>        , CatCreateBinaryOp "<->" "line" "line" "float8"
>        , CatCreateBinaryOp "<->" "circle" "polygon" "float8"
>        , CatCreateBinaryOp "<->" "circle" "circle" "float8"
>        , CatCreateBinaryOp "<<" "int8" "int4" "int8"
>        , CatCreateBinaryOp "<<" "int2" "int4" "int2"
>        , CatCreateBinaryOp "<<" "int4" "int4" "int4"
>        , CatCreateBinaryOp "<<" "point" "point" "bool"
>        , CatCreateBinaryOp "<<" "box" "box" "bool"
>        , CatCreateBinaryOp "<<" "polygon" "polygon" "bool"
>        , CatCreateBinaryOp "<<" "tinterval" "tinterval" "bool"
>        , CatCreateBinaryOp "<<" "circle" "circle" "bool"
>        , CatCreateBinaryOp "<<" "inet" "inet" "bool"
>        , CatCreateBinaryOp "<<" "bit" "int4" "bit"
>        , CatCreateBinaryOp "<<" "anyrange" "anyrange" "bool"
>        , CatCreateBinaryOp "<<=" "inet" "inet" "bool"
>        , CatCreateBinaryOp "<<|" "box" "box" "bool"
>        , CatCreateBinaryOp "<<|" "polygon" "polygon" "bool"
>        , CatCreateBinaryOp "<<|" "circle" "circle" "bool"
>        , CatCreateBinaryOp "<=" "bool" "bool" "bool"
>        , CatCreateBinaryOp "<=" "bytea" "bytea" "bool"
>        , CatCreateBinaryOp "<=" "char" "char" "bool"
>        , CatCreateBinaryOp "<=" "name" "name" "bool"
>        , CatCreateBinaryOp "<=" "int8" "int8" "bool"
>        , CatCreateBinaryOp "<=" "int8" "int2" "bool"
>        , CatCreateBinaryOp "<=" "int8" "int4" "bool"
>        , CatCreateBinaryOp "<=" "int2" "int8" "bool"
>        , CatCreateBinaryOp "<=" "int2" "int2" "bool"
>        , CatCreateBinaryOp "<=" "int2" "int4" "bool"
>        , CatCreateBinaryOp "<=" "int4" "int8" "bool"
>        , CatCreateBinaryOp "<=" "int4" "int2" "bool"
>        , CatCreateBinaryOp "<=" "int4" "int4" "bool"
>        , CatCreateBinaryOp "<=" "text" "text" "bool"
>        , CatCreateBinaryOp "<=" "oid" "oid" "bool"
>        , CatCreateBinaryOp "<=" "tid" "tid" "bool"
>        , CatCreateBinaryOp "<=" "oidvector" "oidvector" "bool"
>        , CatCreateBinaryOp "<=" "lseg" "lseg" "bool"
>        , CatCreateBinaryOp "<=" "path" "path" "bool"
>        , CatCreateBinaryOp "<=" "box" "box" "bool"
>        , CatCreateBinaryOp "<=" "float4" "float4" "bool"
>        , CatCreateBinaryOp "<=" "float4" "float8" "bool"
>        , CatCreateBinaryOp "<=" "float8" "float4" "bool"
>        , CatCreateBinaryOp "<=" "float8" "float8" "bool"
>        , CatCreateBinaryOp "<=" "abstime" "abstime" "bool"
>        , CatCreateBinaryOp "<=" "reltime" "reltime" "bool"
>        , CatCreateBinaryOp "<=" "tinterval" "tinterval" "bool"
>        , CatCreateBinaryOp "<=" "circle" "circle" "bool"
>        , CatCreateBinaryOp "<=" "money" "money" "bool"
>        , CatCreateBinaryOp "<=" "macaddr" "macaddr" "bool"
>        , CatCreateBinaryOp "<=" "inet" "inet" "bool"
>        , CatCreateBinaryOp "<=" "bpchar" "bpchar" "bool"
>        , CatCreateBinaryOp "<=" "date" "date" "bool"
>        , CatCreateBinaryOp "<=" "date" "timestamp" "bool"
>        , CatCreateBinaryOp "<=" "date" "timestamptz" "bool"
>        , CatCreateBinaryOp "<=" "time" "time" "bool"
>        , CatCreateBinaryOp "<=" "timestamp" "date" "bool"
>        , CatCreateBinaryOp "<=" "timestamp" "timestamp" "bool"
>        , CatCreateBinaryOp "<=" "timestamp" "timestamptz" "bool"
>        , CatCreateBinaryOp "<=" "timestamptz" "date" "bool"
>        , CatCreateBinaryOp "<=" "timestamptz" "timestamp" "bool"
>        , CatCreateBinaryOp "<=" "timestamptz" "timestamptz" "bool"
>        , CatCreateBinaryOp "<=" "interval" "interval" "bool"
>        , CatCreateBinaryOp "<=" "timetz" "timetz" "bool"
>        , CatCreateBinaryOp "<=" "bit" "bit" "bool"
>        , CatCreateBinaryOp "<=" "varbit" "varbit" "bool"
>        , CatCreateBinaryOp "<=" "numeric" "numeric" "bool"
>        , CatCreateBinaryOp "<=" "record" "record" "bool"
>        , CatCreateBinaryOp "<=" "anyarray" "anyarray" "bool"
>        , CatCreateBinaryOp "<=" "uuid" "uuid" "bool"
>        , CatCreateBinaryOp "<=" "pg_lsn" "pg_lsn" "bool"
>        , CatCreateBinaryOp "<=" "anyenum" "anyenum" "bool"
>        , CatCreateBinaryOp "<=" "tsvector" "tsvector" "bool"
>        , CatCreateBinaryOp "<=" "tsquery" "tsquery" "bool"
>        , CatCreateBinaryOp "<=" "jsonb" "jsonb" "bool"
>        , CatCreateBinaryOp "<=" "anyrange" "anyrange" "bool"
>        , CatCreateBinaryOp "<>" "bool" "bool" "bool"
>        , CatCreateBinaryOp "<>" "bytea" "bytea" "bool"
>        , CatCreateBinaryOp "<>" "char" "char" "bool"
>        , CatCreateBinaryOp "<>" "name" "name" "bool"
>        , CatCreateBinaryOp "<>" "int8" "int8" "bool"
>        , CatCreateBinaryOp "<>" "int8" "int2" "bool"
>        , CatCreateBinaryOp "<>" "int8" "int4" "bool"
>        , CatCreateBinaryOp "<>" "int2" "int8" "bool"
>        , CatCreateBinaryOp "<>" "int2" "int2" "bool"
>        , CatCreateBinaryOp "<>" "int2" "int4" "bool"
>        , CatCreateBinaryOp "<>" "int4" "int8" "bool"
>        , CatCreateBinaryOp "<>" "int4" "int2" "bool"
>        , CatCreateBinaryOp "<>" "int4" "int4" "bool"
>        , CatCreateBinaryOp "<>" "text" "text" "bool"
>        , CatCreateBinaryOp "<>" "oid" "oid" "bool"
>        , CatCreateBinaryOp "<>" "tid" "tid" "bool"
>        , CatCreateBinaryOp "<>" "oidvector" "oidvector" "bool"
>        , CatCreateBinaryOp "<>" "point" "point" "bool"
>        , CatCreateBinaryOp "<>" "lseg" "lseg" "bool"
>        , CatCreateBinaryOp "<>" "float4" "float4" "bool"
>        , CatCreateBinaryOp "<>" "float4" "float8" "bool"
>        , CatCreateBinaryOp "<>" "float8" "float4" "bool"
>        , CatCreateBinaryOp "<>" "float8" "float8" "bool"
>        , CatCreateBinaryOp "<>" "abstime" "abstime" "bool"
>        , CatCreateBinaryOp "<>" "reltime" "reltime" "bool"
>        , CatCreateBinaryOp "<>" "tinterval" "tinterval" "bool"
>        , CatCreateBinaryOp "<>" "circle" "circle" "bool"
>        , CatCreateBinaryOp "<>" "money" "money" "bool"
>        , CatCreateBinaryOp "<>" "macaddr" "macaddr" "bool"
>        , CatCreateBinaryOp "<>" "inet" "inet" "bool"
>        , CatCreateBinaryOp "<>" "bpchar" "bpchar" "bool"
>        , CatCreateBinaryOp "<>" "date" "date" "bool"
>        , CatCreateBinaryOp "<>" "date" "timestamp" "bool"
>        , CatCreateBinaryOp "<>" "date" "timestamptz" "bool"
>        , CatCreateBinaryOp "<>" "time" "time" "bool"
>        , CatCreateBinaryOp "<>" "timestamp" "date" "bool"
>        , CatCreateBinaryOp "<>" "timestamp" "timestamp" "bool"
>        , CatCreateBinaryOp "<>" "timestamp" "timestamptz" "bool"
>        , CatCreateBinaryOp "<>" "timestamptz" "date" "bool"
>        , CatCreateBinaryOp "<>" "timestamptz" "timestamp" "bool"
>        , CatCreateBinaryOp "<>" "timestamptz" "timestamptz" "bool"
>        , CatCreateBinaryOp "<>" "interval" "interval" "bool"
>        , CatCreateBinaryOp "<>" "timetz" "timetz" "bool"
>        , CatCreateBinaryOp "<>" "bit" "bit" "bool"
>        , CatCreateBinaryOp "<>" "varbit" "varbit" "bool"
>        , CatCreateBinaryOp "<>" "numeric" "numeric" "bool"
>        , CatCreateBinaryOp "<>" "record" "record" "bool"
>        , CatCreateBinaryOp "<>" "anyarray" "anyarray" "bool"
>        , CatCreateBinaryOp "<>" "uuid" "uuid" "bool"
>        , CatCreateBinaryOp "<>" "pg_lsn" "pg_lsn" "bool"
>        , CatCreateBinaryOp "<>" "anyenum" "anyenum" "bool"
>        , CatCreateBinaryOp "<>" "tsvector" "tsvector" "bool"
>        , CatCreateBinaryOp "<>" "tsquery" "tsquery" "bool"
>        , CatCreateBinaryOp "<>" "jsonb" "jsonb" "bool"
>        , CatCreateBinaryOp "<>" "anyrange" "anyrange" "bool"
>        , CatCreateBinaryOp "<?>" "abstime" "tinterval" "bool"
>        , CatCreateBinaryOp "<@" "point" "lseg" "bool"
>        , CatCreateBinaryOp "<@" "point" "path" "bool"
>        , CatCreateBinaryOp "<@" "point" "box" "bool"
>        , CatCreateBinaryOp "<@" "point" "polygon" "bool"
>        , CatCreateBinaryOp "<@" "point" "line" "bool"
>        , CatCreateBinaryOp "<@" "point" "circle" "bool"
>        , CatCreateBinaryOp "<@" "lseg" "box" "bool"
>        , CatCreateBinaryOp "<@" "lseg" "line" "bool"
>        , CatCreateBinaryOp "<@" "box" "box" "bool"
>        , CatCreateBinaryOp "<@" "polygon" "polygon" "bool"
>        , CatCreateBinaryOp "<@" "circle" "circle" "bool"
>        , CatCreateBinaryOp "<@" "anyarray" "anyarray" "bool"
>        , CatCreateBinaryOp "<@" "anyelement" "anyrange" "bool"
>        , CatCreateBinaryOp "<@" "tsquery" "tsquery" "bool"
>        , CatCreateBinaryOp "<@" "jsonb" "jsonb" "bool"
>        , CatCreateBinaryOp "<@" "anyrange" "anyrange" "bool"
>        , CatCreateBinaryOp "<^" "point" "point" "bool"
>        , CatCreateBinaryOp "<^" "box" "box" "bool"
>        , CatCreateBinaryOp "=" "bool" "bool" "bool"
>        , CatCreateBinaryOp "=" "bytea" "bytea" "bool"
>        , CatCreateBinaryOp "=" "char" "char" "bool"
>        , CatCreateBinaryOp "=" "name" "name" "bool"
>        , CatCreateBinaryOp "=" "int8" "int8" "bool"
>        , CatCreateBinaryOp "=" "int8" "int2" "bool"
>        , CatCreateBinaryOp "=" "int8" "int4" "bool"
>        , CatCreateBinaryOp "=" "int2" "int8" "bool"
>        , CatCreateBinaryOp "=" "int2" "int2" "bool"
>        , CatCreateBinaryOp "=" "int2" "int4" "bool"
>        , CatCreateBinaryOp "=" "int2vector" "int2vector" "bool"
>        , CatCreateBinaryOp "=" "int4" "int8" "bool"
>        , CatCreateBinaryOp "=" "int4" "int2" "bool"
>        , CatCreateBinaryOp "=" "int4" "int4" "bool"
>        , CatCreateBinaryOp "=" "text" "text" "bool"
>        , CatCreateBinaryOp "=" "oid" "oid" "bool"
>        , CatCreateBinaryOp "=" "tid" "tid" "bool"
>        , CatCreateBinaryOp "=" "xid" "int4" "bool"
>        , CatCreateBinaryOp "=" "xid" "xid" "bool"
>        , CatCreateBinaryOp "=" "cid" "cid" "bool"
>        , CatCreateBinaryOp "=" "oidvector" "oidvector" "bool"
>        , CatCreateBinaryOp "=" "lseg" "lseg" "bool"
>        , CatCreateBinaryOp "=" "path" "path" "bool"
>        , CatCreateBinaryOp "=" "box" "box" "bool"
>        , CatCreateBinaryOp "=" "line" "line" "bool"
>        , CatCreateBinaryOp "=" "float4" "float4" "bool"
>        , CatCreateBinaryOp "=" "float4" "float8" "bool"
>        , CatCreateBinaryOp "=" "float8" "float4" "bool"
>        , CatCreateBinaryOp "=" "float8" "float8" "bool"
>        , CatCreateBinaryOp "=" "abstime" "abstime" "bool"
>        , CatCreateBinaryOp "=" "reltime" "reltime" "bool"
>        , CatCreateBinaryOp "=" "tinterval" "tinterval" "bool"
>        , CatCreateBinaryOp "=" "circle" "circle" "bool"
>        , CatCreateBinaryOp "=" "money" "money" "bool"
>        , CatCreateBinaryOp "=" "macaddr" "macaddr" "bool"
>        , CatCreateBinaryOp "=" "inet" "inet" "bool"
>        , CatCreateBinaryOp "=" "aclitem" "aclitem" "bool"
>        , CatCreateBinaryOp "=" "bpchar" "bpchar" "bool"
>        , CatCreateBinaryOp "=" "date" "date" "bool"
>        , CatCreateBinaryOp "=" "date" "timestamp" "bool"
>        , CatCreateBinaryOp "=" "date" "timestamptz" "bool"
>        , CatCreateBinaryOp "=" "time" "time" "bool"
>        , CatCreateBinaryOp "=" "timestamp" "date" "bool"
>        , CatCreateBinaryOp "=" "timestamp" "timestamp" "bool"
>        , CatCreateBinaryOp "=" "timestamp" "timestamptz" "bool"
>        , CatCreateBinaryOp "=" "timestamptz" "date" "bool"
>        , CatCreateBinaryOp "=" "timestamptz" "timestamp" "bool"
>        , CatCreateBinaryOp "=" "timestamptz" "timestamptz" "bool"
>        , CatCreateBinaryOp "=" "interval" "interval" "bool"
>        , CatCreateBinaryOp "=" "timetz" "timetz" "bool"
>        , CatCreateBinaryOp "=" "bit" "bit" "bool"
>        , CatCreateBinaryOp "=" "varbit" "varbit" "bool"
>        , CatCreateBinaryOp "=" "numeric" "numeric" "bool"
>        , CatCreateBinaryOp "=" "record" "record" "bool"
>        , CatCreateBinaryOp "=" "anyarray" "anyarray" "bool"
>        , CatCreateBinaryOp "=" "uuid" "uuid" "bool"
>        , CatCreateBinaryOp "=" "pg_lsn" "pg_lsn" "bool"
>        , CatCreateBinaryOp "=" "anyenum" "anyenum" "bool"
>        , CatCreateBinaryOp "=" "tsvector" "tsvector" "bool"
>        , CatCreateBinaryOp "=" "tsquery" "tsquery" "bool"
>        , CatCreateBinaryOp "=" "jsonb" "jsonb" "bool"
>        , CatCreateBinaryOp "=" "anyrange" "anyrange" "bool"
>        , CatCreateBinaryOp ">" "bool" "bool" "bool"
>        , CatCreateBinaryOp ">" "bytea" "bytea" "bool"
>        , CatCreateBinaryOp ">" "char" "char" "bool"
>        , CatCreateBinaryOp ">" "name" "name" "bool"
>        , CatCreateBinaryOp ">" "int8" "int8" "bool"
>        , CatCreateBinaryOp ">" "int8" "int2" "bool"
>        , CatCreateBinaryOp ">" "int8" "int4" "bool"
>        , CatCreateBinaryOp ">" "int2" "int8" "bool"
>        , CatCreateBinaryOp ">" "int2" "int2" "bool"
>        , CatCreateBinaryOp ">" "int2" "int4" "bool"
>        , CatCreateBinaryOp ">" "int4" "int8" "bool"
>        , CatCreateBinaryOp ">" "int4" "int2" "bool"
>        , CatCreateBinaryOp ">" "int4" "int4" "bool"
>        , CatCreateBinaryOp ">" "text" "text" "bool"
>        , CatCreateBinaryOp ">" "oid" "oid" "bool"
>        , CatCreateBinaryOp ">" "tid" "tid" "bool"
>        , CatCreateBinaryOp ">" "oidvector" "oidvector" "bool"
>        , CatCreateBinaryOp ">" "lseg" "lseg" "bool"
>        , CatCreateBinaryOp ">" "path" "path" "bool"
>        , CatCreateBinaryOp ">" "box" "box" "bool"
>        , CatCreateBinaryOp ">" "float4" "float4" "bool"
>        , CatCreateBinaryOp ">" "float4" "float8" "bool"
>        , CatCreateBinaryOp ">" "float8" "float4" "bool"
>        , CatCreateBinaryOp ">" "float8" "float8" "bool"
>        , CatCreateBinaryOp ">" "abstime" "abstime" "bool"
>        , CatCreateBinaryOp ">" "reltime" "reltime" "bool"
>        , CatCreateBinaryOp ">" "tinterval" "tinterval" "bool"
>        , CatCreateBinaryOp ">" "circle" "circle" "bool"
>        , CatCreateBinaryOp ">" "money" "money" "bool"
>        , CatCreateBinaryOp ">" "macaddr" "macaddr" "bool"
>        , CatCreateBinaryOp ">" "inet" "inet" "bool"
>        , CatCreateBinaryOp ">" "bpchar" "bpchar" "bool"
>        , CatCreateBinaryOp ">" "date" "date" "bool"
>        , CatCreateBinaryOp ">" "date" "timestamp" "bool"
>        , CatCreateBinaryOp ">" "date" "timestamptz" "bool"
>        , CatCreateBinaryOp ">" "time" "time" "bool"
>        , CatCreateBinaryOp ">" "timestamp" "date" "bool"
>        , CatCreateBinaryOp ">" "timestamp" "timestamp" "bool"
>        , CatCreateBinaryOp ">" "timestamp" "timestamptz" "bool"
>        , CatCreateBinaryOp ">" "timestamptz" "date" "bool"
>        , CatCreateBinaryOp ">" "timestamptz" "timestamp" "bool"
>        , CatCreateBinaryOp ">" "timestamptz" "timestamptz" "bool"
>        , CatCreateBinaryOp ">" "interval" "interval" "bool"
>        , CatCreateBinaryOp ">" "timetz" "timetz" "bool"
>        , CatCreateBinaryOp ">" "bit" "bit" "bool"
>        , CatCreateBinaryOp ">" "varbit" "varbit" "bool"
>        , CatCreateBinaryOp ">" "numeric" "numeric" "bool"
>        , CatCreateBinaryOp ">" "record" "record" "bool"
>        , CatCreateBinaryOp ">" "anyarray" "anyarray" "bool"
>        , CatCreateBinaryOp ">" "uuid" "uuid" "bool"
>        , CatCreateBinaryOp ">" "pg_lsn" "pg_lsn" "bool"
>        , CatCreateBinaryOp ">" "anyenum" "anyenum" "bool"
>        , CatCreateBinaryOp ">" "tsvector" "tsvector" "bool"
>        , CatCreateBinaryOp ">" "tsquery" "tsquery" "bool"
>        , CatCreateBinaryOp ">" "jsonb" "jsonb" "bool"
>        , CatCreateBinaryOp ">" "anyrange" "anyrange" "bool"
>        , CatCreateBinaryOp ">=" "bool" "bool" "bool"
>        , CatCreateBinaryOp ">=" "bytea" "bytea" "bool"
>        , CatCreateBinaryOp ">=" "char" "char" "bool"
>        , CatCreateBinaryOp ">=" "name" "name" "bool"
>        , CatCreateBinaryOp ">=" "int8" "int8" "bool"
>        , CatCreateBinaryOp ">=" "int8" "int2" "bool"
>        , CatCreateBinaryOp ">=" "int8" "int4" "bool"
>        , CatCreateBinaryOp ">=" "int2" "int8" "bool"
>        , CatCreateBinaryOp ">=" "int2" "int2" "bool"
>        , CatCreateBinaryOp ">=" "int2" "int4" "bool"
>        , CatCreateBinaryOp ">=" "int4" "int8" "bool"
>        , CatCreateBinaryOp ">=" "int4" "int2" "bool"
>        , CatCreateBinaryOp ">=" "int4" "int4" "bool"
>        , CatCreateBinaryOp ">=" "text" "text" "bool"
>        , CatCreateBinaryOp ">=" "oid" "oid" "bool"
>        , CatCreateBinaryOp ">=" "tid" "tid" "bool"
>        , CatCreateBinaryOp ">=" "oidvector" "oidvector" "bool"
>        , CatCreateBinaryOp ">=" "lseg" "lseg" "bool"
>        , CatCreateBinaryOp ">=" "path" "path" "bool"
>        , CatCreateBinaryOp ">=" "box" "box" "bool"
>        , CatCreateBinaryOp ">=" "float4" "float4" "bool"
>        , CatCreateBinaryOp ">=" "float4" "float8" "bool"
>        , CatCreateBinaryOp ">=" "float8" "float4" "bool"
>        , CatCreateBinaryOp ">=" "float8" "float8" "bool"
>        , CatCreateBinaryOp ">=" "abstime" "abstime" "bool"
>        , CatCreateBinaryOp ">=" "reltime" "reltime" "bool"
>        , CatCreateBinaryOp ">=" "tinterval" "tinterval" "bool"
>        , CatCreateBinaryOp ">=" "circle" "circle" "bool"
>        , CatCreateBinaryOp ">=" "money" "money" "bool"
>        , CatCreateBinaryOp ">=" "macaddr" "macaddr" "bool"
>        , CatCreateBinaryOp ">=" "inet" "inet" "bool"
>        , CatCreateBinaryOp ">=" "bpchar" "bpchar" "bool"
>        , CatCreateBinaryOp ">=" "date" "date" "bool"
>        , CatCreateBinaryOp ">=" "date" "timestamp" "bool"
>        , CatCreateBinaryOp ">=" "date" "timestamptz" "bool"
>        , CatCreateBinaryOp ">=" "time" "time" "bool"
>        , CatCreateBinaryOp ">=" "timestamp" "date" "bool"
>        , CatCreateBinaryOp ">=" "timestamp" "timestamp" "bool"
>        , CatCreateBinaryOp ">=" "timestamp" "timestamptz" "bool"
>        , CatCreateBinaryOp ">=" "timestamptz" "date" "bool"
>        , CatCreateBinaryOp ">=" "timestamptz" "timestamp" "bool"
>        , CatCreateBinaryOp ">=" "timestamptz" "timestamptz" "bool"
>        , CatCreateBinaryOp ">=" "interval" "interval" "bool"
>        , CatCreateBinaryOp ">=" "timetz" "timetz" "bool"
>        , CatCreateBinaryOp ">=" "bit" "bit" "bool"
>        , CatCreateBinaryOp ">=" "varbit" "varbit" "bool"
>        , CatCreateBinaryOp ">=" "numeric" "numeric" "bool"
>        , CatCreateBinaryOp ">=" "record" "record" "bool"
>        , CatCreateBinaryOp ">=" "anyarray" "anyarray" "bool"
>        , CatCreateBinaryOp ">=" "uuid" "uuid" "bool"
>        , CatCreateBinaryOp ">=" "pg_lsn" "pg_lsn" "bool"
>        , CatCreateBinaryOp ">=" "anyenum" "anyenum" "bool"
>        , CatCreateBinaryOp ">=" "tsvector" "tsvector" "bool"
>        , CatCreateBinaryOp ">=" "tsquery" "tsquery" "bool"
>        , CatCreateBinaryOp ">=" "jsonb" "jsonb" "bool"
>        , CatCreateBinaryOp ">=" "anyrange" "anyrange" "bool"
>        , CatCreateBinaryOp ">>" "int8" "int4" "int8"
>        , CatCreateBinaryOp ">>" "int2" "int4" "int2"
>        , CatCreateBinaryOp ">>" "int4" "int4" "int4"
>        , CatCreateBinaryOp ">>" "point" "point" "bool"
>        , CatCreateBinaryOp ">>" "box" "box" "bool"
>        , CatCreateBinaryOp ">>" "polygon" "polygon" "bool"
>        , CatCreateBinaryOp ">>" "circle" "circle" "bool"
>        , CatCreateBinaryOp ">>" "inet" "inet" "bool"
>        , CatCreateBinaryOp ">>" "bit" "int4" "bit"
>        , CatCreateBinaryOp ">>" "anyrange" "anyrange" "bool"
>        , CatCreateBinaryOp ">>=" "inet" "inet" "bool"
>        , CatCreateBinaryOp ">^" "point" "point" "bool"
>        , CatCreateBinaryOp ">^" "box" "box" "bool"
>        , CatCreateBinaryOp "?" "jsonb" "text" "bool"
>        , CatCreateBinaryOp "?#" "lseg" "lseg" "bool"
>        , CatCreateBinaryOp "?#" "lseg" "box" "bool"
>        , CatCreateBinaryOp "?#" "lseg" "line" "bool"
>        , CatCreateBinaryOp "?#" "path" "path" "bool"
>        , CatCreateBinaryOp "?#" "box" "box" "bool"
>        , CatCreateBinaryOp "?#" "line" "box" "bool"
>        , CatCreateBinaryOp "?#" "line" "line" "bool"
>        , CatCreateBinaryOp "?&" "jsonb" "_text" "bool"
>        , CatCreateBinaryOp "?-" "point" "point" "bool"
>        , CatCreateBinaryOp "?-|" "lseg" "lseg" "bool"
>        , CatCreateBinaryOp "?-|" "line" "line" "bool"
>        , CatCreateBinaryOp "?|" "point" "point" "bool"
>        , CatCreateBinaryOp "?|" "jsonb" "_text" "bool"
>        , CatCreateBinaryOp "?||" "lseg" "lseg" "bool"
>        , CatCreateBinaryOp "?||" "line" "line" "bool"
>        , CatCreateBinaryOp "@>" "path" "point" "bool"
>        , CatCreateBinaryOp "@>" "box" "point" "bool"
>        , CatCreateBinaryOp "@>" "box" "box" "bool"
>        , CatCreateBinaryOp "@>" "polygon" "point" "bool"
>        , CatCreateBinaryOp "@>" "polygon" "polygon" "bool"
>        , CatCreateBinaryOp "@>" "circle" "point" "bool"
>        , CatCreateBinaryOp "@>" "circle" "circle" "bool"
>        , CatCreateBinaryOp "@>" "_aclitem" "aclitem" "bool"
>        , CatCreateBinaryOp "@>" "anyarray" "anyarray" "bool"
>        , CatCreateBinaryOp "@>" "tsquery" "tsquery" "bool"
>        , CatCreateBinaryOp "@>" "jsonb" "jsonb" "bool"
>        , CatCreateBinaryOp "@>" "anyrange" "anyelement" "bool"
>        , CatCreateBinaryOp "@>" "anyrange" "anyrange" "bool"
>        , CatCreateBinaryOp "@@" "text" "text" "bool"
>        , CatCreateBinaryOp "@@" "text" "tsquery" "bool"
>        , CatCreateBinaryOp "@@" "tsvector" "tsquery" "bool"
>        , CatCreateBinaryOp "@@" "tsquery" "tsvector" "bool"
>        , CatCreateBinaryOp "@@@" "tsvector" "tsquery" "bool"
>        , CatCreateBinaryOp "@@@" "tsquery" "tsvector" "bool"
>        , CatCreateBinaryOp "^" "float8" "float8" "float8"
>        , CatCreateBinaryOp "^" "numeric" "numeric" "numeric"
>        , CatCreateBinaryOp "|" "int8" "int8" "int8"
>        , CatCreateBinaryOp "|" "int2" "int2" "int2"
>        , CatCreateBinaryOp "|" "int4" "int4" "int4"
>        , CatCreateBinaryOp "|" "macaddr" "macaddr" "macaddr"
>        , CatCreateBinaryOp "|" "inet" "inet" "inet"
>        , CatCreateBinaryOp "|" "bit" "bit" "bit"
>        , CatCreateBinaryOp "|&>" "box" "box" "bool"
>        , CatCreateBinaryOp "|&>" "polygon" "polygon" "bool"
>        , CatCreateBinaryOp "|&>" "circle" "circle" "bool"
>        , CatCreateBinaryOp "|>>" "box" "box" "bool"
>        , CatCreateBinaryOp "|>>" "polygon" "polygon" "bool"
>        , CatCreateBinaryOp "|>>" "circle" "circle" "bool"
>        , CatCreateBinaryOp "||" "bytea" "bytea" "bytea"
>        , CatCreateBinaryOp "||" "text" "text" "text"
>        , CatCreateBinaryOp "||" "text" "anynonarray" "text"
>        , CatCreateBinaryOp "||" "varbit" "varbit" "varbit"
>        , CatCreateBinaryOp "||" "anyarray" "anyarray" "anyarray"
>        , CatCreateBinaryOp "||" "anyarray" "anyelement" "anyarray"
>        , CatCreateBinaryOp "||" "anyelement" "anyarray" "anyarray"
>        , CatCreateBinaryOp "||" "anynonarray" "text" "text"
>        , CatCreateBinaryOp "||" "tsvector" "tsvector" "tsvector"
>        , CatCreateBinaryOp "||" "tsquery" "tsquery" "tsquery"
>        , CatCreateBinaryOp "~" "name" "text" "bool"
>        , CatCreateBinaryOp "~" "text" "text" "bool"
>        , CatCreateBinaryOp "~" "path" "point" "bool"
>        , CatCreateBinaryOp "~" "box" "box" "bool"
>        , CatCreateBinaryOp "~" "polygon" "point" "bool"
>        , CatCreateBinaryOp "~" "polygon" "polygon" "bool"
>        , CatCreateBinaryOp "~" "circle" "point" "bool"
>        , CatCreateBinaryOp "~" "circle" "circle" "bool"
>        , CatCreateBinaryOp "~" "_aclitem" "aclitem" "bool"
>        , CatCreateBinaryOp "~" "bpchar" "text" "bool"
>        , CatCreateBinaryOp "~*" "name" "text" "bool"
>        , CatCreateBinaryOp "~*" "text" "text" "bool"
>        , CatCreateBinaryOp "~*" "bpchar" "text" "bool"
>        , CatCreateBinaryOp "~<=~" "text" "text" "bool"
>        , CatCreateBinaryOp "~<=~" "bpchar" "bpchar" "bool"
>        , CatCreateBinaryOp "~<~" "text" "text" "bool"
>        , CatCreateBinaryOp "~<~" "bpchar" "bpchar" "bool"
>        , CatCreateBinaryOp "~=" "point" "point" "bool"
>        , CatCreateBinaryOp "~=" "box" "box" "bool"
>        , CatCreateBinaryOp "~=" "polygon" "polygon" "bool"
>        , CatCreateBinaryOp "~=" "tinterval" "tinterval" "bool"
>        , CatCreateBinaryOp "~=" "circle" "circle" "bool"
>        , CatCreateBinaryOp "~>=~" "text" "text" "bool"
>        , CatCreateBinaryOp "~>=~" "bpchar" "bpchar" "bool"
>        , CatCreateBinaryOp "~>~" "text" "text" "bool"
>        , CatCreateBinaryOp "~>~" "bpchar" "bpchar" "bool"
>        , CatCreateBinaryOp "~~" "bytea" "bytea" "bool"
>        , CatCreateBinaryOp "~~" "name" "text" "bool"
>        , CatCreateBinaryOp "~~" "text" "text" "bool"
>        , CatCreateBinaryOp "~~" "bpchar" "text" "bool"
>        , CatCreateBinaryOp "~~*" "name" "text" "bool"
>        , CatCreateBinaryOp "~~*" "text" "text" "bool"
>        , CatCreateBinaryOp "~~*" "bpchar" "text" "bool"
>        , CatCreateFunction "abbrev" [ "inet" ] False "text"
>        , CatCreateFunction "abbrev" [ "cidr" ] False "text"
>        , CatCreateFunction "abs" [ "float4" ] False "float4"
>        , CatCreateFunction "abs" [ "float8" ] False "float8"
>        , CatCreateFunction "abs" [ "int8" ] False "int8"
>        , CatCreateFunction "abs" [ "int4" ] False "int4"
>        , CatCreateFunction "abs" [ "int2" ] False "int2"
>        , CatCreateFunction "abs" [ "numeric" ] False "numeric"
>        , CatCreateFunction "abstime" [ "timestamptz" ] False "abstime"
>        , CatCreateFunction "abstime" [ "timestamp" ] False "abstime"
>        , CatCreateFunction
>            "abstimeeq" [ "abstime" , "abstime" ] False "bool"
>        , CatCreateFunction
>            "abstimege" [ "abstime" , "abstime" ] False "bool"
>        , CatCreateFunction
>            "abstimegt" [ "abstime" , "abstime" ] False "bool"
>        , CatCreateFunction "abstimein" [ "cstring" ] False "abstime"
>        , CatCreateFunction
>            "abstimele" [ "abstime" , "abstime" ] False "bool"
>        , CatCreateFunction
>            "abstimelt" [ "abstime" , "abstime" ] False "bool"
>        , CatCreateFunction
>            "abstimene" [ "abstime" , "abstime" ] False "bool"
>        , CatCreateFunction "abstimeout" [ "abstime" ] False "cstring"
>        , CatCreateFunction "abstimerecv" [ "internal" ] False "abstime"
>        , CatCreateFunction "abstimesend" [ "abstime" ] False "bytea"
>        , CatCreateFunction
>            "aclcontains" [ "aclitem" , "_aclitem" ] False "bool"
>        , CatCreateFunction
>            "acldefault" [ "char" , "oid" ] False "_aclitem"
>        , CatCreateFunction "aclexplode" [ "_aclitem" ] True "record"
>        , CatCreateFunction
>            "aclinsert" [ "aclitem" , "_aclitem" ] False "_aclitem"
>        , CatCreateFunction
>            "aclitemeq" [ "aclitem" , "aclitem" ] False "bool"
>        , CatCreateFunction "aclitemin" [ "cstring" ] False "aclitem"
>        , CatCreateFunction "aclitemout" [ "aclitem" ] False "cstring"
>        , CatCreateFunction
>            "aclremove" [ "aclitem" , "_aclitem" ] False "_aclitem"
>        , CatCreateFunction "acos" [ "float8" ] False "float8"
>        , CatCreateFunction "age" [ "xid" ] False "int4"
>        , CatCreateFunction
>            "age" [ "timestamptz" , "timestamptz" ] False "interval"
>        , CatCreateFunction "age" [ "timestamptz" ] False "interval"
>        , CatCreateFunction
>            "age" [ "timestamp" , "timestamp" ] False "interval"
>        , CatCreateFunction "age" [ "timestamp" ] False "interval"
>        , CatCreateFunction "any_in" [ "cstring" ] False "any"
>        , CatCreateFunction "any_out" [ "any" ] False "cstring"
>        , CatCreateFunction "anyarray_in" [ "cstring" ] False "anyarray"
>        , CatCreateFunction "anyarray_out" [ "anyarray" ] False "cstring"
>        , CatCreateFunction "anyarray_recv" [ "internal" ] False "anyarray"
>        , CatCreateFunction "anyarray_send" [ "anyarray" ] False "bytea"
>        , CatCreateFunction
>            "anyelement_in" [ "cstring" ] False "anyelement"
>        , CatCreateFunction
>            "anyelement_out" [ "anyelement" ] False "cstring"
>        , CatCreateFunction "anyenum_in" [ "cstring" ] False "anyenum"
>        , CatCreateFunction "anyenum_out" [ "anyenum" ] False "cstring"
>        , CatCreateFunction
>            "anynonarray_in" [ "cstring" ] False "anynonarray"
>        , CatCreateFunction
>            "anynonarray_out" [ "anynonarray" ] False "cstring"
>        , CatCreateFunction
>            "anyrange_in" [ "int4" , "cstring" , "oid" ] False "anyrange"
>        , CatCreateFunction "anyrange_out" [ "anyrange" ] False "cstring"
>        , CatCreateFunction
>            "anytextcat" [ "text" , "anynonarray" ] False "text"
>        , CatCreateFunction "area" [ "box" ] False "float8"
>        , CatCreateFunction "area" [ "path" ] False "float8"
>        , CatCreateFunction "area" [ "circle" ] False "float8"
>        , CatCreateFunction
>            "areajoinsel"
>            [ "internal" , "internal" , "oid" , "internal" , "int2" ]
>            False
>            "float8"
>        , CatCreateFunction
>            "areasel"
>            [ "internal" , "int4" , "oid" , "internal" ]
>            False
>            "float8"
>        , CatCreateFunction
>            "array_agg_finalfn" [ "anyelement" , "internal" ] False "anyarray"
>        , CatCreateFunction
>            "array_agg_transfn" [ "internal" , "anyelement" ] False "internal"
>        , CatCreateFunction
>            "array_append" [ "anyelement" , "anyarray" ] False "anyarray"
>        , CatCreateFunction
>            "array_cat" [ "anyarray" , "anyarray" ] False "anyarray"
>        , CatCreateFunction "array_dims" [ "anyarray" ] False "text"
>        , CatCreateFunction
>            "array_eq" [ "anyarray" , "anyarray" ] False "bool"
>        , CatCreateFunction
>            "array_fill" [ "_int4" , "anyelement" ] False "anyarray"
>        , CatCreateFunction
>            "array_fill" [ "_int4" , "anyelement" , "_int4" ] False "anyarray"
>        , CatCreateFunction
>            "array_ge" [ "anyarray" , "anyarray" ] False "bool"
>        , CatCreateFunction
>            "array_gt" [ "anyarray" , "anyarray" ] False "bool"
>        , CatCreateFunction
>            "array_in" [ "int4" , "cstring" , "oid" ] False "anyarray"
>        , CatCreateFunction
>            "array_larger" [ "anyarray" , "anyarray" ] False "anyarray"
>        , CatCreateFunction
>            "array_le" [ "anyarray" , "anyarray" ] False "bool"
>        , CatCreateFunction
>            "array_length" [ "int4" , "anyarray" ] False "int4"
>        , CatCreateFunction
>            "array_lower" [ "anyarray" , "int4" ] False "int4"
>        , CatCreateFunction
>            "array_lt" [ "anyarray" , "anyarray" ] False "bool"
>        , CatCreateFunction "array_ndims" [ "anyarray" ] False "int4"
>        , CatCreateFunction
>            "array_ne" [ "anyarray" , "anyarray" ] False "bool"
>        , CatCreateFunction "array_out" [ "anyarray" ] False "cstring"
>        , CatCreateFunction
>            "array_prepend" [ "anyelement" , "anyarray" ] False "anyarray"
>        , CatCreateFunction
>            "array_recv" [ "int4" , "internal" , "oid" ] False "anyarray"
>        , CatCreateFunction
>            "array_remove" [ "anyarray" , "anyelement" ] False "anyarray"
>        , CatCreateFunction
>            "array_replace"
>            [ "anyelement" , "anyelement" , "anyarray" ]
>            False
>            "anyarray"
>        , CatCreateFunction "array_send" [ "anyarray" ] False "bytea"
>        , CatCreateFunction
>            "array_smaller" [ "anyarray" , "anyarray" ] False "anyarray"
>        , CatCreateFunction "array_to_json" [ "anyarray" ] False "json"
>        , CatCreateFunction
>            "array_to_json" [ "bool" , "anyarray" ] False "json"
>        , CatCreateFunction
>            "array_to_string" [ "text" , "anyarray" , "text" ] False "text"
>        , CatCreateFunction
>            "array_to_string" [ "text" , "anyarray" ] False "text"
>        , CatCreateFunction "array_typanalyze" [ "internal" ] False "bool"
>        , CatCreateFunction
>            "array_upper" [ "anyarray" , "int4" ] False "int4"
>        , CatCreateFunction
>            "arraycontained" [ "anyarray" , "anyarray" ] False "bool"
>        , CatCreateFunction
>            "arraycontains" [ "anyarray" , "anyarray" ] False "bool"
>        , CatCreateFunction
>            "arraycontjoinsel"
>            [ "internal" , "internal" , "oid" , "internal" , "int2" ]
>            False
>            "float8"
>        , CatCreateFunction
>            "arraycontsel"
>            [ "int4" , "internal" , "internal" , "oid" ]
>            False
>            "float8"
>        , CatCreateFunction
>            "arrayoverlap" [ "anyarray" , "anyarray" ] False "bool"
>        , CatCreateFunction "ascii" [ "text" ] False "int4"
>        , CatCreateFunction
>            "ascii_to_mic"
>            [ "int4" , "int4" , "int4" , "cstring" , "internal" ]
>            False
>            "void"
>        , CatCreateFunction
>            "ascii_to_utf8"
>            [ "int4" , "cstring" , "int4" , "int4" , "internal" ]
>            False
>            "void"
>        , CatCreateFunction "asin" [ "float8" ] False "float8"
>        , CatCreateFunction "atan" [ "float8" ] False "float8"
>        , CatCreateFunction "atan2" [ "float8" , "float8" ] False "float8"
>        , CatCreateFunction
>            "big5_to_euc_tw"
>            [ "int4" , "int4" , "int4" , "internal" , "cstring" ]
>            False
>            "void"
>        , CatCreateFunction
>            "big5_to_mic"
>            [ "int4" , "cstring" , "internal" , "int4" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "big5_to_utf8"
>            [ "int4" , "int4" , "cstring" , "int4" , "internal" ]
>            False
>            "void"
>        , CatCreateFunction "bit" [ "int4" , "int4" ] False "bit"
>        , CatCreateFunction "bit" [ "int4" , "bit" , "bool" ] False "bit"
>        , CatCreateFunction "bit" [ "int8" , "int4" ] False "bit"
>        , CatCreateFunction
>            "bit_in" [ "int4" , "cstring" , "oid" ] False "bit"
>        , CatCreateFunction "bit_length" [ "bytea" ] False "int4"
>        , CatCreateFunction "bit_length" [ "text" ] False "int4"
>        , CatCreateFunction "bit_length" [ "bit" ] False "int4"
>        , CatCreateFunction "bit_out" [ "bit" ] False "cstring"
>        , CatCreateFunction
>            "bit_recv" [ "internal" , "oid" , "int4" ] False "bit"
>        , CatCreateFunction "bit_send" [ "bit" ] False "bytea"
>        , CatCreateFunction "bitand" [ "bit" , "bit" ] False "bit"
>        , CatCreateFunction "bitcat" [ "varbit" , "varbit" ] False "varbit"
>        , CatCreateFunction "bitcmp" [ "bit" , "bit" ] False "int4"
>        , CatCreateFunction "biteq" [ "bit" , "bit" ] False "bool"
>        , CatCreateFunction "bitge" [ "bit" , "bit" ] False "bool"
>        , CatCreateFunction "bitgt" [ "bit" , "bit" ] False "bool"
>        , CatCreateFunction "bitle" [ "bit" , "bit" ] False "bool"
>        , CatCreateFunction "bitlt" [ "bit" , "bit" ] False "bool"
>        , CatCreateFunction "bitne" [ "bit" , "bit" ] False "bool"
>        , CatCreateFunction "bitnot" [ "bit" ] False "bit"
>        , CatCreateFunction "bitor" [ "bit" , "bit" ] False "bit"
>        , CatCreateFunction "bitshiftleft" [ "int4" , "bit" ] False "bit"
>        , CatCreateFunction "bitshiftright" [ "bit" , "int4" ] False "bit"
>        , CatCreateFunction "bittypmodin" [ "_cstring" ] False "int4"
>        , CatCreateFunction "bittypmodout" [ "int4" ] False "cstring"
>        , CatCreateFunction "bitxor" [ "bit" , "bit" ] False "bit"
>        , CatCreateFunction "bool" [ "int4" ] False "bool"
>        , CatCreateFunction
>            "bool_accum" [ "bool" , "internal" ] False "internal"
>        , CatCreateFunction
>            "bool_accum_inv" [ "bool" , "internal" ] False "internal"
>        , CatCreateFunction "bool_alltrue" [ "internal" ] False "bool"
>        , CatCreateFunction "bool_anytrue" [ "internal" ] False "bool"
>        , CatCreateFunction
>            "booland_statefunc" [ "bool" , "bool" ] False "bool"
>        , CatCreateFunction "booleq" [ "bool" , "bool" ] False "bool"
>        , CatCreateFunction "boolge" [ "bool" , "bool" ] False "bool"
>        , CatCreateFunction "boolgt" [ "bool" , "bool" ] False "bool"
>        , CatCreateFunction "boolin" [ "cstring" ] False "bool"
>        , CatCreateFunction "boolle" [ "bool" , "bool" ] False "bool"
>        , CatCreateFunction "boollt" [ "bool" , "bool" ] False "bool"
>        , CatCreateFunction "boolne" [ "bool" , "bool" ] False "bool"
>        , CatCreateFunction
>            "boolor_statefunc" [ "bool" , "bool" ] False "bool"
>        , CatCreateFunction "boolout" [ "bool" ] False "cstring"
>        , CatCreateFunction "boolrecv" [ "internal" ] False "bool"
>        , CatCreateFunction "boolsend" [ "bool" ] False "bytea"
>        , CatCreateFunction "box" [ "point" , "point" ] False "box"
>        , CatCreateFunction "box" [ "polygon" ] False "box"
>        , CatCreateFunction "box" [ "circle" ] False "box"
>        , CatCreateFunction "box_above" [ "box" , "box" ] False "bool"
>        , CatCreateFunction "box_above_eq" [ "box" , "box" ] False "bool"
>        , CatCreateFunction "box_add" [ "point" , "box" ] False "box"
>        , CatCreateFunction "box_below" [ "box" , "box" ] False "bool"
>        , CatCreateFunction "box_below_eq" [ "box" , "box" ] False "bool"
>        , CatCreateFunction "box_center" [ "box" ] False "point"
>        , CatCreateFunction "box_contain" [ "box" , "box" ] False "bool"
>        , CatCreateFunction
>            "box_contain_pt" [ "point" , "box" ] False "bool"
>        , CatCreateFunction "box_contained" [ "box" , "box" ] False "bool"
>        , CatCreateFunction "box_distance" [ "box" , "box" ] False "float8"
>        , CatCreateFunction "box_div" [ "point" , "box" ] False "box"
>        , CatCreateFunction "box_eq" [ "box" , "box" ] False "bool"
>        , CatCreateFunction "box_ge" [ "box" , "box" ] False "bool"
>        , CatCreateFunction "box_gt" [ "box" , "box" ] False "bool"
>        , CatCreateFunction "box_in" [ "cstring" ] False "box"
>        , CatCreateFunction "box_intersect" [ "box" , "box" ] False "box"
>        , CatCreateFunction "box_le" [ "box" , "box" ] False "bool"
>        , CatCreateFunction "box_left" [ "box" , "box" ] False "bool"
>        , CatCreateFunction "box_lt" [ "box" , "box" ] False "bool"
>        , CatCreateFunction "box_mul" [ "point" , "box" ] False "box"
>        , CatCreateFunction "box_out" [ "box" ] False "cstring"
>        , CatCreateFunction "box_overabove" [ "box" , "box" ] False "bool"
>        , CatCreateFunction "box_overbelow" [ "box" , "box" ] False "bool"
>        , CatCreateFunction "box_overlap" [ "box" , "box" ] False "bool"
>        , CatCreateFunction "box_overleft" [ "box" , "box" ] False "bool"
>        , CatCreateFunction "box_overright" [ "box" , "box" ] False "bool"
>        , CatCreateFunction "box_recv" [ "internal" ] False "box"
>        , CatCreateFunction "box_right" [ "box" , "box" ] False "bool"
>        , CatCreateFunction "box_same" [ "box" , "box" ] False "bool"
>        , CatCreateFunction "box_send" [ "box" ] False "bytea"
>        , CatCreateFunction "box_sub" [ "point" , "box" ] False "box"
>        , CatCreateFunction "bpchar" [ "name" ] False "bpchar"
>        , CatCreateFunction
>            "bpchar" [ "bpchar" , "bool" , "int4" ] False "bpchar"
>        , CatCreateFunction "bpchar" [ "char" ] False "bpchar"
>        , CatCreateFunction
>            "bpchar_larger" [ "bpchar" , "bpchar" ] False "bpchar"
>        , CatCreateFunction
>            "bpchar_pattern_ge" [ "bpchar" , "bpchar" ] False "bool"
>        , CatCreateFunction
>            "bpchar_pattern_gt" [ "bpchar" , "bpchar" ] False "bool"
>        , CatCreateFunction
>            "bpchar_pattern_le" [ "bpchar" , "bpchar" ] False "bool"
>        , CatCreateFunction
>            "bpchar_pattern_lt" [ "bpchar" , "bpchar" ] False "bool"
>        , CatCreateFunction
>            "bpchar_smaller" [ "bpchar" , "bpchar" ] False "bpchar"
>        , CatCreateFunction
>            "bpcharcmp" [ "bpchar" , "bpchar" ] False "int4"
>        , CatCreateFunction "bpchareq" [ "bpchar" , "bpchar" ] False "bool"
>        , CatCreateFunction "bpcharge" [ "bpchar" , "bpchar" ] False "bool"
>        , CatCreateFunction "bpchargt" [ "bpchar" , "bpchar" ] False "bool"
>        , CatCreateFunction
>            "bpchariclike" [ "bpchar" , "text" ] False "bool"
>        , CatCreateFunction
>            "bpcharicnlike" [ "bpchar" , "text" ] False "bool"
>        , CatCreateFunction
>            "bpcharicregexeq" [ "text" , "bpchar" ] False "bool"
>        , CatCreateFunction
>            "bpcharicregexne" [ "bpchar" , "text" ] False "bool"
>        , CatCreateFunction
>            "bpcharin" [ "cstring" , "oid" , "int4" ] False "bpchar"
>        , CatCreateFunction "bpcharle" [ "bpchar" , "bpchar" ] False "bool"
>        , CatCreateFunction "bpcharlike" [ "text" , "bpchar" ] False "bool"
>        , CatCreateFunction "bpcharlt" [ "bpchar" , "bpchar" ] False "bool"
>        , CatCreateFunction "bpcharne" [ "bpchar" , "bpchar" ] False "bool"
>        , CatCreateFunction
>            "bpcharnlike" [ "text" , "bpchar" ] False "bool"
>        , CatCreateFunction "bpcharout" [ "bpchar" ] False "cstring"
>        , CatCreateFunction
>            "bpcharrecv" [ "int4" , "oid" , "internal" ] False "bpchar"
>        , CatCreateFunction
>            "bpcharregexeq" [ "text" , "bpchar" ] False "bool"
>        , CatCreateFunction
>            "bpcharregexne" [ "text" , "bpchar" ] False "bool"
>        , CatCreateFunction "bpcharsend" [ "bpchar" ] False "bytea"
>        , CatCreateFunction "bpchartypmodin" [ "_cstring" ] False "int4"
>        , CatCreateFunction "bpchartypmodout" [ "int4" ] False "cstring"
>        , CatCreateFunction "broadcast" [ "inet" ] False "inet"
>        , CatCreateFunction
>            "btabstimecmp" [ "abstime" , "abstime" ] False "int4"
>        , CatCreateFunction
>            "btarraycmp" [ "anyarray" , "anyarray" ] False "int4"
>        , CatCreateFunction
>            "btbeginscan"
>            [ "internal" , "internal" , "internal" ]
>            False
>            "internal"
>        , CatCreateFunction "btboolcmp" [ "bool" , "bool" ] False "int4"
>        , CatCreateFunction
>            "btbpchar_pattern_cmp" [ "bpchar" , "bpchar" ] False "int4"
>        , CatCreateFunction
>            "btbuild" [ "internal" , "internal" , "internal" ] False "internal"
>        , CatCreateFunction "btbuildempty" [ "internal" ] False "void"
>        , CatCreateFunction
>            "btbulkdelete"
>            [ "internal" , "internal" , "internal" , "internal" ]
>            False
>            "internal"
>        , CatCreateFunction "btcanreturn" [ "internal" ] False "bool"
>        , CatCreateFunction "btcharcmp" [ "char" , "char" ] False "int4"
>        , CatCreateFunction
>            "btcostestimate"
>            [ "internal"
>            , "internal"
>            , "internal"
>            , "internal"
>            , "internal"
>            , "internal"
>            , "internal"
>            ]
>            False
>            "void"
>        , CatCreateFunction "btendscan" [ "internal" ] False "void"
>        , CatCreateFunction
>            "btfloat48cmp" [ "float8" , "float4" ] False "int4"
>        , CatCreateFunction
>            "btfloat4cmp" [ "float4" , "float4" ] False "int4"
>        , CatCreateFunction
>            "btfloat4sortsupport" [ "internal" ] False "void"
>        , CatCreateFunction
>            "btfloat84cmp" [ "float4" , "float8" ] False "int4"
>        , CatCreateFunction
>            "btfloat8cmp" [ "float8" , "float8" ] False "int4"
>        , CatCreateFunction
>            "btfloat8sortsupport" [ "internal" ] False "void"
>        , CatCreateFunction
>            "btgetbitmap" [ "internal" , "internal" ] False "int8"
>        , CatCreateFunction
>            "btgettuple" [ "internal" , "internal" ] False "bool"
>        , CatCreateFunction
>            "btinsert"
>            [ "internal"
>            , "internal"
>            , "internal"
>            , "internal"
>            , "internal"
>            , "internal"
>            ]
>            False
>            "bool"
>        , CatCreateFunction "btint24cmp" [ "int4" , "int2" ] False "int4"
>        , CatCreateFunction "btint28cmp" [ "int2" , "int8" ] False "int4"
>        , CatCreateFunction "btint2cmp" [ "int2" , "int2" ] False "int4"
>        , CatCreateFunction "btint2sortsupport" [ "internal" ] False "void"
>        , CatCreateFunction "btint42cmp" [ "int2" , "int4" ] False "int4"
>        , CatCreateFunction "btint48cmp" [ "int4" , "int8" ] False "int4"
>        , CatCreateFunction "btint4cmp" [ "int4" , "int4" ] False "int4"
>        , CatCreateFunction "btint4sortsupport" [ "internal" ] False "void"
>        , CatCreateFunction "btint82cmp" [ "int8" , "int2" ] False "int4"
>        , CatCreateFunction "btint84cmp" [ "int8" , "int4" ] False "int4"
>        , CatCreateFunction "btint8cmp" [ "int8" , "int8" ] False "int4"
>        , CatCreateFunction "btint8sortsupport" [ "internal" ] False "void"
>        , CatCreateFunction "btmarkpos" [ "internal" ] False "void"
>        , CatCreateFunction "btnamecmp" [ "name" , "name" ] False "int4"
>        , CatCreateFunction "btnamesortsupport" [ "internal" ] False "void"
>        , CatCreateFunction "btoidcmp" [ "oid" , "oid" ] False "int4"
>        , CatCreateFunction "btoidsortsupport" [ "internal" ] False "void"
>        , CatCreateFunction
>            "btoidvectorcmp" [ "oidvector" , "oidvector" ] False "int4"
>        , CatCreateFunction "btoptions" [ "bool" , "_text" ] False "bytea"
>        , CatCreateFunction
>            "btrecordcmp" [ "record" , "record" ] False "int4"
>        , CatCreateFunction
>            "btrecordimagecmp" [ "record" , "record" ] False "int4"
>        , CatCreateFunction
>            "btreltimecmp" [ "reltime" , "reltime" ] False "int4"
>        , CatCreateFunction
>            "btrescan"
>            [ "internal" , "internal" , "internal" , "internal" , "internal" ]
>            False
>            "void"
>        , CatCreateFunction "btrestrpos" [ "internal" ] False "void"
>        , CatCreateFunction "btrim" [ "text" , "text" ] False "text"
>        , CatCreateFunction "btrim" [ "text" ] False "text"
>        , CatCreateFunction "btrim" [ "bytea" , "bytea" ] False "bytea"
>        , CatCreateFunction
>            "bttext_pattern_cmp" [ "text" , "text" ] False "int4"
>        , CatCreateFunction "bttextcmp" [ "text" , "text" ] False "int4"
>        , CatCreateFunction "bttidcmp" [ "tid" , "tid" ] False "int4"
>        , CatCreateFunction
>            "bttintervalcmp" [ "tinterval" , "tinterval" ] False "int4"
>        , CatCreateFunction
>            "btvacuumcleanup" [ "internal" , "internal" ] False "internal"
>        , CatCreateFunction
>            "bytea_string_agg_finalfn" [ "internal" ] False "bytea"
>        , CatCreateFunction
>            "bytea_string_agg_transfn"
>            [ "internal" , "bytea" , "bytea" ]
>            False
>            "internal"
>        , CatCreateFunction "byteacat" [ "bytea" , "bytea" ] False "bytea"
>        , CatCreateFunction "byteacmp" [ "bytea" , "bytea" ] False "int4"
>        , CatCreateFunction "byteaeq" [ "bytea" , "bytea" ] False "bool"
>        , CatCreateFunction "byteage" [ "bytea" , "bytea" ] False "bool"
>        , CatCreateFunction "byteagt" [ "bytea" , "bytea" ] False "bool"
>        , CatCreateFunction "byteain" [ "cstring" ] False "bytea"
>        , CatCreateFunction "byteale" [ "bytea" , "bytea" ] False "bool"
>        , CatCreateFunction "bytealike" [ "bytea" , "bytea" ] False "bool"
>        , CatCreateFunction "bytealt" [ "bytea" , "bytea" ] False "bool"
>        , CatCreateFunction "byteane" [ "bytea" , "bytea" ] False "bool"
>        , CatCreateFunction "byteanlike" [ "bytea" , "bytea" ] False "bool"
>        , CatCreateFunction "byteaout" [ "bytea" ] False "cstring"
>        , CatCreateFunction "bytearecv" [ "internal" ] False "bytea"
>        , CatCreateFunction "byteasend" [ "bytea" ] False "bytea"
>        , CatCreateFunction "cardinality" [ "anyarray" ] False "int4"
>        , CatCreateFunction "cash_cmp" [ "money" , "money" ] False "int4"
>        , CatCreateFunction
>            "cash_div_cash" [ "money" , "money" ] False "float8"
>        , CatCreateFunction
>            "cash_div_flt4" [ "float4" , "money" ] False "money"
>        , CatCreateFunction
>            "cash_div_flt8" [ "money" , "float8" ] False "money"
>        , CatCreateFunction
>            "cash_div_int2" [ "money" , "int2" ] False "money"
>        , CatCreateFunction
>            "cash_div_int4" [ "int4" , "money" ] False "money"
>        , CatCreateFunction "cash_eq" [ "money" , "money" ] False "bool"
>        , CatCreateFunction "cash_ge" [ "money" , "money" ] False "bool"
>        , CatCreateFunction "cash_gt" [ "money" , "money" ] False "bool"
>        , CatCreateFunction "cash_in" [ "cstring" ] False "money"
>        , CatCreateFunction "cash_le" [ "money" , "money" ] False "bool"
>        , CatCreateFunction "cash_lt" [ "money" , "money" ] False "bool"
>        , CatCreateFunction "cash_mi" [ "money" , "money" ] False "money"
>        , CatCreateFunction
>            "cash_mul_flt4" [ "float4" , "money" ] False "money"
>        , CatCreateFunction
>            "cash_mul_flt8" [ "money" , "float8" ] False "money"
>        , CatCreateFunction
>            "cash_mul_int2" [ "money" , "int2" ] False "money"
>        , CatCreateFunction
>            "cash_mul_int4" [ "int4" , "money" ] False "money"
>        , CatCreateFunction "cash_ne" [ "money" , "money" ] False "bool"
>        , CatCreateFunction "cash_out" [ "money" ] False "cstring"
>        , CatCreateFunction "cash_pl" [ "money" , "money" ] False "money"
>        , CatCreateFunction "cash_recv" [ "internal" ] False "money"
>        , CatCreateFunction "cash_send" [ "money" ] False "bytea"
>        , CatCreateFunction "cash_words" [ "money" ] False "text"
>        , CatCreateFunction
>            "cashlarger" [ "money" , "money" ] False "money"
>        , CatCreateFunction
>            "cashsmaller" [ "money" , "money" ] False "money"
>        , CatCreateFunction "cbrt" [ "float8" ] False "float8"
>        , CatCreateFunction "ceil" [ "numeric" ] False "numeric"
>        , CatCreateFunction "ceil" [ "float8" ] False "float8"
>        , CatCreateFunction "ceiling" [ "numeric" ] False "numeric"
>        , CatCreateFunction "ceiling" [ "float8" ] False "float8"
>        , CatCreateFunction "center" [ "box" ] False "point"
>        , CatCreateFunction "center" [ "circle" ] False "point"
>        , CatCreateFunction "char" [ "int4" ] False "char"
>        , CatCreateFunction "char" [ "text" ] False "char"
>        , CatCreateFunction "char_length" [ "bpchar" ] False "int4"
>        , CatCreateFunction "char_length" [ "text" ] False "int4"
>        , CatCreateFunction "character_length" [ "bpchar" ] False "int4"
>        , CatCreateFunction "character_length" [ "text" ] False "int4"
>        , CatCreateFunction "chareq" [ "char" , "char" ] False "bool"
>        , CatCreateFunction "charge" [ "char" , "char" ] False "bool"
>        , CatCreateFunction "chargt" [ "char" , "char" ] False "bool"
>        , CatCreateFunction "charin" [ "cstring" ] False "char"
>        , CatCreateFunction "charle" [ "char" , "char" ] False "bool"
>        , CatCreateFunction "charlt" [ "char" , "char" ] False "bool"
>        , CatCreateFunction "charne" [ "char" , "char" ] False "bool"
>        , CatCreateFunction "charout" [ "char" ] False "cstring"
>        , CatCreateFunction "charrecv" [ "internal" ] False "char"
>        , CatCreateFunction "charsend" [ "char" ] False "bytea"
>        , CatCreateFunction "chr" [ "int4" ] False "text"
>        , CatCreateFunction "cideq" [ "cid" , "cid" ] False "bool"
>        , CatCreateFunction "cidin" [ "cstring" ] False "cid"
>        , CatCreateFunction "cidout" [ "cid" ] False "cstring"
>        , CatCreateFunction "cidr" [ "inet" ] False "cidr"
>        , CatCreateFunction "cidr_in" [ "cstring" ] False "cidr"
>        , CatCreateFunction "cidr_out" [ "cidr" ] False "cstring"
>        , CatCreateFunction "cidr_recv" [ "internal" ] False "cidr"
>        , CatCreateFunction "cidr_send" [ "cidr" ] False "bytea"
>        , CatCreateFunction "cidrecv" [ "internal" ] False "cid"
>        , CatCreateFunction "cidsend" [ "cid" ] False "bytea"
>        , CatCreateFunction "circle" [ "point" , "float8" ] False "circle"
>        , CatCreateFunction "circle" [ "polygon" ] False "circle"
>        , CatCreateFunction "circle" [ "box" ] False "circle"
>        , CatCreateFunction
>            "circle_above" [ "circle" , "circle" ] False "bool"
>        , CatCreateFunction
>            "circle_add_pt" [ "circle" , "point" ] False "circle"
>        , CatCreateFunction
>            "circle_below" [ "circle" , "circle" ] False "bool"
>        , CatCreateFunction "circle_center" [ "circle" ] False "point"
>        , CatCreateFunction
>            "circle_contain" [ "circle" , "circle" ] False "bool"
>        , CatCreateFunction
>            "circle_contain_pt" [ "circle" , "point" ] False "bool"
>        , CatCreateFunction
>            "circle_contained" [ "circle" , "circle" ] False "bool"
>        , CatCreateFunction
>            "circle_distance" [ "circle" , "circle" ] False "float8"
>        , CatCreateFunction
>            "circle_div_pt" [ "circle" , "point" ] False "circle"
>        , CatCreateFunction
>            "circle_eq" [ "circle" , "circle" ] False "bool"
>        , CatCreateFunction
>            "circle_ge" [ "circle" , "circle" ] False "bool"
>        , CatCreateFunction
>            "circle_gt" [ "circle" , "circle" ] False "bool"
>        , CatCreateFunction "circle_in" [ "cstring" ] False "circle"
>        , CatCreateFunction
>            "circle_le" [ "circle" , "circle" ] False "bool"
>        , CatCreateFunction
>            "circle_left" [ "circle" , "circle" ] False "bool"
>        , CatCreateFunction
>            "circle_lt" [ "circle" , "circle" ] False "bool"
>        , CatCreateFunction
>            "circle_mul_pt" [ "point" , "circle" ] False "circle"
>        , CatCreateFunction
>            "circle_ne" [ "circle" , "circle" ] False "bool"
>        , CatCreateFunction "circle_out" [ "circle" ] False "cstring"
>        , CatCreateFunction
>            "circle_overabove" [ "circle" , "circle" ] False "bool"
>        , CatCreateFunction
>            "circle_overbelow" [ "circle" , "circle" ] False "bool"
>        , CatCreateFunction
>            "circle_overlap" [ "circle" , "circle" ] False "bool"
>        , CatCreateFunction
>            "circle_overleft" [ "circle" , "circle" ] False "bool"
>        , CatCreateFunction
>            "circle_overright" [ "circle" , "circle" ] False "bool"
>        , CatCreateFunction "circle_recv" [ "internal" ] False "circle"
>        , CatCreateFunction
>            "circle_right" [ "circle" , "circle" ] False "bool"
>        , CatCreateFunction
>            "circle_same" [ "circle" , "circle" ] False "bool"
>        , CatCreateFunction "circle_send" [ "circle" ] False "bytea"
>        , CatCreateFunction
>            "circle_sub_pt" [ "point" , "circle" ] False "circle"
>        , CatCreateFunction "close_lb" [ "line" , "box" ] False "point"
>        , CatCreateFunction "close_ls" [ "lseg" , "line" ] False "point"
>        , CatCreateFunction "close_lseg" [ "lseg" , "lseg" ] False "point"
>        , CatCreateFunction "close_pb" [ "box" , "point" ] False "point"
>        , CatCreateFunction "close_pl" [ "point" , "line" ] False "point"
>        , CatCreateFunction "close_ps" [ "point" , "lseg" ] False "point"
>        , CatCreateFunction "close_sb" [ "lseg" , "box" ] False "point"
>        , CatCreateFunction "close_sl" [ "lseg" , "line" ] False "point"
>        , CatCreateFunction
>            "col_description" [ "int4" , "oid" ] False "text"
>        , CatCreateFunction
>            "contjoinsel"
>            [ "internal" , "int2" , "internal" , "oid" , "internal" ]
>            False
>            "float8"
>        , CatCreateFunction
>            "contsel"
>            [ "int4" , "internal" , "oid" , "internal" ]
>            False
>            "float8"
>        , CatCreateFunction
>            "convert" [ "name" , "bytea" , "name" ] False "bytea"
>        , CatCreateFunction
>            "convert_from" [ "name" , "bytea" ] False "text"
>        , CatCreateFunction "convert_to" [ "text" , "name" ] False "bytea"
>        , CatCreateFunction "cos" [ "float8" ] False "float8"
>        , CatCreateFunction "cot" [ "float8" ] False "float8"
>        , CatCreateFunction "cstring_in" [ "cstring" ] False "cstring"
>        , CatCreateFunction "cstring_out" [ "cstring" ] False "cstring"
>        , CatCreateFunction "cstring_recv" [ "internal" ] False "cstring"
>        , CatCreateFunction "cstring_send" [ "cstring" ] False "bytea"
>        , CatCreateFunction "current_schemas" [ "bool" ] False "_name"
>        , CatCreateFunction "current_setting" [ "text" ] False "text"
>        , CatCreateFunction "currtid" [ "tid" , "oid" ] False "tid"
>        , CatCreateFunction "currtid2" [ "tid" , "text" ] False "tid"
>        , CatCreateFunction "currval" [ "regclass" ] False "int8"
>        , CatCreateFunction
>            "cursor_to_xml"
>            [ "bool" , "int4" , "refcursor" , "bool" , "text" ]
>            False
>            "xml"
>        , CatCreateFunction
>            "cursor_to_xmlschema"
>            [ "text" , "bool" , "bool" , "refcursor" ]
>            False
>            "xml"
>        , CatCreateFunction
>            "database_to_xml" [ "bool" , "bool" , "text" ] False "xml"
>        , CatCreateFunction
>            "database_to_xml_and_xmlschema"
>            [ "bool" , "text" , "bool" ]
>            False
>            "xml"
>        , CatCreateFunction
>            "database_to_xmlschema" [ "text" , "bool" , "bool" ] False "xml"
>        , CatCreateFunction "date" [ "timestamptz" ] False "date"
>        , CatCreateFunction "date" [ "abstime" ] False "date"
>        , CatCreateFunction "date" [ "timestamp" ] False "date"
>        , CatCreateFunction "date_cmp" [ "date" , "date" ] False "int4"
>        , CatCreateFunction
>            "date_cmp_timestamp" [ "timestamp" , "date" ] False "int4"
>        , CatCreateFunction
>            "date_cmp_timestamptz" [ "date" , "timestamptz" ] False "int4"
>        , CatCreateFunction "date_eq" [ "date" , "date" ] False "bool"
>        , CatCreateFunction
>            "date_eq_timestamp" [ "date" , "timestamp" ] False "bool"
>        , CatCreateFunction
>            "date_eq_timestamptz" [ "timestamptz" , "date" ] False "bool"
>        , CatCreateFunction "date_ge" [ "date" , "date" ] False "bool"
>        , CatCreateFunction
>            "date_ge_timestamp" [ "date" , "timestamp" ] False "bool"
>        , CatCreateFunction
>            "date_ge_timestamptz" [ "timestamptz" , "date" ] False "bool"
>        , CatCreateFunction "date_gt" [ "date" , "date" ] False "bool"
>        , CatCreateFunction
>            "date_gt_timestamp" [ "date" , "timestamp" ] False "bool"
>        , CatCreateFunction
>            "date_gt_timestamptz" [ "timestamptz" , "date" ] False "bool"
>        , CatCreateFunction "date_in" [ "cstring" ] False "date"
>        , CatCreateFunction "date_larger" [ "date" , "date" ] False "date"
>        , CatCreateFunction "date_le" [ "date" , "date" ] False "bool"
>        , CatCreateFunction
>            "date_le_timestamp" [ "timestamp" , "date" ] False "bool"
>        , CatCreateFunction
>            "date_le_timestamptz" [ "date" , "timestamptz" ] False "bool"
>        , CatCreateFunction "date_lt" [ "date" , "date" ] False "bool"
>        , CatCreateFunction
>            "date_lt_timestamp" [ "date" , "timestamp" ] False "bool"
>        , CatCreateFunction
>            "date_lt_timestamptz" [ "date" , "timestamptz" ] False "bool"
>        , CatCreateFunction "date_mi" [ "date" , "date" ] False "int4"
>        , CatCreateFunction
>            "date_mi_interval" [ "interval" , "date" ] False "timestamp"
>        , CatCreateFunction "date_mii" [ "int4" , "date" ] False "date"
>        , CatCreateFunction "date_ne" [ "date" , "date" ] False "bool"
>        , CatCreateFunction
>            "date_ne_timestamp" [ "date" , "timestamp" ] False "bool"
>        , CatCreateFunction
>            "date_ne_timestamptz" [ "date" , "timestamptz" ] False "bool"
>        , CatCreateFunction "date_out" [ "date" ] False "cstring"
>        , CatCreateFunction
>            "date_part" [ "timestamptz" , "text" ] False "float8"
>        , CatCreateFunction
>            "date_part" [ "interval" , "text" ] False "float8"
>        , CatCreateFunction
>            "date_part" [ "timetz" , "text" ] False "float8"
>        , CatCreateFunction
>            "date_part" [ "abstime" , "text" ] False "float8"
>        , CatCreateFunction
>            "date_part" [ "text" , "reltime" ] False "float8"
>        , CatCreateFunction "date_part" [ "date" , "text" ] False "float8"
>        , CatCreateFunction "date_part" [ "time" , "text" ] False "float8"
>        , CatCreateFunction
>            "date_part" [ "text" , "timestamp" ] False "float8"
>        , CatCreateFunction
>            "date_pl_interval" [ "interval" , "date" ] False "timestamp"
>        , CatCreateFunction "date_pli" [ "int4" , "date" ] False "date"
>        , CatCreateFunction "date_recv" [ "internal" ] False "date"
>        , CatCreateFunction "date_send" [ "date" ] False "bytea"
>        , CatCreateFunction "date_smaller" [ "date" , "date" ] False "date"
>        , CatCreateFunction "date_sortsupport" [ "internal" ] False "void"
>        , CatCreateFunction
>            "date_trunc" [ "timestamptz" , "text" ] False "timestamptz"
>        , CatCreateFunction
>            "date_trunc" [ "text" , "interval" ] False "interval"
>        , CatCreateFunction
>            "date_trunc" [ "text" , "timestamp" ] False "timestamp"
>        , CatCreateFunction
>            "daterange" [ "date" , "date" ] False "daterange"
>        , CatCreateFunction
>            "daterange" [ "date" , "text" , "date" ] False "daterange"
>        , CatCreateFunction
>            "daterange_canonical" [ "daterange" ] False "daterange"
>        , CatCreateFunction
>            "daterange_subdiff" [ "date" , "date" ] False "float8"
>        , CatCreateFunction
>            "datetime_pl" [ "time" , "date" ] False "timestamp"
>        , CatCreateFunction
>            "datetimetz_pl" [ "date" , "timetz" ] False "timestamptz"
>        , CatCreateFunction "dcbrt" [ "float8" ] False "float8"
>        , CatCreateFunction "decode" [ "text" , "text" ] False "bytea"
>        , CatCreateFunction "degrees" [ "float8" ] False "float8"
>        , CatCreateFunction "dexp" [ "float8" ] False "float8"
>        , CatCreateFunction "diagonal" [ "box" ] False "lseg"
>        , CatCreateFunction "diameter" [ "circle" ] False "float8"
>        , CatCreateFunction "dispell_init" [ "internal" ] False "internal"
>        , CatCreateFunction
>            "dispell_lexize"
>            [ "internal" , "internal" , "internal" , "internal" ]
>            False
>            "internal"
>        , CatCreateFunction
>            "dist_cpoly" [ "polygon" , "circle" ] False "float8"
>        , CatCreateFunction "dist_lb" [ "line" , "box" ] False "float8"
>        , CatCreateFunction "dist_pb" [ "box" , "point" ] False "float8"
>        , CatCreateFunction "dist_pc" [ "circle" , "point" ] False "float8"
>        , CatCreateFunction "dist_pl" [ "point" , "line" ] False "float8"
>        , CatCreateFunction
>            "dist_ppath" [ "point" , "path" ] False "float8"
>        , CatCreateFunction "dist_ps" [ "point" , "lseg" ] False "float8"
>        , CatCreateFunction "dist_sb" [ "lseg" , "box" ] False "float8"
>        , CatCreateFunction "dist_sl" [ "lseg" , "line" ] False "float8"
>        , CatCreateFunction "div" [ "numeric" , "numeric" ] False "numeric"
>        , CatCreateFunction "dlog1" [ "float8" ] False "float8"
>        , CatCreateFunction "dlog10" [ "float8" ] False "float8"
>        , CatCreateFunction
>            "domain_in" [ "oid" , "cstring" , "int4" ] False "any"
>        , CatCreateFunction
>            "domain_recv" [ "oid" , "int4" , "internal" ] False "any"
>        , CatCreateFunction "dpow" [ "float8" , "float8" ] False "float8"
>        , CatCreateFunction "dround" [ "float8" ] False "float8"
>        , CatCreateFunction "dsimple_init" [ "internal" ] False "internal"
>        , CatCreateFunction
>            "dsimple_lexize"
>            [ "internal" , "internal" , "internal" , "internal" ]
>            False
>            "internal"
>        , CatCreateFunction
>            "dsnowball_init" [ "internal" ] False "internal"
>        , CatCreateFunction
>            "dsnowball_lexize"
>            [ "internal" , "internal" , "internal" , "internal" ]
>            False
>            "internal"
>        , CatCreateFunction "dsqrt" [ "float8" ] False "float8"
>        , CatCreateFunction "dsynonym_init" [ "internal" ] False "internal"
>        , CatCreateFunction
>            "dsynonym_lexize"
>            [ "internal" , "internal" , "internal" , "internal" ]
>            False
>            "internal"
>        , CatCreateFunction "dtrunc" [ "float8" ] False "float8"
>        , CatCreateFunction
>            "elem_contained_by_range"
>            [ "anyrange" , "anyelement" ]
>            False
>            "bool"
>        , CatCreateFunction "encode" [ "bytea" , "text" ] False "text"
>        , CatCreateFunction
>            "enum_cmp" [ "anyenum" , "anyenum" ] False "int4"
>        , CatCreateFunction
>            "enum_eq" [ "anyenum" , "anyenum" ] False "bool"
>        , CatCreateFunction "enum_first" [ "anyenum" ] False "anyenum"
>        , CatCreateFunction
>            "enum_ge" [ "anyenum" , "anyenum" ] False "bool"
>        , CatCreateFunction
>            "enum_gt" [ "anyenum" , "anyenum" ] False "bool"
>        , CatCreateFunction "enum_in" [ "oid" , "cstring" ] False "anyenum"
>        , CatCreateFunction
>            "enum_larger" [ "anyenum" , "anyenum" ] False "anyenum"
>        , CatCreateFunction "enum_last" [ "anyenum" ] False "anyenum"
>        , CatCreateFunction
>            "enum_le" [ "anyenum" , "anyenum" ] False "bool"
>        , CatCreateFunction
>            "enum_lt" [ "anyenum" , "anyenum" ] False "bool"
>        , CatCreateFunction
>            "enum_ne" [ "anyenum" , "anyenum" ] False "bool"
>        , CatCreateFunction "enum_out" [ "anyenum" ] False "cstring"
>        , CatCreateFunction
>            "enum_range" [ "anyenum" , "anyenum" ] False "anyarray"
>        , CatCreateFunction "enum_range" [ "anyenum" ] False "anyarray"
>        , CatCreateFunction
>            "enum_recv" [ "internal" , "oid" ] False "anyenum"
>        , CatCreateFunction "enum_send" [ "anyenum" ] False "bytea"
>        , CatCreateFunction
>            "enum_smaller" [ "anyenum" , "anyenum" ] False "anyenum"
>        , CatCreateFunction
>            "eqjoinsel"
>            [ "internal" , "oid" , "internal" , "int2" , "internal" ]
>            False
>            "float8"
>        , CatCreateFunction
>            "eqsel" [ "internal" , "internal" , "int4" , "oid" ] False "float8"
>        , CatCreateFunction
>            "euc_cn_to_mic"
>            [ "cstring" , "int4" , "internal" , "int4" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "euc_cn_to_utf8"
>            [ "int4" , "cstring" , "internal" , "int4" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "euc_jis_2004_to_shift_jis_2004"
>            [ "int4" , "cstring" , "internal" , "int4" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "euc_jis_2004_to_utf8"
>            [ "internal" , "int4" , "cstring" , "int4" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "euc_jp_to_mic"
>            [ "int4" , "int4" , "cstring" , "internal" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "euc_jp_to_sjis"
>            [ "int4" , "cstring" , "int4" , "internal" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "euc_jp_to_utf8"
>            [ "int4" , "int4" , "cstring" , "internal" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "euc_kr_to_mic"
>            [ "cstring" , "int4" , "internal" , "int4" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "euc_kr_to_utf8"
>            [ "internal" , "int4" , "int4" , "cstring" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "euc_tw_to_big5"
>            [ "cstring" , "int4" , "internal" , "int4" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "euc_tw_to_mic"
>            [ "cstring" , "int4" , "int4" , "int4" , "internal" ]
>            False
>            "void"
>        , CatCreateFunction
>            "euc_tw_to_utf8"
>            [ "int4" , "int4" , "int4" , "internal" , "cstring" ]
>            False
>            "void"
>        , CatCreateFunction
>            "event_trigger_in" [ "cstring" ] False "event_trigger"
>        , CatCreateFunction
>            "event_trigger_out" [ "event_trigger" ] False "cstring"
>        , CatCreateFunction "exp" [ "float8" ] False "float8"
>        , CatCreateFunction "exp" [ "numeric" ] False "numeric"
>        , CatCreateFunction "factorial" [ "int8" ] False "numeric"
>        , CatCreateFunction "family" [ "inet" ] False "int4"
>        , CatCreateFunction
>            "fdw_handler_in" [ "cstring" ] False "fdw_handler"
>        , CatCreateFunction
>            "fdw_handler_out" [ "fdw_handler" ] False "cstring"
>        , CatCreateFunction "float4" [ "int2" ] False "float4"
>        , CatCreateFunction "float4" [ "float8" ] False "float4"
>        , CatCreateFunction "float4" [ "int4" ] False "float4"
>        , CatCreateFunction "float4" [ "int8" ] False "float4"
>        , CatCreateFunction "float4" [ "numeric" ] False "float4"
>        , CatCreateFunction
>            "float48div" [ "float8" , "float4" ] False "float8"
>        , CatCreateFunction
>            "float48eq" [ "float4" , "float8" ] False "bool"
>        , CatCreateFunction
>            "float48ge" [ "float4" , "float8" ] False "bool"
>        , CatCreateFunction
>            "float48gt" [ "float4" , "float8" ] False "bool"
>        , CatCreateFunction
>            "float48le" [ "float8" , "float4" ] False "bool"
>        , CatCreateFunction
>            "float48lt" [ "float4" , "float8" ] False "bool"
>        , CatCreateFunction
>            "float48mi" [ "float4" , "float8" ] False "float8"
>        , CatCreateFunction
>            "float48mul" [ "float8" , "float4" ] False "float8"
>        , CatCreateFunction
>            "float48ne" [ "float8" , "float4" ] False "bool"
>        , CatCreateFunction
>            "float48pl" [ "float4" , "float8" ] False "float8"
>        , CatCreateFunction
>            "float4_accum" [ "float4" , "_float8" ] False "_float8"
>        , CatCreateFunction "float4abs" [ "float4" ] False "float4"
>        , CatCreateFunction
>            "float4div" [ "float4" , "float4" ] False "float4"
>        , CatCreateFunction "float4eq" [ "float4" , "float4" ] False "bool"
>        , CatCreateFunction "float4ge" [ "float4" , "float4" ] False "bool"
>        , CatCreateFunction "float4gt" [ "float4" , "float4" ] False "bool"
>        , CatCreateFunction "float4in" [ "cstring" ] False "float4"
>        , CatCreateFunction
>            "float4larger" [ "float4" , "float4" ] False "float4"
>        , CatCreateFunction "float4le" [ "float4" , "float4" ] False "bool"
>        , CatCreateFunction "float4lt" [ "float4" , "float4" ] False "bool"
>        , CatCreateFunction
>            "float4mi" [ "float4" , "float4" ] False "float4"
>        , CatCreateFunction
>            "float4mul" [ "float4" , "float4" ] False "float4"
>        , CatCreateFunction "float4ne" [ "float4" , "float4" ] False "bool"
>        , CatCreateFunction "float4out" [ "float4" ] False "cstring"
>        , CatCreateFunction
>            "float4pl" [ "float4" , "float4" ] False "float4"
>        , CatCreateFunction "float4recv" [ "internal" ] False "float4"
>        , CatCreateFunction "float4send" [ "float4" ] False "bytea"
>        , CatCreateFunction
>            "float4smaller" [ "float4" , "float4" ] False "float4"
>        , CatCreateFunction "float4um" [ "float4" ] False "float4"
>        , CatCreateFunction "float4up" [ "float4" ] False "float4"
>        , CatCreateFunction "float8" [ "int2" ] False "float8"
>        , CatCreateFunction "float8" [ "float4" ] False "float8"
>        , CatCreateFunction "float8" [ "int4" ] False "float8"
>        , CatCreateFunction "float8" [ "int8" ] False "float8"
>        , CatCreateFunction "float8" [ "numeric" ] False "float8"
>        , CatCreateFunction
>            "float84div" [ "float4" , "float8" ] False "float8"
>        , CatCreateFunction
>            "float84eq" [ "float4" , "float8" ] False "bool"
>        , CatCreateFunction
>            "float84ge" [ "float8" , "float4" ] False "bool"
>        , CatCreateFunction
>            "float84gt" [ "float8" , "float4" ] False "bool"
>        , CatCreateFunction
>            "float84le" [ "float4" , "float8" ] False "bool"
>        , CatCreateFunction
>            "float84lt" [ "float8" , "float4" ] False "bool"
>        , CatCreateFunction
>            "float84mi" [ "float4" , "float8" ] False "float8"
>        , CatCreateFunction
>            "float84mul" [ "float8" , "float4" ] False "float8"
>        , CatCreateFunction
>            "float84ne" [ "float8" , "float4" ] False "bool"
>        , CatCreateFunction
>            "float84pl" [ "float8" , "float4" ] False "float8"
>        , CatCreateFunction
>            "float8_accum" [ "_float8" , "float8" ] False "_float8"
>        , CatCreateFunction "float8_avg" [ "_float8" ] False "float8"
>        , CatCreateFunction "float8_corr" [ "_float8" ] False "float8"
>        , CatCreateFunction "float8_covar_pop" [ "_float8" ] False "float8"
>        , CatCreateFunction
>            "float8_covar_samp" [ "_float8" ] False "float8"
>        , CatCreateFunction
>            "float8_regr_accum"
>            [ "float8" , "_float8" , "float8" ]
>            False
>            "_float8"
>        , CatCreateFunction "float8_regr_avgx" [ "_float8" ] False "float8"
>        , CatCreateFunction "float8_regr_avgy" [ "_float8" ] False "float8"
>        , CatCreateFunction
>            "float8_regr_intercept" [ "_float8" ] False "float8"
>        , CatCreateFunction "float8_regr_r2" [ "_float8" ] False "float8"
>        , CatCreateFunction
>            "float8_regr_slope" [ "_float8" ] False "float8"
>        , CatCreateFunction "float8_regr_sxx" [ "_float8" ] False "float8"
>        , CatCreateFunction "float8_regr_sxy" [ "_float8" ] False "float8"
>        , CatCreateFunction "float8_regr_syy" [ "_float8" ] False "float8"
>        , CatCreateFunction
>            "float8_stddev_pop" [ "_float8" ] False "float8"
>        , CatCreateFunction
>            "float8_stddev_samp" [ "_float8" ] False "float8"
>        , CatCreateFunction "float8_var_pop" [ "_float8" ] False "float8"
>        , CatCreateFunction "float8_var_samp" [ "_float8" ] False "float8"
>        , CatCreateFunction "float8abs" [ "float8" ] False "float8"
>        , CatCreateFunction
>            "float8div" [ "float8" , "float8" ] False "float8"
>        , CatCreateFunction "float8eq" [ "float8" , "float8" ] False "bool"
>        , CatCreateFunction "float8ge" [ "float8" , "float8" ] False "bool"
>        , CatCreateFunction "float8gt" [ "float8" , "float8" ] False "bool"
>        , CatCreateFunction "float8in" [ "cstring" ] False "float8"
>        , CatCreateFunction
>            "float8larger" [ "float8" , "float8" ] False "float8"
>        , CatCreateFunction "float8le" [ "float8" , "float8" ] False "bool"
>        , CatCreateFunction "float8lt" [ "float8" , "float8" ] False "bool"
>        , CatCreateFunction
>            "float8mi" [ "float8" , "float8" ] False "float8"
>        , CatCreateFunction
>            "float8mul" [ "float8" , "float8" ] False "float8"
>        , CatCreateFunction "float8ne" [ "float8" , "float8" ] False "bool"
>        , CatCreateFunction "float8out" [ "float8" ] False "cstring"
>        , CatCreateFunction
>            "float8pl" [ "float8" , "float8" ] False "float8"
>        , CatCreateFunction "float8recv" [ "internal" ] False "float8"
>        , CatCreateFunction "float8send" [ "float8" ] False "bytea"
>        , CatCreateFunction
>            "float8smaller" [ "float8" , "float8" ] False "float8"
>        , CatCreateFunction "float8um" [ "float8" ] False "float8"
>        , CatCreateFunction "float8up" [ "float8" ] False "float8"
>        , CatCreateFunction "floor" [ "numeric" ] False "numeric"
>        , CatCreateFunction "floor" [ "float8" ] False "float8"
>        , CatCreateFunction
>            "flt4_mul_cash" [ "money" , "float4" ] False "money"
>        , CatCreateFunction
>            "flt8_mul_cash" [ "money" , "float8" ] False "money"
>        , CatCreateFunction "fmgr_c_validator" [ "oid" ] False "void"
>        , CatCreateFunction
>            "fmgr_internal_validator" [ "oid" ] False "void"
>        , CatCreateFunction "fmgr_sql_validator" [ "oid" ] False "void"
>        , CatCreateFunction "format" [ "text" ] False "text"
>        , CatCreateFunction "format_type" [ "int4" , "oid" ] False "text"
>        , CatCreateFunction
>            "gb18030_to_utf8"
>            [ "int4" , "int4" , "cstring" , "internal" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "gbk_to_utf8"
>            [ "internal" , "int4" , "int4" , "cstring" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "generate_series"
>            [ "timestamp" , "timestamp" , "interval" ]
>            True
>            "timestamp"
>        , CatCreateFunction
>            "generate_series"
>            [ "timestamptz" , "interval" , "timestamptz" ]
>            True
>            "timestamptz"
>        , CatCreateFunction
>            "generate_series" [ "int4" , "int4" , "int4" ] True "int4"
>        , CatCreateFunction
>            "generate_series" [ "int4" , "int4" ] True "int4"
>        , CatCreateFunction
>            "generate_series" [ "int8" , "int8" , "int8" ] True "int8"
>        , CatCreateFunction
>            "generate_series" [ "int8" , "int8" ] True "int8"
>        , CatCreateFunction
>            "generate_subscripts" [ "int4" , "bool" , "anyarray" ] True "int4"
>        , CatCreateFunction
>            "generate_subscripts" [ "anyarray" , "int4" ] True "int4"
>        , CatCreateFunction "get_bit" [ "bytea" , "int4" ] False "int4"
>        , CatCreateFunction "get_bit" [ "bit" , "int4" ] False "int4"
>        , CatCreateFunction "get_byte" [ "int4" , "bytea" ] False "int4"
>        , CatCreateFunction
>            "gin_cmp_prefix"
>            [ "text" , "text" , "internal" , "int2" ]
>            False
>            "int4"
>        , CatCreateFunction
>            "gin_cmp_tslexeme" [ "text" , "text" ] False "int4"
>        , CatCreateFunction
>            "gin_compare_jsonb" [ "text" , "text" ] False "int4"
>        , CatCreateFunction
>            "gin_consistent_jsonb"
>            [ "internal"
>            , "internal"
>            , "internal"
>            , "internal"
>            , "internal"
>            , "int2"
>            , "anyarray"
>            , "int4"
>            ]
>            False
>            "bool"
>        , CatCreateFunction
>            "gin_consistent_jsonb_path"
>            [ "int2"
>            , "anyarray"
>            , "int4"
>            , "internal"
>            , "internal"
>            , "internal"
>            , "internal"
>            , "internal"
>            ]
>            False
>            "bool"
>        , CatCreateFunction
>            "gin_extract_jsonb"
>            [ "internal" , "internal" , "internal" ]
>            False
>            "internal"
>        , CatCreateFunction
>            "gin_extract_jsonb_path"
>            [ "internal" , "internal" , "internal" ]
>            False
>            "internal"
>        , CatCreateFunction
>            "gin_extract_jsonb_query"
>            [ "internal"
>            , "internal"
>            , "internal"
>            , "internal"
>            , "int2"
>            , "anyarray"
>            , "internal"
>            ]
>            False
>            "internal"
>        , CatCreateFunction
>            "gin_extract_jsonb_query_path"
>            [ "internal"
>            , "anyarray"
>            , "internal"
>            , "internal"
>            , "internal"
>            , "internal"
>            , "int2"
>            ]
>            False
>            "internal"
>        , CatCreateFunction
>            "gin_extract_tsquery"
>            [ "tsquery" , "internal" , "int2" , "internal" , "internal" ]
>            False
>            "internal"
>        , CatCreateFunction
>            "gin_extract_tsquery"
>            [ "internal"
>            , "internal"
>            , "tsquery"
>            , "internal"
>            , "internal"
>            , "internal"
>            , "int2"
>            ]
>            False
>            "internal"
>        , CatCreateFunction
>            "gin_extract_tsvector" [ "internal" , "tsvector" ] False "internal"
>        , CatCreateFunction
>            "gin_extract_tsvector"
>            [ "internal" , "internal" , "tsvector" ]
>            False
>            "internal"
>        , CatCreateFunction
>            "gin_triconsistent_jsonb"
>            [ "internal"
>            , "internal"
>            , "int4"
>            , "internal"
>            , "internal"
>            , "int2"
>            , "anyarray"
>            ]
>            False
>            "char"
>        , CatCreateFunction
>            "gin_triconsistent_jsonb_path"
>            [ "int4"
>            , "internal"
>            , "internal"
>            , "anyarray"
>            , "int2"
>            , "internal"
>            , "internal"
>            ]
>            False
>            "char"
>        , CatCreateFunction
>            "gin_tsquery_consistent"
>            [ "tsquery"
>            , "internal"
>            , "internal"
>            , "int4"
>            , "int2"
>            , "internal"
>            ]
>            False
>            "bool"
>        , CatCreateFunction
>            "gin_tsquery_consistent"
>            [ "tsquery"
>            , "internal"
>            , "int4"
>            , "internal"
>            , "internal"
>            , "int2"
>            , "internal"
>            , "internal"
>            ]
>            False
>            "bool"
>        , CatCreateFunction
>            "gin_tsquery_triconsistent"
>            [ "internal"
>            , "internal"
>            , "int2"
>            , "tsquery"
>            , "int4"
>            , "internal"
>            , "internal"
>            ]
>            False
>            "char"
>        , CatCreateFunction
>            "ginarrayconsistent"
>            [ "int2"
>            , "int4"
>            , "anyarray"
>            , "internal"
>            , "internal"
>            , "internal"
>            , "internal"
>            , "internal"
>            ]
>            False
>            "bool"
>        , CatCreateFunction
>            "ginarrayextract"
>            [ "internal" , "anyarray" , "internal" ]
>            False
>            "internal"
>        , CatCreateFunction
>            "ginarrayextract" [ "anyarray" , "internal" ] False "internal"
>        , CatCreateFunction
>            "ginarraytriconsistent"
>            [ "int2"
>            , "anyarray"
>            , "int4"
>            , "internal"
>            , "internal"
>            , "internal"
>            , "internal"
>            ]
>            False
>            "char"
>        , CatCreateFunction
>            "ginbeginscan"
>            [ "internal" , "internal" , "internal" ]
>            False
>            "internal"
>        , CatCreateFunction
>            "ginbuild"
>            [ "internal" , "internal" , "internal" ]
>            False
>            "internal"
>        , CatCreateFunction "ginbuildempty" [ "internal" ] False "void"
>        , CatCreateFunction
>            "ginbulkdelete"
>            [ "internal" , "internal" , "internal" , "internal" ]
>            False
>            "internal"
>        , CatCreateFunction
>            "gincostestimate"
>            [ "internal"
>            , "internal"
>            , "internal"
>            , "internal"
>            , "internal"
>            , "internal"
>            , "internal"
>            ]
>            False
>            "void"
>        , CatCreateFunction "ginendscan" [ "internal" ] False "void"
>        , CatCreateFunction
>            "gingetbitmap" [ "internal" , "internal" ] False "int8"
>        , CatCreateFunction
>            "gininsert"
>            [ "internal"
>            , "internal"
>            , "internal"
>            , "internal"
>            , "internal"
>            , "internal"
>            ]
>            False
>            "bool"
>        , CatCreateFunction "ginmarkpos" [ "internal" ] False "void"
>        , CatCreateFunction "ginoptions" [ "bool" , "_text" ] False "bytea"
>        , CatCreateFunction
>            "ginqueryarrayextract"
>            [ "internal"
>            , "int2"
>            , "internal"
>            , "internal"
>            , "anyarray"
>            , "internal"
>            , "internal"
>            ]
>            False
>            "internal"
>        , CatCreateFunction
>            "ginrescan"
>            [ "internal" , "internal" , "internal" , "internal" , "internal" ]
>            False
>            "void"
>        , CatCreateFunction "ginrestrpos" [ "internal" ] False "void"
>        , CatCreateFunction
>            "ginvacuumcleanup" [ "internal" , "internal" ] False "internal"
>        , CatCreateFunction
>            "gist_box_compress" [ "internal" ] False "internal"
>        , CatCreateFunction
>            "gist_box_consistent"
>            [ "internal" , "internal" , "box" , "int4" , "oid" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "gist_box_decompress" [ "internal" ] False "internal"
>        , CatCreateFunction
>            "gist_box_penalty"
>            [ "internal" , "internal" , "internal" ]
>            False
>            "internal"
>        , CatCreateFunction
>            "gist_box_picksplit" [ "internal" , "internal" ] False "internal"
>        , CatCreateFunction
>            "gist_box_same" [ "box" , "internal" , "box" ] False "internal"
>        , CatCreateFunction
>            "gist_box_union" [ "internal" , "internal" ] False "box"
>        , CatCreateFunction
>            "gist_circle_compress" [ "internal" ] False "internal"
>        , CatCreateFunction
>            "gist_circle_consistent"
>            [ "int4" , "internal" , "circle" , "internal" , "oid" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "gist_point_compress" [ "internal" ] False "internal"
>        , CatCreateFunction
>            "gist_point_consistent"
>            [ "point" , "internal" , "oid" , "int4" , "internal" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "gist_point_distance"
>            [ "internal" , "int4" , "oid" , "point" ]
>            False
>            "float8"
>        , CatCreateFunction
>            "gist_poly_compress" [ "internal" ] False "internal"
>        , CatCreateFunction
>            "gist_poly_consistent"
>            [ "internal" , "polygon" , "internal" , "oid" , "int4" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "gistbeginscan"
>            [ "internal" , "internal" , "internal" ]
>            False
>            "internal"
>        , CatCreateFunction
>            "gistbuild"
>            [ "internal" , "internal" , "internal" ]
>            False
>            "internal"
>        , CatCreateFunction "gistbuildempty" [ "internal" ] False "void"
>        , CatCreateFunction
>            "gistbulkdelete"
>            [ "internal" , "internal" , "internal" , "internal" ]
>            False
>            "internal"
>        , CatCreateFunction
>            "gistcostestimate"
>            [ "internal"
>            , "internal"
>            , "internal"
>            , "internal"
>            , "internal"
>            , "internal"
>            , "internal"
>            ]
>            False
>            "void"
>        , CatCreateFunction "gistendscan" [ "internal" ] False "void"
>        , CatCreateFunction
>            "gistgetbitmap" [ "internal" , "internal" ] False "int8"
>        , CatCreateFunction
>            "gistgettuple" [ "internal" , "internal" ] False "bool"
>        , CatCreateFunction
>            "gistinsert"
>            [ "internal"
>            , "internal"
>            , "internal"
>            , "internal"
>            , "internal"
>            , "internal"
>            ]
>            False
>            "bool"
>        , CatCreateFunction "gistmarkpos" [ "internal" ] False "void"
>        , CatCreateFunction
>            "gistoptions" [ "_text" , "bool" ] False "bytea"
>        , CatCreateFunction
>            "gistrescan"
>            [ "internal" , "internal" , "internal" , "internal" , "internal" ]
>            False
>            "void"
>        , CatCreateFunction "gistrestrpos" [ "internal" ] False "void"
>        , CatCreateFunction
>            "gistvacuumcleanup" [ "internal" , "internal" ] False "internal"
>        , CatCreateFunction
>            "gtsquery_compress" [ "internal" ] False "internal"
>        , CatCreateFunction
>            "gtsquery_consistent"
>            [ "int4" , "oid" , "internal" , "internal" , "internal" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "gtsquery_decompress" [ "internal" ] False "internal"
>        , CatCreateFunction
>            "gtsquery_penalty"
>            [ "internal" , "internal" , "internal" ]
>            False
>            "internal"
>        , CatCreateFunction
>            "gtsquery_picksplit" [ "internal" , "internal" ] False "internal"
>        , CatCreateFunction
>            "gtsquery_same" [ "internal" , "int8" , "int8" ] False "internal"
>        , CatCreateFunction
>            "gtsquery_union" [ "internal" , "internal" ] False "internal"
>        , CatCreateFunction
>            "gtsvector_compress" [ "internal" ] False "internal"
>        , CatCreateFunction
>            "gtsvector_consistent"
>            [ "int4" , "oid" , "internal" , "internal" , "gtsvector" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "gtsvector_decompress" [ "internal" ] False "internal"
>        , CatCreateFunction
>            "gtsvector_penalty"
>            [ "internal" , "internal" , "internal" ]
>            False
>            "internal"
>        , CatCreateFunction
>            "gtsvector_picksplit" [ "internal" , "internal" ] False "internal"
>        , CatCreateFunction
>            "gtsvector_same"
>            [ "gtsvector" , "internal" , "gtsvector" ]
>            False
>            "internal"
>        , CatCreateFunction
>            "gtsvector_union" [ "internal" , "internal" ] False "internal"
>        , CatCreateFunction "gtsvectorin" [ "cstring" ] False "gtsvector"
>        , CatCreateFunction "gtsvectorout" [ "gtsvector" ] False "cstring"
>        , CatCreateFunction
>            "has_any_column_privilege"
>            [ "name" , "text" , "text" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "has_any_column_privilege" [ "text" , "oid" , "name" ] False "bool"
>        , CatCreateFunction
>            "has_any_column_privilege" [ "oid" , "text" , "text" ] False "bool"
>        , CatCreateFunction
>            "has_any_column_privilege" [ "oid" , "oid" , "text" ] False "bool"
>        , CatCreateFunction
>            "has_any_column_privilege" [ "text" , "text" ] False "bool"
>        , CatCreateFunction
>            "has_any_column_privilege" [ "text" , "oid" ] False "bool"
>        , CatCreateFunction
>            "has_column_privilege"
>            [ "name" , "text" , "text" , "text" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "has_column_privilege"
>            [ "int2" , "text" , "name" , "text" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "has_column_privilege"
>            [ "text" , "name" , "oid" , "text" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "has_column_privilege"
>            [ "int2" , "name" , "text" , "oid" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "has_column_privilege"
>            [ "text" , "oid" , "text" , "text" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "has_column_privilege"
>            [ "text" , "text" , "int2" , "oid" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "has_column_privilege"
>            [ "text" , "text" , "oid" , "oid" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "has_column_privilege"
>            [ "oid" , "oid" , "int2" , "text" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "has_column_privilege" [ "text" , "text" , "text" ] False "bool"
>        , CatCreateFunction
>            "has_column_privilege" [ "text" , "text" , "int2" ] False "bool"
>        , CatCreateFunction
>            "has_column_privilege" [ "oid" , "text" , "text" ] False "bool"
>        , CatCreateFunction
>            "has_column_privilege" [ "oid" , "text" , "int2" ] False "bool"
>        , CatCreateFunction
>            "has_database_privilege" [ "name" , "text" , "text" ] False "bool"
>        , CatCreateFunction
>            "has_database_privilege" [ "text" , "oid" , "name" ] False "bool"
>        , CatCreateFunction
>            "has_database_privilege" [ "oid" , "text" , "text" ] False "bool"
>        , CatCreateFunction
>            "has_database_privilege" [ "oid" , "text" , "oid" ] False "bool"
>        , CatCreateFunction
>            "has_database_privilege" [ "text" , "text" ] False "bool"
>        , CatCreateFunction
>            "has_database_privilege" [ "text" , "oid" ] False "bool"
>        , CatCreateFunction
>            "has_foreign_data_wrapper_privilege"
>            [ "name" , "text" , "text" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "has_foreign_data_wrapper_privilege"
>            [ "name" , "oid" , "text" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "has_foreign_data_wrapper_privilege"
>            [ "oid" , "text" , "text" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "has_foreign_data_wrapper_privilege"
>            [ "oid" , "text" , "oid" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "has_foreign_data_wrapper_privilege"
>            [ "text" , "text" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "has_foreign_data_wrapper_privilege"
>            [ "oid" , "text" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "has_function_privilege" [ "text" , "name" , "text" ] False "bool"
>        , CatCreateFunction
>            "has_function_privilege" [ "name" , "text" , "oid" ] False "bool"
>        , CatCreateFunction
>            "has_function_privilege" [ "text" , "oid" , "text" ] False "bool"
>        , CatCreateFunction
>            "has_function_privilege" [ "text" , "oid" , "oid" ] False "bool"
>        , CatCreateFunction
>            "has_function_privilege" [ "text" , "text" ] False "bool"
>        , CatCreateFunction
>            "has_function_privilege" [ "oid" , "text" ] False "bool"
>        , CatCreateFunction
>            "has_language_privilege" [ "text" , "text" , "name" ] False "bool"
>        , CatCreateFunction
>            "has_language_privilege" [ "name" , "oid" , "text" ] False "bool"
>        , CatCreateFunction
>            "has_language_privilege" [ "text" , "text" , "oid" ] False "bool"
>        , CatCreateFunction
>            "has_language_privilege" [ "oid" , "oid" , "text" ] False "bool"
>        , CatCreateFunction
>            "has_language_privilege" [ "text" , "text" ] False "bool"
>        , CatCreateFunction
>            "has_language_privilege" [ "oid" , "text" ] False "bool"
>        , CatCreateFunction
>            "has_schema_privilege" [ "name" , "text" , "text" ] False "bool"
>        , CatCreateFunction
>            "has_schema_privilege" [ "name" , "oid" , "text" ] False "bool"
>        , CatCreateFunction
>            "has_schema_privilege" [ "text" , "text" , "oid" ] False "bool"
>        , CatCreateFunction
>            "has_schema_privilege" [ "oid" , "text" , "oid" ] False "bool"
>        , CatCreateFunction
>            "has_schema_privilege" [ "text" , "text" ] False "bool"
>        , CatCreateFunction
>            "has_schema_privilege" [ "text" , "oid" ] False "bool"
>        , CatCreateFunction
>            "has_sequence_privilege" [ "text" , "text" , "name" ] False "bool"
>        , CatCreateFunction
>            "has_sequence_privilege" [ "name" , "oid" , "text" ] False "bool"
>        , CatCreateFunction
>            "has_sequence_privilege" [ "oid" , "text" , "text" ] False "bool"
>        , CatCreateFunction
>            "has_sequence_privilege" [ "text" , "oid" , "oid" ] False "bool"
>        , CatCreateFunction
>            "has_sequence_privilege" [ "text" , "text" ] False "bool"
>        , CatCreateFunction
>            "has_sequence_privilege" [ "oid" , "text" ] False "bool"
>        , CatCreateFunction
>            "has_server_privilege" [ "name" , "text" , "text" ] False "bool"
>        , CatCreateFunction
>            "has_server_privilege" [ "text" , "name" , "oid" ] False "bool"
>        , CatCreateFunction
>            "has_server_privilege" [ "oid" , "text" , "text" ] False "bool"
>        , CatCreateFunction
>            "has_server_privilege" [ "oid" , "oid" , "text" ] False "bool"
>        , CatCreateFunction
>            "has_server_privilege" [ "text" , "text" ] False "bool"
>        , CatCreateFunction
>            "has_server_privilege" [ "oid" , "text" ] False "bool"
>        , CatCreateFunction
>            "has_table_privilege" [ "text" , "name" , "text" ] False "bool"
>        , CatCreateFunction
>            "has_table_privilege" [ "text" , "oid" , "name" ] False "bool"
>        , CatCreateFunction
>            "has_table_privilege" [ "oid" , "text" , "text" ] False "bool"
>        , CatCreateFunction
>            "has_table_privilege" [ "oid" , "text" , "oid" ] False "bool"
>        , CatCreateFunction
>            "has_table_privilege" [ "text" , "text" ] False "bool"
>        , CatCreateFunction
>            "has_table_privilege" [ "text" , "oid" ] False "bool"
>        , CatCreateFunction
>            "has_tablespace_privilege"
>            [ "name" , "text" , "text" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "has_tablespace_privilege" [ "text" , "oid" , "name" ] False "bool"
>        , CatCreateFunction
>            "has_tablespace_privilege" [ "text" , "text" , "oid" ] False "bool"
>        , CatCreateFunction
>            "has_tablespace_privilege" [ "oid" , "oid" , "text" ] False "bool"
>        , CatCreateFunction
>            "has_tablespace_privilege" [ "text" , "text" ] False "bool"
>        , CatCreateFunction
>            "has_tablespace_privilege" [ "oid" , "text" ] False "bool"
>        , CatCreateFunction
>            "has_type_privilege" [ "name" , "text" , "text" ] False "bool"
>        , CatCreateFunction
>            "has_type_privilege" [ "name" , "text" , "oid" ] False "bool"
>        , CatCreateFunction
>            "has_type_privilege" [ "text" , "text" , "oid" ] False "bool"
>        , CatCreateFunction
>            "has_type_privilege" [ "oid" , "text" , "oid" ] False "bool"
>        , CatCreateFunction
>            "has_type_privilege" [ "text" , "text" ] False "bool"
>        , CatCreateFunction
>            "has_type_privilege" [ "oid" , "text" ] False "bool"
>        , CatCreateFunction "hash_aclitem" [ "aclitem" ] False "int4"
>        , CatCreateFunction "hash_array" [ "anyarray" ] False "int4"
>        , CatCreateFunction "hash_numeric" [ "numeric" ] False "int4"
>        , CatCreateFunction "hash_range" [ "anyrange" ] False "int4"
>        , CatCreateFunction
>            "hashbeginscan"
>            [ "internal" , "internal" , "internal" ]
>            False
>            "internal"
>        , CatCreateFunction "hashbpchar" [ "bpchar" ] False "int4"
>        , CatCreateFunction
>            "hashbuild"
>            [ "internal" , "internal" , "internal" ]
>            False
>            "internal"
>        , CatCreateFunction "hashbuildempty" [ "internal" ] False "void"
>        , CatCreateFunction
>            "hashbulkdelete"
>            [ "internal" , "internal" , "internal" , "internal" ]
>            False
>            "internal"
>        , CatCreateFunction "hashchar" [ "char" ] False "int4"
>        , CatCreateFunction
>            "hashcostestimate"
>            [ "internal"
>            , "internal"
>            , "internal"
>            , "internal"
>            , "internal"
>            , "internal"
>            , "internal"
>            ]
>            False
>            "void"
>        , CatCreateFunction "hashendscan" [ "internal" ] False "void"
>        , CatCreateFunction "hashenum" [ "anyenum" ] False "int4"
>        , CatCreateFunction "hashfloat4" [ "float4" ] False "int4"
>        , CatCreateFunction "hashfloat8" [ "float8" ] False "int4"
>        , CatCreateFunction
>            "hashgetbitmap" [ "internal" , "internal" ] False "int8"
>        , CatCreateFunction
>            "hashgettuple" [ "internal" , "internal" ] False "bool"
>        , CatCreateFunction "hashinet" [ "inet" ] False "int4"
>        , CatCreateFunction
>            "hashinsert"
>            [ "internal"
>            , "internal"
>            , "internal"
>            , "internal"
>            , "internal"
>            , "internal"
>            ]
>            False
>            "bool"
>        , CatCreateFunction "hashint2" [ "int2" ] False "int4"
>        , CatCreateFunction "hashint2vector" [ "int2vector" ] False "int4"
>        , CatCreateFunction "hashint4" [ "int4" ] False "int4"
>        , CatCreateFunction "hashint8" [ "int8" ] False "int4"
>        , CatCreateFunction "hashmacaddr" [ "macaddr" ] False "int4"
>        , CatCreateFunction "hashmarkpos" [ "internal" ] False "void"
>        , CatCreateFunction "hashname" [ "name" ] False "int4"
>        , CatCreateFunction "hashoid" [ "oid" ] False "int4"
>        , CatCreateFunction "hashoidvector" [ "oidvector" ] False "int4"
>        , CatCreateFunction
>            "hashoptions" [ "_text" , "bool" ] False "bytea"
>        , CatCreateFunction
>            "hashrescan"
>            [ "internal" , "internal" , "internal" , "internal" , "internal" ]
>            False
>            "void"
>        , CatCreateFunction "hashrestrpos" [ "internal" ] False "void"
>        , CatCreateFunction "hashtext" [ "text" ] False "int4"
>        , CatCreateFunction
>            "hashvacuumcleanup" [ "internal" , "internal" ] False "internal"
>        , CatCreateFunction "hashvarlena" [ "internal" ] False "int4"
>        , CatCreateFunction "height" [ "box" ] False "float8"
>        , CatCreateFunction "host" [ "inet" ] False "text"
>        , CatCreateFunction "hostmask" [ "inet" ] False "inet"
>        , CatCreateFunction
>            "iclikejoinsel"
>            [ "int2" , "internal" , "internal" , "internal" , "oid" ]
>            False
>            "float8"
>        , CatCreateFunction
>            "iclikesel"
>            [ "int4" , "internal" , "internal" , "oid" ]
>            False
>            "float8"
>        , CatCreateFunction
>            "icnlikejoinsel"
>            [ "internal" , "int2" , "internal" , "internal" , "oid" ]
>            False
>            "float8"
>        , CatCreateFunction
>            "icnlikesel"
>            [ "internal" , "oid" , "int4" , "internal" ]
>            False
>            "float8"
>        , CatCreateFunction
>            "icregexeqjoinsel"
>            [ "internal" , "int2" , "internal" , "oid" , "internal" ]
>            False
>            "float8"
>        , CatCreateFunction
>            "icregexeqsel"
>            [ "internal" , "int4" , "oid" , "internal" ]
>            False
>            "float8"
>        , CatCreateFunction
>            "icregexnejoinsel"
>            [ "internal" , "internal" , "oid" , "internal" , "int2" ]
>            False
>            "float8"
>        , CatCreateFunction
>            "icregexnesel"
>            [ "internal" , "int4" , "internal" , "oid" ]
>            False
>            "float8"
>        , CatCreateFunction
>            "inet_gist_compress" [ "internal" ] False "internal"
>        , CatCreateFunction
>            "inet_gist_consistent"
>            [ "int4" , "inet" , "oid" , "internal" , "internal" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "inet_gist_decompress" [ "internal" ] False "internal"
>        , CatCreateFunction
>            "inet_gist_penalty"
>            [ "internal" , "internal" , "internal" ]
>            False
>            "internal"
>        , CatCreateFunction
>            "inet_gist_picksplit" [ "internal" , "internal" ] False "internal"
>        , CatCreateFunction
>            "inet_gist_same" [ "inet" , "inet" , "internal" ] False "internal"
>        , CatCreateFunction
>            "inet_gist_union" [ "internal" , "internal" ] False "internal"
>        , CatCreateFunction "inet_in" [ "cstring" ] False "inet"
>        , CatCreateFunction "inet_out" [ "inet" ] False "cstring"
>        , CatCreateFunction "inet_recv" [ "internal" ] False "inet"
>        , CatCreateFunction "inet_send" [ "inet" ] False "bytea"
>        , CatCreateFunction "inetand" [ "inet" , "inet" ] False "inet"
>        , CatCreateFunction "inetmi" [ "inet" , "inet" ] False "int8"
>        , CatCreateFunction "inetmi_int8" [ "int8" , "inet" ] False "inet"
>        , CatCreateFunction "inetnot" [ "inet" ] False "inet"
>        , CatCreateFunction "inetor" [ "inet" , "inet" ] False "inet"
>        , CatCreateFunction "inetpl" [ "inet" , "int8" ] False "inet"
>        , CatCreateFunction "initcap" [ "text" ] False "text"
>        , CatCreateFunction "int2" [ "float8" ] False "int2"
>        , CatCreateFunction "int2" [ "float4" ] False "int2"
>        , CatCreateFunction "int2" [ "int4" ] False "int2"
>        , CatCreateFunction "int2" [ "int8" ] False "int2"
>        , CatCreateFunction "int2" [ "numeric" ] False "int2"
>        , CatCreateFunction "int24div" [ "int2" , "int4" ] False "int4"
>        , CatCreateFunction "int24eq" [ "int4" , "int2" ] False "bool"
>        , CatCreateFunction "int24ge" [ "int4" , "int2" ] False "bool"
>        , CatCreateFunction "int24gt" [ "int2" , "int4" ] False "bool"
>        , CatCreateFunction "int24le" [ "int4" , "int2" ] False "bool"
>        , CatCreateFunction "int24lt" [ "int4" , "int2" ] False "bool"
>        , CatCreateFunction "int24mi" [ "int4" , "int2" ] False "int4"
>        , CatCreateFunction "int24mul" [ "int2" , "int4" ] False "int4"
>        , CatCreateFunction "int24ne" [ "int4" , "int2" ] False "bool"
>        , CatCreateFunction "int24pl" [ "int2" , "int4" ] False "int4"
>        , CatCreateFunction "int28div" [ "int2" , "int8" ] False "int8"
>        , CatCreateFunction "int28eq" [ "int8" , "int2" ] False "bool"
>        , CatCreateFunction "int28ge" [ "int2" , "int8" ] False "bool"
>        , CatCreateFunction "int28gt" [ "int8" , "int2" ] False "bool"
>        , CatCreateFunction "int28le" [ "int2" , "int8" ] False "bool"
>        , CatCreateFunction "int28lt" [ "int8" , "int2" ] False "bool"
>        , CatCreateFunction "int28mi" [ "int2" , "int8" ] False "int8"
>        , CatCreateFunction "int28mul" [ "int2" , "int8" ] False "int8"
>        , CatCreateFunction "int28ne" [ "int8" , "int2" ] False "bool"
>        , CatCreateFunction "int28pl" [ "int2" , "int8" ] False "int8"
>        , CatCreateFunction
>            "int2_accum" [ "internal" , "int2" ] False "internal"
>        , CatCreateFunction
>            "int2_accum_inv" [ "int2" , "internal" ] False "internal"
>        , CatCreateFunction
>            "int2_avg_accum" [ "_int8" , "int2" ] False "_int8"
>        , CatCreateFunction
>            "int2_avg_accum_inv" [ "_int8" , "int2" ] False "_int8"
>        , CatCreateFunction
>            "int2_mul_cash" [ "int2" , "money" ] False "money"
>        , CatCreateFunction "int2_sum" [ "int8" , "int2" ] False "int8"
>        , CatCreateFunction "int2abs" [ "int2" ] False "int2"
>        , CatCreateFunction "int2and" [ "int2" , "int2" ] False "int2"
>        , CatCreateFunction "int2div" [ "int2" , "int2" ] False "int2"
>        , CatCreateFunction "int2eq" [ "int2" , "int2" ] False "bool"
>        , CatCreateFunction "int2ge" [ "int2" , "int2" ] False "bool"
>        , CatCreateFunction "int2gt" [ "int2" , "int2" ] False "bool"
>        , CatCreateFunction "int2in" [ "cstring" ] False "int2"
>        , CatCreateFunction "int2int4_sum" [ "_int8" ] False "int8"
>        , CatCreateFunction "int2larger" [ "int2" , "int2" ] False "int2"
>        , CatCreateFunction "int2le" [ "int2" , "int2" ] False "bool"
>        , CatCreateFunction "int2lt" [ "int2" , "int2" ] False "bool"
>        , CatCreateFunction "int2mi" [ "int2" , "int2" ] False "int2"
>        , CatCreateFunction "int2mod" [ "int2" , "int2" ] False "int2"
>        , CatCreateFunction "int2mul" [ "int2" , "int2" ] False "int2"
>        , CatCreateFunction "int2ne" [ "int2" , "int2" ] False "bool"
>        , CatCreateFunction "int2not" [ "int2" ] False "int2"
>        , CatCreateFunction "int2or" [ "int2" , "int2" ] False "int2"
>        , CatCreateFunction "int2out" [ "int2" ] False "cstring"
>        , CatCreateFunction "int2pl" [ "int2" , "int2" ] False "int2"
>        , CatCreateFunction "int2recv" [ "internal" ] False "int2"
>        , CatCreateFunction "int2send" [ "int2" ] False "bytea"
>        , CatCreateFunction "int2shl" [ "int2" , "int4" ] False "int2"
>        , CatCreateFunction "int2shr" [ "int4" , "int2" ] False "int2"
>        , CatCreateFunction "int2smaller" [ "int2" , "int2" ] False "int2"
>        , CatCreateFunction "int2um" [ "int2" ] False "int2"
>        , CatCreateFunction "int2up" [ "int2" ] False "int2"
>        , CatCreateFunction
>            "int2vectoreq" [ "int2vector" , "int2vector" ] False "bool"
>        , CatCreateFunction "int2vectorin" [ "cstring" ] False "int2vector"
>        , CatCreateFunction
>            "int2vectorout" [ "int2vector" ] False "cstring"
>        , CatCreateFunction
>            "int2vectorrecv" [ "internal" ] False "int2vector"
>        , CatCreateFunction "int2vectorsend" [ "int2vector" ] False "bytea"
>        , CatCreateFunction "int2xor" [ "int2" , "int2" ] False "int2"
>        , CatCreateFunction "int4" [ "char" ] False "int4"
>        , CatCreateFunction "int4" [ "int2" ] False "int4"
>        , CatCreateFunction "int4" [ "float8" ] False "int4"
>        , CatCreateFunction "int4" [ "float4" ] False "int4"
>        , CatCreateFunction "int4" [ "int8" ] False "int4"
>        , CatCreateFunction "int4" [ "bit" ] False "int4"
>        , CatCreateFunction "int4" [ "numeric" ] False "int4"
>        , CatCreateFunction "int4" [ "bool" ] False "int4"
>        , CatCreateFunction "int42div" [ "int2" , "int4" ] False "int4"
>        , CatCreateFunction "int42eq" [ "int4" , "int2" ] False "bool"
>        , CatCreateFunction "int42ge" [ "int4" , "int2" ] False "bool"
>        , CatCreateFunction "int42gt" [ "int4" , "int2" ] False "bool"
>        , CatCreateFunction "int42le" [ "int2" , "int4" ] False "bool"
>        , CatCreateFunction "int42lt" [ "int4" , "int2" ] False "bool"
>        , CatCreateFunction "int42mi" [ "int4" , "int2" ] False "int4"
>        , CatCreateFunction "int42mul" [ "int4" , "int2" ] False "int4"
>        , CatCreateFunction "int42ne" [ "int2" , "int4" ] False "bool"
>        , CatCreateFunction "int42pl" [ "int4" , "int2" ] False "int4"
>        , CatCreateFunction "int48div" [ "int8" , "int4" ] False "int8"
>        , CatCreateFunction "int48eq" [ "int8" , "int4" ] False "bool"
>        , CatCreateFunction "int48ge" [ "int4" , "int8" ] False "bool"
>        , CatCreateFunction "int48gt" [ "int4" , "int8" ] False "bool"
>        , CatCreateFunction "int48le" [ "int8" , "int4" ] False "bool"
>        , CatCreateFunction "int48lt" [ "int4" , "int8" ] False "bool"
>        , CatCreateFunction "int48mi" [ "int4" , "int8" ] False "int8"
>        , CatCreateFunction "int48mul" [ "int4" , "int8" ] False "int8"
>        , CatCreateFunction "int48ne" [ "int4" , "int8" ] False "bool"
>        , CatCreateFunction "int48pl" [ "int8" , "int4" ] False "int8"
>        , CatCreateFunction
>            "int4_accum" [ "int4" , "internal" ] False "internal"
>        , CatCreateFunction
>            "int4_accum_inv" [ "internal" , "int4" ] False "internal"
>        , CatCreateFunction
>            "int4_avg_accum" [ "int4" , "_int8" ] False "_int8"
>        , CatCreateFunction
>            "int4_avg_accum_inv" [ "int4" , "_int8" ] False "_int8"
>        , CatCreateFunction
>            "int4_mul_cash" [ "money" , "int4" ] False "money"
>        , CatCreateFunction "int4_sum" [ "int4" , "int8" ] False "int8"
>        , CatCreateFunction "int4abs" [ "int4" ] False "int4"
>        , CatCreateFunction "int4and" [ "int4" , "int4" ] False "int4"
>        , CatCreateFunction "int4div" [ "int4" , "int4" ] False "int4"
>        , CatCreateFunction "int4eq" [ "int4" , "int4" ] False "bool"
>        , CatCreateFunction "int4ge" [ "int4" , "int4" ] False "bool"
>        , CatCreateFunction "int4gt" [ "int4" , "int4" ] False "bool"
>        , CatCreateFunction "int4in" [ "cstring" ] False "int4"
>        , CatCreateFunction "int4inc" [ "int4" ] False "int4"
>        , CatCreateFunction "int4larger" [ "int4" , "int4" ] False "int4"
>        , CatCreateFunction "int4le" [ "int4" , "int4" ] False "bool"
>        , CatCreateFunction "int4lt" [ "int4" , "int4" ] False "bool"
>        , CatCreateFunction "int4mi" [ "int4" , "int4" ] False "int4"
>        , CatCreateFunction "int4mod" [ "int4" , "int4" ] False "int4"
>        , CatCreateFunction "int4mul" [ "int4" , "int4" ] False "int4"
>        , CatCreateFunction "int4ne" [ "int4" , "int4" ] False "bool"
>        , CatCreateFunction "int4not" [ "int4" ] False "int4"
>        , CatCreateFunction "int4or" [ "int4" , "int4" ] False "int4"
>        , CatCreateFunction "int4out" [ "int4" ] False "cstring"
>        , CatCreateFunction "int4pl" [ "int4" , "int4" ] False "int4"
>        , CatCreateFunction
>            "int4range" [ "int4" , "int4" ] False "int4range"
>        , CatCreateFunction
>            "int4range" [ "int4" , "text" , "int4" ] False "int4range"
>        , CatCreateFunction
>            "int4range_canonical" [ "int4range" ] False "int4range"
>        , CatCreateFunction
>            "int4range_subdiff" [ "int4" , "int4" ] False "float8"
>        , CatCreateFunction "int4recv" [ "internal" ] False "int4"
>        , CatCreateFunction "int4send" [ "int4" ] False "bytea"
>        , CatCreateFunction "int4shl" [ "int4" , "int4" ] False "int4"
>        , CatCreateFunction "int4shr" [ "int4" , "int4" ] False "int4"
>        , CatCreateFunction "int4smaller" [ "int4" , "int4" ] False "int4"
>        , CatCreateFunction "int4um" [ "int4" ] False "int4"
>        , CatCreateFunction "int4up" [ "int4" ] False "int4"
>        , CatCreateFunction "int4xor" [ "int4" , "int4" ] False "int4"
>        , CatCreateFunction "int8" [ "int4" ] False "int8"
>        , CatCreateFunction "int8" [ "float8" ] False "int8"
>        , CatCreateFunction "int8" [ "float4" ] False "int8"
>        , CatCreateFunction "int8" [ "int2" ] False "int8"
>        , CatCreateFunction "int8" [ "oid" ] False "int8"
>        , CatCreateFunction "int8" [ "numeric" ] False "int8"
>        , CatCreateFunction "int8" [ "bit" ] False "int8"
>        , CatCreateFunction "int82div" [ "int2" , "int8" ] False "int8"
>        , CatCreateFunction "int82eq" [ "int8" , "int2" ] False "bool"
>        , CatCreateFunction "int82ge" [ "int2" , "int8" ] False "bool"
>        , CatCreateFunction "int82gt" [ "int8" , "int2" ] False "bool"
>        , CatCreateFunction "int82le" [ "int8" , "int2" ] False "bool"
>        , CatCreateFunction "int82lt" [ "int8" , "int2" ] False "bool"
>        , CatCreateFunction "int82mi" [ "int2" , "int8" ] False "int8"
>        , CatCreateFunction "int82mul" [ "int8" , "int2" ] False "int8"
>        , CatCreateFunction "int82ne" [ "int8" , "int2" ] False "bool"
>        , CatCreateFunction "int82pl" [ "int2" , "int8" ] False "int8"
>        , CatCreateFunction "int84div" [ "int8" , "int4" ] False "int8"
>        , CatCreateFunction "int84eq" [ "int4" , "int8" ] False "bool"
>        , CatCreateFunction "int84ge" [ "int8" , "int4" ] False "bool"
>        , CatCreateFunction "int84gt" [ "int8" , "int4" ] False "bool"
>        , CatCreateFunction "int84le" [ "int8" , "int4" ] False "bool"
>        , CatCreateFunction "int84lt" [ "int8" , "int4" ] False "bool"
>        , CatCreateFunction "int84mi" [ "int4" , "int8" ] False "int8"
>        , CatCreateFunction "int84mul" [ "int4" , "int8" ] False "int8"
>        , CatCreateFunction "int84ne" [ "int4" , "int8" ] False "bool"
>        , CatCreateFunction "int84pl" [ "int4" , "int8" ] False "int8"
>        , CatCreateFunction
>            "int8_accum" [ "internal" , "int8" ] False "internal"
>        , CatCreateFunction
>            "int8_accum_inv" [ "int8" , "internal" ] False "internal"
>        , CatCreateFunction "int8_avg" [ "_int8" ] False "numeric"
>        , CatCreateFunction
>            "int8_avg_accum" [ "internal" , "int8" ] False "internal"
>        , CatCreateFunction
>            "int8_sum" [ "numeric" , "int8" ] False "numeric"
>        , CatCreateFunction "int8abs" [ "int8" ] False "int8"
>        , CatCreateFunction "int8and" [ "int8" , "int8" ] False "int8"
>        , CatCreateFunction "int8dec" [ "int8" ] False "int8"
>        , CatCreateFunction "int8dec_any" [ "any" , "int8" ] False "int8"
>        , CatCreateFunction "int8div" [ "int8" , "int8" ] False "int8"
>        , CatCreateFunction "int8eq" [ "int8" , "int8" ] False "bool"
>        , CatCreateFunction "int8ge" [ "int8" , "int8" ] False "bool"
>        , CatCreateFunction "int8gt" [ "int8" , "int8" ] False "bool"
>        , CatCreateFunction "int8in" [ "cstring" ] False "int8"
>        , CatCreateFunction "int8inc" [ "int8" ] False "int8"
>        , CatCreateFunction "int8inc_any" [ "any" , "int8" ] False "int8"
>        , CatCreateFunction
>            "int8inc_float8_float8"
>            [ "float8" , "int8" , "float8" ]
>            False
>            "int8"
>        , CatCreateFunction "int8larger" [ "int8" , "int8" ] False "int8"
>        , CatCreateFunction "int8le" [ "int8" , "int8" ] False "bool"
>        , CatCreateFunction "int8lt" [ "int8" , "int8" ] False "bool"
>        , CatCreateFunction "int8mi" [ "int8" , "int8" ] False "int8"
>        , CatCreateFunction "int8mod" [ "int8" , "int8" ] False "int8"
>        , CatCreateFunction "int8mul" [ "int8" , "int8" ] False "int8"
>        , CatCreateFunction "int8ne" [ "int8" , "int8" ] False "bool"
>        , CatCreateFunction "int8not" [ "int8" ] False "int8"
>        , CatCreateFunction "int8or" [ "int8" , "int8" ] False "int8"
>        , CatCreateFunction "int8out" [ "int8" ] False "cstring"
>        , CatCreateFunction "int8pl" [ "int8" , "int8" ] False "int8"
>        , CatCreateFunction "int8pl_inet" [ "int8" , "inet" ] False "inet"
>        , CatCreateFunction
>            "int8range" [ "int8" , "int8" ] False "int8range"
>        , CatCreateFunction
>            "int8range" [ "int8" , "int8" , "text" ] False "int8range"
>        , CatCreateFunction
>            "int8range_canonical" [ "int8range" ] False "int8range"
>        , CatCreateFunction
>            "int8range_subdiff" [ "int8" , "int8" ] False "float8"
>        , CatCreateFunction "int8recv" [ "internal" ] False "int8"
>        , CatCreateFunction "int8send" [ "int8" ] False "bytea"
>        , CatCreateFunction "int8shl" [ "int8" , "int4" ] False "int8"
>        , CatCreateFunction "int8shr" [ "int8" , "int4" ] False "int8"
>        , CatCreateFunction "int8smaller" [ "int8" , "int8" ] False "int8"
>        , CatCreateFunction "int8um" [ "int8" ] False "int8"
>        , CatCreateFunction "int8up" [ "int8" ] False "int8"
>        , CatCreateFunction "int8xor" [ "int8" , "int8" ] False "int8"
>        , CatCreateFunction
>            "integer_pl_date" [ "date" , "int4" ] False "date"
>        , CatCreateFunction "inter_lb" [ "line" , "box" ] False "bool"
>        , CatCreateFunction "inter_sb" [ "box" , "lseg" ] False "bool"
>        , CatCreateFunction "inter_sl" [ "lseg" , "line" ] False "bool"
>        , CatCreateFunction "internal_in" [ "cstring" ] False "internal"
>        , CatCreateFunction "internal_out" [ "internal" ] False "cstring"
>        , CatCreateFunction "interval" [ "reltime" ] False "interval"
>        , CatCreateFunction
>            "interval" [ "int4" , "interval" ] False "interval"
>        , CatCreateFunction "interval" [ "time" ] False "interval"
>        , CatCreateFunction
>            "interval_accum" [ "interval" , "_interval" ] False "_interval"
>        , CatCreateFunction
>            "interval_accum_inv" [ "_interval" , "interval" ] False "_interval"
>        , CatCreateFunction "interval_avg" [ "_interval" ] False "interval"
>        , CatCreateFunction
>            "interval_cmp" [ "interval" , "interval" ] False "int4"
>        , CatCreateFunction
>            "interval_div" [ "float8" , "interval" ] False "interval"
>        , CatCreateFunction
>            "interval_eq" [ "interval" , "interval" ] False "bool"
>        , CatCreateFunction
>            "interval_ge" [ "interval" , "interval" ] False "bool"
>        , CatCreateFunction
>            "interval_gt" [ "interval" , "interval" ] False "bool"
>        , CatCreateFunction "interval_hash" [ "interval" ] False "int4"
>        , CatCreateFunction
>            "interval_in" [ "cstring" , "int4" , "oid" ] False "interval"
>        , CatCreateFunction
>            "interval_larger" [ "interval" , "interval" ] False "interval"
>        , CatCreateFunction
>            "interval_le" [ "interval" , "interval" ] False "bool"
>        , CatCreateFunction
>            "interval_lt" [ "interval" , "interval" ] False "bool"
>        , CatCreateFunction
>            "interval_mi" [ "interval" , "interval" ] False "interval"
>        , CatCreateFunction
>            "interval_mul" [ "float8" , "interval" ] False "interval"
>        , CatCreateFunction
>            "interval_ne" [ "interval" , "interval" ] False "bool"
>        , CatCreateFunction "interval_out" [ "interval" ] False "cstring"
>        , CatCreateFunction
>            "interval_pl" [ "interval" , "interval" ] False "interval"
>        , CatCreateFunction
>            "interval_pl_date" [ "interval" , "date" ] False "timestamp"
>        , CatCreateFunction
>            "interval_pl_time" [ "interval" , "time" ] False "time"
>        , CatCreateFunction
>            "interval_pl_timestamp"
>            [ "timestamp" , "interval" ]
>            False
>            "timestamp"
>        , CatCreateFunction
>            "interval_pl_timestamptz"
>            [ "timestamptz" , "interval" ]
>            False
>            "timestamptz"
>        , CatCreateFunction
>            "interval_pl_timetz" [ "interval" , "timetz" ] False "timetz"
>        , CatCreateFunction
>            "interval_recv" [ "int4" , "oid" , "internal" ] False "interval"
>        , CatCreateFunction "interval_send" [ "interval" ] False "bytea"
>        , CatCreateFunction
>            "interval_smaller" [ "interval" , "interval" ] False "interval"
>        , CatCreateFunction
>            "interval_transform" [ "internal" ] False "internal"
>        , CatCreateFunction "interval_um" [ "interval" ] False "interval"
>        , CatCreateFunction "intervaltypmodin" [ "_cstring" ] False "int4"
>        , CatCreateFunction "intervaltypmodout" [ "int4" ] False "cstring"
>        , CatCreateFunction
>            "intinterval" [ "abstime" , "tinterval" ] False "bool"
>        , CatCreateFunction "isclosed" [ "path" ] False "bool"
>        , CatCreateFunction "isempty" [ "anyrange" ] False "bool"
>        , CatCreateFunction "isfinite" [ "abstime" ] False "bool"
>        , CatCreateFunction "isfinite" [ "date" ] False "bool"
>        , CatCreateFunction "isfinite" [ "timestamptz" ] False "bool"
>        , CatCreateFunction "isfinite" [ "interval" ] False "bool"
>        , CatCreateFunction "isfinite" [ "timestamp" ] False "bool"
>        , CatCreateFunction
>            "ishorizontal" [ "point" , "point" ] False "bool"
>        , CatCreateFunction "ishorizontal" [ "lseg" ] False "bool"
>        , CatCreateFunction "ishorizontal" [ "line" ] False "bool"
>        , CatCreateFunction
>            "iso8859_1_to_utf8"
>            [ "int4" , "internal" , "cstring" , "int4" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "iso8859_to_utf8"
>            [ "int4" , "int4" , "internal" , "int4" , "cstring" ]
>            False
>            "void"
>        , CatCreateFunction
>            "iso_to_koi8r"
>            [ "cstring" , "int4" , "internal" , "int4" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "iso_to_mic"
>            [ "int4" , "int4" , "int4" , "cstring" , "internal" ]
>            False
>            "void"
>        , CatCreateFunction
>            "iso_to_win1251"
>            [ "int4" , "int4" , "internal" , "cstring" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "iso_to_win866"
>            [ "internal" , "int4" , "cstring" , "int4" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction "isopen" [ "path" ] False "bool"
>        , CatCreateFunction "isparallel" [ "lseg" , "lseg" ] False "bool"
>        , CatCreateFunction "isparallel" [ "line" , "line" ] False "bool"
>        , CatCreateFunction "isperp" [ "lseg" , "lseg" ] False "bool"
>        , CatCreateFunction "isperp" [ "line" , "line" ] False "bool"
>        , CatCreateFunction "isvertical" [ "point" , "point" ] False "bool"
>        , CatCreateFunction "isvertical" [ "lseg" ] False "bool"
>        , CatCreateFunction "isvertical" [ "line" ] False "bool"
>        , CatCreateFunction
>            "johab_to_utf8"
>            [ "int4" , "cstring" , "internal" , "int4" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction "json_agg_finalfn" [ "internal" ] False "json"
>        , CatCreateFunction
>            "json_agg_transfn" [ "internal" , "anyelement" ] False "internal"
>        , CatCreateFunction
>            "json_array_element" [ "int4" , "json" ] False "json"
>        , CatCreateFunction
>            "json_array_element_text" [ "int4" , "json" ] False "text"
>        , CatCreateFunction "json_array_elements" [ "json" ] True "json"
>        , CatCreateFunction
>            "json_array_elements_text" [ "json" ] True "text"
>        , CatCreateFunction "json_array_length" [ "json" ] False "int4"
>        , CatCreateFunction "json_each" [ "json" ] True "record"
>        , CatCreateFunction "json_each_text" [ "json" ] True "record"
>        , CatCreateFunction "json_in" [ "cstring" ] False "json"
>        , CatCreateFunction "json_object" [ "_text" ] False "json"
>        , CatCreateFunction
>            "json_object" [ "_text" , "_text" ] False "json"
>        , CatCreateFunction
>            "json_object_agg_finalfn" [ "internal" ] False "json"
>        , CatCreateFunction
>            "json_object_agg_transfn"
>            [ "any" , "internal" , "any" ]
>            False
>            "internal"
>        , CatCreateFunction
>            "json_object_field" [ "text" , "json" ] False "json"
>        , CatCreateFunction
>            "json_object_field_text" [ "json" , "text" ] False "text"
>        , CatCreateFunction "json_object_keys" [ "json" ] True "text"
>        , CatCreateFunction "json_out" [ "json" ] False "cstring"
>        , CatCreateFunction
>            "json_populate_record"
>            [ "json" , "anyelement" , "bool" ]
>            False
>            "anyelement"
>        , CatCreateFunction
>            "json_populate_recordset"
>            [ "anyelement" , "json" , "bool" ]
>            True
>            "anyelement"
>        , CatCreateFunction "json_recv" [ "internal" ] False "json"
>        , CatCreateFunction "json_send" [ "json" ] False "bytea"
>        , CatCreateFunction "json_to_record" [ "json" ] False "record"
>        , CatCreateFunction "json_to_recordset" [ "json" ] True "record"
>        , CatCreateFunction "json_typeof" [ "json" ] False "text"
>        , CatCreateFunction
>            "jsonb_array_element" [ "int4" , "jsonb" ] False "jsonb"
>        , CatCreateFunction
>            "jsonb_array_element_text" [ "int4" , "jsonb" ] False "text"
>        , CatCreateFunction "jsonb_array_elements" [ "jsonb" ] True "jsonb"
>        , CatCreateFunction
>            "jsonb_array_elements_text" [ "jsonb" ] True "text"
>        , CatCreateFunction "jsonb_array_length" [ "jsonb" ] False "int4"
>        , CatCreateFunction "jsonb_cmp" [ "jsonb" , "jsonb" ] False "int4"
>        , CatCreateFunction
>            "jsonb_contained" [ "jsonb" , "jsonb" ] False "bool"
>        , CatCreateFunction
>            "jsonb_contains" [ "jsonb" , "jsonb" ] False "bool"
>        , CatCreateFunction "jsonb_each" [ "jsonb" ] True "record"
>        , CatCreateFunction "jsonb_each_text" [ "jsonb" ] True "record"
>        , CatCreateFunction "jsonb_eq" [ "jsonb" , "jsonb" ] False "bool"
>        , CatCreateFunction
>            "jsonb_exists" [ "jsonb" , "text" ] False "bool"
>        , CatCreateFunction
>            "jsonb_exists_all" [ "_text" , "jsonb" ] False "bool"
>        , CatCreateFunction
>            "jsonb_exists_any" [ "_text" , "jsonb" ] False "bool"
>        , CatCreateFunction "jsonb_ge" [ "jsonb" , "jsonb" ] False "bool"
>        , CatCreateFunction "jsonb_gt" [ "jsonb" , "jsonb" ] False "bool"
>        , CatCreateFunction "jsonb_hash" [ "jsonb" ] False "int4"
>        , CatCreateFunction "jsonb_in" [ "cstring" ] False "jsonb"
>        , CatCreateFunction "jsonb_le" [ "jsonb" , "jsonb" ] False "bool"
>        , CatCreateFunction "jsonb_lt" [ "jsonb" , "jsonb" ] False "bool"
>        , CatCreateFunction "jsonb_ne" [ "jsonb" , "jsonb" ] False "bool"
>        , CatCreateFunction
>            "jsonb_object_field" [ "text" , "jsonb" ] False "jsonb"
>        , CatCreateFunction
>            "jsonb_object_field_text" [ "jsonb" , "text" ] False "text"
>        , CatCreateFunction "jsonb_object_keys" [ "jsonb" ] True "text"
>        , CatCreateFunction "jsonb_out" [ "jsonb" ] False "cstring"
>        , CatCreateFunction
>            "jsonb_populate_record"
>            [ "jsonb" , "anyelement" ]
>            False
>            "anyelement"
>        , CatCreateFunction
>            "jsonb_populate_recordset"
>            [ "anyelement" , "jsonb" ]
>            True
>            "anyelement"
>        , CatCreateFunction "jsonb_recv" [ "internal" ] False "jsonb"
>        , CatCreateFunction "jsonb_send" [ "jsonb" ] False "bytea"
>        , CatCreateFunction "jsonb_to_record" [ "jsonb" ] False "record"
>        , CatCreateFunction "jsonb_to_recordset" [ "jsonb" ] True "record"
>        , CatCreateFunction "jsonb_typeof" [ "jsonb" ] False "text"
>        , CatCreateFunction "justify_days" [ "interval" ] False "interval"
>        , CatCreateFunction "justify_hours" [ "interval" ] False "interval"
>        , CatCreateFunction
>            "justify_interval" [ "interval" ] False "interval"
>        , CatCreateFunction
>            "koi8r_to_iso"
>            [ "int4" , "int4" , "cstring" , "internal" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "koi8r_to_mic"
>            [ "int4" , "int4" , "internal" , "cstring" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "koi8r_to_utf8"
>            [ "int4" , "cstring" , "int4" , "int4" , "internal" ]
>            False
>            "void"
>        , CatCreateFunction
>            "koi8r_to_win1251"
>            [ "int4" , "internal" , "cstring" , "int4" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "koi8r_to_win866"
>            [ "cstring" , "int4" , "int4" , "int4" , "internal" ]
>            False
>            "void"
>        , CatCreateFunction
>            "koi8u_to_utf8"
>            [ "internal" , "int4" , "int4" , "int4" , "cstring" ]
>            False
>            "void"
>        , CatCreateFunction
>            "language_handler_in" [ "cstring" ] False "language_handler"
>        , CatCreateFunction
>            "language_handler_out" [ "language_handler" ] False "cstring"
>        , CatCreateFunction
>            "latin1_to_mic"
>            [ "int4" , "cstring" , "int4" , "internal" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "latin2_to_mic"
>            [ "int4" , "int4" , "cstring" , "int4" , "internal" ]
>            False
>            "void"
>        , CatCreateFunction
>            "latin2_to_win1250"
>            [ "int4" , "int4" , "cstring" , "internal" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "latin3_to_mic"
>            [ "int4" , "int4" , "int4" , "cstring" , "internal" ]
>            False
>            "void"
>        , CatCreateFunction
>            "latin4_to_mic"
>            [ "internal" , "int4" , "int4" , "cstring" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction "left" [ "int4" , "text" ] False "text"
>        , CatCreateFunction "length" [ "text" ] False "int4"
>        , CatCreateFunction "length" [ "bpchar" ] False "int4"
>        , CatCreateFunction "length" [ "lseg" ] False "float8"
>        , CatCreateFunction "length" [ "path" ] False "float8"
>        , CatCreateFunction "length" [ "bit" ] False "int4"
>        , CatCreateFunction "length" [ "bytea" , "name" ] False "int4"
>        , CatCreateFunction "length" [ "bytea" ] False "int4"
>        , CatCreateFunction "length" [ "tsvector" ] False "int4"
>        , CatCreateFunction "like" [ "text" , "text" ] False "bool"
>        , CatCreateFunction "like" [ "text" , "name" ] False "bool"
>        , CatCreateFunction "like" [ "bytea" , "bytea" ] False "bool"
>        , CatCreateFunction "like_escape" [ "text" , "text" ] False "text"
>        , CatCreateFunction
>            "like_escape" [ "bytea" , "bytea" ] False "bytea"
>        , CatCreateFunction
>            "likejoinsel"
>            [ "int2" , "internal" , "internal" , "internal" , "oid" ]
>            False
>            "float8"
>        , CatCreateFunction
>            "likesel"
>            [ "int4" , "oid" , "internal" , "internal" ]
>            False
>            "float8"
>        , CatCreateFunction "line" [ "point" , "point" ] False "line"
>        , CatCreateFunction
>            "line_distance" [ "line" , "line" ] False "float8"
>        , CatCreateFunction "line_eq" [ "line" , "line" ] False "bool"
>        , CatCreateFunction "line_horizontal" [ "line" ] False "bool"
>        , CatCreateFunction "line_in" [ "cstring" ] False "line"
>        , CatCreateFunction
>            "line_interpt" [ "line" , "line" ] False "point"
>        , CatCreateFunction
>            "line_intersect" [ "line" , "line" ] False "bool"
>        , CatCreateFunction "line_out" [ "line" ] False "cstring"
>        , CatCreateFunction
>            "line_parallel" [ "line" , "line" ] False "bool"
>        , CatCreateFunction "line_perp" [ "line" , "line" ] False "bool"
>        , CatCreateFunction "line_recv" [ "internal" ] False "line"
>        , CatCreateFunction "line_send" [ "line" ] False "bytea"
>        , CatCreateFunction "line_vertical" [ "line" ] False "bool"
>        , CatCreateFunction "ln" [ "float8" ] False "float8"
>        , CatCreateFunction "ln" [ "numeric" ] False "numeric"
>        , CatCreateFunction "lo_close" [ "int4" ] False "int4"
>        , CatCreateFunction "lo_creat" [ "int4" ] False "oid"
>        , CatCreateFunction "lo_create" [ "oid" ] False "oid"
>        , CatCreateFunction "lo_export" [ "text" , "oid" ] False "int4"
>        , CatCreateFunction "lo_from_bytea" [ "oid" , "bytea" ] False "oid"
>        , CatCreateFunction "lo_get" [ "oid" ] False "bytea"
>        , CatCreateFunction
>            "lo_get" [ "int8" , "int4" , "oid" ] False "bytea"
>        , CatCreateFunction "lo_import" [ "text" ] False "oid"
>        , CatCreateFunction "lo_import" [ "oid" , "text" ] False "oid"
>        , CatCreateFunction
>            "lo_lseek" [ "int4" , "int4" , "int4" ] False "int4"
>        , CatCreateFunction
>            "lo_lseek64" [ "int4" , "int8" , "int4" ] False "int8"
>        , CatCreateFunction "lo_open" [ "int4" , "oid" ] False "int4"
>        , CatCreateFunction
>            "lo_put" [ "bytea" , "oid" , "int8" ] False "void"
>        , CatCreateFunction "lo_tell" [ "int4" ] False "int4"
>        , CatCreateFunction "lo_tell64" [ "int4" ] False "int8"
>        , CatCreateFunction "lo_truncate" [ "int4" , "int4" ] False "int4"
>        , CatCreateFunction
>            "lo_truncate64" [ "int4" , "int8" ] False "int4"
>        , CatCreateFunction "lo_unlink" [ "oid" ] False "int4"
>        , CatCreateFunction "log" [ "float8" ] False "float8"
>        , CatCreateFunction "log" [ "numeric" , "numeric" ] False "numeric"
>        , CatCreateFunction "log" [ "numeric" ] False "numeric"
>        , CatCreateFunction "loread" [ "int4" , "int4" ] False "bytea"
>        , CatCreateFunction "lower" [ "text" ] False "text"
>        , CatCreateFunction "lower" [ "anyrange" ] False "anyelement"
>        , CatCreateFunction "lower_inc" [ "anyrange" ] False "bool"
>        , CatCreateFunction "lower_inf" [ "anyrange" ] False "bool"
>        , CatCreateFunction "lowrite" [ "bytea" , "int4" ] False "int4"
>        , CatCreateFunction
>            "lpad" [ "int4" , "text" , "text" ] False "text"
>        , CatCreateFunction "lpad" [ "text" , "int4" ] False "text"
>        , CatCreateFunction "lseg" [ "point" , "point" ] False "lseg"
>        , CatCreateFunction "lseg" [ "box" ] False "lseg"
>        , CatCreateFunction "lseg_center" [ "lseg" ] False "point"
>        , CatCreateFunction
>            "lseg_distance" [ "lseg" , "lseg" ] False "float8"
>        , CatCreateFunction "lseg_eq" [ "lseg" , "lseg" ] False "bool"
>        , CatCreateFunction "lseg_ge" [ "lseg" , "lseg" ] False "bool"
>        , CatCreateFunction "lseg_gt" [ "lseg" , "lseg" ] False "bool"
>        , CatCreateFunction "lseg_horizontal" [ "lseg" ] False "bool"
>        , CatCreateFunction "lseg_in" [ "cstring" ] False "lseg"
>        , CatCreateFunction
>            "lseg_interpt" [ "lseg" , "lseg" ] False "point"
>        , CatCreateFunction
>            "lseg_intersect" [ "lseg" , "lseg" ] False "bool"
>        , CatCreateFunction "lseg_le" [ "lseg" , "lseg" ] False "bool"
>        , CatCreateFunction "lseg_length" [ "lseg" ] False "float8"
>        , CatCreateFunction "lseg_lt" [ "lseg" , "lseg" ] False "bool"
>        , CatCreateFunction "lseg_ne" [ "lseg" , "lseg" ] False "bool"
>        , CatCreateFunction "lseg_out" [ "lseg" ] False "cstring"
>        , CatCreateFunction
>            "lseg_parallel" [ "lseg" , "lseg" ] False "bool"
>        , CatCreateFunction "lseg_perp" [ "lseg" , "lseg" ] False "bool"
>        , CatCreateFunction "lseg_recv" [ "internal" ] False "lseg"
>        , CatCreateFunction "lseg_send" [ "lseg" ] False "bytea"
>        , CatCreateFunction "lseg_vertical" [ "lseg" ] False "bool"
>        , CatCreateFunction "ltrim" [ "text" , "text" ] False "text"
>        , CatCreateFunction "ltrim" [ "text" ] False "text"
>        , CatCreateFunction
>            "macaddr_and" [ "macaddr" , "macaddr" ] False "macaddr"
>        , CatCreateFunction
>            "macaddr_cmp" [ "macaddr" , "macaddr" ] False "int4"
>        , CatCreateFunction
>            "macaddr_eq" [ "macaddr" , "macaddr" ] False "bool"
>        , CatCreateFunction
>            "macaddr_ge" [ "macaddr" , "macaddr" ] False "bool"
>        , CatCreateFunction
>            "macaddr_gt" [ "macaddr" , "macaddr" ] False "bool"
>        , CatCreateFunction "macaddr_in" [ "cstring" ] False "macaddr"
>        , CatCreateFunction
>            "macaddr_le" [ "macaddr" , "macaddr" ] False "bool"
>        , CatCreateFunction
>            "macaddr_lt" [ "macaddr" , "macaddr" ] False "bool"
>        , CatCreateFunction
>            "macaddr_ne" [ "macaddr" , "macaddr" ] False "bool"
>        , CatCreateFunction "macaddr_not" [ "macaddr" ] False "macaddr"
>        , CatCreateFunction
>            "macaddr_or" [ "macaddr" , "macaddr" ] False "macaddr"
>        , CatCreateFunction "macaddr_out" [ "macaddr" ] False "cstring"
>        , CatCreateFunction "macaddr_recv" [ "internal" ] False "macaddr"
>        , CatCreateFunction "macaddr_send" [ "macaddr" ] False "bytea"
>        , CatCreateFunction
>            "make_date" [ "int4" , "int4" , "int4" ] False "date"
>        , CatCreateFunction
>            "make_interval"
>            [ "int4" , "int4" , "float8" , "int4" , "int4" , "int4" , "int4" ]
>            False
>            "interval"
>        , CatCreateFunction
>            "make_time" [ "int4" , "float8" , "int4" ] False "time"
>        , CatCreateFunction
>            "make_timestamp"
>            [ "float8" , "int4" , "int4" , "int4" , "int4" , "int4" ]
>            False
>            "timestamp"
>        , CatCreateFunction
>            "make_timestamptz"
>            [ "int4" , "int4" , "float8" , "int4" , "int4" , "int4" ]
>            False
>            "timestamptz"
>        , CatCreateFunction
>            "make_timestamptz"
>            [ "float8" , "int4" , "int4" , "int4" , "int4" , "int4" , "text" ]
>            False
>            "timestamptz"
>        , CatCreateFunction
>            "makeaclitem" [ "oid" , "oid" , "text" , "bool" ] False "aclitem"
>        , CatCreateFunction "masklen" [ "inet" ] False "int4"
>        , CatCreateFunction "md5" [ "text" ] False "text"
>        , CatCreateFunction "md5" [ "bytea" ] False "text"
>        , CatCreateFunction
>            "mic_to_ascii"
>            [ "int4" , "int4" , "internal" , "cstring" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "mic_to_big5"
>            [ "int4" , "int4" , "int4" , "cstring" , "internal" ]
>            False
>            "void"
>        , CatCreateFunction
>            "mic_to_euc_cn"
>            [ "int4" , "int4" , "internal" , "int4" , "cstring" ]
>            False
>            "void"
>        , CatCreateFunction
>            "mic_to_euc_jp"
>            [ "int4" , "int4" , "cstring" , "int4" , "internal" ]
>            False
>            "void"
>        , CatCreateFunction
>            "mic_to_euc_kr"
>            [ "cstring" , "int4" , "int4" , "internal" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "mic_to_euc_tw"
>            [ "int4" , "internal" , "int4" , "int4" , "cstring" ]
>            False
>            "void"
>        , CatCreateFunction
>            "mic_to_iso"
>            [ "int4" , "cstring" , "internal" , "int4" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "mic_to_koi8r"
>            [ "int4" , "cstring" , "internal" , "int4" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "mic_to_latin1"
>            [ "cstring" , "int4" , "int4" , "int4" , "internal" ]
>            False
>            "void"
>        , CatCreateFunction
>            "mic_to_latin2"
>            [ "int4" , "internal" , "int4" , "int4" , "cstring" ]
>            False
>            "void"
>        , CatCreateFunction
>            "mic_to_latin3"
>            [ "internal" , "int4" , "int4" , "int4" , "cstring" ]
>            False
>            "void"
>        , CatCreateFunction
>            "mic_to_latin4"
>            [ "int4" , "internal" , "cstring" , "int4" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "mic_to_sjis"
>            [ "cstring" , "int4" , "int4" , "int4" , "internal" ]
>            False
>            "void"
>        , CatCreateFunction
>            "mic_to_win1250"
>            [ "int4" , "int4" , "internal" , "cstring" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "mic_to_win1251"
>            [ "int4" , "int4" , "internal" , "cstring" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "mic_to_win866"
>            [ "int4" , "cstring" , "int4" , "internal" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "mktinterval" [ "abstime" , "abstime" ] False "tinterval"
>        , CatCreateFunction "mod" [ "int2" , "int2" ] False "int2"
>        , CatCreateFunction "mod" [ "int4" , "int4" ] False "int4"
>        , CatCreateFunction "mod" [ "int8" , "int8" ] False "int8"
>        , CatCreateFunction "mod" [ "numeric" , "numeric" ] False "numeric"
>        , CatCreateFunction
>            "mode_final" [ "internal" , "anyelement" ] False "anyelement"
>        , CatCreateFunction "money" [ "int4" ] False "money"
>        , CatCreateFunction "money" [ "int8" ] False "money"
>        , CatCreateFunction "money" [ "numeric" ] False "money"
>        , CatCreateFunction
>            "mul_d_interval" [ "interval" , "float8" ] False "interval"
>        , CatCreateFunction "name" [ "text" ] False "name"
>        , CatCreateFunction "name" [ "bpchar" ] False "name"
>        , CatCreateFunction "name" [ "varchar" ] False "name"
>        , CatCreateFunction "nameeq" [ "name" , "name" ] False "bool"
>        , CatCreateFunction "namege" [ "name" , "name" ] False "bool"
>        , CatCreateFunction "namegt" [ "name" , "name" ] False "bool"
>        , CatCreateFunction "nameiclike" [ "text" , "name" ] False "bool"
>        , CatCreateFunction "nameicnlike" [ "name" , "text" ] False "bool"
>        , CatCreateFunction
>            "nameicregexeq" [ "name" , "text" ] False "bool"
>        , CatCreateFunction
>            "nameicregexne" [ "text" , "name" ] False "bool"
>        , CatCreateFunction "namein" [ "cstring" ] False "name"
>        , CatCreateFunction "namele" [ "name" , "name" ] False "bool"
>        , CatCreateFunction "namelike" [ "name" , "text" ] False "bool"
>        , CatCreateFunction "namelt" [ "name" , "name" ] False "bool"
>        , CatCreateFunction "namene" [ "name" , "name" ] False "bool"
>        , CatCreateFunction "namenlike" [ "name" , "text" ] False "bool"
>        , CatCreateFunction "nameout" [ "name" ] False "cstring"
>        , CatCreateFunction "namerecv" [ "internal" ] False "name"
>        , CatCreateFunction "nameregexeq" [ "name" , "text" ] False "bool"
>        , CatCreateFunction "nameregexne" [ "text" , "name" ] False "bool"
>        , CatCreateFunction "namesend" [ "name" ] False "bytea"
>        , CatCreateFunction
>            "neqjoinsel"
>            [ "internal" , "int2" , "internal" , "oid" , "internal" ]
>            False
>            "float8"
>        , CatCreateFunction
>            "neqsel"
>            [ "int4" , "oid" , "internal" , "internal" ]
>            False
>            "float8"
>        , CatCreateFunction "netmask" [ "inet" ] False "inet"
>        , CatCreateFunction "network" [ "inet" ] False "cidr"
>        , CatCreateFunction "network_cmp" [ "inet" , "inet" ] False "int4"
>        , CatCreateFunction "network_eq" [ "inet" , "inet" ] False "bool"
>        , CatCreateFunction "network_ge" [ "inet" , "inet" ] False "bool"
>        , CatCreateFunction "network_gt" [ "inet" , "inet" ] False "bool"
>        , CatCreateFunction "network_le" [ "inet" , "inet" ] False "bool"
>        , CatCreateFunction "network_lt" [ "inet" , "inet" ] False "bool"
>        , CatCreateFunction "network_ne" [ "inet" , "inet" ] False "bool"
>        , CatCreateFunction
>            "network_overlap" [ "inet" , "inet" ] False "bool"
>        , CatCreateFunction "network_sub" [ "inet" , "inet" ] False "bool"
>        , CatCreateFunction
>            "network_subeq" [ "inet" , "inet" ] False "bool"
>        , CatCreateFunction "network_sup" [ "inet" , "inet" ] False "bool"
>        , CatCreateFunction
>            "network_supeq" [ "inet" , "inet" ] False "bool"
>        , CatCreateFunction
>            "networkjoinsel"
>            [ "int2" , "internal" , "internal" , "oid" , "internal" ]
>            False
>            "float8"
>        , CatCreateFunction
>            "networksel"
>            [ "internal" , "int4" , "internal" , "oid" ]
>            False
>            "float8"
>        , CatCreateFunction "nextval" [ "regclass" ] False "int8"
>        , CatCreateFunction
>            "nlikejoinsel"
>            [ "int2" , "internal" , "oid" , "internal" , "internal" ]
>            False
>            "float8"
>        , CatCreateFunction
>            "nlikesel"
>            [ "internal" , "internal" , "int4" , "oid" ]
>            False
>            "float8"
>        , CatCreateFunction "notlike" [ "text" , "text" ] False "bool"
>        , CatCreateFunction "notlike" [ "text" , "name" ] False "bool"
>        , CatCreateFunction "notlike" [ "bytea" , "bytea" ] False "bool"
>        , CatCreateFunction "npoints" [ "path" ] False "int4"
>        , CatCreateFunction "npoints" [ "polygon" ] False "int4"
>        , CatCreateFunction
>            "numeric" [ "int4" , "numeric" ] False "numeric"
>        , CatCreateFunction "numeric" [ "int4" ] False "numeric"
>        , CatCreateFunction "numeric" [ "float4" ] False "numeric"
>        , CatCreateFunction "numeric" [ "float8" ] False "numeric"
>        , CatCreateFunction "numeric" [ "int8" ] False "numeric"
>        , CatCreateFunction "numeric" [ "int2" ] False "numeric"
>        , CatCreateFunction "numeric" [ "money" ] False "numeric"
>        , CatCreateFunction "numeric_abs" [ "numeric" ] False "numeric"
>        , CatCreateFunction
>            "numeric_accum" [ "internal" , "numeric" ] False "internal"
>        , CatCreateFunction
>            "numeric_accum_inv" [ "numeric" , "internal" ] False "internal"
>        , CatCreateFunction
>            "numeric_add" [ "numeric" , "numeric" ] False "numeric"
>        , CatCreateFunction "numeric_avg" [ "internal" ] False "numeric"
>        , CatCreateFunction
>            "numeric_avg_accum" [ "internal" , "numeric" ] False "internal"
>        , CatCreateFunction
>            "numeric_cmp" [ "numeric" , "numeric" ] False "int4"
>        , CatCreateFunction
>            "numeric_div" [ "numeric" , "numeric" ] False "numeric"
>        , CatCreateFunction
>            "numeric_div_trunc" [ "numeric" , "numeric" ] False "numeric"
>        , CatCreateFunction
>            "numeric_eq" [ "numeric" , "numeric" ] False "bool"
>        , CatCreateFunction "numeric_exp" [ "numeric" ] False "numeric"
>        , CatCreateFunction "numeric_fac" [ "int8" ] False "numeric"
>        , CatCreateFunction
>            "numeric_ge" [ "numeric" , "numeric" ] False "bool"
>        , CatCreateFunction
>            "numeric_gt" [ "numeric" , "numeric" ] False "bool"
>        , CatCreateFunction
>            "numeric_in" [ "cstring" , "oid" , "int4" ] False "numeric"
>        , CatCreateFunction "numeric_inc" [ "numeric" ] False "numeric"
>        , CatCreateFunction
>            "numeric_larger" [ "numeric" , "numeric" ] False "numeric"
>        , CatCreateFunction
>            "numeric_le" [ "numeric" , "numeric" ] False "bool"
>        , CatCreateFunction "numeric_ln" [ "numeric" ] False "numeric"
>        , CatCreateFunction
>            "numeric_log" [ "numeric" , "numeric" ] False "numeric"
>        , CatCreateFunction
>            "numeric_lt" [ "numeric" , "numeric" ] False "bool"
>        , CatCreateFunction
>            "numeric_mod" [ "numeric" , "numeric" ] False "numeric"
>        , CatCreateFunction
>            "numeric_mul" [ "numeric" , "numeric" ] False "numeric"
>        , CatCreateFunction
>            "numeric_ne" [ "numeric" , "numeric" ] False "bool"
>        , CatCreateFunction "numeric_out" [ "numeric" ] False "cstring"
>        , CatCreateFunction
>            "numeric_power" [ "numeric" , "numeric" ] False "numeric"
>        , CatCreateFunction
>            "numeric_recv" [ "internal" , "oid" , "int4" ] False "numeric"
>        , CatCreateFunction "numeric_send" [ "numeric" ] False "bytea"
>        , CatCreateFunction
>            "numeric_smaller" [ "numeric" , "numeric" ] False "numeric"
>        , CatCreateFunction "numeric_sqrt" [ "numeric" ] False "numeric"
>        , CatCreateFunction
>            "numeric_stddev_pop" [ "internal" ] False "numeric"
>        , CatCreateFunction
>            "numeric_stddev_samp" [ "internal" ] False "numeric"
>        , CatCreateFunction
>            "numeric_sub" [ "numeric" , "numeric" ] False "numeric"
>        , CatCreateFunction "numeric_sum" [ "internal" ] False "numeric"
>        , CatCreateFunction
>            "numeric_transform" [ "internal" ] False "internal"
>        , CatCreateFunction "numeric_uminus" [ "numeric" ] False "numeric"
>        , CatCreateFunction "numeric_uplus" [ "numeric" ] False "numeric"
>        , CatCreateFunction
>            "numeric_var_pop" [ "internal" ] False "numeric"
>        , CatCreateFunction
>            "numeric_var_samp" [ "internal" ] False "numeric"
>        , CatCreateFunction "numerictypmodin" [ "_cstring" ] False "int4"
>        , CatCreateFunction "numerictypmodout" [ "int4" ] False "cstring"
>        , CatCreateFunction "numnode" [ "tsquery" ] False "int4"
>        , CatCreateFunction
>            "numrange" [ "numeric" , "numeric" ] False "numrange"
>        , CatCreateFunction
>            "numrange" [ "text" , "numeric" , "numeric" ] False "numrange"
>        , CatCreateFunction
>            "numrange_subdiff" [ "numeric" , "numeric" ] False "float8"
>        , CatCreateFunction
>            "obj_description" [ "name" , "oid" ] False "text"
>        , CatCreateFunction "obj_description" [ "oid" ] False "text"
>        , CatCreateFunction "octet_length" [ "bytea" ] False "int4"
>        , CatCreateFunction "octet_length" [ "text" ] False "int4"
>        , CatCreateFunction "octet_length" [ "bpchar" ] False "int4"
>        , CatCreateFunction "octet_length" [ "bit" ] False "int4"
>        , CatCreateFunction "oid" [ "int8" ] False "oid"
>        , CatCreateFunction "oideq" [ "oid" , "oid" ] False "bool"
>        , CatCreateFunction "oidge" [ "oid" , "oid" ] False "bool"
>        , CatCreateFunction "oidgt" [ "oid" , "oid" ] False "bool"
>        , CatCreateFunction "oidin" [ "cstring" ] False "oid"
>        , CatCreateFunction "oidlarger" [ "oid" , "oid" ] False "oid"
>        , CatCreateFunction "oidle" [ "oid" , "oid" ] False "bool"
>        , CatCreateFunction "oidlt" [ "oid" , "oid" ] False "bool"
>        , CatCreateFunction "oidne" [ "oid" , "oid" ] False "bool"
>        , CatCreateFunction "oidout" [ "oid" ] False "cstring"
>        , CatCreateFunction "oidrecv" [ "internal" ] False "oid"
>        , CatCreateFunction "oidsend" [ "oid" ] False "bytea"
>        , CatCreateFunction "oidsmaller" [ "oid" , "oid" ] False "oid"
>        , CatCreateFunction
>            "oidvectoreq" [ "oidvector" , "oidvector" ] False "bool"
>        , CatCreateFunction
>            "oidvectorge" [ "oidvector" , "oidvector" ] False "bool"
>        , CatCreateFunction
>            "oidvectorgt" [ "oidvector" , "oidvector" ] False "bool"
>        , CatCreateFunction "oidvectorin" [ "cstring" ] False "oidvector"
>        , CatCreateFunction
>            "oidvectorle" [ "oidvector" , "oidvector" ] False "bool"
>        , CatCreateFunction
>            "oidvectorlt" [ "oidvector" , "oidvector" ] False "bool"
>        , CatCreateFunction
>            "oidvectorne" [ "oidvector" , "oidvector" ] False "bool"
>        , CatCreateFunction "oidvectorout" [ "oidvector" ] False "cstring"
>        , CatCreateFunction
>            "oidvectorrecv" [ "internal" ] False "oidvector"
>        , CatCreateFunction "oidvectorsend" [ "oidvector" ] False "bytea"
>        , CatCreateFunction "oidvectortypes" [ "oidvector" ] False "text"
>        , CatCreateFunction "on_pb" [ "box" , "point" ] False "bool"
>        , CatCreateFunction "on_pl" [ "point" , "line" ] False "bool"
>        , CatCreateFunction "on_ppath" [ "path" , "point" ] False "bool"
>        , CatCreateFunction "on_ps" [ "lseg" , "point" ] False "bool"
>        , CatCreateFunction "on_sb" [ "box" , "lseg" ] False "bool"
>        , CatCreateFunction "on_sl" [ "lseg" , "line" ] False "bool"
>        , CatCreateFunction "opaque_in" [ "cstring" ] False "opaque"
>        , CatCreateFunction "opaque_out" [ "opaque" ] False "cstring"
>        , CatCreateFunction
>            "ordered_set_transition" [ "internal" , "any" ] False "internal"
>        , CatCreateFunction
>            "overlaps"
>            [ "timetz" , "timetz" , "timetz" , "timetz" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "overlaps"
>            [ "timestamptz" , "timestamptz" , "timestamptz" , "timestamptz" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "overlaps"
>            [ "interval" , "timestamptz" , "interval" , "timestamptz" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "overlaps"
>            [ "interval" , "timestamptz" , "timestamptz" , "timestamptz" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "overlaps"
>            [ "timestamptz" , "timestamptz" , "interval" , "timestamptz" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "overlaps" [ "time" , "time" , "time" , "time" ] False "bool"
>        , CatCreateFunction
>            "overlaps"
>            [ "time" , "interval" , "time" , "interval" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "overlaps" [ "interval" , "time" , "time" , "time" ] False "bool"
>        , CatCreateFunction
>            "overlaps" [ "time" , "time" , "interval" , "time" ] False "bool"
>        , CatCreateFunction
>            "overlaps"
>            [ "timestamp" , "timestamp" , "timestamp" , "timestamp" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "overlaps"
>            [ "timestamp" , "interval" , "timestamp" , "interval" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "overlaps"
>            [ "timestamp" , "timestamp" , "timestamp" , "interval" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "overlaps"
>            [ "timestamp" , "interval" , "timestamp" , "timestamp" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "overlay" [ "bytea" , "bytea" , "int4" , "int4" ] False "bytea"
>        , CatCreateFunction
>            "overlay" [ "int4" , "bytea" , "bytea" ] False "bytea"
>        , CatCreateFunction
>            "overlay" [ "int4" , "int4" , "text" , "text" ] False "text"
>        , CatCreateFunction
>            "overlay" [ "text" , "text" , "int4" ] False "text"
>        , CatCreateFunction
>            "overlay" [ "bit" , "int4" , "int4" , "bit" ] False "bit"
>        , CatCreateFunction
>            "overlay" [ "int4" , "bit" , "bit" ] False "bit"
>        , CatCreateFunction "path" [ "polygon" ] False "path"
>        , CatCreateFunction "path_add" [ "path" , "path" ] False "path"
>        , CatCreateFunction "path_add_pt" [ "path" , "point" ] False "path"
>        , CatCreateFunction "path_center" [ "path" ] False "point"
>        , CatCreateFunction
>            "path_contain_pt" [ "point" , "path" ] False "bool"
>        , CatCreateFunction
>            "path_distance" [ "path" , "path" ] False "float8"
>        , CatCreateFunction "path_div_pt" [ "point" , "path" ] False "path"
>        , CatCreateFunction "path_in" [ "cstring" ] False "path"
>        , CatCreateFunction "path_inter" [ "path" , "path" ] False "bool"
>        , CatCreateFunction "path_length" [ "path" ] False "float8"
>        , CatCreateFunction "path_mul_pt" [ "path" , "point" ] False "path"
>        , CatCreateFunction "path_n_eq" [ "path" , "path" ] False "bool"
>        , CatCreateFunction "path_n_ge" [ "path" , "path" ] False "bool"
>        , CatCreateFunction "path_n_gt" [ "path" , "path" ] False "bool"
>        , CatCreateFunction "path_n_le" [ "path" , "path" ] False "bool"
>        , CatCreateFunction "path_n_lt" [ "path" , "path" ] False "bool"
>        , CatCreateFunction "path_npoints" [ "path" ] False "int4"
>        , CatCreateFunction "path_out" [ "path" ] False "cstring"
>        , CatCreateFunction "path_recv" [ "internal" ] False "path"
>        , CatCreateFunction "path_send" [ "path" ] False "bytea"
>        , CatCreateFunction "path_sub_pt" [ "path" , "point" ] False "path"
>        , CatCreateFunction "pclose" [ "path" ] False "path"
>        , CatCreateFunction
>            "percentile_cont_float8_final"
>            [ "internal" , "float8" ]
>            False
>            "float8"
>        , CatCreateFunction
>            "percentile_cont_float8_multi_final"
>            [ "_float8" , "internal" ]
>            False
>            "_float8"
>        , CatCreateFunction
>            "percentile_cont_interval_final"
>            [ "internal" , "float8" ]
>            False
>            "interval"
>        , CatCreateFunction
>            "percentile_cont_interval_multi_final"
>            [ "_float8" , "internal" ]
>            False
>            "_interval"
>        , CatCreateFunction
>            "percentile_disc_final"
>            [ "float8" , "anyelement" , "internal" ]
>            False
>            "anyelement"
>        , CatCreateFunction
>            "percentile_disc_multi_final"
>            [ "internal" , "_float8" , "anyelement" ]
>            False
>            "anyarray"
>        , CatCreateFunction "pg_advisory_lock" [ "int8" ] False "void"
>        , CatCreateFunction
>            "pg_advisory_lock" [ "int4" , "int4" ] False "void"
>        , CatCreateFunction
>            "pg_advisory_lock_shared" [ "int8" ] False "void"
>        , CatCreateFunction
>            "pg_advisory_lock_shared" [ "int4" , "int4" ] False "void"
>        , CatCreateFunction "pg_advisory_unlock" [ "int8" ] False "bool"
>        , CatCreateFunction
>            "pg_advisory_unlock" [ "int4" , "int4" ] False "bool"
>        , CatCreateFunction
>            "pg_advisory_unlock_shared" [ "int8" ] False "bool"
>        , CatCreateFunction
>            "pg_advisory_unlock_shared" [ "int4" , "int4" ] False "bool"
>        , CatCreateFunction "pg_advisory_xact_lock" [ "int8" ] False "void"
>        , CatCreateFunction
>            "pg_advisory_xact_lock" [ "int4" , "int4" ] False "void"
>        , CatCreateFunction
>            "pg_advisory_xact_lock_shared" [ "int8" ] False "void"
>        , CatCreateFunction
>            "pg_advisory_xact_lock_shared" [ "int4" , "int4" ] False "void"
>        , CatCreateFunction "pg_cancel_backend" [ "int4" ] False "bool"
>        , CatCreateFunction "pg_char_to_encoding" [ "name" ] False "int4"
>        , CatCreateFunction "pg_collation_for" [ "any" ] False "text"
>        , CatCreateFunction
>            "pg_collation_is_visible" [ "oid" ] False "bool"
>        , CatCreateFunction
>            "pg_column_is_updatable"
>            [ "regclass" , "int2" , "bool" ]
>            False
>            "bool"
>        , CatCreateFunction "pg_column_size" [ "any" ] False "int4"
>        , CatCreateFunction
>            "pg_conversion_is_visible" [ "oid" ] False "bool"
>        , CatCreateFunction
>            "pg_create_logical_replication_slot"
>            [ "name" , "name" ]
>            False
>            "record"
>        , CatCreateFunction
>            "pg_create_physical_replication_slot" [ "name" ] False "record"
>        , CatCreateFunction
>            "pg_create_restore_point" [ "text" ] False "pg_lsn"
>        , CatCreateFunction "pg_database_size" [ "name" ] False "int8"
>        , CatCreateFunction "pg_database_size" [ "oid" ] False "int8"
>        , CatCreateFunction
>            "pg_describe_object" [ "oid" , "oid" , "int4" ] False "text"
>        , CatCreateFunction
>            "pg_drop_replication_slot" [ "name" ] False "void"
>        , CatCreateFunction
>            "pg_encoding_max_length" [ "int4" ] False "int4"
>        , CatCreateFunction "pg_encoding_to_char" [ "int4" ] False "name"
>        , CatCreateFunction
>            "pg_extension_config_dump" [ "regclass" , "text" ] False "void"
>        , CatCreateFunction
>            "pg_extension_update_paths" [ "name" ] True "record"
>        , CatCreateFunction
>            "pg_filenode_relation" [ "oid" , "oid" ] False "regclass"
>        , CatCreateFunction "pg_function_is_visible" [ "oid" ] False "bool"
>        , CatCreateFunction "pg_get_constraintdef" [ "oid" ] False "text"
>        , CatCreateFunction
>            "pg_get_constraintdef" [ "oid" , "bool" ] False "text"
>        , CatCreateFunction
>            "pg_get_expr" [ "oid" , "pg_node_tree" ] False "text"
>        , CatCreateFunction
>            "pg_get_expr" [ "oid" , "bool" , "pg_node_tree" ] False "text"
>        , CatCreateFunction
>            "pg_get_function_arg_default" [ "oid" , "int4" ] False "text"
>        , CatCreateFunction
>            "pg_get_function_arguments" [ "oid" ] False "text"
>        , CatCreateFunction
>            "pg_get_function_identity_arguments" [ "oid" ] False "text"
>        , CatCreateFunction "pg_get_function_result" [ "oid" ] False "text"
>        , CatCreateFunction "pg_get_functiondef" [ "oid" ] False "text"
>        , CatCreateFunction "pg_get_indexdef" [ "oid" ] False "text"
>        , CatCreateFunction
>            "pg_get_indexdef" [ "oid" , "int4" , "bool" ] False "text"
>        , CatCreateFunction
>            "pg_get_multixact_members" [ "xid" ] True "record"
>        , CatCreateFunction "pg_get_ruledef" [ "oid" ] False "text"
>        , CatCreateFunction
>            "pg_get_ruledef" [ "oid" , "bool" ] False "text"
>        , CatCreateFunction
>            "pg_get_serial_sequence" [ "text" , "text" ] False "text"
>        , CatCreateFunction "pg_get_triggerdef" [ "oid" ] False "text"
>        , CatCreateFunction
>            "pg_get_triggerdef" [ "bool" , "oid" ] False "text"
>        , CatCreateFunction "pg_get_userbyid" [ "oid" ] False "name"
>        , CatCreateFunction "pg_get_viewdef" [ "text" ] False "text"
>        , CatCreateFunction "pg_get_viewdef" [ "oid" ] False "text"
>        , CatCreateFunction
>            "pg_get_viewdef" [ "bool" , "text" ] False "text"
>        , CatCreateFunction
>            "pg_get_viewdef" [ "oid" , "bool" ] False "text"
>        , CatCreateFunction
>            "pg_get_viewdef" [ "int4" , "oid" ] False "text"
>        , CatCreateFunction
>            "pg_has_role" [ "text" , "name" , "name" ] False "bool"
>        , CatCreateFunction
>            "pg_has_role" [ "text" , "name" , "oid" ] False "bool"
>        , CatCreateFunction
>            "pg_has_role" [ "oid" , "name" , "text" ] False "bool"
>        , CatCreateFunction
>            "pg_has_role" [ "oid" , "oid" , "text" ] False "bool"
>        , CatCreateFunction "pg_has_role" [ "text" , "name" ] False "bool"
>        , CatCreateFunction "pg_has_role" [ "oid" , "text" ] False "bool"
>        , CatCreateFunction
>            "pg_identify_object" [ "oid" , "int4" , "oid" ] False "record"
>        , CatCreateFunction "pg_indexes_size" [ "regclass" ] False "int8"
>        , CatCreateFunction
>            "pg_is_other_temp_schema" [ "oid" ] False "bool"
>        , CatCreateFunction "pg_ls_dir" [ "text" ] True "text"
>        , CatCreateFunction
>            "pg_lsn_cmp" [ "pg_lsn" , "pg_lsn" ] False "int4"
>        , CatCreateFunction
>            "pg_lsn_eq" [ "pg_lsn" , "pg_lsn" ] False "bool"
>        , CatCreateFunction
>            "pg_lsn_ge" [ "pg_lsn" , "pg_lsn" ] False "bool"
>        , CatCreateFunction
>            "pg_lsn_gt" [ "pg_lsn" , "pg_lsn" ] False "bool"
>        , CatCreateFunction "pg_lsn_hash" [ "pg_lsn" ] False "int4"
>        , CatCreateFunction "pg_lsn_in" [ "cstring" ] False "pg_lsn"
>        , CatCreateFunction
>            "pg_lsn_le" [ "pg_lsn" , "pg_lsn" ] False "bool"
>        , CatCreateFunction
>            "pg_lsn_lt" [ "pg_lsn" , "pg_lsn" ] False "bool"
>        , CatCreateFunction
>            "pg_lsn_mi" [ "pg_lsn" , "pg_lsn" ] False "numeric"
>        , CatCreateFunction
>            "pg_lsn_ne" [ "pg_lsn" , "pg_lsn" ] False "bool"
>        , CatCreateFunction "pg_lsn_out" [ "pg_lsn" ] False "cstring"
>        , CatCreateFunction "pg_lsn_recv" [ "internal" ] False "pg_lsn"
>        , CatCreateFunction "pg_lsn_send" [ "pg_lsn" ] False "bytea"
>        , CatCreateFunction
>            "pg_node_tree_in" [ "cstring" ] False "pg_node_tree"
>        , CatCreateFunction
>            "pg_node_tree_out" [ "pg_node_tree" ] False "cstring"
>        , CatCreateFunction
>            "pg_node_tree_recv" [ "internal" ] False "pg_node_tree"
>        , CatCreateFunction
>            "pg_node_tree_send" [ "pg_node_tree" ] False "bytea"
>        , CatCreateFunction "pg_notify" [ "text" , "text" ] False "void"
>        , CatCreateFunction "pg_opclass_is_visible" [ "oid" ] False "bool"
>        , CatCreateFunction "pg_operator_is_visible" [ "oid" ] False "bool"
>        , CatCreateFunction "pg_opfamily_is_visible" [ "oid" ] False "bool"
>        , CatCreateFunction "pg_options_to_table" [ "_text" ] True "record"
>        , CatCreateFunction
>            "pg_read_binary_file" [ "int8" , "text" , "int8" ] False "bytea"
>        , CatCreateFunction "pg_read_binary_file" [ "text" ] False "bytea"
>        , CatCreateFunction
>            "pg_read_file" [ "text" , "int8" , "int8" ] False "text"
>        , CatCreateFunction "pg_read_file" [ "text" ] False "text"
>        , CatCreateFunction
>            "pg_relation_filenode" [ "regclass" ] False "oid"
>        , CatCreateFunction
>            "pg_relation_filepath" [ "regclass" ] False "text"
>        , CatCreateFunction
>            "pg_relation_is_updatable" [ "bool" , "regclass" ] False "int4"
>        , CatCreateFunction "pg_relation_size" [ "regclass" ] False "int8"
>        , CatCreateFunction
>            "pg_relation_size" [ "regclass" , "text" ] False "int8"
>        , CatCreateFunction
>            "pg_sequence_parameters" [ "oid" ] False "record"
>        , CatCreateFunction "pg_size_pretty" [ "int8" ] False "text"
>        , CatCreateFunction "pg_size_pretty" [ "numeric" ] False "text"
>        , CatCreateFunction "pg_sleep" [ "float8" ] False "void"
>        , CatCreateFunction "pg_sleep_for" [ "interval" ] False "void"
>        , CatCreateFunction "pg_sleep_until" [ "timestamptz" ] False "void"
>        , CatCreateFunction
>            "pg_start_backup" [ "bool" , "text" ] False "pg_lsn"
>        , CatCreateFunction "pg_stat_file" [ "text" ] False "record"
>        , CatCreateFunction "pg_stat_get_activity" [ "int4" ] True "record"
>        , CatCreateFunction
>            "pg_stat_get_analyze_count" [ "oid" ] False "int8"
>        , CatCreateFunction
>            "pg_stat_get_autoanalyze_count" [ "oid" ] False "int8"
>        , CatCreateFunction
>            "pg_stat_get_autovacuum_count" [ "oid" ] False "int8"
>        , CatCreateFunction
>            "pg_stat_get_backend_activity" [ "int4" ] False "text"
>        , CatCreateFunction
>            "pg_stat_get_backend_activity_start" [ "int4" ] False "timestamptz"
>        , CatCreateFunction
>            "pg_stat_get_backend_client_addr" [ "int4" ] False "inet"
>        , CatCreateFunction
>            "pg_stat_get_backend_client_port" [ "int4" ] False "int4"
>        , CatCreateFunction
>            "pg_stat_get_backend_dbid" [ "int4" ] False "oid"
>        , CatCreateFunction
>            "pg_stat_get_backend_pid" [ "int4" ] False "int4"
>        , CatCreateFunction
>            "pg_stat_get_backend_start" [ "int4" ] False "timestamptz"
>        , CatCreateFunction
>            "pg_stat_get_backend_userid" [ "int4" ] False "oid"
>        , CatCreateFunction
>            "pg_stat_get_backend_waiting" [ "int4" ] False "bool"
>        , CatCreateFunction
>            "pg_stat_get_backend_xact_start" [ "int4" ] False "timestamptz"
>        , CatCreateFunction
>            "pg_stat_get_blocks_fetched" [ "oid" ] False "int8"
>        , CatCreateFunction "pg_stat_get_blocks_hit" [ "oid" ] False "int8"
>        , CatCreateFunction
>            "pg_stat_get_db_blk_read_time" [ "oid" ] False "float8"
>        , CatCreateFunction
>            "pg_stat_get_db_blk_write_time" [ "oid" ] False "float8"
>        , CatCreateFunction
>            "pg_stat_get_db_blocks_fetched" [ "oid" ] False "int8"
>        , CatCreateFunction
>            "pg_stat_get_db_blocks_hit" [ "oid" ] False "int8"
>        , CatCreateFunction
>            "pg_stat_get_db_conflict_all" [ "oid" ] False "int8"
>        , CatCreateFunction
>            "pg_stat_get_db_conflict_bufferpin" [ "oid" ] False "int8"
>        , CatCreateFunction
>            "pg_stat_get_db_conflict_lock" [ "oid" ] False "int8"
>        , CatCreateFunction
>            "pg_stat_get_db_conflict_snapshot" [ "oid" ] False "int8"
>        , CatCreateFunction
>            "pg_stat_get_db_conflict_startup_deadlock" [ "oid" ] False "int8"
>        , CatCreateFunction
>            "pg_stat_get_db_conflict_tablespace" [ "oid" ] False "int8"
>        , CatCreateFunction
>            "pg_stat_get_db_deadlocks" [ "oid" ] False "int8"
>        , CatCreateFunction
>            "pg_stat_get_db_numbackends" [ "oid" ] False "int4"
>        , CatCreateFunction
>            "pg_stat_get_db_stat_reset_time" [ "oid" ] False "timestamptz"
>        , CatCreateFunction
>            "pg_stat_get_db_temp_bytes" [ "oid" ] False "int8"
>        , CatCreateFunction
>            "pg_stat_get_db_temp_files" [ "oid" ] False "int8"
>        , CatCreateFunction
>            "pg_stat_get_db_tuples_deleted" [ "oid" ] False "int8"
>        , CatCreateFunction
>            "pg_stat_get_db_tuples_fetched" [ "oid" ] False "int8"
>        , CatCreateFunction
>            "pg_stat_get_db_tuples_inserted" [ "oid" ] False "int8"
>        , CatCreateFunction
>            "pg_stat_get_db_tuples_returned" [ "oid" ] False "int8"
>        , CatCreateFunction
>            "pg_stat_get_db_tuples_updated" [ "oid" ] False "int8"
>        , CatCreateFunction
>            "pg_stat_get_db_xact_commit" [ "oid" ] False "int8"
>        , CatCreateFunction
>            "pg_stat_get_db_xact_rollback" [ "oid" ] False "int8"
>        , CatCreateFunction
>            "pg_stat_get_dead_tuples" [ "oid" ] False "int8"
>        , CatCreateFunction
>            "pg_stat_get_function_calls" [ "oid" ] False "int8"
>        , CatCreateFunction
>            "pg_stat_get_function_self_time" [ "oid" ] False "float8"
>        , CatCreateFunction
>            "pg_stat_get_function_total_time" [ "oid" ] False "float8"
>        , CatCreateFunction
>            "pg_stat_get_last_analyze_time" [ "oid" ] False "timestamptz"
>        , CatCreateFunction
>            "pg_stat_get_last_autoanalyze_time" [ "oid" ] False "timestamptz"
>        , CatCreateFunction
>            "pg_stat_get_last_autovacuum_time" [ "oid" ] False "timestamptz"
>        , CatCreateFunction
>            "pg_stat_get_last_vacuum_time" [ "oid" ] False "timestamptz"
>        , CatCreateFunction
>            "pg_stat_get_live_tuples" [ "oid" ] False "int8"
>        , CatCreateFunction
>            "pg_stat_get_mod_since_analyze" [ "oid" ] False "int8"
>        , CatCreateFunction "pg_stat_get_numscans" [ "oid" ] False "int8"
>        , CatCreateFunction
>            "pg_stat_get_tuples_deleted" [ "oid" ] False "int8"
>        , CatCreateFunction
>            "pg_stat_get_tuples_fetched" [ "oid" ] False "int8"
>        , CatCreateFunction
>            "pg_stat_get_tuples_hot_updated" [ "oid" ] False "int8"
>        , CatCreateFunction
>            "pg_stat_get_tuples_inserted" [ "oid" ] False "int8"
>        , CatCreateFunction
>            "pg_stat_get_tuples_returned" [ "oid" ] False "int8"
>        , CatCreateFunction
>            "pg_stat_get_tuples_updated" [ "oid" ] False "int8"
>        , CatCreateFunction
>            "pg_stat_get_vacuum_count" [ "oid" ] False "int8"
>        , CatCreateFunction
>            "pg_stat_get_xact_blocks_fetched" [ "oid" ] False "int8"
>        , CatCreateFunction
>            "pg_stat_get_xact_blocks_hit" [ "oid" ] False "int8"
>        , CatCreateFunction
>            "pg_stat_get_xact_function_calls" [ "oid" ] False "int8"
>        , CatCreateFunction
>            "pg_stat_get_xact_function_self_time" [ "oid" ] False "float8"
>        , CatCreateFunction
>            "pg_stat_get_xact_function_total_time" [ "oid" ] False "float8"
>        , CatCreateFunction
>            "pg_stat_get_xact_numscans" [ "oid" ] False "int8"
>        , CatCreateFunction
>            "pg_stat_get_xact_tuples_deleted" [ "oid" ] False "int8"
>        , CatCreateFunction
>            "pg_stat_get_xact_tuples_fetched" [ "oid" ] False "int8"
>        , CatCreateFunction
>            "pg_stat_get_xact_tuples_hot_updated" [ "oid" ] False "int8"
>        , CatCreateFunction
>            "pg_stat_get_xact_tuples_inserted" [ "oid" ] False "int8"
>        , CatCreateFunction
>            "pg_stat_get_xact_tuples_returned" [ "oid" ] False "int8"
>        , CatCreateFunction
>            "pg_stat_get_xact_tuples_updated" [ "oid" ] False "int8"
>        , CatCreateFunction "pg_stat_reset_shared" [ "text" ] False "void"
>        , CatCreateFunction
>            "pg_stat_reset_single_function_counters" [ "oid" ] False "void"
>        , CatCreateFunction
>            "pg_stat_reset_single_table_counters" [ "oid" ] False "void"
>        , CatCreateFunction "pg_table_is_visible" [ "oid" ] False "bool"
>        , CatCreateFunction "pg_table_size" [ "regclass" ] False "int8"
>        , CatCreateFunction "pg_tablespace_databases" [ "oid" ] True "oid"
>        , CatCreateFunction "pg_tablespace_location" [ "oid" ] False "text"
>        , CatCreateFunction "pg_tablespace_size" [ "oid" ] False "int8"
>        , CatCreateFunction "pg_tablespace_size" [ "name" ] False "int8"
>        , CatCreateFunction "pg_terminate_backend" [ "int4" ] False "bool"
>        , CatCreateFunction
>            "pg_total_relation_size" [ "regclass" ] False "int8"
>        , CatCreateFunction "pg_try_advisory_lock" [ "int8" ] False "bool"
>        , CatCreateFunction
>            "pg_try_advisory_lock" [ "int4" , "int4" ] False "bool"
>        , CatCreateFunction
>            "pg_try_advisory_lock_shared" [ "int8" ] False "bool"
>        , CatCreateFunction
>            "pg_try_advisory_lock_shared" [ "int4" , "int4" ] False "bool"
>        , CatCreateFunction
>            "pg_try_advisory_xact_lock" [ "int8" ] False "bool"
>        , CatCreateFunction
>            "pg_try_advisory_xact_lock" [ "int4" , "int4" ] False "bool"
>        , CatCreateFunction
>            "pg_try_advisory_xact_lock_shared" [ "int8" ] False "bool"
>        , CatCreateFunction
>            "pg_try_advisory_xact_lock_shared" [ "int4" , "int4" ] False "bool"
>        , CatCreateFunction
>            "pg_ts_config_is_visible" [ "oid" ] False "bool"
>        , CatCreateFunction "pg_ts_dict_is_visible" [ "oid" ] False "bool"
>        , CatCreateFunction
>            "pg_ts_parser_is_visible" [ "oid" ] False "bool"
>        , CatCreateFunction
>            "pg_ts_template_is_visible" [ "oid" ] False "bool"
>        , CatCreateFunction "pg_type_is_visible" [ "oid" ] False "bool"
>        , CatCreateFunction "pg_typeof" [ "any" ] False "regtype"
>        , CatCreateFunction
>            "pg_xlog_location_diff" [ "pg_lsn" , "pg_lsn" ] False "numeric"
>        , CatCreateFunction "pg_xlogfile_name" [ "pg_lsn" ] False "text"
>        , CatCreateFunction
>            "pg_xlogfile_name_offset" [ "pg_lsn" ] False "record"
>        , CatCreateFunction
>            "plainto_tsquery" [ "regconfig" , "text" ] False "tsquery"
>        , CatCreateFunction "plainto_tsquery" [ "text" ] False "tsquery"
>        , CatCreateFunction
>            "plpgsql_inline_handler" [ "internal" ] False "void"
>        , CatCreateFunction "plpgsql_validator" [ "oid" ] False "void"
>        , CatCreateFunction "point" [ "circle" ] False "point"
>        , CatCreateFunction "point" [ "float8" , "float8" ] False "point"
>        , CatCreateFunction "point" [ "lseg" ] False "point"
>        , CatCreateFunction "point" [ "path" ] False "point"
>        , CatCreateFunction "point" [ "box" ] False "point"
>        , CatCreateFunction "point" [ "polygon" ] False "point"
>        , CatCreateFunction
>            "point_above" [ "point" , "point" ] False "bool"
>        , CatCreateFunction "point_add" [ "point" , "point" ] False "point"
>        , CatCreateFunction
>            "point_below" [ "point" , "point" ] False "bool"
>        , CatCreateFunction
>            "point_distance" [ "point" , "point" ] False "float8"
>        , CatCreateFunction "point_div" [ "point" , "point" ] False "point"
>        , CatCreateFunction "point_eq" [ "point" , "point" ] False "bool"
>        , CatCreateFunction
>            "point_horiz" [ "point" , "point" ] False "bool"
>        , CatCreateFunction "point_in" [ "cstring" ] False "point"
>        , CatCreateFunction "point_left" [ "point" , "point" ] False "bool"
>        , CatCreateFunction "point_mul" [ "point" , "point" ] False "point"
>        , CatCreateFunction "point_ne" [ "point" , "point" ] False "bool"
>        , CatCreateFunction "point_out" [ "point" ] False "cstring"
>        , CatCreateFunction "point_recv" [ "internal" ] False "point"
>        , CatCreateFunction
>            "point_right" [ "point" , "point" ] False "bool"
>        , CatCreateFunction "point_send" [ "point" ] False "bytea"
>        , CatCreateFunction "point_sub" [ "point" , "point" ] False "point"
>        , CatCreateFunction "point_vert" [ "point" , "point" ] False "bool"
>        , CatCreateFunction
>            "poly_above" [ "polygon" , "polygon" ] False "bool"
>        , CatCreateFunction
>            "poly_below" [ "polygon" , "polygon" ] False "bool"
>        , CatCreateFunction "poly_center" [ "polygon" ] False "point"
>        , CatCreateFunction
>            "poly_contain" [ "polygon" , "polygon" ] False "bool"
>        , CatCreateFunction
>            "poly_contain_pt" [ "polygon" , "point" ] False "bool"
>        , CatCreateFunction
>            "poly_contained" [ "polygon" , "polygon" ] False "bool"
>        , CatCreateFunction
>            "poly_distance" [ "polygon" , "polygon" ] False "float8"
>        , CatCreateFunction "poly_in" [ "cstring" ] False "polygon"
>        , CatCreateFunction
>            "poly_left" [ "polygon" , "polygon" ] False "bool"
>        , CatCreateFunction "poly_npoints" [ "polygon" ] False "int4"
>        , CatCreateFunction "poly_out" [ "polygon" ] False "cstring"
>        , CatCreateFunction
>            "poly_overabove" [ "polygon" , "polygon" ] False "bool"
>        , CatCreateFunction
>            "poly_overbelow" [ "polygon" , "polygon" ] False "bool"
>        , CatCreateFunction
>            "poly_overlap" [ "polygon" , "polygon" ] False "bool"
>        , CatCreateFunction
>            "poly_overleft" [ "polygon" , "polygon" ] False "bool"
>        , CatCreateFunction
>            "poly_overright" [ "polygon" , "polygon" ] False "bool"
>        , CatCreateFunction "poly_recv" [ "internal" ] False "polygon"
>        , CatCreateFunction
>            "poly_right" [ "polygon" , "polygon" ] False "bool"
>        , CatCreateFunction
>            "poly_same" [ "polygon" , "polygon" ] False "bool"
>        , CatCreateFunction "poly_send" [ "polygon" ] False "bytea"
>        , CatCreateFunction "polygon" [ "box" ] False "polygon"
>        , CatCreateFunction "polygon" [ "path" ] False "polygon"
>        , CatCreateFunction "polygon" [ "circle" , "int4" ] False "polygon"
>        , CatCreateFunction "polygon" [ "circle" ] False "polygon"
>        , CatCreateFunction "popen" [ "path" ] False "path"
>        , CatCreateFunction "position" [ "text" , "text" ] False "int4"
>        , CatCreateFunction "position" [ "bit" , "bit" ] False "int4"
>        , CatCreateFunction "position" [ "bytea" , "bytea" ] False "int4"
>        , CatCreateFunction
>            "positionjoinsel"
>            [ "int2" , "internal" , "internal" , "oid" , "internal" ]
>            False
>            "float8"
>        , CatCreateFunction
>            "positionsel"
>            [ "oid" , "internal" , "int4" , "internal" ]
>            False
>            "float8"
>        , CatCreateFunction
>            "postgresql_fdw_validator" [ "_text" , "oid" ] False "bool"
>        , CatCreateFunction "pow" [ "float8" , "float8" ] False "float8"
>        , CatCreateFunction "pow" [ "numeric" , "numeric" ] False "numeric"
>        , CatCreateFunction "power" [ "float8" , "float8" ] False "float8"
>        , CatCreateFunction
>            "power" [ "numeric" , "numeric" ] False "numeric"
>        , CatCreateFunction "prsd_end" [ "internal" ] False "void"
>        , CatCreateFunction
>            "prsd_headline"
>            [ "internal" , "tsquery" , "internal" ]
>            False
>            "internal"
>        , CatCreateFunction "prsd_lextype" [ "internal" ] False "internal"
>        , CatCreateFunction
>            "prsd_nexttoken"
>            [ "internal" , "internal" , "internal" ]
>            False
>            "internal"
>        , CatCreateFunction
>            "prsd_start" [ "int4" , "internal" ] False "internal"
>        , CatCreateFunction
>            "pt_contained_circle" [ "circle" , "point" ] False "bool"
>        , CatCreateFunction
>            "pt_contained_poly" [ "polygon" , "point" ] False "bool"
>        , CatCreateFunction
>            "query_to_xml" [ "text" , "text" , "bool" , "bool" ] False "xml"
>        , CatCreateFunction
>            "query_to_xml_and_xmlschema"
>            [ "text" , "bool" , "text" , "bool" ]
>            False
>            "xml"
>        , CatCreateFunction
>            "query_to_xmlschema"
>            [ "bool" , "text" , "text" , "bool" ]
>            False
>            "xml"
>        , CatCreateFunction "querytree" [ "tsquery" ] False "text"
>        , CatCreateFunction "quote_ident" [ "text" ] False "text"
>        , CatCreateFunction "quote_literal" [ "text" ] False "text"
>        , CatCreateFunction "quote_literal" [ "anyelement" ] False "text"
>        , CatCreateFunction "quote_nullable" [ "text" ] False "text"
>        , CatCreateFunction "quote_nullable" [ "anyelement" ] False "text"
>        , CatCreateFunction "radians" [ "float8" ] False "float8"
>        , CatCreateFunction "radius" [ "circle" ] False "float8"
>        , CatCreateFunction
>            "range_adjacent" [ "anyrange" , "anyrange" ] False "bool"
>        , CatCreateFunction
>            "range_after" [ "anyrange" , "anyrange" ] False "bool"
>        , CatCreateFunction
>            "range_before" [ "anyrange" , "anyrange" ] False "bool"
>        , CatCreateFunction
>            "range_cmp" [ "anyrange" , "anyrange" ] False "int4"
>        , CatCreateFunction
>            "range_contained_by" [ "anyrange" , "anyrange" ] False "bool"
>        , CatCreateFunction
>            "range_contains" [ "anyrange" , "anyrange" ] False "bool"
>        , CatCreateFunction
>            "range_contains_elem" [ "anyelement" , "anyrange" ] False "bool"
>        , CatCreateFunction
>            "range_eq" [ "anyrange" , "anyrange" ] False "bool"
>        , CatCreateFunction
>            "range_ge" [ "anyrange" , "anyrange" ] False "bool"
>        , CatCreateFunction
>            "range_gist_compress" [ "internal" ] False "internal"
>        , CatCreateFunction
>            "range_gist_consistent"
>            [ "oid" , "internal" , "int4" , "internal" , "anyrange" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "range_gist_decompress" [ "internal" ] False "internal"
>        , CatCreateFunction
>            "range_gist_penalty"
>            [ "internal" , "internal" , "internal" ]
>            False
>            "internal"
>        , CatCreateFunction
>            "range_gist_picksplit" [ "internal" , "internal" ] False "internal"
>        , CatCreateFunction
>            "range_gist_same"
>            [ "internal" , "anyrange" , "anyrange" ]
>            False
>            "internal"
>        , CatCreateFunction
>            "range_gist_union" [ "internal" , "internal" ] False "internal"
>        , CatCreateFunction
>            "range_gt" [ "anyrange" , "anyrange" ] False "bool"
>        , CatCreateFunction
>            "range_in" [ "cstring" , "oid" , "int4" ] False "anyrange"
>        , CatCreateFunction
>            "range_intersect" [ "anyrange" , "anyrange" ] False "anyrange"
>        , CatCreateFunction
>            "range_le" [ "anyrange" , "anyrange" ] False "bool"
>        , CatCreateFunction
>            "range_lt" [ "anyrange" , "anyrange" ] False "bool"
>        , CatCreateFunction
>            "range_minus" [ "anyrange" , "anyrange" ] False "anyrange"
>        , CatCreateFunction
>            "range_ne" [ "anyrange" , "anyrange" ] False "bool"
>        , CatCreateFunction "range_out" [ "anyrange" ] False "cstring"
>        , CatCreateFunction
>            "range_overlaps" [ "anyrange" , "anyrange" ] False "bool"
>        , CatCreateFunction
>            "range_overleft" [ "anyrange" , "anyrange" ] False "bool"
>        , CatCreateFunction
>            "range_overright" [ "anyrange" , "anyrange" ] False "bool"
>        , CatCreateFunction
>            "range_recv" [ "internal" , "oid" , "int4" ] False "anyrange"
>        , CatCreateFunction "range_send" [ "anyrange" ] False "bytea"
>        , CatCreateFunction "range_typanalyze" [ "internal" ] False "bool"
>        , CatCreateFunction
>            "range_union" [ "anyrange" , "anyrange" ] False "anyrange"
>        , CatCreateFunction
>            "rangesel"
>            [ "internal" , "internal" , "oid" , "int4" ]
>            False
>            "float8"
>        , CatCreateFunction
>            "record_eq" [ "record" , "record" ] False "bool"
>        , CatCreateFunction
>            "record_ge" [ "record" , "record" ] False "bool"
>        , CatCreateFunction
>            "record_gt" [ "record" , "record" ] False "bool"
>        , CatCreateFunction
>            "record_image_eq" [ "record" , "record" ] False "bool"
>        , CatCreateFunction
>            "record_image_ge" [ "record" , "record" ] False "bool"
>        , CatCreateFunction
>            "record_image_gt" [ "record" , "record" ] False "bool"
>        , CatCreateFunction
>            "record_image_le" [ "record" , "record" ] False "bool"
>        , CatCreateFunction
>            "record_image_lt" [ "record" , "record" ] False "bool"
>        , CatCreateFunction
>            "record_image_ne" [ "record" , "record" ] False "bool"
>        , CatCreateFunction
>            "record_in" [ "int4" , "cstring" , "oid" ] False "record"
>        , CatCreateFunction
>            "record_le" [ "record" , "record" ] False "bool"
>        , CatCreateFunction
>            "record_lt" [ "record" , "record" ] False "bool"
>        , CatCreateFunction
>            "record_ne" [ "record" , "record" ] False "bool"
>        , CatCreateFunction "record_out" [ "record" ] False "cstring"
>        , CatCreateFunction
>            "record_recv" [ "oid" , "int4" , "internal" ] False "record"
>        , CatCreateFunction "record_send" [ "record" ] False "bytea"
>        , CatCreateFunction "regclass" [ "text" ] False "regclass"
>        , CatCreateFunction "regclassin" [ "cstring" ] False "regclass"
>        , CatCreateFunction "regclassout" [ "regclass" ] False "cstring"
>        , CatCreateFunction "regclassrecv" [ "internal" ] False "regclass"
>        , CatCreateFunction "regclasssend" [ "regclass" ] False "bytea"
>        , CatCreateFunction "regconfigin" [ "cstring" ] False "regconfig"
>        , CatCreateFunction "regconfigout" [ "regconfig" ] False "cstring"
>        , CatCreateFunction
>            "regconfigrecv" [ "internal" ] False "regconfig"
>        , CatCreateFunction "regconfigsend" [ "regconfig" ] False "bytea"
>        , CatCreateFunction
>            "regdictionaryin" [ "cstring" ] False "regdictionary"
>        , CatCreateFunction
>            "regdictionaryout" [ "regdictionary" ] False "cstring"
>        , CatCreateFunction
>            "regdictionaryrecv" [ "internal" ] False "regdictionary"
>        , CatCreateFunction
>            "regdictionarysend" [ "regdictionary" ] False "bytea"
>        , CatCreateFunction
>            "regexeqjoinsel"
>            [ "internal" , "oid" , "internal" , "internal" , "int2" ]
>            False
>            "float8"
>        , CatCreateFunction
>            "regexeqsel"
>            [ "internal" , "oid" , "int4" , "internal" ]
>            False
>            "float8"
>        , CatCreateFunction
>            "regexnejoinsel"
>            [ "int2" , "internal" , "internal" , "oid" , "internal" ]
>            False
>            "float8"
>        , CatCreateFunction
>            "regexnesel"
>            [ "internal" , "oid" , "internal" , "int4" ]
>            False
>            "float8"
>        , CatCreateFunction
>            "regexp_matches" [ "text" , "text" ] True "_text"
>        , CatCreateFunction
>            "regexp_matches" [ "text" , "text" , "text" ] True "_text"
>        , CatCreateFunction
>            "regexp_replace" [ "text" , "text" , "text" ] False "text"
>        , CatCreateFunction
>            "regexp_replace" [ "text" , "text" , "text" , "text" ] False "text"
>        , CatCreateFunction
>            "regexp_split_to_array" [ "text" , "text" ] False "_text"
>        , CatCreateFunction
>            "regexp_split_to_array" [ "text" , "text" , "text" ] False "_text"
>        , CatCreateFunction
>            "regexp_split_to_table" [ "text" , "text" ] True "text"
>        , CatCreateFunction
>            "regexp_split_to_table" [ "text" , "text" , "text" ] True "text"
>        , CatCreateFunction
>            "regoperatorin" [ "cstring" ] False "regoperator"
>        , CatCreateFunction
>            "regoperatorout" [ "regoperator" ] False "cstring"
>        , CatCreateFunction
>            "regoperatorrecv" [ "internal" ] False "regoperator"
>        , CatCreateFunction
>            "regoperatorsend" [ "regoperator" ] False "bytea"
>        , CatCreateFunction "regoperin" [ "cstring" ] False "regoper"
>        , CatCreateFunction "regoperout" [ "regoper" ] False "cstring"
>        , CatCreateFunction "regoperrecv" [ "internal" ] False "regoper"
>        , CatCreateFunction "regopersend" [ "regoper" ] False "bytea"
>        , CatCreateFunction
>            "regprocedurein" [ "cstring" ] False "regprocedure"
>        , CatCreateFunction
>            "regprocedureout" [ "regprocedure" ] False "cstring"
>        , CatCreateFunction
>            "regprocedurerecv" [ "internal" ] False "regprocedure"
>        , CatCreateFunction
>            "regproceduresend" [ "regprocedure" ] False "bytea"
>        , CatCreateFunction "regprocin" [ "cstring" ] False "regproc"
>        , CatCreateFunction "regprocout" [ "regproc" ] False "cstring"
>        , CatCreateFunction "regprocrecv" [ "internal" ] False "regproc"
>        , CatCreateFunction "regprocsend" [ "regproc" ] False "bytea"
>        , CatCreateFunction "regtypein" [ "cstring" ] False "regtype"
>        , CatCreateFunction "regtypeout" [ "regtype" ] False "cstring"
>        , CatCreateFunction "regtyperecv" [ "internal" ] False "regtype"
>        , CatCreateFunction "regtypesend" [ "regtype" ] False "bytea"
>        , CatCreateFunction "reltime" [ "interval" ] False "reltime"
>        , CatCreateFunction
>            "reltimeeq" [ "reltime" , "reltime" ] False "bool"
>        , CatCreateFunction
>            "reltimege" [ "reltime" , "reltime" ] False "bool"
>        , CatCreateFunction
>            "reltimegt" [ "reltime" , "reltime" ] False "bool"
>        , CatCreateFunction "reltimein" [ "cstring" ] False "reltime"
>        , CatCreateFunction
>            "reltimele" [ "reltime" , "reltime" ] False "bool"
>        , CatCreateFunction
>            "reltimelt" [ "reltime" , "reltime" ] False "bool"
>        , CatCreateFunction
>            "reltimene" [ "reltime" , "reltime" ] False "bool"
>        , CatCreateFunction "reltimeout" [ "reltime" ] False "cstring"
>        , CatCreateFunction "reltimerecv" [ "internal" ] False "reltime"
>        , CatCreateFunction "reltimesend" [ "reltime" ] False "bytea"
>        , CatCreateFunction "repeat" [ "int4" , "text" ] False "text"
>        , CatCreateFunction
>            "replace" [ "text" , "text" , "text" ] False "text"
>        , CatCreateFunction "reverse" [ "text" ] False "text"
>        , CatCreateFunction "right" [ "text" , "int4" ] False "text"
>        , CatCreateFunction "round" [ "float8" ] False "float8"
>        , CatCreateFunction "round" [ "int4" , "numeric" ] False "numeric"
>        , CatCreateFunction "round" [ "numeric" ] False "numeric"
>        , CatCreateFunction "row_to_json" [ "record" ] False "json"
>        , CatCreateFunction
>            "row_to_json" [ "record" , "bool" ] False "json"
>        , CatCreateFunction
>            "rpad" [ "text" , "int4" , "text" ] False "text"
>        , CatCreateFunction "rpad" [ "int4" , "text" ] False "text"
>        , CatCreateFunction "rtrim" [ "text" , "text" ] False "text"
>        , CatCreateFunction "rtrim" [ "text" ] False "text"
>        , CatCreateFunction
>            "scalargtjoinsel"
>            [ "internal" , "internal" , "internal" , "int2" , "oid" ]
>            False
>            "float8"
>        , CatCreateFunction
>            "scalargtsel"
>            [ "internal" , "internal" , "int4" , "oid" ]
>            False
>            "float8"
>        , CatCreateFunction
>            "scalarltjoinsel"
>            [ "internal" , "internal" , "internal" , "int2" , "oid" ]
>            False
>            "float8"
>        , CatCreateFunction
>            "scalarltsel"
>            [ "oid" , "internal" , "int4" , "internal" ]
>            False
>            "float8"
>        , CatCreateFunction
>            "schema_to_xml" [ "bool" , "text" , "bool" , "name" ] False "xml"
>        , CatCreateFunction
>            "schema_to_xml_and_xmlschema"
>            [ "name" , "bool" , "text" , "bool" ]
>            False
>            "xml"
>        , CatCreateFunction
>            "schema_to_xmlschema"
>            [ "bool" , "bool" , "name" , "text" ]
>            False
>            "xml"
>        , CatCreateFunction
>            "set_bit" [ "int4" , "bytea" , "int4" ] False "bytea"
>        , CatCreateFunction
>            "set_bit" [ "int4" , "bit" , "int4" ] False "bit"
>        , CatCreateFunction
>            "set_byte" [ "int4" , "bytea" , "int4" ] False "bytea"
>        , CatCreateFunction
>            "set_config" [ "text" , "text" , "bool" ] False "text"
>        , CatCreateFunction "set_masklen" [ "int4" , "inet" ] False "inet"
>        , CatCreateFunction "set_masklen" [ "int4" , "cidr" ] False "cidr"
>        , CatCreateFunction "setseed" [ "float8" ] False "void"
>        , CatCreateFunction "setval" [ "regclass" , "int8" ] False "int8"
>        , CatCreateFunction
>            "setval" [ "regclass" , "int8" , "bool" ] False "int8"
>        , CatCreateFunction
>            "setweight" [ "tsvector" , "char" ] False "tsvector"
>        , CatCreateFunction "shell_in" [ "cstring" ] False "opaque"
>        , CatCreateFunction "shell_out" [ "opaque" ] False "cstring"
>        , CatCreateFunction
>            "shift_jis_2004_to_euc_jis_2004"
>            [ "int4" , "cstring" , "int4" , "internal" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "shift_jis_2004_to_utf8"
>            [ "int4" , "int4" , "internal" , "cstring" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "shobj_description" [ "name" , "oid" ] False "text"
>        , CatCreateFunction "sign" [ "numeric" ] False "numeric"
>        , CatCreateFunction "sign" [ "float8" ] False "float8"
>        , CatCreateFunction
>            "similar_escape" [ "text" , "text" ] False "text"
>        , CatCreateFunction "sin" [ "float8" ] False "float8"
>        , CatCreateFunction
>            "sjis_to_euc_jp"
>            [ "int4" , "int4" , "int4" , "internal" , "cstring" ]
>            False
>            "void"
>        , CatCreateFunction
>            "sjis_to_mic"
>            [ "internal" , "int4" , "cstring" , "int4" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "sjis_to_utf8"
>            [ "int4" , "int4" , "cstring" , "internal" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction "slope" [ "point" , "point" ] False "float8"
>        , CatCreateFunction "smgreq" [ "smgr" , "smgr" ] False "bool"
>        , CatCreateFunction "smgrin" [ "cstring" ] False "smgr"
>        , CatCreateFunction "smgrne" [ "smgr" , "smgr" ] False "bool"
>        , CatCreateFunction "smgrout" [ "smgr" ] False "cstring"
>        , CatCreateFunction
>            "spg_kd_choose" [ "internal" , "internal" ] False "void"
>        , CatCreateFunction
>            "spg_kd_config" [ "internal" , "internal" ] False "void"
>        , CatCreateFunction
>            "spg_kd_inner_consistent" [ "internal" , "internal" ] False "void"
>        , CatCreateFunction
>            "spg_kd_picksplit" [ "internal" , "internal" ] False "void"
>        , CatCreateFunction
>            "spg_quad_choose" [ "internal" , "internal" ] False "void"
>        , CatCreateFunction
>            "spg_quad_config" [ "internal" , "internal" ] False "void"
>        , CatCreateFunction
>            "spg_quad_inner_consistent"
>            [ "internal" , "internal" ]
>            False
>            "void"
>        , CatCreateFunction
>            "spg_quad_leaf_consistent" [ "internal" , "internal" ] False "bool"
>        , CatCreateFunction
>            "spg_quad_picksplit" [ "internal" , "internal" ] False "void"
>        , CatCreateFunction
>            "spg_range_quad_choose" [ "internal" , "internal" ] False "void"
>        , CatCreateFunction
>            "spg_range_quad_config" [ "internal" , "internal" ] False "void"
>        , CatCreateFunction
>            "spg_range_quad_inner_consistent"
>            [ "internal" , "internal" ]
>            False
>            "void"
>        , CatCreateFunction
>            "spg_range_quad_leaf_consistent"
>            [ "internal" , "internal" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "spg_range_quad_picksplit" [ "internal" , "internal" ] False "void"
>        , CatCreateFunction
>            "spg_text_choose" [ "internal" , "internal" ] False "void"
>        , CatCreateFunction
>            "spg_text_config" [ "internal" , "internal" ] False "void"
>        , CatCreateFunction
>            "spg_text_inner_consistent"
>            [ "internal" , "internal" ]
>            False
>            "void"
>        , CatCreateFunction
>            "spg_text_leaf_consistent" [ "internal" , "internal" ] False "bool"
>        , CatCreateFunction
>            "spg_text_picksplit" [ "internal" , "internal" ] False "void"
>        , CatCreateFunction
>            "spgbeginscan"
>            [ "internal" , "internal" , "internal" ]
>            False
>            "internal"
>        , CatCreateFunction
>            "spgbuild"
>            [ "internal" , "internal" , "internal" ]
>            False
>            "internal"
>        , CatCreateFunction "spgbuildempty" [ "internal" ] False "void"
>        , CatCreateFunction
>            "spgbulkdelete"
>            [ "internal" , "internal" , "internal" , "internal" ]
>            False
>            "internal"
>        , CatCreateFunction "spgcanreturn" [ "internal" ] False "bool"
>        , CatCreateFunction
>            "spgcostestimate"
>            [ "internal"
>            , "internal"
>            , "internal"
>            , "internal"
>            , "internal"
>            , "internal"
>            , "internal"
>            ]
>            False
>            "void"
>        , CatCreateFunction "spgendscan" [ "internal" ] False "void"
>        , CatCreateFunction
>            "spggetbitmap" [ "internal" , "internal" ] False "int8"
>        , CatCreateFunction
>            "spggettuple" [ "internal" , "internal" ] False "bool"
>        , CatCreateFunction
>            "spginsert"
>            [ "internal"
>            , "internal"
>            , "internal"
>            , "internal"
>            , "internal"
>            , "internal"
>            ]
>            False
>            "bool"
>        , CatCreateFunction "spgmarkpos" [ "internal" ] False "void"
>        , CatCreateFunction "spgoptions" [ "bool" , "_text" ] False "bytea"
>        , CatCreateFunction
>            "spgrescan"
>            [ "internal" , "internal" , "internal" , "internal" , "internal" ]
>            False
>            "void"
>        , CatCreateFunction "spgrestrpos" [ "internal" ] False "void"
>        , CatCreateFunction
>            "spgvacuumcleanup" [ "internal" , "internal" ] False "internal"
>        , CatCreateFunction
>            "split_part" [ "text" , "int4" , "text" ] False "text"
>        , CatCreateFunction "sqrt" [ "float8" ] False "float8"
>        , CatCreateFunction "sqrt" [ "numeric" ] False "numeric"
>        , CatCreateFunction
>            "string_agg_finalfn" [ "internal" ] False "text"
>        , CatCreateFunction
>            "string_agg_transfn"
>            [ "internal" , "text" , "text" ]
>            False
>            "internal"
>        , CatCreateFunction
>            "string_to_array" [ "text" , "text" , "text" ] False "_text"
>        , CatCreateFunction
>            "string_to_array" [ "text" , "text" ] False "_text"
>        , CatCreateFunction "strip" [ "tsvector" ] False "tsvector"
>        , CatCreateFunction "strpos" [ "text" , "text" ] False "int4"
>        , CatCreateFunction
>            "substr" [ "text" , "int4" , "int4" ] False "text"
>        , CatCreateFunction "substr" [ "text" , "int4" ] False "text"
>        , CatCreateFunction
>            "substr" [ "int4" , "bytea" , "int4" ] False "bytea"
>        , CatCreateFunction "substr" [ "int4" , "bytea" ] False "bytea"
>        , CatCreateFunction
>            "substring" [ "text" , "int4" , "int4" ] False "text"
>        , CatCreateFunction "substring" [ "int4" , "text" ] False "text"
>        , CatCreateFunction
>            "substring" [ "bit" , "int4" , "int4" ] False "bit"
>        , CatCreateFunction "substring" [ "int4" , "bit" ] False "bit"
>        , CatCreateFunction
>            "substring" [ "int4" , "int4" , "bytea" ] False "bytea"
>        , CatCreateFunction "substring" [ "bytea" , "int4" ] False "bytea"
>        , CatCreateFunction "substring" [ "text" , "text" ] False "text"
>        , CatCreateFunction
>            "substring" [ "text" , "text" , "text" ] False "text"
>        , CatCreateFunction
>            "table_to_xml"
>            [ "regclass" , "bool" , "bool" , "text" ]
>            False
>            "xml"
>        , CatCreateFunction
>            "table_to_xml_and_xmlschema"
>            [ "bool" , "text" , "bool" , "regclass" ]
>            False
>            "xml"
>        , CatCreateFunction
>            "table_to_xmlschema"
>            [ "text" , "regclass" , "bool" , "bool" ]
>            False
>            "xml"
>        , CatCreateFunction "tan" [ "float8" ] False "float8"
>        , CatCreateFunction "text" [ "bpchar" ] False "text"
>        , CatCreateFunction "text" [ "name" ] False "text"
>        , CatCreateFunction "text" [ "inet" ] False "text"
>        , CatCreateFunction "text" [ "char" ] False "text"
>        , CatCreateFunction "text" [ "xml" ] False "text"
>        , CatCreateFunction "text" [ "bool" ] False "text"
>        , CatCreateFunction "text_ge" [ "text" , "text" ] False "bool"
>        , CatCreateFunction "text_gt" [ "text" , "text" ] False "bool"
>        , CatCreateFunction "text_larger" [ "text" , "text" ] False "text"
>        , CatCreateFunction "text_le" [ "text" , "text" ] False "bool"
>        , CatCreateFunction "text_lt" [ "text" , "text" ] False "bool"
>        , CatCreateFunction
>            "text_pattern_ge" [ "text" , "text" ] False "bool"
>        , CatCreateFunction
>            "text_pattern_gt" [ "text" , "text" ] False "bool"
>        , CatCreateFunction
>            "text_pattern_le" [ "text" , "text" ] False "bool"
>        , CatCreateFunction
>            "text_pattern_lt" [ "text" , "text" ] False "bool"
>        , CatCreateFunction "text_smaller" [ "text" , "text" ] False "text"
>        , CatCreateFunction
>            "textanycat" [ "anynonarray" , "text" ] False "text"
>        , CatCreateFunction "textcat" [ "text" , "text" ] False "text"
>        , CatCreateFunction "texteq" [ "text" , "text" ] False "bool"
>        , CatCreateFunction "texticlike" [ "text" , "text" ] False "bool"
>        , CatCreateFunction "texticnlike" [ "text" , "text" ] False "bool"
>        , CatCreateFunction
>            "texticregexeq" [ "text" , "text" ] False "bool"
>        , CatCreateFunction
>            "texticregexne" [ "text" , "text" ] False "bool"
>        , CatCreateFunction "textin" [ "cstring" ] False "text"
>        , CatCreateFunction "textlen" [ "text" ] False "int4"
>        , CatCreateFunction "textlike" [ "text" , "text" ] False "bool"
>        , CatCreateFunction "textne" [ "text" , "text" ] False "bool"
>        , CatCreateFunction "textnlike" [ "text" , "text" ] False "bool"
>        , CatCreateFunction "textout" [ "text" ] False "cstring"
>        , CatCreateFunction "textrecv" [ "internal" ] False "text"
>        , CatCreateFunction "textregexeq" [ "text" , "text" ] False "bool"
>        , CatCreateFunction "textregexne" [ "text" , "text" ] False "bool"
>        , CatCreateFunction "textsend" [ "text" ] False "bytea"
>        , CatCreateFunction
>            "thesaurus_init" [ "internal" ] False "internal"
>        , CatCreateFunction
>            "thesaurus_lexize"
>            [ "internal" , "internal" , "internal" , "internal" ]
>            False
>            "internal"
>        , CatCreateFunction "tideq" [ "tid" , "tid" ] False "bool"
>        , CatCreateFunction "tidge" [ "tid" , "tid" ] False "bool"
>        , CatCreateFunction "tidgt" [ "tid" , "tid" ] False "bool"
>        , CatCreateFunction "tidin" [ "cstring" ] False "tid"
>        , CatCreateFunction "tidlarger" [ "tid" , "tid" ] False "tid"
>        , CatCreateFunction "tidle" [ "tid" , "tid" ] False "bool"
>        , CatCreateFunction "tidlt" [ "tid" , "tid" ] False "bool"
>        , CatCreateFunction "tidne" [ "tid" , "tid" ] False "bool"
>        , CatCreateFunction "tidout" [ "tid" ] False "cstring"
>        , CatCreateFunction "tidrecv" [ "internal" ] False "tid"
>        , CatCreateFunction "tidsend" [ "tid" ] False "bytea"
>        , CatCreateFunction "tidsmaller" [ "tid" , "tid" ] False "tid"
>        , CatCreateFunction "time" [ "timestamp" ] False "time"
>        , CatCreateFunction "time" [ "abstime" ] False "time"
>        , CatCreateFunction "time" [ "interval" ] False "time"
>        , CatCreateFunction "time" [ "time" , "int4" ] False "time"
>        , CatCreateFunction "time" [ "timestamptz" ] False "time"
>        , CatCreateFunction "time" [ "timetz" ] False "time"
>        , CatCreateFunction "time_cmp" [ "time" , "time" ] False "int4"
>        , CatCreateFunction "time_eq" [ "time" , "time" ] False "bool"
>        , CatCreateFunction "time_ge" [ "time" , "time" ] False "bool"
>        , CatCreateFunction "time_gt" [ "time" , "time" ] False "bool"
>        , CatCreateFunction "time_hash" [ "time" ] False "int4"
>        , CatCreateFunction
>            "time_in" [ "int4" , "cstring" , "oid" ] False "time"
>        , CatCreateFunction "time_larger" [ "time" , "time" ] False "time"
>        , CatCreateFunction "time_le" [ "time" , "time" ] False "bool"
>        , CatCreateFunction "time_lt" [ "time" , "time" ] False "bool"
>        , CatCreateFunction
>            "time_mi_interval" [ "interval" , "time" ] False "time"
>        , CatCreateFunction
>            "time_mi_time" [ "time" , "time" ] False "interval"
>        , CatCreateFunction "time_ne" [ "time" , "time" ] False "bool"
>        , CatCreateFunction "time_out" [ "time" ] False "cstring"
>        , CatCreateFunction
>            "time_pl_interval" [ "time" , "interval" ] False "time"
>        , CatCreateFunction
>            "time_recv" [ "internal" , "oid" , "int4" ] False "time"
>        , CatCreateFunction "time_send" [ "time" ] False "bytea"
>        , CatCreateFunction "time_smaller" [ "time" , "time" ] False "time"
>        , CatCreateFunction
>            "time_transform" [ "internal" ] False "internal"
>        , CatCreateFunction
>            "timedate_pl" [ "time" , "date" ] False "timestamp"
>        , CatCreateFunction
>            "timemi" [ "abstime" , "reltime" ] False "abstime"
>        , CatCreateFunction
>            "timepl" [ "abstime" , "reltime" ] False "abstime"
>        , CatCreateFunction
>            "timestamp" [ "timestamp" , "int4" ] False "timestamp"
>        , CatCreateFunction "timestamp" [ "abstime" ] False "timestamp"
>        , CatCreateFunction "timestamp" [ "date" ] False "timestamp"
>        , CatCreateFunction
>            "timestamp" [ "date" , "time" ] False "timestamp"
>        , CatCreateFunction "timestamp" [ "timestamptz" ] False "timestamp"
>        , CatCreateFunction
>            "timestamp_cmp" [ "timestamp" , "timestamp" ] False "int4"
>        , CatCreateFunction
>            "timestamp_cmp_date" [ "date" , "timestamp" ] False "int4"
>        , CatCreateFunction
>            "timestamp_cmp_timestamptz"
>            [ "timestamptz" , "timestamp" ]
>            False
>            "int4"
>        , CatCreateFunction
>            "timestamp_eq" [ "timestamp" , "timestamp" ] False "bool"
>        , CatCreateFunction
>            "timestamp_eq_date" [ "timestamp" , "date" ] False "bool"
>        , CatCreateFunction
>            "timestamp_eq_timestamptz"
>            [ "timestamptz" , "timestamp" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "timestamp_ge" [ "timestamp" , "timestamp" ] False "bool"
>        , CatCreateFunction
>            "timestamp_ge_date" [ "timestamp" , "date" ] False "bool"
>        , CatCreateFunction
>            "timestamp_ge_timestamptz"
>            [ "timestamptz" , "timestamp" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "timestamp_gt" [ "timestamp" , "timestamp" ] False "bool"
>        , CatCreateFunction
>            "timestamp_gt_date" [ "date" , "timestamp" ] False "bool"
>        , CatCreateFunction
>            "timestamp_gt_timestamptz"
>            [ "timestamptz" , "timestamp" ]
>            False
>            "bool"
>        , CatCreateFunction "timestamp_hash" [ "timestamp" ] False "int4"
>        , CatCreateFunction
>            "timestamp_in" [ "cstring" , "oid" , "int4" ] False "timestamp"
>        , CatCreateFunction
>            "timestamp_larger" [ "timestamp" , "timestamp" ] False "timestamp"
>        , CatCreateFunction
>            "timestamp_le" [ "timestamp" , "timestamp" ] False "bool"
>        , CatCreateFunction
>            "timestamp_le_date" [ "timestamp" , "date" ] False "bool"
>        , CatCreateFunction
>            "timestamp_le_timestamptz"
>            [ "timestamp" , "timestamptz" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "timestamp_lt" [ "timestamp" , "timestamp" ] False "bool"
>        , CatCreateFunction
>            "timestamp_lt_date" [ "date" , "timestamp" ] False "bool"
>        , CatCreateFunction
>            "timestamp_lt_timestamptz"
>            [ "timestamp" , "timestamptz" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "timestamp_mi" [ "timestamp" , "timestamp" ] False "interval"
>        , CatCreateFunction
>            "timestamp_mi_interval"
>            [ "timestamp" , "interval" ]
>            False
>            "timestamp"
>        , CatCreateFunction
>            "timestamp_ne" [ "timestamp" , "timestamp" ] False "bool"
>        , CatCreateFunction
>            "timestamp_ne_date" [ "timestamp" , "date" ] False "bool"
>        , CatCreateFunction
>            "timestamp_ne_timestamptz"
>            [ "timestamp" , "timestamptz" ]
>            False
>            "bool"
>        , CatCreateFunction "timestamp_out" [ "timestamp" ] False "cstring"
>        , CatCreateFunction
>            "timestamp_pl_interval"
>            [ "timestamp" , "interval" ]
>            False
>            "timestamp"
>        , CatCreateFunction
>            "timestamp_recv" [ "internal" , "int4" , "oid" ] False "timestamp"
>        , CatCreateFunction "timestamp_send" [ "timestamp" ] False "bytea"
>        , CatCreateFunction
>            "timestamp_smaller" [ "timestamp" , "timestamp" ] False "timestamp"
>        , CatCreateFunction
>            "timestamp_sortsupport" [ "internal" ] False "void"
>        , CatCreateFunction
>            "timestamp_transform" [ "internal" ] False "internal"
>        , CatCreateFunction "timestamptypmodin" [ "_cstring" ] False "int4"
>        , CatCreateFunction "timestamptypmodout" [ "int4" ] False "cstring"
>        , CatCreateFunction "timestamptz" [ "abstime" ] False "timestamptz"
>        , CatCreateFunction "timestamptz" [ "date" ] False "timestamptz"
>        , CatCreateFunction
>            "timestamptz" [ "time" , "date" ] False "timestamptz"
>        , CatCreateFunction
>            "timestamptz" [ "timetz" , "date" ] False "timestamptz"
>        , CatCreateFunction
>            "timestamptz" [ "timestamptz" , "int4" ] False "timestamptz"
>        , CatCreateFunction
>            "timestamptz" [ "timestamp" ] False "timestamptz"
>        , CatCreateFunction
>            "timestamptz_cmp" [ "timestamptz" , "timestamptz" ] False "int4"
>        , CatCreateFunction
>            "timestamptz_cmp_date" [ "date" , "timestamptz" ] False "int4"
>        , CatCreateFunction
>            "timestamptz_cmp_timestamp"
>            [ "timestamp" , "timestamptz" ]
>            False
>            "int4"
>        , CatCreateFunction
>            "timestamptz_eq" [ "timestamptz" , "timestamptz" ] False "bool"
>        , CatCreateFunction
>            "timestamptz_eq_date" [ "timestamptz" , "date" ] False "bool"
>        , CatCreateFunction
>            "timestamptz_eq_timestamp"
>            [ "timestamp" , "timestamptz" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "timestamptz_ge" [ "timestamptz" , "timestamptz" ] False "bool"
>        , CatCreateFunction
>            "timestamptz_ge_date" [ "timestamptz" , "date" ] False "bool"
>        , CatCreateFunction
>            "timestamptz_ge_timestamp"
>            [ "timestamptz" , "timestamp" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "timestamptz_gt" [ "timestamptz" , "timestamptz" ] False "bool"
>        , CatCreateFunction
>            "timestamptz_gt_date" [ "timestamptz" , "date" ] False "bool"
>        , CatCreateFunction
>            "timestamptz_gt_timestamp"
>            [ "timestamptz" , "timestamp" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "timestamptz_in" [ "oid" , "cstring" , "int4" ] False "timestamptz"
>        , CatCreateFunction
>            "timestamptz_larger"
>            [ "timestamptz" , "timestamptz" ]
>            False
>            "timestamptz"
>        , CatCreateFunction
>            "timestamptz_le" [ "timestamptz" , "timestamptz" ] False "bool"
>        , CatCreateFunction
>            "timestamptz_le_date" [ "date" , "timestamptz" ] False "bool"
>        , CatCreateFunction
>            "timestamptz_le_timestamp"
>            [ "timestamp" , "timestamptz" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "timestamptz_lt" [ "timestamptz" , "timestamptz" ] False "bool"
>        , CatCreateFunction
>            "timestamptz_lt_date" [ "timestamptz" , "date" ] False "bool"
>        , CatCreateFunction
>            "timestamptz_lt_timestamp"
>            [ "timestamptz" , "timestamp" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "timestamptz_mi" [ "timestamptz" , "timestamptz" ] False "interval"
>        , CatCreateFunction
>            "timestamptz_mi_interval"
>            [ "timestamptz" , "interval" ]
>            False
>            "timestamptz"
>        , CatCreateFunction
>            "timestamptz_ne" [ "timestamptz" , "timestamptz" ] False "bool"
>        , CatCreateFunction
>            "timestamptz_ne_date" [ "date" , "timestamptz" ] False "bool"
>        , CatCreateFunction
>            "timestamptz_ne_timestamp"
>            [ "timestamptz" , "timestamp" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "timestamptz_out" [ "timestamptz" ] False "cstring"
>        , CatCreateFunction
>            "timestamptz_pl_interval"
>            [ "timestamptz" , "interval" ]
>            False
>            "timestamptz"
>        , CatCreateFunction
>            "timestamptz_recv"
>            [ "oid" , "int4" , "internal" ]
>            False
>            "timestamptz"
>        , CatCreateFunction
>            "timestamptz_send" [ "timestamptz" ] False "bytea"
>        , CatCreateFunction
>            "timestamptz_smaller"
>            [ "timestamptz" , "timestamptz" ]
>            False
>            "timestamptz"
>        , CatCreateFunction
>            "timestamptztypmodin" [ "_cstring" ] False "int4"
>        , CatCreateFunction
>            "timestamptztypmodout" [ "int4" ] False "cstring"
>        , CatCreateFunction "timetypmodin" [ "_cstring" ] False "int4"
>        , CatCreateFunction "timetypmodout" [ "int4" ] False "cstring"
>        , CatCreateFunction "timetz" [ "timestamptz" ] False "timetz"
>        , CatCreateFunction "timetz" [ "int4" , "timetz" ] False "timetz"
>        , CatCreateFunction "timetz" [ "time" ] False "timetz"
>        , CatCreateFunction
>            "timetz_cmp" [ "timetz" , "timetz" ] False "int4"
>        , CatCreateFunction
>            "timetz_eq" [ "timetz" , "timetz" ] False "bool"
>        , CatCreateFunction
>            "timetz_ge" [ "timetz" , "timetz" ] False "bool"
>        , CatCreateFunction
>            "timetz_gt" [ "timetz" , "timetz" ] False "bool"
>        , CatCreateFunction "timetz_hash" [ "timetz" ] False "int4"
>        , CatCreateFunction
>            "timetz_in" [ "int4" , "oid" , "cstring" ] False "timetz"
>        , CatCreateFunction
>            "timetz_larger" [ "timetz" , "timetz" ] False "timetz"
>        , CatCreateFunction
>            "timetz_le" [ "timetz" , "timetz" ] False "bool"
>        , CatCreateFunction
>            "timetz_lt" [ "timetz" , "timetz" ] False "bool"
>        , CatCreateFunction
>            "timetz_mi_interval" [ "interval" , "timetz" ] False "timetz"
>        , CatCreateFunction
>            "timetz_ne" [ "timetz" , "timetz" ] False "bool"
>        , CatCreateFunction "timetz_out" [ "timetz" ] False "cstring"
>        , CatCreateFunction
>            "timetz_pl_interval" [ "interval" , "timetz" ] False "timetz"
>        , CatCreateFunction
>            "timetz_recv" [ "oid" , "internal" , "int4" ] False "timetz"
>        , CatCreateFunction "timetz_send" [ "timetz" ] False "bytea"
>        , CatCreateFunction
>            "timetz_smaller" [ "timetz" , "timetz" ] False "timetz"
>        , CatCreateFunction
>            "timetzdate_pl" [ "date" , "timetz" ] False "timestamptz"
>        , CatCreateFunction "timetztypmodin" [ "_cstring" ] False "int4"
>        , CatCreateFunction "timetztypmodout" [ "int4" ] False "cstring"
>        , CatCreateFunction
>            "timezone" [ "interval" , "timestamptz" ] False "timestamp"
>        , CatCreateFunction
>            "timezone" [ "text" , "timestamptz" ] False "timestamp"
>        , CatCreateFunction "timezone" [ "text" , "timetz" ] False "timetz"
>        , CatCreateFunction
>            "timezone" [ "interval" , "timetz" ] False "timetz"
>        , CatCreateFunction
>            "timezone" [ "text" , "timestamp" ] False "timestamptz"
>        , CatCreateFunction
>            "timezone" [ "timestamp" , "interval" ] False "timestamptz"
>        , CatCreateFunction
>            "tinterval" [ "abstime" , "abstime" ] False "tinterval"
>        , CatCreateFunction
>            "tintervalct" [ "tinterval" , "tinterval" ] False "bool"
>        , CatCreateFunction "tintervalend" [ "tinterval" ] False "abstime"
>        , CatCreateFunction
>            "tintervaleq" [ "tinterval" , "tinterval" ] False "bool"
>        , CatCreateFunction
>            "tintervalge" [ "tinterval" , "tinterval" ] False "bool"
>        , CatCreateFunction
>            "tintervalgt" [ "tinterval" , "tinterval" ] False "bool"
>        , CatCreateFunction "tintervalin" [ "cstring" ] False "tinterval"
>        , CatCreateFunction
>            "tintervalle" [ "tinterval" , "tinterval" ] False "bool"
>        , CatCreateFunction
>            "tintervalleneq" [ "reltime" , "tinterval" ] False "bool"
>        , CatCreateFunction
>            "tintervallenge" [ "reltime" , "tinterval" ] False "bool"
>        , CatCreateFunction
>            "tintervallengt" [ "reltime" , "tinterval" ] False "bool"
>        , CatCreateFunction
>            "tintervallenle" [ "tinterval" , "reltime" ] False "bool"
>        , CatCreateFunction
>            "tintervallenlt" [ "tinterval" , "reltime" ] False "bool"
>        , CatCreateFunction
>            "tintervallenne" [ "reltime" , "tinterval" ] False "bool"
>        , CatCreateFunction
>            "tintervallt" [ "tinterval" , "tinterval" ] False "bool"
>        , CatCreateFunction
>            "tintervalne" [ "tinterval" , "tinterval" ] False "bool"
>        , CatCreateFunction "tintervalout" [ "tinterval" ] False "cstring"
>        , CatCreateFunction
>            "tintervalov" [ "tinterval" , "tinterval" ] False "bool"
>        , CatCreateFunction
>            "tintervalrecv" [ "internal" ] False "tinterval"
>        , CatCreateFunction "tintervalrel" [ "tinterval" ] False "reltime"
>        , CatCreateFunction
>            "tintervalsame" [ "tinterval" , "tinterval" ] False "bool"
>        , CatCreateFunction "tintervalsend" [ "tinterval" ] False "bytea"
>        , CatCreateFunction
>            "tintervalstart" [ "tinterval" ] False "abstime"
>        , CatCreateFunction "to_ascii" [ "text" ] False "text"
>        , CatCreateFunction "to_ascii" [ "text" , "int4" ] False "text"
>        , CatCreateFunction "to_ascii" [ "text" , "name" ] False "text"
>        , CatCreateFunction "to_char" [ "text" , "interval" ] False "text"
>        , CatCreateFunction
>            "to_char" [ "timestamptz" , "text" ] False "text"
>        , CatCreateFunction "to_char" [ "text" , "numeric" ] False "text"
>        , CatCreateFunction "to_char" [ "text" , "int4" ] False "text"
>        , CatCreateFunction "to_char" [ "text" , "int8" ] False "text"
>        , CatCreateFunction "to_char" [ "float4" , "text" ] False "text"
>        , CatCreateFunction "to_char" [ "text" , "float8" ] False "text"
>        , CatCreateFunction "to_char" [ "timestamp" , "text" ] False "text"
>        , CatCreateFunction "to_date" [ "text" , "text" ] False "date"
>        , CatCreateFunction "to_hex" [ "int4" ] False "text"
>        , CatCreateFunction "to_hex" [ "int8" ] False "text"
>        , CatCreateFunction "to_json" [ "anyelement" ] False "json"
>        , CatCreateFunction "to_number" [ "text" , "text" ] False "numeric"
>        , CatCreateFunction "to_regclass" [ "cstring" ] False "regclass"
>        , CatCreateFunction "to_regoper" [ "cstring" ] False "regoper"
>        , CatCreateFunction
>            "to_regoperator" [ "cstring" ] False "regoperator"
>        , CatCreateFunction "to_regproc" [ "cstring" ] False "regproc"
>        , CatCreateFunction
>            "to_regprocedure" [ "cstring" ] False "regprocedure"
>        , CatCreateFunction "to_regtype" [ "cstring" ] False "regtype"
>        , CatCreateFunction "to_timestamp" [ "float8" ] False "timestamptz"
>        , CatCreateFunction
>            "to_timestamp" [ "text" , "text" ] False "timestamptz"
>        , CatCreateFunction
>            "to_tsquery" [ "regconfig" , "text" ] False "tsquery"
>        , CatCreateFunction "to_tsquery" [ "text" ] False "tsquery"
>        , CatCreateFunction
>            "to_tsvector" [ "regconfig" , "text" ] False "tsvector"
>        , CatCreateFunction "to_tsvector" [ "text" ] False "tsvector"
>        , CatCreateFunction
>            "translate" [ "text" , "text" , "text" ] False "text"
>        , CatCreateFunction "trigger_in" [ "cstring" ] False "trigger"
>        , CatCreateFunction "trigger_out" [ "trigger" ] False "cstring"
>        , CatCreateFunction "trunc" [ "macaddr" ] False "macaddr"
>        , CatCreateFunction "trunc" [ "float8" ] False "float8"
>        , CatCreateFunction "trunc" [ "int4" , "numeric" ] False "numeric"
>        , CatCreateFunction "trunc" [ "numeric" ] False "numeric"
>        , CatCreateFunction
>            "ts_debug" [ "text" , "regconfig" ] True "record"
>        , CatCreateFunction "ts_debug" [ "text" ] True "record"
>        , CatCreateFunction
>            "ts_headline"
>            [ "text" , "tsquery" , "text" , "regconfig" ]
>            False
>            "text"
>        , CatCreateFunction
>            "ts_headline" [ "text" , "regconfig" , "tsquery" ] False "text"
>        , CatCreateFunction
>            "ts_headline" [ "text" , "text" , "tsquery" ] False "text"
>        , CatCreateFunction
>            "ts_headline" [ "text" , "tsquery" ] False "text"
>        , CatCreateFunction
>            "ts_lexize" [ "regdictionary" , "text" ] False "_text"
>        , CatCreateFunction
>            "ts_match_qv" [ "tsquery" , "tsvector" ] False "bool"
>        , CatCreateFunction
>            "ts_match_tq" [ "tsquery" , "text" ] False "bool"
>        , CatCreateFunction "ts_match_tt" [ "text" , "text" ] False "bool"
>        , CatCreateFunction
>            "ts_match_vq" [ "tsquery" , "tsvector" ] False "bool"
>        , CatCreateFunction "ts_parse" [ "oid" , "text" ] True "record"
>        , CatCreateFunction "ts_parse" [ "text" , "text" ] True "record"
>        , CatCreateFunction
>            "ts_rank"
>            [ "int4" , "_float4" , "tsvector" , "tsquery" ]
>            False
>            "float4"
>        , CatCreateFunction
>            "ts_rank" [ "tsvector" , "tsquery" , "_float4" ] False "float4"
>        , CatCreateFunction
>            "ts_rank" [ "tsquery" , "int4" , "tsvector" ] False "float4"
>        , CatCreateFunction
>            "ts_rank" [ "tsvector" , "tsquery" ] False "float4"
>        , CatCreateFunction
>            "ts_rank_cd"
>            [ "tsvector" , "_float4" , "tsquery" , "int4" ]
>            False
>            "float4"
>        , CatCreateFunction
>            "ts_rank_cd" [ "_float4" , "tsvector" , "tsquery" ] False "float4"
>        , CatCreateFunction
>            "ts_rank_cd" [ "tsquery" , "tsvector" , "int4" ] False "float4"
>        , CatCreateFunction
>            "ts_rank_cd" [ "tsvector" , "tsquery" ] False "float4"
>        , CatCreateFunction
>            "ts_rewrite" [ "tsquery" , "tsquery" , "tsquery" ] False "tsquery"
>        , CatCreateFunction
>            "ts_rewrite" [ "text" , "tsquery" ] False "tsquery"
>        , CatCreateFunction "ts_stat" [ "text" ] True "record"
>        , CatCreateFunction "ts_stat" [ "text" , "text" ] True "record"
>        , CatCreateFunction "ts_token_type" [ "oid" ] True "record"
>        , CatCreateFunction "ts_token_type" [ "text" ] True "record"
>        , CatCreateFunction "ts_typanalyze" [ "internal" ] False "bool"
>        , CatCreateFunction
>            "tsmatchjoinsel"
>            [ "oid" , "internal" , "internal" , "int2" , "internal" ]
>            False
>            "float8"
>        , CatCreateFunction
>            "tsmatchsel"
>            [ "int4" , "internal" , "oid" , "internal" ]
>            False
>            "float8"
>        , CatCreateFunction
>            "tsq_mcontained" [ "tsquery" , "tsquery" ] False "bool"
>        , CatCreateFunction
>            "tsq_mcontains" [ "tsquery" , "tsquery" ] False "bool"
>        , CatCreateFunction
>            "tsquery_and" [ "tsquery" , "tsquery" ] False "tsquery"
>        , CatCreateFunction
>            "tsquery_cmp" [ "tsquery" , "tsquery" ] False "int4"
>        , CatCreateFunction
>            "tsquery_eq" [ "tsquery" , "tsquery" ] False "bool"
>        , CatCreateFunction
>            "tsquery_ge" [ "tsquery" , "tsquery" ] False "bool"
>        , CatCreateFunction
>            "tsquery_gt" [ "tsquery" , "tsquery" ] False "bool"
>        , CatCreateFunction
>            "tsquery_le" [ "tsquery" , "tsquery" ] False "bool"
>        , CatCreateFunction
>            "tsquery_lt" [ "tsquery" , "tsquery" ] False "bool"
>        , CatCreateFunction
>            "tsquery_ne" [ "tsquery" , "tsquery" ] False "bool"
>        , CatCreateFunction "tsquery_not" [ "tsquery" ] False "tsquery"
>        , CatCreateFunction
>            "tsquery_or" [ "tsquery" , "tsquery" ] False "tsquery"
>        , CatCreateFunction "tsqueryin" [ "cstring" ] False "tsquery"
>        , CatCreateFunction "tsqueryout" [ "tsquery" ] False "cstring"
>        , CatCreateFunction "tsqueryrecv" [ "internal" ] False "tsquery"
>        , CatCreateFunction "tsquerysend" [ "tsquery" ] False "bytea"
>        , CatCreateFunction
>            "tsrange" [ "timestamp" , "timestamp" ] False "tsrange"
>        , CatCreateFunction
>            "tsrange" [ "timestamp" , "timestamp" , "text" ] False "tsrange"
>        , CatCreateFunction
>            "tsrange_subdiff" [ "timestamp" , "timestamp" ] False "float8"
>        , CatCreateFunction
>            "tstzrange" [ "timestamptz" , "timestamptz" ] False "tstzrange"
>        , CatCreateFunction
>            "tstzrange"
>            [ "timestamptz" , "text" , "timestamptz" ]
>            False
>            "tstzrange"
>        , CatCreateFunction
>            "tstzrange_subdiff"
>            [ "timestamptz" , "timestamptz" ]
>            False
>            "float8"
>        , CatCreateFunction
>            "tsvector_cmp" [ "tsvector" , "tsvector" ] False "int4"
>        , CatCreateFunction
>            "tsvector_concat" [ "tsvector" , "tsvector" ] False "tsvector"
>        , CatCreateFunction
>            "tsvector_eq" [ "tsvector" , "tsvector" ] False "bool"
>        , CatCreateFunction
>            "tsvector_ge" [ "tsvector" , "tsvector" ] False "bool"
>        , CatCreateFunction
>            "tsvector_gt" [ "tsvector" , "tsvector" ] False "bool"
>        , CatCreateFunction
>            "tsvector_le" [ "tsvector" , "tsvector" ] False "bool"
>        , CatCreateFunction
>            "tsvector_lt" [ "tsvector" , "tsvector" ] False "bool"
>        , CatCreateFunction
>            "tsvector_ne" [ "tsvector" , "tsvector" ] False "bool"
>        , CatCreateFunction "tsvectorin" [ "cstring" ] False "tsvector"
>        , CatCreateFunction "tsvectorout" [ "tsvector" ] False "cstring"
>        , CatCreateFunction "tsvectorrecv" [ "internal" ] False "tsvector"
>        , CatCreateFunction "tsvectorsend" [ "tsvector" ] False "bytea"
>        , CatCreateFunction
>            "txid_snapshot_in" [ "cstring" ] False "txid_snapshot"
>        , CatCreateFunction
>            "txid_snapshot_out" [ "txid_snapshot" ] False "cstring"
>        , CatCreateFunction
>            "txid_snapshot_recv" [ "internal" ] False "txid_snapshot"
>        , CatCreateFunction
>            "txid_snapshot_send" [ "txid_snapshot" ] False "bytea"
>        , CatCreateFunction
>            "txid_snapshot_xip" [ "txid_snapshot" ] True "int8"
>        , CatCreateFunction
>            "txid_snapshot_xmax" [ "txid_snapshot" ] False "int8"
>        , CatCreateFunction
>            "txid_snapshot_xmin" [ "txid_snapshot" ] False "int8"
>        , CatCreateFunction
>            "txid_visible_in_snapshot"
>            [ "int8" , "txid_snapshot" ]
>            False
>            "bool"
>        , CatCreateFunction
>            "uhc_to_utf8"
>            [ "internal" , "int4" , "int4" , "cstring" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction "unknownin" [ "cstring" ] False "unknown"
>        , CatCreateFunction "unknownout" [ "unknown" ] False "cstring"
>        , CatCreateFunction "unknownrecv" [ "internal" ] False "unknown"
>        , CatCreateFunction "unknownsend" [ "unknown" ] False "bytea"
>        , CatCreateFunction "unnest" [ "anyarray" ] True "anyelement"
>        , CatCreateFunction "upper" [ "text" ] False "text"
>        , CatCreateFunction "upper" [ "anyrange" ] False "anyelement"
>        , CatCreateFunction "upper_inc" [ "anyrange" ] False "bool"
>        , CatCreateFunction "upper_inf" [ "anyrange" ] False "bool"
>        , CatCreateFunction
>            "utf8_to_ascii"
>            [ "internal" , "int4" , "cstring" , "int4" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "utf8_to_big5"
>            [ "int4" , "int4" , "internal" , "int4" , "cstring" ]
>            False
>            "void"
>        , CatCreateFunction
>            "utf8_to_euc_cn"
>            [ "cstring" , "int4" , "int4" , "internal" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "utf8_to_euc_jis_2004"
>            [ "int4" , "internal" , "int4" , "cstring" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "utf8_to_euc_jp"
>            [ "cstring" , "int4" , "int4" , "internal" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "utf8_to_euc_kr"
>            [ "int4" , "int4" , "int4" , "internal" , "cstring" ]
>            False
>            "void"
>        , CatCreateFunction
>            "utf8_to_euc_tw"
>            [ "cstring" , "int4" , "internal" , "int4" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "utf8_to_gb18030"
>            [ "internal" , "int4" , "int4" , "cstring" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "utf8_to_gbk"
>            [ "int4" , "int4" , "int4" , "cstring" , "internal" ]
>            False
>            "void"
>        , CatCreateFunction
>            "utf8_to_iso8859"
>            [ "int4" , "int4" , "cstring" , "internal" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "utf8_to_iso8859_1"
>            [ "internal" , "int4" , "cstring" , "int4" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "utf8_to_johab"
>            [ "internal" , "cstring" , "int4" , "int4" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "utf8_to_koi8r"
>            [ "internal" , "int4" , "int4" , "int4" , "cstring" ]
>            False
>            "void"
>        , CatCreateFunction
>            "utf8_to_koi8u"
>            [ "int4" , "int4" , "cstring" , "internal" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "utf8_to_shift_jis_2004"
>            [ "int4" , "internal" , "cstring" , "int4" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "utf8_to_sjis"
>            [ "cstring" , "int4" , "internal" , "int4" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "utf8_to_uhc"
>            [ "int4" , "int4" , "int4" , "internal" , "cstring" ]
>            False
>            "void"
>        , CatCreateFunction
>            "utf8_to_win"
>            [ "internal" , "int4" , "cstring" , "int4" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction "uuid_cmp" [ "uuid" , "uuid" ] False "int4"
>        , CatCreateFunction "uuid_eq" [ "uuid" , "uuid" ] False "bool"
>        , CatCreateFunction "uuid_ge" [ "uuid" , "uuid" ] False "bool"
>        , CatCreateFunction "uuid_gt" [ "uuid" , "uuid" ] False "bool"
>        , CatCreateFunction "uuid_hash" [ "uuid" ] False "int4"
>        , CatCreateFunction "uuid_in" [ "cstring" ] False "uuid"
>        , CatCreateFunction "uuid_le" [ "uuid" , "uuid" ] False "bool"
>        , CatCreateFunction "uuid_lt" [ "uuid" , "uuid" ] False "bool"
>        , CatCreateFunction "uuid_ne" [ "uuid" , "uuid" ] False "bool"
>        , CatCreateFunction "uuid_out" [ "uuid" ] False "cstring"
>        , CatCreateFunction "uuid_recv" [ "internal" ] False "uuid"
>        , CatCreateFunction "uuid_send" [ "uuid" ] False "bytea"
>        , CatCreateFunction
>            "varbit" [ "bool" , "int4" , "varbit" ] False "varbit"
>        , CatCreateFunction
>            "varbit_in" [ "int4" , "oid" , "cstring" ] False "varbit"
>        , CatCreateFunction "varbit_out" [ "varbit" ] False "cstring"
>        , CatCreateFunction
>            "varbit_recv" [ "internal" , "int4" , "oid" ] False "varbit"
>        , CatCreateFunction "varbit_send" [ "varbit" ] False "bytea"
>        , CatCreateFunction
>            "varbit_transform" [ "internal" ] False "internal"
>        , CatCreateFunction
>            "varbitcmp" [ "varbit" , "varbit" ] False "int4"
>        , CatCreateFunction "varbiteq" [ "varbit" , "varbit" ] False "bool"
>        , CatCreateFunction "varbitge" [ "varbit" , "varbit" ] False "bool"
>        , CatCreateFunction "varbitgt" [ "varbit" , "varbit" ] False "bool"
>        , CatCreateFunction "varbitle" [ "varbit" , "varbit" ] False "bool"
>        , CatCreateFunction "varbitlt" [ "varbit" , "varbit" ] False "bool"
>        , CatCreateFunction "varbitne" [ "varbit" , "varbit" ] False "bool"
>        , CatCreateFunction "varbittypmodin" [ "_cstring" ] False "int4"
>        , CatCreateFunction "varbittypmodout" [ "int4" ] False "cstring"
>        , CatCreateFunction
>            "varchar" [ "bool" , "int4" , "varchar" ] False "varchar"
>        , CatCreateFunction "varchar" [ "name" ] False "varchar"
>        , CatCreateFunction
>            "varchar_transform" [ "internal" ] False "internal"
>        , CatCreateFunction
>            "varcharin" [ "int4" , "oid" , "cstring" ] False "varchar"
>        , CatCreateFunction "varcharout" [ "varchar" ] False "cstring"
>        , CatCreateFunction
>            "varcharrecv" [ "internal" , "oid" , "int4" ] False "varchar"
>        , CatCreateFunction "varcharsend" [ "varchar" ] False "bytea"
>        , CatCreateFunction "varchartypmodin" [ "_cstring" ] False "int4"
>        , CatCreateFunction "varchartypmodout" [ "int4" ] False "cstring"
>        , CatCreateFunction "void_in" [ "cstring" ] False "void"
>        , CatCreateFunction "void_out" [ "void" ] False "cstring"
>        , CatCreateFunction "void_recv" [ "internal" ] False "void"
>        , CatCreateFunction "void_send" [ "void" ] False "bytea"
>        , CatCreateFunction "width" [ "box" ] False "float8"
>        , CatCreateFunction
>            "width_bucket"
>            [ "int4" , "float8" , "float8" , "float8" ]
>            False
>            "int4"
>        , CatCreateFunction
>            "width_bucket"
>            [ "numeric" , "numeric" , "int4" , "numeric" ]
>            False
>            "int4"
>        , CatCreateFunction
>            "win1250_to_latin2"
>            [ "int4" , "int4" , "internal" , "cstring" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "win1250_to_mic"
>            [ "internal" , "int4" , "int4" , "int4" , "cstring" ]
>            False
>            "void"
>        , CatCreateFunction
>            "win1251_to_iso"
>            [ "cstring" , "int4" , "internal" , "int4" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "win1251_to_koi8r"
>            [ "int4" , "internal" , "cstring" , "int4" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "win1251_to_mic"
>            [ "int4" , "int4" , "cstring" , "internal" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "win1251_to_win866"
>            [ "int4" , "int4" , "internal" , "cstring" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "win866_to_iso"
>            [ "int4" , "int4" , "cstring" , "internal" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "win866_to_koi8r"
>            [ "cstring" , "int4" , "int4" , "internal" , "int4" ]
>            False
>            "void"
>        , CatCreateFunction
>            "win866_to_mic"
>            [ "cstring" , "int4" , "int4" , "int4" , "internal" ]
>            False
>            "void"
>        , CatCreateFunction
>            "win866_to_win1251"
>            [ "internal" , "int4" , "int4" , "int4" , "cstring" ]
>            False
>            "void"
>        , CatCreateFunction
>            "win_to_utf8"
>            [ "int4" , "int4" , "internal" , "int4" , "cstring" ]
>            False
>            "void"
>        , CatCreateFunction "xideq" [ "xid" , "xid" ] False "bool"
>        , CatCreateFunction "xideqint4" [ "xid" , "int4" ] False "bool"
>        , CatCreateFunction "xidin" [ "cstring" ] False "xid"
>        , CatCreateFunction "xidout" [ "xid" ] False "cstring"
>        , CatCreateFunction "xidrecv" [ "internal" ] False "xid"
>        , CatCreateFunction "xidsend" [ "xid" ] False "bytea"
>        , CatCreateFunction "xml" [ "text" ] False "xml"
>        , CatCreateFunction "xml_in" [ "cstring" ] False "xml"
>        , CatCreateFunction "xml_is_well_formed" [ "text" ] False "bool"
>        , CatCreateFunction
>            "xml_is_well_formed_content" [ "text" ] False "bool"
>        , CatCreateFunction
>            "xml_is_well_formed_document" [ "text" ] False "bool"
>        , CatCreateFunction "xml_out" [ "xml" ] False "cstring"
>        , CatCreateFunction "xml_recv" [ "internal" ] False "xml"
>        , CatCreateFunction "xml_send" [ "xml" ] False "bytea"
>        , CatCreateFunction "xmlcomment" [ "text" ] False "xml"
>        , CatCreateFunction "xmlconcat2" [ "xml" , "xml" ] False "xml"
>        , CatCreateFunction "xmlexists" [ "xml" , "text" ] False "bool"
>        , CatCreateFunction "xmlvalidate" [ "xml" , "text" ] False "bool"
>        , CatCreateFunction
>            "xpath" [ "xml" , "text" , "_text" ] False "_xml"
>        , CatCreateFunction "xpath" [ "text" , "xml" ] False "_xml"
>        , CatCreateFunction
>            "xpath_exists" [ "text" , "_text" , "xml" ] False "bool"
>        , CatCreateFunction "xpath_exists" [ "text" , "xml" ] False "bool"
>        , CatCreateAggregate "array_agg" [ "anyelement" ] "anyarray"
>        , CatCreateAggregate "avg" [ "int8" ] "numeric"
>        , CatCreateAggregate "avg" [ "int4" ] "numeric"
>        , CatCreateAggregate "avg" [ "int2" ] "numeric"
>        , CatCreateAggregate "avg" [ "numeric" ] "numeric"
>        , CatCreateAggregate "avg" [ "float4" ] "float8"
>        , CatCreateAggregate "avg" [ "float8" ] "float8"
>        , CatCreateAggregate "avg" [ "interval" ] "interval"
>        , CatCreateAggregate "bit_and" [ "int2" ] "int2"
>        , CatCreateAggregate "bit_and" [ "int4" ] "int4"
>        , CatCreateAggregate "bit_and" [ "int8" ] "int8"
>        , CatCreateAggregate "bit_and" [ "bit" ] "bit"
>        , CatCreateAggregate "bit_or" [ "int2" ] "int2"
>        , CatCreateAggregate "bit_or" [ "int4" ] "int4"
>        , CatCreateAggregate "bit_or" [ "int8" ] "int8"
>        , CatCreateAggregate "bit_or" [ "bit" ] "bit"
>        , CatCreateAggregate "bool_and" [ "bool" ] "bool"
>        , CatCreateAggregate "bool_or" [ "bool" ] "bool"
>        , CatCreateAggregate "corr" [ "float8" , "float8" ] "float8"
>        , CatCreateAggregate "count" [ "any" ] "int8"
>        , CatCreateAggregate "covar_pop" [ "float8" , "float8" ] "float8"
>        , CatCreateAggregate "covar_samp" [ "float8" , "float8" ] "float8"
>        , CatCreateAggregate "every" [ "bool" ] "bool"
>        , CatCreateAggregate "json_agg" [ "anyelement" ] "json"
>        , CatCreateAggregate "json_object_agg" [ "any" , "any" ] "json"
>        , CatCreateAggregate "max" [ "anyarray" ] "anyarray"
>        , CatCreateAggregate "max" [ "int8" ] "int8"
>        , CatCreateAggregate "max" [ "int4" ] "int4"
>        , CatCreateAggregate "max" [ "int2" ] "int2"
>        , CatCreateAggregate "max" [ "oid" ] "oid"
>        , CatCreateAggregate "max" [ "float4" ] "float4"
>        , CatCreateAggregate "max" [ "float8" ] "float8"
>        , CatCreateAggregate "max" [ "abstime" ] "abstime"
>        , CatCreateAggregate "max" [ "date" ] "date"
>        , CatCreateAggregate "max" [ "time" ] "time"
>        , CatCreateAggregate "max" [ "timetz" ] "timetz"
>        , CatCreateAggregate "max" [ "money" ] "money"
>        , CatCreateAggregate "max" [ "timestamp" ] "timestamp"
>        , CatCreateAggregate "max" [ "timestamptz" ] "timestamptz"
>        , CatCreateAggregate "max" [ "interval" ] "interval"
>        , CatCreateAggregate "max" [ "text" ] "text"
>        , CatCreateAggregate "max" [ "numeric" ] "numeric"
>        , CatCreateAggregate "max" [ "bpchar" ] "bpchar"
>        , CatCreateAggregate "max" [ "tid" ] "tid"
>        , CatCreateAggregate "max" [ "anyenum" ] "anyenum"
>        , CatCreateAggregate "min" [ "anyarray" ] "anyarray"
>        , CatCreateAggregate "min" [ "int8" ] "int8"
>        , CatCreateAggregate "min" [ "int4" ] "int4"
>        , CatCreateAggregate "min" [ "int2" ] "int2"
>        , CatCreateAggregate "min" [ "oid" ] "oid"
>        , CatCreateAggregate "min" [ "float4" ] "float4"
>        , CatCreateAggregate "min" [ "float8" ] "float8"
>        , CatCreateAggregate "min" [ "abstime" ] "abstime"
>        , CatCreateAggregate "min" [ "date" ] "date"
>        , CatCreateAggregate "min" [ "time" ] "time"
>        , CatCreateAggregate "min" [ "timetz" ] "timetz"
>        , CatCreateAggregate "min" [ "money" ] "money"
>        , CatCreateAggregate "min" [ "timestamp" ] "timestamp"
>        , CatCreateAggregate "min" [ "timestamptz" ] "timestamptz"
>        , CatCreateAggregate "min" [ "interval" ] "interval"
>        , CatCreateAggregate "min" [ "text" ] "text"
>        , CatCreateAggregate "min" [ "numeric" ] "numeric"
>        , CatCreateAggregate "min" [ "bpchar" ] "bpchar"
>        , CatCreateAggregate "min" [ "tid" ] "tid"
>        , CatCreateAggregate "min" [ "anyenum" ] "anyenum"
>        , CatCreateAggregate "mode" [ "anyelement" ] "anyelement"
>        , CatCreateAggregate
>            "percentile_cont" [ "float8" , "float8" ] "float8"
>        , CatCreateAggregate
>            "percentile_cont" [ "float8" , "interval" ] "interval"
>        , CatCreateAggregate
>            "percentile_cont" [ "float8" , "_float8" ] "_float8"
>        , CatCreateAggregate
>            "percentile_cont" [ "interval" , "_float8" ] "_interval"
>        , CatCreateAggregate
>            "percentile_disc" [ "float8" , "anyelement" ] "anyelement"
>        , CatCreateAggregate
>            "percentile_disc" [ "anyelement" , "_float8" ] "anyarray"
>        , CatCreateAggregate "regr_avgx" [ "float8" , "float8" ] "float8"
>        , CatCreateAggregate "regr_avgy" [ "float8" , "float8" ] "float8"
>        , CatCreateAggregate "regr_count" [ "float8" , "float8" ] "int8"
>        , CatCreateAggregate
>            "regr_intercept" [ "float8" , "float8" ] "float8"
>        , CatCreateAggregate "regr_r2" [ "float8" , "float8" ] "float8"
>        , CatCreateAggregate "regr_slope" [ "float8" , "float8" ] "float8"
>        , CatCreateAggregate "regr_sxx" [ "float8" , "float8" ] "float8"
>        , CatCreateAggregate "regr_sxy" [ "float8" , "float8" ] "float8"
>        , CatCreateAggregate "regr_syy" [ "float8" , "float8" ] "float8"
>        , CatCreateAggregate "stddev" [ "int8" ] "numeric"
>        , CatCreateAggregate "stddev" [ "int4" ] "numeric"
>        , CatCreateAggregate "stddev" [ "int2" ] "numeric"
>        , CatCreateAggregate "stddev" [ "float4" ] "float8"
>        , CatCreateAggregate "stddev" [ "float8" ] "float8"
>        , CatCreateAggregate "stddev" [ "numeric" ] "numeric"
>        , CatCreateAggregate "stddev_pop" [ "int8" ] "numeric"
>        , CatCreateAggregate "stddev_pop" [ "int4" ] "numeric"
>        , CatCreateAggregate "stddev_pop" [ "int2" ] "numeric"
>        , CatCreateAggregate "stddev_pop" [ "float4" ] "float8"
>        , CatCreateAggregate "stddev_pop" [ "float8" ] "float8"
>        , CatCreateAggregate "stddev_pop" [ "numeric" ] "numeric"
>        , CatCreateAggregate "stddev_samp" [ "int8" ] "numeric"
>        , CatCreateAggregate "stddev_samp" [ "int4" ] "numeric"
>        , CatCreateAggregate "stddev_samp" [ "int2" ] "numeric"
>        , CatCreateAggregate "stddev_samp" [ "float4" ] "float8"
>        , CatCreateAggregate "stddev_samp" [ "float8" ] "float8"
>        , CatCreateAggregate "stddev_samp" [ "numeric" ] "numeric"
>        , CatCreateAggregate "string_agg" [ "text" , "text" ] "text"
>        , CatCreateAggregate "string_agg" [ "bytea" , "bytea" ] "bytea"
>        , CatCreateAggregate "sum" [ "int8" ] "numeric"
>        , CatCreateAggregate "sum" [ "int4" ] "int8"
>        , CatCreateAggregate "sum" [ "int2" ] "int8"
>        , CatCreateAggregate "sum" [ "float4" ] "float4"
>        , CatCreateAggregate "sum" [ "float8" ] "float8"
>        , CatCreateAggregate "sum" [ "money" ] "money"
>        , CatCreateAggregate "sum" [ "interval" ] "interval"
>        , CatCreateAggregate "sum" [ "numeric" ] "numeric"
>        , CatCreateAggregate "var_pop" [ "int8" ] "numeric"
>        , CatCreateAggregate "var_pop" [ "int4" ] "numeric"
>        , CatCreateAggregate "var_pop" [ "int2" ] "numeric"
>        , CatCreateAggregate "var_pop" [ "float4" ] "float8"
>        , CatCreateAggregate "var_pop" [ "float8" ] "float8"
>        , CatCreateAggregate "var_pop" [ "numeric" ] "numeric"
>        , CatCreateAggregate "var_samp" [ "int8" ] "numeric"
>        , CatCreateAggregate "var_samp" [ "int4" ] "numeric"
>        , CatCreateAggregate "var_samp" [ "int2" ] "numeric"
>        , CatCreateAggregate "var_samp" [ "float4" ] "float8"
>        , CatCreateAggregate "var_samp" [ "float8" ] "float8"
>        , CatCreateAggregate "var_samp" [ "numeric" ] "numeric"
>        , CatCreateAggregate "variance" [ "int8" ] "numeric"
>        , CatCreateAggregate "variance" [ "int4" ] "numeric"
>        , CatCreateAggregate "variance" [ "int2" ] "numeric"
>        , CatCreateAggregate "variance" [ "float4" ] "float8"
>        , CatCreateAggregate "variance" [ "float8" ] "float8"
>        , CatCreateAggregate "variance" [ "numeric" ] "numeric"
>        , CatCreateAggregate "xmlagg" [ "xml" ] "xml"
>        , CatCreateCast "abstime" "date" AssignmentCastContext
>        , CatCreateCast "abstime" "int4" ExplicitCastContext
>        , CatCreateCast "abstime" "time" AssignmentCastContext
>        , CatCreateCast "abstime" "timestamp" ImplicitCastContext
>        , CatCreateCast "abstime" "timestamptz" ImplicitCastContext
>        , CatCreateCast "bit" "bit" ImplicitCastContext
>        , CatCreateCast "bit" "int4" ExplicitCastContext
>        , CatCreateCast "bit" "int8" ExplicitCastContext
>        , CatCreateCast "bit" "varbit" ImplicitCastContext
>        , CatCreateCast "bool" "bpchar" AssignmentCastContext
>        , CatCreateCast "bool" "int4" ExplicitCastContext
>        , CatCreateCast "bool" "text" AssignmentCastContext
>        , CatCreateCast "bool" "varchar" AssignmentCastContext
>        , CatCreateCast "box" "circle" ExplicitCastContext
>        , CatCreateCast "box" "lseg" ExplicitCastContext
>        , CatCreateCast "box" "point" ExplicitCastContext
>        , CatCreateCast "box" "polygon" AssignmentCastContext
>        , CatCreateCast "bpchar" "bpchar" ImplicitCastContext
>        , CatCreateCast "bpchar" "char" AssignmentCastContext
>        , CatCreateCast "bpchar" "name" ImplicitCastContext
>        , CatCreateCast "bpchar" "text" ImplicitCastContext
>        , CatCreateCast "bpchar" "varchar" ImplicitCastContext
>        , CatCreateCast "bpchar" "xml" ExplicitCastContext
>        , CatCreateCast "char" "bpchar" AssignmentCastContext
>        , CatCreateCast "char" "int4" ExplicitCastContext
>        , CatCreateCast "char" "text" ImplicitCastContext
>        , CatCreateCast "char" "varchar" AssignmentCastContext
>        , CatCreateCast "cidr" "bpchar" AssignmentCastContext
>        , CatCreateCast "cidr" "inet" ImplicitCastContext
>        , CatCreateCast "cidr" "text" AssignmentCastContext
>        , CatCreateCast "cidr" "varchar" AssignmentCastContext
>        , CatCreateCast "circle" "box" ExplicitCastContext
>        , CatCreateCast "circle" "point" ExplicitCastContext
>        , CatCreateCast "circle" "polygon" ExplicitCastContext
>        , CatCreateCast "date" "timestamp" ImplicitCastContext
>        , CatCreateCast "date" "timestamptz" ImplicitCastContext
>        , CatCreateCast "float4" "float8" ImplicitCastContext
>        , CatCreateCast "float4" "int2" AssignmentCastContext
>        , CatCreateCast "float4" "int4" AssignmentCastContext
>        , CatCreateCast "float4" "int8" AssignmentCastContext
>        , CatCreateCast "float4" "numeric" AssignmentCastContext
>        , CatCreateCast "float8" "float4" AssignmentCastContext
>        , CatCreateCast "float8" "int2" AssignmentCastContext
>        , CatCreateCast "float8" "int4" AssignmentCastContext
>        , CatCreateCast "float8" "int8" AssignmentCastContext
>        , CatCreateCast "float8" "numeric" AssignmentCastContext
>        , CatCreateCast "inet" "bpchar" AssignmentCastContext
>        , CatCreateCast "inet" "cidr" AssignmentCastContext
>        , CatCreateCast "inet" "text" AssignmentCastContext
>        , CatCreateCast "inet" "varchar" AssignmentCastContext
>        , CatCreateCast "int2" "float4" ImplicitCastContext
>        , CatCreateCast "int2" "float8" ImplicitCastContext
>        , CatCreateCast "int2" "int4" ImplicitCastContext
>        , CatCreateCast "int2" "int8" ImplicitCastContext
>        , CatCreateCast "int2" "numeric" ImplicitCastContext
>        , CatCreateCast "int2" "oid" ImplicitCastContext
>        , CatCreateCast "int2" "regclass" ImplicitCastContext
>        , CatCreateCast "int2" "regconfig" ImplicitCastContext
>        , CatCreateCast "int2" "regdictionary" ImplicitCastContext
>        , CatCreateCast "int2" "regoper" ImplicitCastContext
>        , CatCreateCast "int2" "regoperator" ImplicitCastContext
>        , CatCreateCast "int2" "regproc" ImplicitCastContext
>        , CatCreateCast "int2" "regprocedure" ImplicitCastContext
>        , CatCreateCast "int2" "regtype" ImplicitCastContext
>        , CatCreateCast "int4" "abstime" ExplicitCastContext
>        , CatCreateCast "int4" "bit" ExplicitCastContext
>        , CatCreateCast "int4" "bool" ExplicitCastContext
>        , CatCreateCast "int4" "char" ExplicitCastContext
>        , CatCreateCast "int4" "float4" ImplicitCastContext
>        , CatCreateCast "int4" "float8" ImplicitCastContext
>        , CatCreateCast "int4" "int2" AssignmentCastContext
>        , CatCreateCast "int4" "int8" ImplicitCastContext
>        , CatCreateCast "int4" "money" AssignmentCastContext
>        , CatCreateCast "int4" "numeric" ImplicitCastContext
>        , CatCreateCast "int4" "oid" ImplicitCastContext
>        , CatCreateCast "int4" "regclass" ImplicitCastContext
>        , CatCreateCast "int4" "regconfig" ImplicitCastContext
>        , CatCreateCast "int4" "regdictionary" ImplicitCastContext
>        , CatCreateCast "int4" "regoper" ImplicitCastContext
>        , CatCreateCast "int4" "regoperator" ImplicitCastContext
>        , CatCreateCast "int4" "regproc" ImplicitCastContext
>        , CatCreateCast "int4" "regprocedure" ImplicitCastContext
>        , CatCreateCast "int4" "regtype" ImplicitCastContext
>        , CatCreateCast "int4" "reltime" ExplicitCastContext
>        , CatCreateCast "int8" "bit" ExplicitCastContext
>        , CatCreateCast "int8" "float4" ImplicitCastContext
>        , CatCreateCast "int8" "float8" ImplicitCastContext
>        , CatCreateCast "int8" "int2" AssignmentCastContext
>        , CatCreateCast "int8" "int4" AssignmentCastContext
>        , CatCreateCast "int8" "money" AssignmentCastContext
>        , CatCreateCast "int8" "numeric" ImplicitCastContext
>        , CatCreateCast "int8" "oid" ImplicitCastContext
>        , CatCreateCast "int8" "regclass" ImplicitCastContext
>        , CatCreateCast "int8" "regconfig" ImplicitCastContext
>        , CatCreateCast "int8" "regdictionary" ImplicitCastContext
>        , CatCreateCast "int8" "regoper" ImplicitCastContext
>        , CatCreateCast "int8" "regoperator" ImplicitCastContext
>        , CatCreateCast "int8" "regproc" ImplicitCastContext
>        , CatCreateCast "int8" "regprocedure" ImplicitCastContext
>        , CatCreateCast "int8" "regtype" ImplicitCastContext
>        , CatCreateCast "interval" "interval" ImplicitCastContext
>        , CatCreateCast "interval" "reltime" AssignmentCastContext
>        , CatCreateCast "interval" "time" AssignmentCastContext
>        , CatCreateCast "json" "jsonb" ExplicitCastContext
>        , CatCreateCast "jsonb" "json" ExplicitCastContext
>        , CatCreateCast "lseg" "point" ExplicitCastContext
>        , CatCreateCast "money" "numeric" AssignmentCastContext
>        , CatCreateCast "name" "bpchar" AssignmentCastContext
>        , CatCreateCast "name" "text" ImplicitCastContext
>        , CatCreateCast "name" "varchar" AssignmentCastContext
>        , CatCreateCast "numeric" "float4" ImplicitCastContext
>        , CatCreateCast "numeric" "float8" ImplicitCastContext
>        , CatCreateCast "numeric" "int2" AssignmentCastContext
>        , CatCreateCast "numeric" "int4" AssignmentCastContext
>        , CatCreateCast "numeric" "int8" AssignmentCastContext
>        , CatCreateCast "numeric" "money" AssignmentCastContext
>        , CatCreateCast "numeric" "numeric" ImplicitCastContext
>        , CatCreateCast "oid" "int4" AssignmentCastContext
>        , CatCreateCast "oid" "int8" AssignmentCastContext
>        , CatCreateCast "oid" "regclass" ImplicitCastContext
>        , CatCreateCast "oid" "regconfig" ImplicitCastContext
>        , CatCreateCast "oid" "regdictionary" ImplicitCastContext
>        , CatCreateCast "oid" "regoper" ImplicitCastContext
>        , CatCreateCast "oid" "regoperator" ImplicitCastContext
>        , CatCreateCast "oid" "regproc" ImplicitCastContext
>        , CatCreateCast "oid" "regprocedure" ImplicitCastContext
>        , CatCreateCast "oid" "regtype" ImplicitCastContext
>        , CatCreateCast "path" "point" ExplicitCastContext
>        , CatCreateCast "path" "polygon" AssignmentCastContext
>        , CatCreateCast "pg_node_tree" "text" ImplicitCastContext
>        , CatCreateCast "polygon" "box" ExplicitCastContext
>        , CatCreateCast "polygon" "circle" ExplicitCastContext
>        , CatCreateCast "polygon" "path" AssignmentCastContext
>        , CatCreateCast "polygon" "point" ExplicitCastContext
>        , CatCreateCast "regclass" "int4" AssignmentCastContext
>        , CatCreateCast "regclass" "int8" AssignmentCastContext
>        , CatCreateCast "regclass" "oid" ImplicitCastContext
>        , CatCreateCast "regconfig" "int4" AssignmentCastContext
>        , CatCreateCast "regconfig" "int8" AssignmentCastContext
>        , CatCreateCast "regconfig" "oid" ImplicitCastContext
>        , CatCreateCast "regdictionary" "int4" AssignmentCastContext
>        , CatCreateCast "regdictionary" "int8" AssignmentCastContext
>        , CatCreateCast "regdictionary" "oid" ImplicitCastContext
>        , CatCreateCast "regoper" "int4" AssignmentCastContext
>        , CatCreateCast "regoper" "int8" AssignmentCastContext
>        , CatCreateCast "regoper" "oid" ImplicitCastContext
>        , CatCreateCast "regoper" "regoperator" ImplicitCastContext
>        , CatCreateCast "regoperator" "int4" AssignmentCastContext
>        , CatCreateCast "regoperator" "int8" AssignmentCastContext
>        , CatCreateCast "regoperator" "oid" ImplicitCastContext
>        , CatCreateCast "regoperator" "regoper" ImplicitCastContext
>        , CatCreateCast "regproc" "int4" AssignmentCastContext
>        , CatCreateCast "regproc" "int8" AssignmentCastContext
>        , CatCreateCast "regproc" "oid" ImplicitCastContext
>        , CatCreateCast "regproc" "regprocedure" ImplicitCastContext
>        , CatCreateCast "regprocedure" "int4" AssignmentCastContext
>        , CatCreateCast "regprocedure" "int8" AssignmentCastContext
>        , CatCreateCast "regprocedure" "oid" ImplicitCastContext
>        , CatCreateCast "regprocedure" "regproc" ImplicitCastContext
>        , CatCreateCast "regtype" "int4" AssignmentCastContext
>        , CatCreateCast "regtype" "int8" AssignmentCastContext
>        , CatCreateCast "regtype" "oid" ImplicitCastContext
>        , CatCreateCast "reltime" "int4" ExplicitCastContext
>        , CatCreateCast "reltime" "interval" ImplicitCastContext
>        , CatCreateCast "text" "bpchar" ImplicitCastContext
>        , CatCreateCast "text" "char" AssignmentCastContext
>        , CatCreateCast "text" "name" ImplicitCastContext
>        , CatCreateCast "text" "regclass" ImplicitCastContext
>        , CatCreateCast "text" "varchar" ImplicitCastContext
>        , CatCreateCast "text" "xml" ExplicitCastContext
>        , CatCreateCast "time" "interval" ImplicitCastContext
>        , CatCreateCast "time" "time" ImplicitCastContext
>        , CatCreateCast "time" "timetz" ImplicitCastContext
>        , CatCreateCast "timestamp" "abstime" AssignmentCastContext
>        , CatCreateCast "timestamp" "date" AssignmentCastContext
>        , CatCreateCast "timestamp" "time" AssignmentCastContext
>        , CatCreateCast "timestamp" "timestamp" ImplicitCastContext
>        , CatCreateCast "timestamp" "timestamptz" ImplicitCastContext
>        , CatCreateCast "timestamptz" "abstime" AssignmentCastContext
>        , CatCreateCast "timestamptz" "date" AssignmentCastContext
>        , CatCreateCast "timestamptz" "time" AssignmentCastContext
>        , CatCreateCast "timestamptz" "timestamp" AssignmentCastContext
>        , CatCreateCast "timestamptz" "timestamptz" ImplicitCastContext
>        , CatCreateCast "timestamptz" "timetz" AssignmentCastContext
>        , CatCreateCast "timetz" "time" AssignmentCastContext
>        , CatCreateCast "timetz" "timetz" ImplicitCastContext
>        , CatCreateCast "varbit" "bit" ImplicitCastContext
>        , CatCreateCast "varbit" "varbit" ImplicitCastContext
>        , CatCreateCast "varchar" "bpchar" ImplicitCastContext
>        , CatCreateCast "varchar" "char" AssignmentCastContext
>        , CatCreateCast "varchar" "name" ImplicitCastContext
>        , CatCreateCast "varchar" "regclass" ImplicitCastContext
>        , CatCreateCast "varchar" "text" ImplicitCastContext
>        , CatCreateCast "varchar" "varchar" ImplicitCastContext
>        , CatCreateCast "varchar" "xml" ExplicitCastContext
>        , CatCreateCast "xml" "bpchar" AssignmentCastContext
>        , CatCreateCast "xml" "text" AssignmentCastContext
>        , CatCreateCast "xml" "varchar" AssignmentCastContext
>        , CatCreateTypeCategoryEntry "abstime" ( "D" , False )
>        , CatCreateTypeCategoryEntry "aclitem" ( "U" , False )
>        , CatCreateTypeCategoryEntry "bit" ( "V" , False )
>        , CatCreateTypeCategoryEntry "bool" ( "B" , True )
>        , CatCreateTypeCategoryEntry "box" ( "G" , False )
>        , CatCreateTypeCategoryEntry "bpchar" ( "S" , False )
>        , CatCreateTypeCategoryEntry "bytea" ( "U" , False )
>        , CatCreateTypeCategoryEntry "char" ( "S" , False )
>        , CatCreateTypeCategoryEntry "cid" ( "U" , False )
>        , CatCreateTypeCategoryEntry "cidr" ( "I" , False )
>        , CatCreateTypeCategoryEntry "circle" ( "G" , False )
>        , CatCreateTypeCategoryEntry "date" ( "D" , False )
>        , CatCreateTypeCategoryEntry "float4" ( "N" , False )
>        , CatCreateTypeCategoryEntry "float8" ( "N" , True )
>        , CatCreateTypeCategoryEntry "gtsvector" ( "U" , False )
>        , CatCreateTypeCategoryEntry "inet" ( "I" , True )
>        , CatCreateTypeCategoryEntry "int2" ( "N" , False )
>        , CatCreateTypeCategoryEntry "int2vector" ( "A" , False )
>        , CatCreateTypeCategoryEntry "int4" ( "N" , False )
>        , CatCreateTypeCategoryEntry "int8" ( "N" , False )
>        , CatCreateTypeCategoryEntry "interval" ( "T" , True )
>        , CatCreateTypeCategoryEntry "json" ( "U" , False )
>        , CatCreateTypeCategoryEntry "jsonb" ( "U" , False )
>        , CatCreateTypeCategoryEntry "line" ( "G" , False )
>        , CatCreateTypeCategoryEntry "lseg" ( "G" , False )
>        , CatCreateTypeCategoryEntry "macaddr" ( "U" , False )
>        , CatCreateTypeCategoryEntry "money" ( "N" , False )
>        , CatCreateTypeCategoryEntry "name" ( "S" , False )
>        , CatCreateTypeCategoryEntry "numeric" ( "N" , False )
>        , CatCreateTypeCategoryEntry "oid" ( "N" , True )
>        , CatCreateTypeCategoryEntry "oidvector" ( "A" , False )
>        , CatCreateTypeCategoryEntry "path" ( "G" , False )
>        , CatCreateTypeCategoryEntry "pg_lsn" ( "U" , False )
>        , CatCreateTypeCategoryEntry "point" ( "G" , False )
>        , CatCreateTypeCategoryEntry "polygon" ( "G" , False )
>        , CatCreateTypeCategoryEntry "refcursor" ( "U" , False )
>        , CatCreateTypeCategoryEntry "regclass" ( "N" , False )
>        , CatCreateTypeCategoryEntry "regconfig" ( "N" , False )
>        , CatCreateTypeCategoryEntry "regdictionary" ( "N" , False )
>        , CatCreateTypeCategoryEntry "regoper" ( "N" , False )
>        , CatCreateTypeCategoryEntry "regoperator" ( "N" , False )
>        , CatCreateTypeCategoryEntry "regproc" ( "N" , False )
>        , CatCreateTypeCategoryEntry "regprocedure" ( "N" , False )
>        , CatCreateTypeCategoryEntry "regtype" ( "N" , False )
>        , CatCreateTypeCategoryEntry "reltime" ( "T" , False )
>        , CatCreateTypeCategoryEntry "text" ( "S" , True )
>        , CatCreateTypeCategoryEntry "tid" ( "U" , False )
>        , CatCreateTypeCategoryEntry "time" ( "D" , False )
>        , CatCreateTypeCategoryEntry "timestamp" ( "D" , False )
>        , CatCreateTypeCategoryEntry "timestamptz" ( "D" , True )
>        , CatCreateTypeCategoryEntry "timetz" ( "D" , False )
>        , CatCreateTypeCategoryEntry "tinterval" ( "T" , False )
>        , CatCreateTypeCategoryEntry "tsquery" ( "U" , False )
>        , CatCreateTypeCategoryEntry "tsvector" ( "U" , False )
>        , CatCreateTypeCategoryEntry "txid_snapshot" ( "U" , False )
>        , CatCreateTypeCategoryEntry "uuid" ( "U" , False )
>        , CatCreateTypeCategoryEntry "varbit" ( "V" , True )
>        , CatCreateTypeCategoryEntry "varchar" ( "S" , False )
>        , CatCreateTypeCategoryEntry "xid" ( "U" , False )
>        , CatCreateTypeCategoryEntry "xml" ( "U" , False )
>        ]


