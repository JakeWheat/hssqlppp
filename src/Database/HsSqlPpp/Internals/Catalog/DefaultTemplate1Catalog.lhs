


This file is auto generated, to regenerate use the
regenDefaultTemplate1catalog.sh script. You will need postgresql
installed to do this.

> module Database.HsSqlPpp.Internals.Catalog.DefaultTemplate1Catalog
>      (defaultTemplate1Catalog) where
>
> import Database.HsSqlPpp.Internals.Catalog.CatalogInternal
> import Database.HsSqlPpp.Internals.TypesInternal
>
> defaultTemplate1Catalog :: Catalog
> defaultTemplate1Catalog =
>     (\l -> case l of
>              Left x -> error $ show x
>              Right e -> e) $
>      updateCatalog defaultCatalog

    
>        [CatCreateScalar (ScalarType "bool") "B" True,
>         CatCreateScalar (ScalarType "bytea") "U" False,
>         CatCreateScalar (ScalarType "char") "S" False,
>         CatCreateScalar (ScalarType "name") "S" False,
>         CatCreateScalar (ScalarType "int8") "N" False,
>         CatCreateScalar (ScalarType "int2") "N" False,
>         CatCreateScalar (ScalarType "int2vector") "A" False,
>         CatCreateScalar (ScalarType "int4") "N" False,
>         CatCreateScalar (ScalarType "regproc") "N" False,
>         CatCreateScalar (ScalarType "text") "S" True,
>         CatCreateScalar (ScalarType "oid") "N" True,
>         CatCreateScalar (ScalarType "tid") "U" False,
>         CatCreateScalar (ScalarType "xid") "U" False,
>         CatCreateScalar (ScalarType "cid") "U" False,
>         CatCreateScalar (ScalarType "oidvector") "A" False,
>         CatCreateScalar (ScalarType "xml") "U" False,
>         CatCreateScalar (ScalarType "point") "G" False,
>         CatCreateScalar (ScalarType "lseg") "G" False,
>         CatCreateScalar (ScalarType "path") "G" False,
>         CatCreateScalar (ScalarType "box") "G" False,
>         CatCreateScalar (ScalarType "polygon") "G" False,
>         CatCreateScalar (ScalarType "line") "G" False,
>         CatCreateScalar (ScalarType "float4") "N" False,
>         CatCreateScalar (ScalarType "float8") "N" True,
>         CatCreateScalar (ScalarType "abstime") "D" False,
>         CatCreateScalar (ScalarType "reltime") "T" False,
>         CatCreateScalar (ScalarType "tinterval") "T" False,
>         CatCreateScalar (ScalarType "circle") "G" False,
>         CatCreateScalar (ScalarType "money") "N" False,
>         CatCreateScalar (ScalarType "macaddr") "U" False,
>         CatCreateScalar (ScalarType "inet") "I" True,
>         CatCreateScalar (ScalarType "cidr") "I" False,
>         CatCreateScalar (ScalarType "aclitem") "U" False,
>         CatCreateScalar (ScalarType "bpchar") "S" False,
>         CatCreateScalar (ScalarType "varchar") "S" False,
>         CatCreateScalar (ScalarType "date") "D" False,
>         CatCreateScalar (ScalarType "time") "D" False,
>         CatCreateScalar (ScalarType "timestamp") "D" False,
>         CatCreateScalar (ScalarType "timestamptz") "D" True,
>         CatCreateScalar (ScalarType "interval") "T" True,
>         CatCreateScalar (ScalarType "timetz") "D" False,
>         CatCreateScalar (ScalarType "bit") "V" False,
>         CatCreateScalar (ScalarType "varbit") "V" True,
>         CatCreateScalar (ScalarType "numeric") "N" False,
>         CatCreateScalar (ScalarType "refcursor") "U" False,
>         CatCreateScalar (ScalarType "regprocedure") "N" False,
>         CatCreateScalar (ScalarType "regoper") "N" False,
>         CatCreateScalar (ScalarType "regoperator") "N" False,
>         CatCreateScalar (ScalarType "regclass") "N" False,
>         CatCreateScalar (ScalarType "regtype") "N" False,
>         CatCreateScalar (ScalarType "uuid") "U" False,
>         CatCreateScalar (ScalarType "tsvector") "U" False,
>         CatCreateScalar (ScalarType "gtsvector") "U" False,
>         CatCreateScalar (ScalarType "tsquery") "U" False,
>         CatCreateScalar (ScalarType "regconfig") "N" False,
>         CatCreateScalar (ScalarType "regdictionary") "N" False,
>         CatCreateScalar (ScalarType "txid_snapshot") "U" False,
>         CatCreateDomain (DomainType "information_schema.cardinal_number")
>           (ScalarType "int4"),
>         CatCreateDomain (DomainType "information_schema.character_data")
>           (ScalarType "varchar"),
>         CatCreateDomain (DomainType "information_schema.sql_identifier")
>           (ScalarType "varchar"),
>         CatCreateDomain (DomainType "information_schema.time_stamp")
>           (ScalarType "timestamptz"),
>         CatCreateDomain (DomainType "information_schema.yes_or_no")
>           (ScalarType "varchar"),
>         CatCreateCast (ScalarType "int8") (ScalarType "int2")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "int8") (ScalarType "int4")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "int8") (ScalarType "float4")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "int8") (ScalarType "float8")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "int8") (ScalarType "numeric")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "int2") (ScalarType "int8")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "int2") (ScalarType "int4")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "int2") (ScalarType "float4")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "int2") (ScalarType "float8")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "int2") (ScalarType "numeric")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "int4") (ScalarType "int8")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "int4") (ScalarType "int2")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "int4") (ScalarType "float4")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "int4") (ScalarType "float8")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "int4") (ScalarType "numeric")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "float4") (ScalarType "int8")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "float4") (ScalarType "int2")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "float4") (ScalarType "int4")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "float4") (ScalarType "float8")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "float4") (ScalarType "numeric")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "float8") (ScalarType "int8")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "float8") (ScalarType "int2")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "float8") (ScalarType "int4")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "float8") (ScalarType "float4")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "float8") (ScalarType "numeric")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "numeric") (ScalarType "int8")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "numeric") (ScalarType "int2")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "numeric") (ScalarType "int4")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "numeric") (ScalarType "float4")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "numeric") (ScalarType "float8")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "money") (ScalarType "numeric")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "numeric") (ScalarType "money")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "int4") (ScalarType "money")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "int8") (ScalarType "money")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "int4") (ScalarType "bool")
>           ExplicitCastContext,
>         CatCreateCast (ScalarType "bool") (ScalarType "int4")
>           ExplicitCastContext,
>         CatCreateCast (ScalarType "int8") (ScalarType "oid")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "int2") (ScalarType "oid")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "int4") (ScalarType "oid")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "oid") (ScalarType "int8")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "oid") (ScalarType "int4")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "oid") (ScalarType "regproc")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "regproc") (ScalarType "oid")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "int8") (ScalarType "regproc")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "int2") (ScalarType "regproc")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "int4") (ScalarType "regproc")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "regproc") (ScalarType "int8")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "regproc") (ScalarType "int4")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "regproc") (ScalarType "regprocedure")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "regprocedure") (ScalarType "regproc")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "oid") (ScalarType "regprocedure")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "regprocedure") (ScalarType "oid")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "int8") (ScalarType "regprocedure")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "int2") (ScalarType "regprocedure")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "int4") (ScalarType "regprocedure")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "regprocedure") (ScalarType "int8")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "regprocedure") (ScalarType "int4")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "oid") (ScalarType "regoper")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "regoper") (ScalarType "oid")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "int8") (ScalarType "regoper")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "int2") (ScalarType "regoper")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "int4") (ScalarType "regoper")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "regoper") (ScalarType "int8")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "regoper") (ScalarType "int4")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "regoper") (ScalarType "regoperator")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "regoperator") (ScalarType "regoper")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "oid") (ScalarType "regoperator")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "regoperator") (ScalarType "oid")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "int8") (ScalarType "regoperator")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "int2") (ScalarType "regoperator")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "int4") (ScalarType "regoperator")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "regoperator") (ScalarType "int8")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "regoperator") (ScalarType "int4")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "oid") (ScalarType "regclass")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "regclass") (ScalarType "oid")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "int8") (ScalarType "regclass")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "int2") (ScalarType "regclass")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "int4") (ScalarType "regclass")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "regclass") (ScalarType "int8")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "regclass") (ScalarType "int4")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "oid") (ScalarType "regtype")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "regtype") (ScalarType "oid")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "int8") (ScalarType "regtype")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "int2") (ScalarType "regtype")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "int4") (ScalarType "regtype")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "regtype") (ScalarType "int8")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "regtype") (ScalarType "int4")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "oid") (ScalarType "regconfig")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "regconfig") (ScalarType "oid")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "int8") (ScalarType "regconfig")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "int2") (ScalarType "regconfig")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "int4") (ScalarType "regconfig")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "regconfig") (ScalarType "int8")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "regconfig") (ScalarType "int4")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "oid") (ScalarType "regdictionary")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "regdictionary") (ScalarType "oid")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "int8") (ScalarType "regdictionary")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "int2") (ScalarType "regdictionary")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "int4") (ScalarType "regdictionary")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "regdictionary") (ScalarType "int8")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "regdictionary") (ScalarType "int4")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "text") (ScalarType "regclass")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "varchar") (ScalarType "regclass")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "text") (ScalarType "bpchar")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "text") (ScalarType "varchar")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "bpchar") (ScalarType "text")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "bpchar") (ScalarType "varchar")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "varchar") (ScalarType "text")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "varchar") (ScalarType "bpchar")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "char") (ScalarType "text")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "char") (ScalarType "bpchar")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "char") (ScalarType "varchar")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "name") (ScalarType "text")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "name") (ScalarType "bpchar")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "name") (ScalarType "varchar")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "text") (ScalarType "char")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "bpchar") (ScalarType "char")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "varchar") (ScalarType "char")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "text") (ScalarType "name")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "bpchar") (ScalarType "name")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "varchar") (ScalarType "name")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "char") (ScalarType "int4")
>           ExplicitCastContext,
>         CatCreateCast (ScalarType "int4") (ScalarType "char")
>           ExplicitCastContext,
>         CatCreateCast (ScalarType "pg_node_tree") (ScalarType "text")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "abstime") (ScalarType "date")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "abstime") (ScalarType "time")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "abstime") (ScalarType "timestamp")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "abstime") (ScalarType "timestamptz")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "reltime") (ScalarType "interval")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "date") (ScalarType "timestamp")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "date") (ScalarType "timestamptz")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "time") (ScalarType "interval")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "time") (ScalarType "timetz")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "timestamp") (ScalarType "abstime")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "timestamp") (ScalarType "date")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "timestamp") (ScalarType "time")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "timestamp") (ScalarType "timestamptz")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "timestamptz") (ScalarType "abstime")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "timestamptz") (ScalarType "date")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "timestamptz") (ScalarType "time")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "timestamptz") (ScalarType "timestamp")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "timestamptz") (ScalarType "timetz")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "interval") (ScalarType "reltime")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "interval") (ScalarType "time")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "timetz") (ScalarType "time")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "int4") (ScalarType "abstime")
>           ExplicitCastContext,
>         CatCreateCast (ScalarType "abstime") (ScalarType "int4")
>           ExplicitCastContext,
>         CatCreateCast (ScalarType "int4") (ScalarType "reltime")
>           ExplicitCastContext,
>         CatCreateCast (ScalarType "reltime") (ScalarType "int4")
>           ExplicitCastContext,
>         CatCreateCast (ScalarType "lseg") (ScalarType "point")
>           ExplicitCastContext,
>         CatCreateCast (ScalarType "path") (ScalarType "point")
>           ExplicitCastContext,
>         CatCreateCast (ScalarType "path") (ScalarType "polygon")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "box") (ScalarType "point")
>           ExplicitCastContext,
>         CatCreateCast (ScalarType "box") (ScalarType "lseg")
>           ExplicitCastContext,
>         CatCreateCast (ScalarType "box") (ScalarType "polygon")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "box") (ScalarType "circle")
>           ExplicitCastContext,
>         CatCreateCast (ScalarType "polygon") (ScalarType "point")
>           ExplicitCastContext,
>         CatCreateCast (ScalarType "polygon") (ScalarType "path")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "polygon") (ScalarType "box")
>           ExplicitCastContext,
>         CatCreateCast (ScalarType "polygon") (ScalarType "circle")
>           ExplicitCastContext,
>         CatCreateCast (ScalarType "circle") (ScalarType "point")
>           ExplicitCastContext,
>         CatCreateCast (ScalarType "circle") (ScalarType "box")
>           ExplicitCastContext,
>         CatCreateCast (ScalarType "circle") (ScalarType "polygon")
>           ExplicitCastContext,
>         CatCreateCast (ScalarType "cidr") (ScalarType "inet")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "inet") (ScalarType "cidr")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "bit") (ScalarType "varbit")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "varbit") (ScalarType "bit")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "int8") (ScalarType "bit")
>           ExplicitCastContext,
>         CatCreateCast (ScalarType "int4") (ScalarType "bit")
>           ExplicitCastContext,
>         CatCreateCast (ScalarType "bit") (ScalarType "int8")
>           ExplicitCastContext,
>         CatCreateCast (ScalarType "bit") (ScalarType "int4")
>           ExplicitCastContext,
>         CatCreateCast (ScalarType "cidr") (ScalarType "text")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "inet") (ScalarType "text")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "bool") (ScalarType "text")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "xml") (ScalarType "text")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "text") (ScalarType "xml")
>           ExplicitCastContext,
>         CatCreateCast (ScalarType "cidr") (ScalarType "varchar")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "inet") (ScalarType "varchar")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "bool") (ScalarType "varchar")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "xml") (ScalarType "varchar")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "varchar") (ScalarType "xml")
>           ExplicitCastContext,
>         CatCreateCast (ScalarType "cidr") (ScalarType "bpchar")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "inet") (ScalarType "bpchar")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "bool") (ScalarType "bpchar")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "xml") (ScalarType "bpchar")
>           AssignmentCastContext,
>         CatCreateCast (ScalarType "bpchar") (ScalarType "xml")
>           ExplicitCastContext,
>         CatCreateCast (ScalarType "bpchar") (ScalarType "bpchar")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "varchar") (ScalarType "varchar")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "time") (ScalarType "time")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "timestamp") (ScalarType "timestamp")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "timestamptz") (ScalarType "timestamptz")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "interval") (ScalarType "interval")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "timetz") (ScalarType "timetz")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "bit") (ScalarType "bit")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "varbit") (ScalarType "varbit")
>           ImplicitCastContext,
>         CatCreateCast (ScalarType "numeric") (ScalarType "numeric")
>           ImplicitCastContext,
>         CatCreateFunction FunPrefix "~" [ScalarType "bit"]
>           (ScalarType "bit")
>           False,
>         CatCreateFunction FunPrefix "~" [ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunPrefix "~" [ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunPrefix "~" [ScalarType "inet"]
>           (ScalarType "inet")
>           False,
>         CatCreateFunction FunPrefix "~" [ScalarType "int2"]
>           (ScalarType "int2")
>           False,
>         CatCreateFunction FunPrefix "||/" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunPrefix "|/" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunPrefix "|" [ScalarType "tinterval"]
>           (ScalarType "abstime")
>           False,
>         CatCreateFunction FunPrefix "@@" [ScalarType "lseg"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunPrefix "@@" [ScalarType "polygon"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunPrefix "@@" [ScalarType "circle"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunPrefix "@@" [ScalarType "box"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunPrefix "@@" [ScalarType "path"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunPrefix "@-@" [ScalarType "lseg"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunPrefix "@-@" [ScalarType "path"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunPrefix "@" [ScalarType "int2"]
>           (ScalarType "int2")
>           False,
>         CatCreateFunction FunPrefix "@" [ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunPrefix "@" [ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunPrefix "@" [ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunPrefix "@" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunPrefix "@" [ScalarType "float4"]
>           (ScalarType "float4")
>           False,
>         CatCreateFunction FunPrefix "?|" [ScalarType "lseg"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunPrefix "?|" [ScalarType "line"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunPrefix "?-" [ScalarType "lseg"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunPrefix "?-" [ScalarType "line"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunPrefix "-" [ScalarType "interval"]
>           (ScalarType "interval")
>           False,
>         CatCreateFunction FunPrefix "-" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunPrefix "-" [ScalarType "float4"]
>           (ScalarType "float4")
>           False,
>         CatCreateFunction FunPrefix "-" [ScalarType "int2"]
>           (ScalarType "int2")
>           False,
>         CatCreateFunction FunPrefix "-" [ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunPrefix "-" [ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunPrefix "-" [ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunPrefix "+" [ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunPrefix "+" [ScalarType "int2"]
>           (ScalarType "int2")
>           False,
>         CatCreateFunction FunPrefix "+" [ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunPrefix "+" [ScalarType "float4"]
>           (ScalarType "float4")
>           False,
>         CatCreateFunction FunPrefix "+" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunPrefix "+" [ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunPrefix "#" [ScalarType "path"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunPrefix "#" [ScalarType "polygon"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunPrefix "!!" [ScalarType "tsquery"]
>           (ScalarType "tsquery")
>           False,
>         CatCreateFunction FunPrefix "!!" [ScalarType "int8"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunPostfix "!" [ScalarType "int8"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunBinary "~~*"
>           [ScalarType "bpchar", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "~~*"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "~~*"
>           [ScalarType "name", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "~~"
>           [ScalarType "bpchar", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "~~"
>           [ScalarType "name", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "~~"
>           [ScalarType "bytea", ScalarType "bytea"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "~~"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "~>~"
>           [ScalarType "bpchar", ScalarType "bpchar"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "~>~"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "~>=~"
>           [ScalarType "bpchar", ScalarType "bpchar"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "~>=~"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "~="
>           [ScalarType "circle", ScalarType "circle"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "~="
>           [ScalarType "point", ScalarType "point"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "~="
>           [ScalarType "tinterval", ScalarType "tinterval"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "~="
>           [ScalarType "box", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "~="
>           [ScalarType "polygon", ScalarType "polygon"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "~<~"
>           [ScalarType "bpchar", ScalarType "bpchar"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "~<~"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "~<=~"
>           [ScalarType "bpchar", ScalarType "bpchar"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "~<=~"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "~*"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "~*"
>           [ScalarType "name", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "~*"
>           [ScalarType "bpchar", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "~"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "~"
>           [ScalarType "name", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "~"
>           [ScalarType "bpchar", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "~"
>           [ArrayType (ScalarType "aclitem"), ScalarType "aclitem"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "~"
>           [ScalarType "circle", ScalarType "point"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "~"
>           [ScalarType "polygon", ScalarType "point"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "~"
>           [ScalarType "path", ScalarType "point"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "~"
>           [ScalarType "circle", ScalarType "circle"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "~"
>           [ScalarType "box", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "~"
>           [ScalarType "polygon", ScalarType "polygon"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "||"
>           [ScalarType "bytea", ScalarType "bytea"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunBinary "||"
>           [ScalarType "text", Pseudo AnyNonArray]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunBinary "||"
>           [Pseudo AnyNonArray, ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunBinary "||"
>           [ScalarType "tsquery", ScalarType "tsquery"]
>           (ScalarType "tsquery")
>           False,
>         CatCreateFunction FunBinary "||"
>           [ScalarType "varbit", ScalarType "varbit"]
>           (ScalarType "varbit")
>           False,
>         CatCreateFunction FunBinary "||" [Pseudo AnyArray, Pseudo AnyArray]
>           (Pseudo AnyArray)
>           False,
>         CatCreateFunction FunBinary "||"
>           [Pseudo AnyArray, Pseudo AnyElement]
>           (Pseudo AnyArray)
>           False,
>         CatCreateFunction FunBinary "||"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunBinary "||"
>           [ScalarType "tsvector", ScalarType "tsvector"]
>           (ScalarType "tsvector")
>           False,
>         CatCreateFunction FunBinary "||"
>           [Pseudo AnyElement, Pseudo AnyArray]
>           (Pseudo AnyArray)
>           False,
>         CatCreateFunction FunBinary "|>>"
>           [ScalarType "box", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "|>>"
>           [ScalarType "circle", ScalarType "circle"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "|>>"
>           [ScalarType "polygon", ScalarType "polygon"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "|&>"
>           [ScalarType "polygon", ScalarType "polygon"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "|&>"
>           [ScalarType "box", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "|&>"
>           [ScalarType "circle", ScalarType "circle"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "|"
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunBinary "|"
>           [ScalarType "int8", ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunBinary "|"
>           [ScalarType "int2", ScalarType "int2"]
>           (ScalarType "int2")
>           False,
>         CatCreateFunction FunBinary "|"
>           [ScalarType "inet", ScalarType "inet"]
>           (ScalarType "inet")
>           False,
>         CatCreateFunction FunBinary "|"
>           [ScalarType "bit", ScalarType "bit"]
>           (ScalarType "bit")
>           False,
>         CatCreateFunction FunBinary "^"
>           [ScalarType "float8", ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunBinary "^"
>           [ScalarType "numeric", ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunBinary "@@@"
>           [ScalarType "tsvector", ScalarType "tsquery"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "@@@"
>           [ScalarType "tsquery", ScalarType "tsvector"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "@@"
>           [ScalarType "tsquery", ScalarType "tsvector"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "@@"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "@@"
>           [ScalarType "tsvector", ScalarType "tsquery"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "@@"
>           [ScalarType "text", ScalarType "tsquery"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "@>"
>           [ScalarType "circle", ScalarType "circle"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "@>"
>           [ScalarType "polygon", ScalarType "polygon"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "@>"
>           [ScalarType "tsquery", ScalarType "tsquery"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "@>"
>           [ScalarType "box", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "@>"
>           [ArrayType (ScalarType "aclitem"), ScalarType "aclitem"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "@>"
>           [ScalarType "box", ScalarType "point"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "@>"
>           [ScalarType "circle", ScalarType "point"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "@>"
>           [ScalarType "polygon", ScalarType "point"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "@>"
>           [ScalarType "path", ScalarType "point"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "@>" [Pseudo AnyArray, Pseudo AnyArray]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "?||"
>           [ScalarType "line", ScalarType "line"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "?||"
>           [ScalarType "lseg", ScalarType "lseg"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "?|"
>           [ScalarType "point", ScalarType "point"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "?-|"
>           [ScalarType "lseg", ScalarType "lseg"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "?-|"
>           [ScalarType "line", ScalarType "line"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "?-"
>           [ScalarType "point", ScalarType "point"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "?#"
>           [ScalarType "line", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "?#"
>           [ScalarType "lseg", ScalarType "lseg"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "?#"
>           [ScalarType "lseg", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "?#"
>           [ScalarType "line", ScalarType "line"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "?#"
>           [ScalarType "lseg", ScalarType "line"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "?#"
>           [ScalarType "box", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "?#"
>           [ScalarType "path", ScalarType "path"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">^"
>           [ScalarType "point", ScalarType "point"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">^"
>           [ScalarType "box", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">>="
>           [ScalarType "inet", ScalarType "inet"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">>"
>           [ScalarType "bit", ScalarType "int4"]
>           (ScalarType "bit")
>           False,
>         CatCreateFunction FunBinary ">>"
>           [ScalarType "inet", ScalarType "inet"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">>"
>           [ScalarType "circle", ScalarType "circle"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">>"
>           [ScalarType "polygon", ScalarType "polygon"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">>"
>           [ScalarType "box", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">>"
>           [ScalarType "int8", ScalarType "int4"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunBinary ">>"
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunBinary ">>"
>           [ScalarType "int2", ScalarType "int4"]
>           (ScalarType "int2")
>           False,
>         CatCreateFunction FunBinary ">>"
>           [ScalarType "point", ScalarType "point"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "int2", ScalarType "int2"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "lseg", ScalarType "lseg"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "date", ScalarType "timestamp"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "bytea", ScalarType "bytea"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "timestamptz", ScalarType "timestamptz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">=" [Pseudo AnyArray, Pseudo AnyArray]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "abstime", ScalarType "abstime"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "date", ScalarType "timestamptz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "tsquery", ScalarType "tsquery"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "date", ScalarType "date"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "reltime", ScalarType "reltime"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "time", ScalarType "time"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "timestamp", ScalarType "date"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "interval", ScalarType "interval"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "name", ScalarType "name"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "timetz", ScalarType "timetz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "float4", ScalarType "float8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "timestamptz", ScalarType "date"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "bool", ScalarType "bool"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "bpchar", ScalarType "bpchar"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "float8", ScalarType "float8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "circle", ScalarType "circle"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "box", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "oid", ScalarType "oid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "int4", ScalarType "int8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "tsvector", ScalarType "tsvector"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "inet", ScalarType "inet"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "oidvector", ScalarType "oidvector"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "timestamp", ScalarType "timestamptz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "timestamptz", ScalarType "timestamp"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "int8", ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "macaddr", ScalarType "macaddr"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "int8", ScalarType "int2"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">=" [Pseudo Record, Pseudo Record]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "int8", ScalarType "int8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "float4", ScalarType "float4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "tid", ScalarType "tid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "money", ScalarType "money"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "int2", ScalarType "int8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "char", ScalarType "char"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "tinterval", ScalarType "tinterval"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "uuid", ScalarType "uuid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "numeric", ScalarType "numeric"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "path", ScalarType "path"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "float8", ScalarType "float4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "varbit", ScalarType "varbit"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">=" [Pseudo AnyEnum, Pseudo AnyEnum]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "bit", ScalarType "bit"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "timestamp", ScalarType "timestamp"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "int2", ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "int4", ScalarType "int2"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">="
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "circle", ScalarType "circle"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "tsquery", ScalarType "tsquery"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "tsvector", ScalarType "tsvector"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">" [Pseudo AnyEnum, Pseudo AnyEnum]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "uuid", ScalarType "uuid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "int4", ScalarType "int8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "tid", ScalarType "tid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "int8", ScalarType "int8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "int8", ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "timestamptz", ScalarType "timestamp"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "timestamp", ScalarType "timestamptz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "box", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "timestamptz", ScalarType "date"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "timestamp", ScalarType "date"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "date", ScalarType "timestamptz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "int2", ScalarType "int2"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "date", ScalarType "timestamp"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "int2", ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "timestamp", ScalarType "timestamp"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "int4", ScalarType "int2"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "bytea", ScalarType "bytea"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "abstime", ScalarType "abstime"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "reltime", ScalarType "reltime"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "oid", ScalarType "oid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "oidvector", ScalarType "oidvector"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "float4", ScalarType "float4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "int8", ScalarType "int2"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "char", ScalarType "char"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "int2", ScalarType "int8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "name", ScalarType "name"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "varbit", ScalarType "varbit"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "float8", ScalarType "float8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "bit", ScalarType "bit"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "path", ScalarType "path"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "numeric", ScalarType "numeric"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "tinterval", ScalarType "tinterval"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "money", ScalarType "money"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "inet", ScalarType "inet"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "macaddr", ScalarType "macaddr"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "bpchar", ScalarType "bpchar"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "lseg", ScalarType "lseg"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">" [Pseudo AnyArray, Pseudo AnyArray]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "date", ScalarType "date"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "time", ScalarType "time"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "timetz", ScalarType "timetz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "float4", ScalarType "float8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">" [Pseudo Record, Pseudo Record]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "bool", ScalarType "bool"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "float8", ScalarType "float4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "timestamptz", ScalarType "timestamptz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary ">"
>           [ScalarType "interval", ScalarType "interval"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "bpchar", ScalarType "bpchar"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "float4", ScalarType "float4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "inet", ScalarType "inet"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "tsvector", ScalarType "tsvector"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "oidvector", ScalarType "oidvector"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "oid", ScalarType "oid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "macaddr", ScalarType "macaddr"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "=" [Pseudo AnyArray, Pseudo AnyArray]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "line", ScalarType "line"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "bytea", ScalarType "bytea"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "reltime", ScalarType "reltime"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "abstime", ScalarType "abstime"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "timestamp", ScalarType "timestamp"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "int4", ScalarType "int2"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "int2", ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "date", ScalarType "timestamp"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "int2", ScalarType "int2"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "name", ScalarType "name"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "date", ScalarType "timestamptz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "date", ScalarType "date"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "timestamptz", ScalarType "timestamptz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "time", ScalarType "time"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "interval", ScalarType "interval"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "timestamp", ScalarType "date"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "timetz", ScalarType "timetz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "tsquery", ScalarType "tsquery"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "box", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "float4", ScalarType "float8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "char", ScalarType "char"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "timestamptz", ScalarType "date"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "bool", ScalarType "bool"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "timestamp", ScalarType "timestamptz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "circle", ScalarType "circle"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "int4", ScalarType "int8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "timestamptz", ScalarType "timestamp"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "int8", ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "int8", ScalarType "int8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "float8", ScalarType "float4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "lseg", ScalarType "lseg"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "tid", ScalarType "tid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "uuid", ScalarType "uuid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "int2vector", ScalarType "int2vector"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "cid", ScalarType "cid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "xid", ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "=" [Pseudo Record, Pseudo Record]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "=" [Pseudo AnyEnum, Pseudo AnyEnum]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "xid", ScalarType "xid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "money", ScalarType "money"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "tinterval", ScalarType "tinterval"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "numeric", ScalarType "numeric"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "bit", ScalarType "bit"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "path", ScalarType "path"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "aclitem", ScalarType "aclitem"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "float8", ScalarType "float8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "varbit", ScalarType "varbit"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "int2", ScalarType "int8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "="
>           [ScalarType "int8", ScalarType "int2"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<^"
>           [ScalarType "point", ScalarType "point"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<^"
>           [ScalarType "box", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<@"
>           [ScalarType "tsquery", ScalarType "tsquery"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<@" [Pseudo AnyArray, Pseudo AnyArray]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<@"
>           [ScalarType "lseg", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<@"
>           [ScalarType "lseg", ScalarType "line"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<@"
>           [ScalarType "point", ScalarType "lseg"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<@"
>           [ScalarType "circle", ScalarType "circle"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<@"
>           [ScalarType "point", ScalarType "circle"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<@"
>           [ScalarType "point", ScalarType "polygon"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<@"
>           [ScalarType "point", ScalarType "path"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<@"
>           [ScalarType "point", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<@"
>           [ScalarType "box", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<@"
>           [ScalarType "polygon", ScalarType "polygon"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<@"
>           [ScalarType "point", ScalarType "line"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<?>"
>           [ScalarType "abstime", ScalarType "tinterval"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "bool", ScalarType "bool"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "timetz", ScalarType "timetz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "int4", ScalarType "int8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "timestamptz", ScalarType "timestamp"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "date", ScalarType "timestamptz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "timestamptz", ScalarType "timestamptz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "timestamp", ScalarType "date"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "time", ScalarType "time"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "inet", ScalarType "inet"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "bpchar", ScalarType "bpchar"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "macaddr", ScalarType "macaddr"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "timestamp", ScalarType "timestamptz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "tsvector", ScalarType "tsvector"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>" [Pseudo Record, Pseudo Record]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "reltime", ScalarType "reltime"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>" [Pseudo AnyArray, Pseudo AnyArray]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "date", ScalarType "date"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "lseg", ScalarType "lseg"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "timestamptz", ScalarType "date"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "oidvector", ScalarType "oidvector"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "bytea", ScalarType "bytea"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "float4", ScalarType "float4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "bit", ScalarType "bit"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "name", ScalarType "name"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>" [Pseudo AnyEnum, Pseudo AnyEnum]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "abstime", ScalarType "abstime"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "oid", ScalarType "oid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "tsquery", ScalarType "tsquery"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "uuid", ScalarType "uuid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "timestamp", ScalarType "timestamp"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "int4", ScalarType "int2"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "int2", ScalarType "int8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "int2", ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "numeric", ScalarType "numeric"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "float8", ScalarType "float4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "tid", ScalarType "tid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "tinterval", ScalarType "tinterval"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "char", ScalarType "char"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "interval", ScalarType "interval"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "int8", ScalarType "int8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "point", ScalarType "point"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "date", ScalarType "timestamp"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "money", ScalarType "money"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "float4", ScalarType "float8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "varbit", ScalarType "varbit"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "int2", ScalarType "int2"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "circle", ScalarType "circle"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "int8", ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "int8", ScalarType "int2"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<>"
>           [ScalarType "float8", ScalarType "float8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "varbit", ScalarType "varbit"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "name", ScalarType "name"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "float4", ScalarType "float4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "int2", ScalarType "int8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "char", ScalarType "char"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "tsvector", ScalarType "tsvector"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<=" [Pseudo AnyEnum, Pseudo AnyEnum]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "circle", ScalarType "circle"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "uuid", ScalarType "uuid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "float8", ScalarType "float4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "tid", ScalarType "tid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "float4", ScalarType "float8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "int8", ScalarType "int2"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "int8", ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "timetz", ScalarType "timetz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "timestamptz", ScalarType "timestamp"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "time", ScalarType "time"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "timestamp", ScalarType "timestamptz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "date", ScalarType "date"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<=" [Pseudo AnyArray, Pseudo AnyArray]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "lseg", ScalarType "lseg"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "timestamptz", ScalarType "date"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "bpchar", ScalarType "bpchar"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "macaddr", ScalarType "macaddr"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "inet", ScalarType "inet"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "box", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "timestamp", ScalarType "date"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<=" [Pseudo Record, Pseudo Record]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "interval", ScalarType "interval"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "int4", ScalarType "int8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "date", ScalarType "timestamptz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "money", ScalarType "money"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "int2", ScalarType "int2"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "date", ScalarType "timestamp"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "tinterval", ScalarType "tinterval"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "bool", ScalarType "bool"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "timestamp", ScalarType "timestamp"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "numeric", ScalarType "numeric"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "int2", ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "int4", ScalarType "int2"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "path", ScalarType "path"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "bit", ScalarType "bit"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "bytea", ScalarType "bytea"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "abstime", ScalarType "abstime"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "tsquery", ScalarType "tsquery"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "reltime", ScalarType "reltime"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "oid", ScalarType "oid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "oidvector", ScalarType "oidvector"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "timestamptz", ScalarType "timestamptz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "float8", ScalarType "float8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<="
>           [ScalarType "int8", ScalarType "int8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<<|"
>           [ScalarType "polygon", ScalarType "polygon"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<<|"
>           [ScalarType "box", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<<|"
>           [ScalarType "circle", ScalarType "circle"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<<="
>           [ScalarType "inet", ScalarType "inet"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<<"
>           [ScalarType "int8", ScalarType "int4"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunBinary "<<"
>           [ScalarType "polygon", ScalarType "polygon"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<<"
>           [ScalarType "point", ScalarType "point"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<<"
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunBinary "<<"
>           [ScalarType "box", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<<"
>           [ScalarType "int2", ScalarType "int4"]
>           (ScalarType "int2")
>           False,
>         CatCreateFunction FunBinary "<<"
>           [ScalarType "inet", ScalarType "inet"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<<"
>           [ScalarType "circle", ScalarType "circle"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<<"
>           [ScalarType "tinterval", ScalarType "tinterval"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<<"
>           [ScalarType "bit", ScalarType "int4"]
>           (ScalarType "bit")
>           False,
>         CatCreateFunction FunBinary "<->"
>           [ScalarType "line", ScalarType "box"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunBinary "<->"
>           [ScalarType "circle", ScalarType "polygon"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunBinary "<->"
>           [ScalarType "polygon", ScalarType "polygon"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunBinary "<->"
>           [ScalarType "lseg", ScalarType "lseg"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunBinary "<->"
>           [ScalarType "lseg", ScalarType "line"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunBinary "<->"
>           [ScalarType "line", ScalarType "line"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunBinary "<->"
>           [ScalarType "point", ScalarType "line"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunBinary "<->"
>           [ScalarType "point", ScalarType "lseg"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunBinary "<->"
>           [ScalarType "path", ScalarType "path"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunBinary "<->"
>           [ScalarType "lseg", ScalarType "box"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunBinary "<->"
>           [ScalarType "box", ScalarType "box"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunBinary "<->"
>           [ScalarType "point", ScalarType "point"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunBinary "<->"
>           [ScalarType "point", ScalarType "circle"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunBinary "<->"
>           [ScalarType "circle", ScalarType "circle"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunBinary "<->"
>           [ScalarType "point", ScalarType "path"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunBinary "<->"
>           [ScalarType "point", ScalarType "box"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunBinary "<#>"
>           [ScalarType "abstime", ScalarType "abstime"]
>           (ScalarType "tinterval")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "varbit", ScalarType "varbit"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "char", ScalarType "char"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "int2", ScalarType "int8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "timestamp", ScalarType "timestamptz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "lseg", ScalarType "lseg"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<" [Pseudo AnyArray, Pseudo AnyArray]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "date", ScalarType "date"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "int4", ScalarType "int8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "macaddr", ScalarType "macaddr"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "bpchar", ScalarType "bpchar"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "timestamptz", ScalarType "timestamp"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "inet", ScalarType "inet"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "tsquery", ScalarType "tsquery"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "timestamptz", ScalarType "date"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "time", ScalarType "time"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "timestamp", ScalarType "date"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "int8", ScalarType "int2"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "timetz", ScalarType "timetz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "int8", ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "float4", ScalarType "float4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "date", ScalarType "timestamptz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "money", ScalarType "money"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "float4", ScalarType "float8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "int8", ScalarType "int8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "name", ScalarType "name"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "oidvector", ScalarType "oidvector"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "float8", ScalarType "float8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "date", ScalarType "timestamp"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "tinterval", ScalarType "tinterval"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "circle", ScalarType "circle"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "tid", ScalarType "tid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "int2", ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "float8", ScalarType "float4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "int4", ScalarType "int2"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "numeric", ScalarType "numeric"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "timestamptz", ScalarType "timestamptz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "timestamp", ScalarType "timestamp"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "uuid", ScalarType "uuid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "path", ScalarType "path"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<" [Pseudo AnyEnum, Pseudo AnyEnum]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "bit", ScalarType "bit"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "bool", ScalarType "bool"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "abstime", ScalarType "abstime"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<" [Pseudo Record, Pseudo Record]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "bytea", ScalarType "bytea"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "tsvector", ScalarType "tsvector"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "reltime", ScalarType "reltime"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "int2", ScalarType "int2"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "oid", ScalarType "oid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "interval", ScalarType "interval"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "<"
>           [ScalarType "box", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "/"
>           [ScalarType "circle", ScalarType "point"]
>           (ScalarType "circle")
>           False,
>         CatCreateFunction FunBinary "/"
>           [ScalarType "float8", ScalarType "float4"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunBinary "/"
>           [ScalarType "float4", ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunBinary "/"
>           [ScalarType "interval", ScalarType "float8"]
>           (ScalarType "interval")
>           False,
>         CatCreateFunction FunBinary "/"
>           [ScalarType "money", ScalarType "money"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunBinary "/"
>           [ScalarType "money", ScalarType "int2"]
>           (ScalarType "money")
>           False,
>         CatCreateFunction FunBinary "/"
>           [ScalarType "money", ScalarType "int4"]
>           (ScalarType "money")
>           False,
>         CatCreateFunction FunBinary "/"
>           [ScalarType "money", ScalarType "float8"]
>           (ScalarType "money")
>           False,
>         CatCreateFunction FunBinary "/"
>           [ScalarType "money", ScalarType "float4"]
>           (ScalarType "money")
>           False,
>         CatCreateFunction FunBinary "/"
>           [ScalarType "box", ScalarType "point"]
>           (ScalarType "box")
>           False,
>         CatCreateFunction FunBinary "/"
>           [ScalarType "numeric", ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunBinary "/"
>           [ScalarType "point", ScalarType "point"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunBinary "/"
>           [ScalarType "int2", ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunBinary "/"
>           [ScalarType "int8", ScalarType "int2"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunBinary "/"
>           [ScalarType "int4", ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunBinary "/"
>           [ScalarType "int8", ScalarType "int4"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunBinary "/"
>           [ScalarType "int8", ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunBinary "/"
>           [ScalarType "float8", ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunBinary "/"
>           [ScalarType "float4", ScalarType "float4"]
>           (ScalarType "float4")
>           False,
>         CatCreateFunction FunBinary "/"
>           [ScalarType "int4", ScalarType "int2"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunBinary "/"
>           [ScalarType "int2", ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunBinary "/"
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunBinary "/"
>           [ScalarType "int2", ScalarType "int2"]
>           (ScalarType "int2")
>           False,
>         CatCreateFunction FunBinary "/"
>           [ScalarType "path", ScalarType "point"]
>           (ScalarType "path")
>           False,
>         CatCreateFunction FunBinary "-"
>           [ScalarType "int2", ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunBinary "-"
>           [ScalarType "float8", ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunBinary "-"
>           [ScalarType "date", ScalarType "interval"]
>           (ScalarType "timestamp")
>           False,
>         CatCreateFunction FunBinary "-"
>           [ScalarType "time", ScalarType "interval"]
>           (ScalarType "time")
>           False,
>         CatCreateFunction FunBinary "-"
>           [ScalarType "path", ScalarType "point"]
>           (ScalarType "path")
>           False,
>         CatCreateFunction FunBinary "-"
>           [ScalarType "interval", ScalarType "interval"]
>           (ScalarType "interval")
>           False,
>         CatCreateFunction FunBinary "-"
>           [ScalarType "float4", ScalarType "float4"]
>           (ScalarType "float4")
>           False,
>         CatCreateFunction FunBinary "-"
>           [ScalarType "date", ScalarType "date"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunBinary "-"
>           [ScalarType "float4", ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunBinary "-"
>           [ScalarType "int4", ScalarType "int2"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunBinary "-"
>           [ScalarType "int2", ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunBinary "-"
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunBinary "-"
>           [ScalarType "int2", ScalarType "int2"]
>           (ScalarType "int2")
>           False,
>         CatCreateFunction FunBinary "-"
>           [ScalarType "money", ScalarType "money"]
>           (ScalarType "money")
>           False,
>         CatCreateFunction FunBinary "-"
>           [ScalarType "abstime", ScalarType "reltime"]
>           (ScalarType "abstime")
>           False,
>         CatCreateFunction FunBinary "-"
>           [ScalarType "int8", ScalarType "int2"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunBinary "-"
>           [ScalarType "timestamptz", ScalarType "interval"]
>           (ScalarType "timestamptz")
>           False,
>         CatCreateFunction FunBinary "-"
>           [ScalarType "timestamptz", ScalarType "timestamptz"]
>           (ScalarType "interval")
>           False,
>         CatCreateFunction FunBinary "-"
>           [ScalarType "time", ScalarType "time"]
>           (ScalarType "interval")
>           False,
>         CatCreateFunction FunBinary "-"
>           [ArrayType (ScalarType "aclitem"), ScalarType "aclitem"]
>           (ArrayType (ScalarType "aclitem"))
>           False,
>         CatCreateFunction FunBinary "-"
>           [ScalarType "int4", ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunBinary "-"
>           [ScalarType "inet", ScalarType "inet"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunBinary "-"
>           [ScalarType "int8", ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunBinary "-"
>           [ScalarType "numeric", ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunBinary "-"
>           [ScalarType "circle", ScalarType "point"]
>           (ScalarType "circle")
>           False,
>         CatCreateFunction FunBinary "-"
>           [ScalarType "float8", ScalarType "float4"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunBinary "-"
>           [ScalarType "inet", ScalarType "int8"]
>           (ScalarType "inet")
>           False,
>         CatCreateFunction FunBinary "-"
>           [ScalarType "box", ScalarType "point"]
>           (ScalarType "box")
>           False,
>         CatCreateFunction FunBinary "-"
>           [ScalarType "point", ScalarType "point"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunBinary "-"
>           [ScalarType "timetz", ScalarType "interval"]
>           (ScalarType "timetz")
>           False,
>         CatCreateFunction FunBinary "-"
>           [ScalarType "timestamp", ScalarType "timestamp"]
>           (ScalarType "interval")
>           False,
>         CatCreateFunction FunBinary "-"
>           [ScalarType "timestamp", ScalarType "interval"]
>           (ScalarType "timestamp")
>           False,
>         CatCreateFunction FunBinary "-"
>           [ScalarType "int8", ScalarType "int4"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunBinary "-"
>           [ScalarType "date", ScalarType "int4"]
>           (ScalarType "date")
>           False,
>         CatCreateFunction FunBinary "+"
>           [ScalarType "numeric", ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunBinary "+"
>           [ScalarType "interval", ScalarType "timetz"]
>           (ScalarType "timetz")
>           False,
>         CatCreateFunction FunBinary "+"
>           [ScalarType "box", ScalarType "point"]
>           (ScalarType "box")
>           False,
>         CatCreateFunction FunBinary "+"
>           [ScalarType "timestamp", ScalarType "interval"]
>           (ScalarType "timestamp")
>           False,
>         CatCreateFunction FunBinary "+"
>           [ScalarType "interval", ScalarType "date"]
>           (ScalarType "timestamp")
>           False,
>         CatCreateFunction FunBinary "+"
>           [ScalarType "timetz", ScalarType "date"]
>           (ScalarType "timestamptz")
>           False,
>         CatCreateFunction FunBinary "+"
>           [ScalarType "date", ScalarType "int4"]
>           (ScalarType "date")
>           False,
>         CatCreateFunction FunBinary "+"
>           [ScalarType "int8", ScalarType "int2"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunBinary "+"
>           [ScalarType "int8", ScalarType "inet"]
>           (ScalarType "inet")
>           False,
>         CatCreateFunction FunBinary "+"
>           [ScalarType "money", ScalarType "money"]
>           (ScalarType "money")
>           False,
>         CatCreateFunction FunBinary "+"
>           [ScalarType "time", ScalarType "interval"]
>           (ScalarType "time")
>           False,
>         CatCreateFunction FunBinary "+"
>           [ScalarType "int2", ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunBinary "+"
>           [ScalarType "interval", ScalarType "time"]
>           (ScalarType "time")
>           False,
>         CatCreateFunction FunBinary "+"
>           [ScalarType "int8", ScalarType "int4"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunBinary "+"
>           [ScalarType "inet", ScalarType "int8"]
>           (ScalarType "inet")
>           False,
>         CatCreateFunction FunBinary "+"
>           [ArrayType (ScalarType "aclitem"), ScalarType "aclitem"]
>           (ArrayType (ScalarType "aclitem"))
>           False,
>         CatCreateFunction FunBinary "+"
>           [ScalarType "date", ScalarType "interval"]
>           (ScalarType "timestamp")
>           False,
>         CatCreateFunction FunBinary "+"
>           [ScalarType "interval", ScalarType "interval"]
>           (ScalarType "interval")
>           False,
>         CatCreateFunction FunBinary "+"
>           [ScalarType "float8", ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunBinary "+"
>           [ScalarType "float4", ScalarType "float4"]
>           (ScalarType "float4")
>           False,
>         CatCreateFunction FunBinary "+"
>           [ScalarType "point", ScalarType "point"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunBinary "+"
>           [ScalarType "abstime", ScalarType "reltime"]
>           (ScalarType "abstime")
>           False,
>         CatCreateFunction FunBinary "+"
>           [ScalarType "timestamptz", ScalarType "interval"]
>           (ScalarType "timestamptz")
>           False,
>         CatCreateFunction FunBinary "+"
>           [ScalarType "circle", ScalarType "point"]
>           (ScalarType "circle")
>           False,
>         CatCreateFunction FunBinary "+"
>           [ScalarType "float8", ScalarType "float4"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunBinary "+"
>           [ScalarType "int8", ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunBinary "+"
>           [ScalarType "path", ScalarType "path"]
>           (ScalarType "path")
>           False,
>         CatCreateFunction FunBinary "+"
>           [ScalarType "path", ScalarType "point"]
>           (ScalarType "path")
>           False,
>         CatCreateFunction FunBinary "+"
>           [ScalarType "int4", ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunBinary "+"
>           [ScalarType "date", ScalarType "timetz"]
>           (ScalarType "timestamptz")
>           False,
>         CatCreateFunction FunBinary "+"
>           [ScalarType "int4", ScalarType "date"]
>           (ScalarType "date")
>           False,
>         CatCreateFunction FunBinary "+"
>           [ScalarType "float4", ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunBinary "+"
>           [ScalarType "interval", ScalarType "timestamptz"]
>           (ScalarType "timestamptz")
>           False,
>         CatCreateFunction FunBinary "+"
>           [ScalarType "int4", ScalarType "int2"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunBinary "+"
>           [ScalarType "int2", ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunBinary "+"
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunBinary "+"
>           [ScalarType "int2", ScalarType "int2"]
>           (ScalarType "int2")
>           False,
>         CatCreateFunction FunBinary "+"
>           [ScalarType "date", ScalarType "time"]
>           (ScalarType "timestamp")
>           False,
>         CatCreateFunction FunBinary "+"
>           [ScalarType "timetz", ScalarType "interval"]
>           (ScalarType "timetz")
>           False,
>         CatCreateFunction FunBinary "+"
>           [ScalarType "interval", ScalarType "timestamp"]
>           (ScalarType "timestamp")
>           False,
>         CatCreateFunction FunBinary "+"
>           [ScalarType "time", ScalarType "date"]
>           (ScalarType "timestamp")
>           False,
>         CatCreateFunction FunBinary "*"
>           [ScalarType "int8", ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunBinary "*"
>           [ScalarType "int8", ScalarType "int4"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunBinary "*"
>           [ScalarType "int4", ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunBinary "*"
>           [ScalarType "int8", ScalarType "int2"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunBinary "*"
>           [ScalarType "int2", ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunBinary "*"
>           [ScalarType "float8", ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunBinary "*"
>           [ScalarType "float4", ScalarType "float4"]
>           (ScalarType "float4")
>           False,
>         CatCreateFunction FunBinary "*"
>           [ScalarType "point", ScalarType "point"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunBinary "*"
>           [ScalarType "path", ScalarType "point"]
>           (ScalarType "path")
>           False,
>         CatCreateFunction FunBinary "*"
>           [ScalarType "int4", ScalarType "int2"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunBinary "*"
>           [ScalarType "int2", ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunBinary "*"
>           [ScalarType "numeric", ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunBinary "*"
>           [ScalarType "box", ScalarType "point"]
>           (ScalarType "box")
>           False,
>         CatCreateFunction FunBinary "*"
>           [ScalarType "money", ScalarType "float4"]
>           (ScalarType "money")
>           False,
>         CatCreateFunction FunBinary "*"
>           [ScalarType "float4", ScalarType "money"]
>           (ScalarType "money")
>           False,
>         CatCreateFunction FunBinary "*"
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunBinary "*"
>           [ScalarType "money", ScalarType "float8"]
>           (ScalarType "money")
>           False,
>         CatCreateFunction FunBinary "*"
>           [ScalarType "money", ScalarType "int4"]
>           (ScalarType "money")
>           False,
>         CatCreateFunction FunBinary "*"
>           [ScalarType "money", ScalarType "int2"]
>           (ScalarType "money")
>           False,
>         CatCreateFunction FunBinary "*"
>           [ScalarType "float8", ScalarType "money"]
>           (ScalarType "money")
>           False,
>         CatCreateFunction FunBinary "*"
>           [ScalarType "int4", ScalarType "money"]
>           (ScalarType "money")
>           False,
>         CatCreateFunction FunBinary "*"
>           [ScalarType "int2", ScalarType "money"]
>           (ScalarType "money")
>           False,
>         CatCreateFunction FunBinary "*"
>           [ScalarType "float8", ScalarType "interval"]
>           (ScalarType "interval")
>           False,
>         CatCreateFunction FunBinary "*"
>           [ScalarType "interval", ScalarType "float8"]
>           (ScalarType "interval")
>           False,
>         CatCreateFunction FunBinary "*"
>           [ScalarType "float4", ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunBinary "*"
>           [ScalarType "float8", ScalarType "float4"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunBinary "*"
>           [ScalarType "circle", ScalarType "point"]
>           (ScalarType "circle")
>           False,
>         CatCreateFunction FunBinary "*"
>           [ScalarType "int2", ScalarType "int2"]
>           (ScalarType "int2")
>           False,
>         CatCreateFunction FunBinary "&>"
>           [ScalarType "circle", ScalarType "circle"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "&>"
>           [ScalarType "box", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "&>"
>           [ScalarType "polygon", ScalarType "polygon"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "&<|"
>           [ScalarType "polygon", ScalarType "polygon"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "&<|"
>           [ScalarType "box", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "&<|"
>           [ScalarType "circle", ScalarType "circle"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "&<"
>           [ScalarType "polygon", ScalarType "polygon"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "&<"
>           [ScalarType "circle", ScalarType "circle"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "&<"
>           [ScalarType "box", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "&&"
>           [ScalarType "polygon", ScalarType "polygon"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "&&" [Pseudo AnyArray, Pseudo AnyArray]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "&&"
>           [ScalarType "tinterval", ScalarType "tinterval"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "&&"
>           [ScalarType "box", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "&&"
>           [ScalarType "tsquery", ScalarType "tsquery"]
>           (ScalarType "tsquery")
>           False,
>         CatCreateFunction FunBinary "&&"
>           [ScalarType "circle", ScalarType "circle"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "&"
>           [ScalarType "int2", ScalarType "int2"]
>           (ScalarType "int2")
>           False,
>         CatCreateFunction FunBinary "&"
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunBinary "&"
>           [ScalarType "bit", ScalarType "bit"]
>           (ScalarType "bit")
>           False,
>         CatCreateFunction FunBinary "&"
>           [ScalarType "int8", ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunBinary "&"
>           [ScalarType "inet", ScalarType "inet"]
>           (ScalarType "inet")
>           False,
>         CatCreateFunction FunBinary "%"
>           [ScalarType "numeric", ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunBinary "%"
>           [ScalarType "int8", ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunBinary "%"
>           [ScalarType "int2", ScalarType "int2"]
>           (ScalarType "int2")
>           False,
>         CatCreateFunction FunBinary "%"
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunBinary "#>="
>           [ScalarType "tinterval", ScalarType "reltime"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "#>"
>           [ScalarType "tinterval", ScalarType "reltime"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "#="
>           [ScalarType "tinterval", ScalarType "reltime"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "#<>"
>           [ScalarType "tinterval", ScalarType "reltime"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "#<="
>           [ScalarType "tinterval", ScalarType "reltime"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "#<"
>           [ScalarType "tinterval", ScalarType "reltime"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "##"
>           [ScalarType "line", ScalarType "lseg"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunBinary "##"
>           [ScalarType "lseg", ScalarType "lseg"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunBinary "##"
>           [ScalarType "point", ScalarType "box"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunBinary "##"
>           [ScalarType "point", ScalarType "lseg"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunBinary "##"
>           [ScalarType "point", ScalarType "line"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunBinary "##"
>           [ScalarType "lseg", ScalarType "box"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunBinary "##"
>           [ScalarType "line", ScalarType "box"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunBinary "##"
>           [ScalarType "lseg", ScalarType "line"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunBinary "#"
>           [ScalarType "box", ScalarType "box"]
>           (ScalarType "box")
>           False,
>         CatCreateFunction FunBinary "#"
>           [ScalarType "bit", ScalarType "bit"]
>           (ScalarType "bit")
>           False,
>         CatCreateFunction FunBinary "#"
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunBinary "#"
>           [ScalarType "lseg", ScalarType "lseg"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunBinary "#"
>           [ScalarType "int2", ScalarType "int2"]
>           (ScalarType "int2")
>           False,
>         CatCreateFunction FunBinary "#"
>           [ScalarType "line", ScalarType "line"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunBinary "#"
>           [ScalarType "int8", ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunBinary "!~~*"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "!~~*"
>           [ScalarType "bpchar", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "!~~*"
>           [ScalarType "name", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "!~~"
>           [ScalarType "name", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "!~~"
>           [ScalarType "bytea", ScalarType "bytea"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "!~~"
>           [ScalarType "bpchar", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "!~~"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "!~*"
>           [ScalarType "name", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "!~*"
>           [ScalarType "bpchar", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "!~*"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "!~"
>           [ScalarType "bpchar", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "!~"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunBinary "!~"
>           [ScalarType "name", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "RI_FKey_cascade_del" [] (Pseudo Trigger)
>           False,
>         CatCreateFunction FunName "RI_FKey_cascade_upd" [] (Pseudo Trigger)
>           False,
>         CatCreateFunction FunName "RI_FKey_check_ins" [] (Pseudo Trigger)
>           False,
>         CatCreateFunction FunName "RI_FKey_check_upd" [] (Pseudo Trigger)
>           False,
>         CatCreateFunction FunName "RI_FKey_noaction_del" []
>           (Pseudo Trigger)
>           False,
>         CatCreateFunction FunName "RI_FKey_noaction_upd" []
>           (Pseudo Trigger)
>           False,
>         CatCreateFunction FunName "RI_FKey_restrict_del" []
>           (Pseudo Trigger)
>           False,
>         CatCreateFunction FunName "RI_FKey_restrict_upd" []
>           (Pseudo Trigger)
>           False,
>         CatCreateFunction FunName "RI_FKey_setdefault_del" []
>           (Pseudo Trigger)
>           False,
>         CatCreateFunction FunName "RI_FKey_setdefault_upd" []
>           (Pseudo Trigger)
>           False,
>         CatCreateFunction FunName "RI_FKey_setnull_del" [] (Pseudo Trigger)
>           False,
>         CatCreateFunction FunName "RI_FKey_setnull_upd" [] (Pseudo Trigger)
>           False,
>         CatCreateFunction FunName "abbrev" [ScalarType "cidr"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "abbrev" [ScalarType "inet"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "abs" [ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "abs" [ScalarType "int2"]
>           (ScalarType "int2")
>           False,
>         CatCreateFunction FunName "abs" [ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "abs" [ScalarType "float4"]
>           (ScalarType "float4")
>           False,
>         CatCreateFunction FunName "abs" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "abs" [ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "abstime" [ScalarType "timestamp"]
>           (ScalarType "abstime")
>           False,
>         CatCreateFunction FunName "abstime" [ScalarType "timestamptz"]
>           (ScalarType "abstime")
>           False,
>         CatCreateFunction FunName "abstimeeq"
>           [ScalarType "abstime", ScalarType "abstime"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "abstimege"
>           [ScalarType "abstime", ScalarType "abstime"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "abstimegt"
>           [ScalarType "abstime", ScalarType "abstime"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "abstimein" [Pseudo Cstring]
>           (ScalarType "abstime")
>           False,
>         CatCreateFunction FunName "abstimele"
>           [ScalarType "abstime", ScalarType "abstime"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "abstimelt"
>           [ScalarType "abstime", ScalarType "abstime"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "abstimene"
>           [ScalarType "abstime", ScalarType "abstime"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "abstimeout" [ScalarType "abstime"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "abstimerecv" [Pseudo Internal]
>           (ScalarType "abstime")
>           False,
>         CatCreateFunction FunName "abstimesend" [ScalarType "abstime"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "aclcontains"
>           [ArrayType (ScalarType "aclitem"), ScalarType "aclitem"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "aclexplode"
>           [ArrayType (ScalarType "aclitem")]
>           (SetOfType (Pseudo Record))
>           False,
>         CatCreateFunction FunName "aclinsert"
>           [ArrayType (ScalarType "aclitem"), ScalarType "aclitem"]
>           (ArrayType (ScalarType "aclitem"))
>           False,
>         CatCreateFunction FunName "aclitemeq"
>           [ScalarType "aclitem", ScalarType "aclitem"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "aclitemin" [Pseudo Cstring]
>           (ScalarType "aclitem")
>           False,
>         CatCreateFunction FunName "aclitemout" [ScalarType "aclitem"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "aclremove"
>           [ArrayType (ScalarType "aclitem"), ScalarType "aclitem"]
>           (ArrayType (ScalarType "aclitem"))
>           False,
>         CatCreateFunction FunName "acos" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "age" [ScalarType "xid"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "age" [ScalarType "timestamp"]
>           (ScalarType "interval")
>           False,
>         CatCreateFunction FunName "age" [ScalarType "timestamptz"]
>           (ScalarType "interval")
>           False,
>         CatCreateFunction FunName "age"
>           [ScalarType "timestamp", ScalarType "timestamp"]
>           (ScalarType "interval")
>           False,
>         CatCreateFunction FunName "age"
>           [ScalarType "timestamptz", ScalarType "timestamptz"]
>           (ScalarType "interval")
>           False,
>         CatCreateFunction FunName "any_in" [Pseudo Cstring] (Pseudo Any)
>           False,
>         CatCreateFunction FunName "any_out" [Pseudo Any] (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "anyarray_in" [Pseudo Cstring]
>           (Pseudo AnyArray)
>           False,
>         CatCreateFunction FunName "anyarray_out" [Pseudo AnyArray]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "anyarray_recv" [Pseudo Internal]
>           (Pseudo AnyArray)
>           False,
>         CatCreateFunction FunName "anyarray_send" [Pseudo AnyArray]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "anyelement_in" [Pseudo Cstring]
>           (Pseudo AnyElement)
>           False,
>         CatCreateFunction FunName "anyelement_out" [Pseudo AnyElement]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "anyenum_in" [Pseudo Cstring]
>           (Pseudo AnyEnum)
>           False,
>         CatCreateFunction FunName "anyenum_out" [Pseudo AnyEnum]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "anynonarray_in" [Pseudo Cstring]
>           (Pseudo AnyNonArray)
>           False,
>         CatCreateFunction FunName "anynonarray_out" [Pseudo AnyNonArray]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "anytextcat"
>           [Pseudo AnyNonArray, ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "area" [ScalarType "path"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "area" [ScalarType "box"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "area" [ScalarType "circle"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "areajoinsel"
>           [Pseudo Internal, ScalarType "oid", Pseudo Internal,
>            ScalarType "int2", Pseudo Internal]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "areasel"
>           [Pseudo Internal, ScalarType "oid", Pseudo Internal,
>            ScalarType "int4"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "array_agg_finalfn" [Pseudo Internal]
>           (Pseudo AnyArray)
>           False,
>         CatCreateFunction FunName "array_agg_transfn"
>           [Pseudo Internal, Pseudo AnyElement]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "array_append"
>           [Pseudo AnyArray, Pseudo AnyElement]
>           (Pseudo AnyArray)
>           False,
>         CatCreateFunction FunName "array_cat"
>           [Pseudo AnyArray, Pseudo AnyArray]
>           (Pseudo AnyArray)
>           False,
>         CatCreateFunction FunName "array_dims" [Pseudo AnyArray]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "array_eq"
>           [Pseudo AnyArray, Pseudo AnyArray]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "array_fill"
>           [Pseudo AnyElement, ArrayType (ScalarType "int4")]
>           (Pseudo AnyArray)
>           False,
>         CatCreateFunction FunName "array_fill"
>           [Pseudo AnyElement, ArrayType (ScalarType "int4"),
>            ArrayType (ScalarType "int4")]
>           (Pseudo AnyArray)
>           False,
>         CatCreateFunction FunName "array_ge"
>           [Pseudo AnyArray, Pseudo AnyArray]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "array_gt"
>           [Pseudo AnyArray, Pseudo AnyArray]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "array_in"
>           [Pseudo Cstring, ScalarType "oid", ScalarType "int4"]
>           (Pseudo AnyArray)
>           False,
>         CatCreateFunction FunName "array_larger"
>           [Pseudo AnyArray, Pseudo AnyArray]
>           (Pseudo AnyArray)
>           False,
>         CatCreateFunction FunName "array_le"
>           [Pseudo AnyArray, Pseudo AnyArray]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "array_length"
>           [Pseudo AnyArray, ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "array_lower"
>           [Pseudo AnyArray, ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "array_lt"
>           [Pseudo AnyArray, Pseudo AnyArray]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "array_ndims" [Pseudo AnyArray]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "array_ne"
>           [Pseudo AnyArray, Pseudo AnyArray]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "array_out" [Pseudo AnyArray]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "array_prepend"
>           [Pseudo AnyElement, Pseudo AnyArray]
>           (Pseudo AnyArray)
>           False,
>         CatCreateFunction FunName "array_recv"
>           [Pseudo Internal, ScalarType "oid", ScalarType "int4"]
>           (Pseudo AnyArray)
>           False,
>         CatCreateFunction FunName "array_send" [Pseudo AnyArray]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "array_smaller"
>           [Pseudo AnyArray, Pseudo AnyArray]
>           (Pseudo AnyArray)
>           False,
>         CatCreateFunction FunName "array_to_string"
>           [Pseudo AnyArray, ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "array_to_string"
>           [Pseudo AnyArray, ScalarType "text", ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "array_upper"
>           [Pseudo AnyArray, ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "arraycontained"
>           [Pseudo AnyArray, Pseudo AnyArray]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "arraycontains"
>           [Pseudo AnyArray, Pseudo AnyArray]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "arrayoverlap"
>           [Pseudo AnyArray, Pseudo AnyArray]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "ascii" [ScalarType "text"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "ascii_to_mic"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "ascii_to_utf8"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "asin" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "atan" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "atan2"
>           [ScalarType "float8", ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "big5_to_euc_tw"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "big5_to_mic"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "big5_to_utf8"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "bit"
>           [ScalarType "int8", ScalarType "int4"]
>           (ScalarType "bit")
>           False,
>         CatCreateFunction FunName "bit"
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "bit")
>           False,
>         CatCreateFunction FunName "bit"
>           [ScalarType "bit", ScalarType "int4", ScalarType "bool"]
>           (ScalarType "bit")
>           False,
>         CatCreateFunction FunName "bit_in"
>           [Pseudo Cstring, ScalarType "oid", ScalarType "int4"]
>           (ScalarType "bit")
>           False,
>         CatCreateFunction FunName "bit_length" [ScalarType "bytea"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "bit_length" [ScalarType "text"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "bit_length" [ScalarType "bit"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "bit_out" [ScalarType "bit"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "bit_recv"
>           [Pseudo Internal, ScalarType "oid", ScalarType "int4"]
>           (ScalarType "bit")
>           False,
>         CatCreateFunction FunName "bit_send" [ScalarType "bit"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "bitand"
>           [ScalarType "bit", ScalarType "bit"]
>           (ScalarType "bit")
>           False,
>         CatCreateFunction FunName "bitcat"
>           [ScalarType "varbit", ScalarType "varbit"]
>           (ScalarType "varbit")
>           False,
>         CatCreateFunction FunName "bitcmp"
>           [ScalarType "bit", ScalarType "bit"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "biteq"
>           [ScalarType "bit", ScalarType "bit"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "bitge"
>           [ScalarType "bit", ScalarType "bit"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "bitgt"
>           [ScalarType "bit", ScalarType "bit"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "bitle"
>           [ScalarType "bit", ScalarType "bit"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "bitlt"
>           [ScalarType "bit", ScalarType "bit"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "bitne"
>           [ScalarType "bit", ScalarType "bit"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "bitnot" [ScalarType "bit"]
>           (ScalarType "bit")
>           False,
>         CatCreateFunction FunName "bitor"
>           [ScalarType "bit", ScalarType "bit"]
>           (ScalarType "bit")
>           False,
>         CatCreateFunction FunName "bitshiftleft"
>           [ScalarType "bit", ScalarType "int4"]
>           (ScalarType "bit")
>           False,
>         CatCreateFunction FunName "bitshiftright"
>           [ScalarType "bit", ScalarType "int4"]
>           (ScalarType "bit")
>           False,
>         CatCreateFunction FunName "bittypmodin"
>           [ArrayType (Pseudo Cstring)]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "bittypmodout" [ScalarType "int4"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "bitxor"
>           [ScalarType "bit", ScalarType "bit"]
>           (ScalarType "bit")
>           False,
>         CatCreateFunction FunName "bool" [ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "booland_statefunc"
>           [ScalarType "bool", ScalarType "bool"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "booleq"
>           [ScalarType "bool", ScalarType "bool"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "boolge"
>           [ScalarType "bool", ScalarType "bool"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "boolgt"
>           [ScalarType "bool", ScalarType "bool"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "boolin" [Pseudo Cstring]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "boolle"
>           [ScalarType "bool", ScalarType "bool"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "boollt"
>           [ScalarType "bool", ScalarType "bool"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "boolne"
>           [ScalarType "bool", ScalarType "bool"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "boolor_statefunc"
>           [ScalarType "bool", ScalarType "bool"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "boolout" [ScalarType "bool"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "boolrecv" [Pseudo Internal]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "boolsend" [ScalarType "bool"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "box" [ScalarType "polygon"]
>           (ScalarType "box")
>           False,
>         CatCreateFunction FunName "box" [ScalarType "circle"]
>           (ScalarType "box")
>           False,
>         CatCreateFunction FunName "box"
>           [ScalarType "point", ScalarType "point"]
>           (ScalarType "box")
>           False,
>         CatCreateFunction FunName "box_above"
>           [ScalarType "box", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "box_above_eq"
>           [ScalarType "box", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "box_add"
>           [ScalarType "box", ScalarType "point"]
>           (ScalarType "box")
>           False,
>         CatCreateFunction FunName "box_below"
>           [ScalarType "box", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "box_below_eq"
>           [ScalarType "box", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "box_center" [ScalarType "box"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunName "box_contain"
>           [ScalarType "box", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "box_contain_pt"
>           [ScalarType "box", ScalarType "point"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "box_contained"
>           [ScalarType "box", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "box_distance"
>           [ScalarType "box", ScalarType "box"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "box_div"
>           [ScalarType "box", ScalarType "point"]
>           (ScalarType "box")
>           False,
>         CatCreateFunction FunName "box_eq"
>           [ScalarType "box", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "box_ge"
>           [ScalarType "box", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "box_gt"
>           [ScalarType "box", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "box_in" [Pseudo Cstring]
>           (ScalarType "box")
>           False,
>         CatCreateFunction FunName "box_intersect"
>           [ScalarType "box", ScalarType "box"]
>           (ScalarType "box")
>           False,
>         CatCreateFunction FunName "box_le"
>           [ScalarType "box", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "box_left"
>           [ScalarType "box", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "box_lt"
>           [ScalarType "box", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "box_mul"
>           [ScalarType "box", ScalarType "point"]
>           (ScalarType "box")
>           False,
>         CatCreateFunction FunName "box_out" [ScalarType "box"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "box_overabove"
>           [ScalarType "box", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "box_overbelow"
>           [ScalarType "box", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "box_overlap"
>           [ScalarType "box", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "box_overleft"
>           [ScalarType "box", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "box_overright"
>           [ScalarType "box", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "box_recv" [Pseudo Internal]
>           (ScalarType "box")
>           False,
>         CatCreateFunction FunName "box_right"
>           [ScalarType "box", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "box_same"
>           [ScalarType "box", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "box_send" [ScalarType "box"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "box_sub"
>           [ScalarType "box", ScalarType "point"]
>           (ScalarType "box")
>           False,
>         CatCreateFunction FunName "bpchar" [ScalarType "char"]
>           (ScalarType "bpchar")
>           False,
>         CatCreateFunction FunName "bpchar" [ScalarType "name"]
>           (ScalarType "bpchar")
>           False,
>         CatCreateFunction FunName "bpchar"
>           [ScalarType "bpchar", ScalarType "int4", ScalarType "bool"]
>           (ScalarType "bpchar")
>           False,
>         CatCreateFunction FunName "bpchar_larger"
>           [ScalarType "bpchar", ScalarType "bpchar"]
>           (ScalarType "bpchar")
>           False,
>         CatCreateFunction FunName "bpchar_pattern_ge"
>           [ScalarType "bpchar", ScalarType "bpchar"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "bpchar_pattern_gt"
>           [ScalarType "bpchar", ScalarType "bpchar"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "bpchar_pattern_le"
>           [ScalarType "bpchar", ScalarType "bpchar"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "bpchar_pattern_lt"
>           [ScalarType "bpchar", ScalarType "bpchar"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "bpchar_smaller"
>           [ScalarType "bpchar", ScalarType "bpchar"]
>           (ScalarType "bpchar")
>           False,
>         CatCreateFunction FunName "bpcharcmp"
>           [ScalarType "bpchar", ScalarType "bpchar"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "bpchareq"
>           [ScalarType "bpchar", ScalarType "bpchar"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "bpcharge"
>           [ScalarType "bpchar", ScalarType "bpchar"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "bpchargt"
>           [ScalarType "bpchar", ScalarType "bpchar"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "bpchariclike"
>           [ScalarType "bpchar", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "bpcharicnlike"
>           [ScalarType "bpchar", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "bpcharicregexeq"
>           [ScalarType "bpchar", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "bpcharicregexne"
>           [ScalarType "bpchar", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "bpcharin"
>           [Pseudo Cstring, ScalarType "oid", ScalarType "int4"]
>           (ScalarType "bpchar")
>           False,
>         CatCreateFunction FunName "bpcharle"
>           [ScalarType "bpchar", ScalarType "bpchar"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "bpcharlike"
>           [ScalarType "bpchar", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "bpcharlt"
>           [ScalarType "bpchar", ScalarType "bpchar"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "bpcharne"
>           [ScalarType "bpchar", ScalarType "bpchar"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "bpcharnlike"
>           [ScalarType "bpchar", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "bpcharout" [ScalarType "bpchar"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "bpcharrecv"
>           [Pseudo Internal, ScalarType "oid", ScalarType "int4"]
>           (ScalarType "bpchar")
>           False,
>         CatCreateFunction FunName "bpcharregexeq"
>           [ScalarType "bpchar", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "bpcharregexne"
>           [ScalarType "bpchar", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "bpcharsend" [ScalarType "bpchar"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "bpchartypmodin"
>           [ArrayType (Pseudo Cstring)]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "bpchartypmodout" [ScalarType "int4"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "broadcast" [ScalarType "inet"]
>           (ScalarType "inet")
>           False,
>         CatCreateFunction FunName "btabstimecmp"
>           [ScalarType "abstime", ScalarType "abstime"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "btarraycmp"
>           [Pseudo AnyArray, Pseudo AnyArray]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "btbeginscan"
>           [Pseudo Internal, Pseudo Internal, Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "btboolcmp"
>           [ScalarType "bool", ScalarType "bool"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "btbpchar_pattern_cmp"
>           [ScalarType "bpchar", ScalarType "bpchar"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "btbuild"
>           [Pseudo Internal, Pseudo Internal, Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "btbuildempty" [Pseudo Internal]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "btbulkdelete"
>           [Pseudo Internal, Pseudo Internal, Pseudo Internal,
>            Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "btcharcmp"
>           [ScalarType "char", ScalarType "char"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "btcostestimate"
>           [Pseudo Internal, Pseudo Internal, Pseudo Internal,
>            Pseudo Internal, Pseudo Internal, Pseudo Internal, Pseudo Internal,
>            Pseudo Internal, Pseudo Internal]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "btendscan" [Pseudo Internal]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "btfloat48cmp"
>           [ScalarType "float4", ScalarType "float8"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "btfloat4cmp"
>           [ScalarType "float4", ScalarType "float4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "btfloat84cmp"
>           [ScalarType "float8", ScalarType "float4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "btfloat8cmp"
>           [ScalarType "float8", ScalarType "float8"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "btgetbitmap"
>           [Pseudo Internal, Pseudo Internal]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "btgettuple"
>           [Pseudo Internal, Pseudo Internal]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "btinsert"
>           [Pseudo Internal, Pseudo Internal, Pseudo Internal,
>            Pseudo Internal, Pseudo Internal, Pseudo Internal]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "btint24cmp"
>           [ScalarType "int2", ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "btint28cmp"
>           [ScalarType "int2", ScalarType "int8"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "btint2cmp"
>           [ScalarType "int2", ScalarType "int2"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "btint42cmp"
>           [ScalarType "int4", ScalarType "int2"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "btint48cmp"
>           [ScalarType "int4", ScalarType "int8"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "btint4cmp"
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "btint82cmp"
>           [ScalarType "int8", ScalarType "int2"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "btint84cmp"
>           [ScalarType "int8", ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "btint8cmp"
>           [ScalarType "int8", ScalarType "int8"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "btmarkpos" [Pseudo Internal]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "btnamecmp"
>           [ScalarType "name", ScalarType "name"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "btoidcmp"
>           [ScalarType "oid", ScalarType "oid"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "btoidvectorcmp"
>           [ScalarType "oidvector", ScalarType "oidvector"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "btoptions"
>           [ArrayType (ScalarType "text"), ScalarType "bool"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "btrecordcmp"
>           [Pseudo Record, Pseudo Record]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "btreltimecmp"
>           [ScalarType "reltime", ScalarType "reltime"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "btrescan"
>           [Pseudo Internal, Pseudo Internal, Pseudo Internal,
>            Pseudo Internal, Pseudo Internal]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "btrestrpos" [Pseudo Internal]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "btrim" [ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "btrim"
>           [ScalarType "bytea", ScalarType "bytea"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "btrim"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "bttext_pattern_cmp"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "bttextcmp"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "bttidcmp"
>           [ScalarType "tid", ScalarType "tid"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "bttintervalcmp"
>           [ScalarType "tinterval", ScalarType "tinterval"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "btvacuumcleanup"
>           [Pseudo Internal, Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "byteacat"
>           [ScalarType "bytea", ScalarType "bytea"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "byteacmp"
>           [ScalarType "bytea", ScalarType "bytea"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "byteaeq"
>           [ScalarType "bytea", ScalarType "bytea"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "byteage"
>           [ScalarType "bytea", ScalarType "bytea"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "byteagt"
>           [ScalarType "bytea", ScalarType "bytea"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "byteain" [Pseudo Cstring]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "byteale"
>           [ScalarType "bytea", ScalarType "bytea"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "bytealike"
>           [ScalarType "bytea", ScalarType "bytea"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "bytealt"
>           [ScalarType "bytea", ScalarType "bytea"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "byteane"
>           [ScalarType "bytea", ScalarType "bytea"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "byteanlike"
>           [ScalarType "bytea", ScalarType "bytea"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "byteaout" [ScalarType "bytea"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "bytearecv" [Pseudo Internal]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "byteasend" [ScalarType "bytea"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "cash_cmp"
>           [ScalarType "money", ScalarType "money"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "cash_div_cash"
>           [ScalarType "money", ScalarType "money"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "cash_div_flt4"
>           [ScalarType "money", ScalarType "float4"]
>           (ScalarType "money")
>           False,
>         CatCreateFunction FunName "cash_div_flt8"
>           [ScalarType "money", ScalarType "float8"]
>           (ScalarType "money")
>           False,
>         CatCreateFunction FunName "cash_div_int2"
>           [ScalarType "money", ScalarType "int2"]
>           (ScalarType "money")
>           False,
>         CatCreateFunction FunName "cash_div_int4"
>           [ScalarType "money", ScalarType "int4"]
>           (ScalarType "money")
>           False,
>         CatCreateFunction FunName "cash_eq"
>           [ScalarType "money", ScalarType "money"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "cash_ge"
>           [ScalarType "money", ScalarType "money"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "cash_gt"
>           [ScalarType "money", ScalarType "money"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "cash_in" [Pseudo Cstring]
>           (ScalarType "money")
>           False,
>         CatCreateFunction FunName "cash_le"
>           [ScalarType "money", ScalarType "money"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "cash_lt"
>           [ScalarType "money", ScalarType "money"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "cash_mi"
>           [ScalarType "money", ScalarType "money"]
>           (ScalarType "money")
>           False,
>         CatCreateFunction FunName "cash_mul_flt4"
>           [ScalarType "money", ScalarType "float4"]
>           (ScalarType "money")
>           False,
>         CatCreateFunction FunName "cash_mul_flt8"
>           [ScalarType "money", ScalarType "float8"]
>           (ScalarType "money")
>           False,
>         CatCreateFunction FunName "cash_mul_int2"
>           [ScalarType "money", ScalarType "int2"]
>           (ScalarType "money")
>           False,
>         CatCreateFunction FunName "cash_mul_int4"
>           [ScalarType "money", ScalarType "int4"]
>           (ScalarType "money")
>           False,
>         CatCreateFunction FunName "cash_ne"
>           [ScalarType "money", ScalarType "money"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "cash_out" [ScalarType "money"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "cash_pl"
>           [ScalarType "money", ScalarType "money"]
>           (ScalarType "money")
>           False,
>         CatCreateFunction FunName "cash_recv" [Pseudo Internal]
>           (ScalarType "money")
>           False,
>         CatCreateFunction FunName "cash_send" [ScalarType "money"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "cash_words" [ScalarType "money"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "cashlarger"
>           [ScalarType "money", ScalarType "money"]
>           (ScalarType "money")
>           False,
>         CatCreateFunction FunName "cashsmaller"
>           [ScalarType "money", ScalarType "money"]
>           (ScalarType "money")
>           False,
>         CatCreateFunction FunName "cbrt" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "ceil" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "ceil" [ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "ceiling" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "ceiling" [ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "center" [ScalarType "box"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunName "center" [ScalarType "circle"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunName "char" [ScalarType "int4"]
>           (ScalarType "char")
>           False,
>         CatCreateFunction FunName "char" [ScalarType "text"]
>           (ScalarType "char")
>           False,
>         CatCreateFunction FunName "char_length" [ScalarType "text"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "char_length" [ScalarType "bpchar"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "character_length" [ScalarType "text"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "character_length" [ScalarType "bpchar"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "chareq"
>           [ScalarType "char", ScalarType "char"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "charge"
>           [ScalarType "char", ScalarType "char"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "chargt"
>           [ScalarType "char", ScalarType "char"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "charin" [Pseudo Cstring]
>           (ScalarType "char")
>           False,
>         CatCreateFunction FunName "charle"
>           [ScalarType "char", ScalarType "char"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "charlt"
>           [ScalarType "char", ScalarType "char"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "charne"
>           [ScalarType "char", ScalarType "char"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "charout" [ScalarType "char"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "charrecv" [Pseudo Internal]
>           (ScalarType "char")
>           False,
>         CatCreateFunction FunName "charsend" [ScalarType "char"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "chr" [ScalarType "int4"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "cideq"
>           [ScalarType "cid", ScalarType "cid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "cidin" [Pseudo Cstring]
>           (ScalarType "cid")
>           False,
>         CatCreateFunction FunName "cidout" [ScalarType "cid"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "cidr" [ScalarType "inet"]
>           (ScalarType "cidr")
>           False,
>         CatCreateFunction FunName "cidr_in" [Pseudo Cstring]
>           (ScalarType "cidr")
>           False,
>         CatCreateFunction FunName "cidr_out" [ScalarType "cidr"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "cidr_recv" [Pseudo Internal]
>           (ScalarType "cidr")
>           False,
>         CatCreateFunction FunName "cidr_send" [ScalarType "cidr"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "cidrecv" [Pseudo Internal]
>           (ScalarType "cid")
>           False,
>         CatCreateFunction FunName "cidsend" [ScalarType "cid"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "circle" [ScalarType "box"]
>           (ScalarType "circle")
>           False,
>         CatCreateFunction FunName "circle" [ScalarType "polygon"]
>           (ScalarType "circle")
>           False,
>         CatCreateFunction FunName "circle"
>           [ScalarType "point", ScalarType "float8"]
>           (ScalarType "circle")
>           False,
>         CatCreateFunction FunName "circle_above"
>           [ScalarType "circle", ScalarType "circle"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "circle_add_pt"
>           [ScalarType "circle", ScalarType "point"]
>           (ScalarType "circle")
>           False,
>         CatCreateFunction FunName "circle_below"
>           [ScalarType "circle", ScalarType "circle"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "circle_center" [ScalarType "circle"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunName "circle_contain"
>           [ScalarType "circle", ScalarType "circle"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "circle_contain_pt"
>           [ScalarType "circle", ScalarType "point"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "circle_contained"
>           [ScalarType "circle", ScalarType "circle"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "circle_distance"
>           [ScalarType "circle", ScalarType "circle"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "circle_div_pt"
>           [ScalarType "circle", ScalarType "point"]
>           (ScalarType "circle")
>           False,
>         CatCreateFunction FunName "circle_eq"
>           [ScalarType "circle", ScalarType "circle"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "circle_ge"
>           [ScalarType "circle", ScalarType "circle"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "circle_gt"
>           [ScalarType "circle", ScalarType "circle"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "circle_in" [Pseudo Cstring]
>           (ScalarType "circle")
>           False,
>         CatCreateFunction FunName "circle_le"
>           [ScalarType "circle", ScalarType "circle"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "circle_left"
>           [ScalarType "circle", ScalarType "circle"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "circle_lt"
>           [ScalarType "circle", ScalarType "circle"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "circle_mul_pt"
>           [ScalarType "circle", ScalarType "point"]
>           (ScalarType "circle")
>           False,
>         CatCreateFunction FunName "circle_ne"
>           [ScalarType "circle", ScalarType "circle"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "circle_out" [ScalarType "circle"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "circle_overabove"
>           [ScalarType "circle", ScalarType "circle"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "circle_overbelow"
>           [ScalarType "circle", ScalarType "circle"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "circle_overlap"
>           [ScalarType "circle", ScalarType "circle"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "circle_overleft"
>           [ScalarType "circle", ScalarType "circle"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "circle_overright"
>           [ScalarType "circle", ScalarType "circle"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "circle_recv" [Pseudo Internal]
>           (ScalarType "circle")
>           False,
>         CatCreateFunction FunName "circle_right"
>           [ScalarType "circle", ScalarType "circle"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "circle_same"
>           [ScalarType "circle", ScalarType "circle"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "circle_send" [ScalarType "circle"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "circle_sub_pt"
>           [ScalarType "circle", ScalarType "point"]
>           (ScalarType "circle")
>           False,
>         CatCreateFunction FunName "clock_timestamp" []
>           (ScalarType "timestamptz")
>           False,
>         CatCreateFunction FunName "close_lb"
>           [ScalarType "line", ScalarType "box"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunName "close_ls"
>           [ScalarType "line", ScalarType "lseg"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunName "close_lseg"
>           [ScalarType "lseg", ScalarType "lseg"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunName "close_pb"
>           [ScalarType "point", ScalarType "box"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunName "close_pl"
>           [ScalarType "point", ScalarType "line"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunName "close_ps"
>           [ScalarType "point", ScalarType "lseg"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunName "close_sb"
>           [ScalarType "lseg", ScalarType "box"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunName "close_sl"
>           [ScalarType "lseg", ScalarType "line"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunName "col_description"
>           [ScalarType "oid", ScalarType "int4"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "contjoinsel"
>           [Pseudo Internal, ScalarType "oid", Pseudo Internal,
>            ScalarType "int2", Pseudo Internal]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "contsel"
>           [Pseudo Internal, ScalarType "oid", Pseudo Internal,
>            ScalarType "int4"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "convert"
>           [ScalarType "bytea", ScalarType "name", ScalarType "name"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "convert_from"
>           [ScalarType "bytea", ScalarType "name"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "convert_to"
>           [ScalarType "text", ScalarType "name"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "cos" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "cot" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "cstring_in" [Pseudo Cstring]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "cstring_out" [Pseudo Cstring]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "cstring_recv" [Pseudo Internal]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "cstring_send" [Pseudo Cstring]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "current_database" [] (ScalarType "name")
>           False,
>         CatCreateFunction FunName "current_query" [] (ScalarType "text")
>           False,
>         CatCreateFunction FunName "current_schema" [] (ScalarType "name")
>           False,
>         CatCreateFunction FunName "current_schemas" [ScalarType "bool"]
>           (ArrayType (ScalarType "name"))
>           False,
>         CatCreateFunction FunName "current_setting" [ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "current_user" [] (ScalarType "name")
>           False,
>         CatCreateFunction FunName "currtid"
>           [ScalarType "oid", ScalarType "tid"]
>           (ScalarType "tid")
>           False,
>         CatCreateFunction FunName "currtid2"
>           [ScalarType "text", ScalarType "tid"]
>           (ScalarType "tid")
>           False,
>         CatCreateFunction FunName "currval" [ScalarType "regclass"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "cursor_to_xml"
>           [ScalarType "refcursor", ScalarType "int4", ScalarType "bool",
>            ScalarType "bool", ScalarType "text"]
>           (ScalarType "xml")
>           False,
>         CatCreateFunction FunName "cursor_to_xmlschema"
>           [ScalarType "refcursor", ScalarType "bool", ScalarType "bool",
>            ScalarType "text"]
>           (ScalarType "xml")
>           False,
>         CatCreateFunction FunName "database_to_xml"
>           [ScalarType "bool", ScalarType "bool", ScalarType "text"]
>           (ScalarType "xml")
>           False,
>         CatCreateFunction FunName "database_to_xml_and_xmlschema"
>           [ScalarType "bool", ScalarType "bool", ScalarType "text"]
>           (ScalarType "xml")
>           False,
>         CatCreateFunction FunName "database_to_xmlschema"
>           [ScalarType "bool", ScalarType "bool", ScalarType "text"]
>           (ScalarType "xml")
>           False,
>         CatCreateFunction FunName "date" [ScalarType "abstime"]
>           (ScalarType "date")
>           False,
>         CatCreateFunction FunName "date" [ScalarType "timestamp"]
>           (ScalarType "date")
>           False,
>         CatCreateFunction FunName "date" [ScalarType "timestamptz"]
>           (ScalarType "date")
>           False,
>         CatCreateFunction FunName "date_cmp"
>           [ScalarType "date", ScalarType "date"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "date_cmp_timestamp"
>           [ScalarType "date", ScalarType "timestamp"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "date_cmp_timestamptz"
>           [ScalarType "date", ScalarType "timestamptz"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "date_eq"
>           [ScalarType "date", ScalarType "date"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "date_eq_timestamp"
>           [ScalarType "date", ScalarType "timestamp"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "date_eq_timestamptz"
>           [ScalarType "date", ScalarType "timestamptz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "date_ge"
>           [ScalarType "date", ScalarType "date"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "date_ge_timestamp"
>           [ScalarType "date", ScalarType "timestamp"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "date_ge_timestamptz"
>           [ScalarType "date", ScalarType "timestamptz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "date_gt"
>           [ScalarType "date", ScalarType "date"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "date_gt_timestamp"
>           [ScalarType "date", ScalarType "timestamp"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "date_gt_timestamptz"
>           [ScalarType "date", ScalarType "timestamptz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "date_in" [Pseudo Cstring]
>           (ScalarType "date")
>           False,
>         CatCreateFunction FunName "date_larger"
>           [ScalarType "date", ScalarType "date"]
>           (ScalarType "date")
>           False,
>         CatCreateFunction FunName "date_le"
>           [ScalarType "date", ScalarType "date"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "date_le_timestamp"
>           [ScalarType "date", ScalarType "timestamp"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "date_le_timestamptz"
>           [ScalarType "date", ScalarType "timestamptz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "date_lt"
>           [ScalarType "date", ScalarType "date"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "date_lt_timestamp"
>           [ScalarType "date", ScalarType "timestamp"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "date_lt_timestamptz"
>           [ScalarType "date", ScalarType "timestamptz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "date_mi"
>           [ScalarType "date", ScalarType "date"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "date_mi_interval"
>           [ScalarType "date", ScalarType "interval"]
>           (ScalarType "timestamp")
>           False,
>         CatCreateFunction FunName "date_mii"
>           [ScalarType "date", ScalarType "int4"]
>           (ScalarType "date")
>           False,
>         CatCreateFunction FunName "date_ne"
>           [ScalarType "date", ScalarType "date"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "date_ne_timestamp"
>           [ScalarType "date", ScalarType "timestamp"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "date_ne_timestamptz"
>           [ScalarType "date", ScalarType "timestamptz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "date_out" [ScalarType "date"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "date_part"
>           [ScalarType "text", ScalarType "abstime"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "date_part"
>           [ScalarType "text", ScalarType "reltime"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "date_part"
>           [ScalarType "text", ScalarType "date"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "date_part"
>           [ScalarType "text", ScalarType "time"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "date_part"
>           [ScalarType "text", ScalarType "timestamp"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "date_part"
>           [ScalarType "text", ScalarType "timestamptz"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "date_part"
>           [ScalarType "text", ScalarType "interval"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "date_part"
>           [ScalarType "text", ScalarType "timetz"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "date_pl_interval"
>           [ScalarType "date", ScalarType "interval"]
>           (ScalarType "timestamp")
>           False,
>         CatCreateFunction FunName "date_pli"
>           [ScalarType "date", ScalarType "int4"]
>           (ScalarType "date")
>           False,
>         CatCreateFunction FunName "date_recv" [Pseudo Internal]
>           (ScalarType "date")
>           False,
>         CatCreateFunction FunName "date_send" [ScalarType "date"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "date_smaller"
>           [ScalarType "date", ScalarType "date"]
>           (ScalarType "date")
>           False,
>         CatCreateFunction FunName "date_trunc"
>           [ScalarType "text", ScalarType "timestamp"]
>           (ScalarType "timestamp")
>           False,
>         CatCreateFunction FunName "date_trunc"
>           [ScalarType "text", ScalarType "timestamptz"]
>           (ScalarType "timestamptz")
>           False,
>         CatCreateFunction FunName "date_trunc"
>           [ScalarType "text", ScalarType "interval"]
>           (ScalarType "interval")
>           False,
>         CatCreateFunction FunName "datetime_pl"
>           [ScalarType "date", ScalarType "time"]
>           (ScalarType "timestamp")
>           False,
>         CatCreateFunction FunName "datetimetz_pl"
>           [ScalarType "date", ScalarType "timetz"]
>           (ScalarType "timestamptz")
>           False,
>         CatCreateFunction FunName "dcbrt" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "decode"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "degrees" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "dexp" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "diagonal" [ScalarType "box"]
>           (ScalarType "lseg")
>           False,
>         CatCreateFunction FunName "diameter" [ScalarType "circle"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "dispell_init" [Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "dispell_lexize"
>           [Pseudo Internal, Pseudo Internal, Pseudo Internal,
>            Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "dist_cpoly"
>           [ScalarType "circle", ScalarType "polygon"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "dist_lb"
>           [ScalarType "line", ScalarType "box"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "dist_pb"
>           [ScalarType "point", ScalarType "box"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "dist_pc"
>           [ScalarType "point", ScalarType "circle"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "dist_pl"
>           [ScalarType "point", ScalarType "line"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "dist_ppath"
>           [ScalarType "point", ScalarType "path"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "dist_ps"
>           [ScalarType "point", ScalarType "lseg"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "dist_sb"
>           [ScalarType "lseg", ScalarType "box"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "dist_sl"
>           [ScalarType "lseg", ScalarType "line"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "div"
>           [ScalarType "numeric", ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "dlog1" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "dlog10" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "domain_in"
>           [Pseudo Cstring, ScalarType "oid", ScalarType "int4"]
>           (Pseudo Any)
>           False,
>         CatCreateFunction FunName "domain_recv"
>           [Pseudo Internal, ScalarType "oid", ScalarType "int4"]
>           (Pseudo Any)
>           False,
>         CatCreateFunction FunName "dpow"
>           [ScalarType "float8", ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "dround" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "dsimple_init" [Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "dsimple_lexize"
>           [Pseudo Internal, Pseudo Internal, Pseudo Internal,
>            Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "dsnowball_init" [Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "dsnowball_lexize"
>           [Pseudo Internal, Pseudo Internal, Pseudo Internal,
>            Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "dsqrt" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "dsynonym_init" [Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "dsynonym_lexize"
>           [Pseudo Internal, Pseudo Internal, Pseudo Internal,
>            Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "dtrunc" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "encode"
>           [ScalarType "bytea", ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "enum_cmp"
>           [Pseudo AnyEnum, Pseudo AnyEnum]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "enum_eq"
>           [Pseudo AnyEnum, Pseudo AnyEnum]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "enum_first" [Pseudo AnyEnum]
>           (Pseudo AnyEnum)
>           False,
>         CatCreateFunction FunName "enum_ge"
>           [Pseudo AnyEnum, Pseudo AnyEnum]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "enum_gt"
>           [Pseudo AnyEnum, Pseudo AnyEnum]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "enum_in"
>           [Pseudo Cstring, ScalarType "oid"]
>           (Pseudo AnyEnum)
>           False,
>         CatCreateFunction FunName "enum_larger"
>           [Pseudo AnyEnum, Pseudo AnyEnum]
>           (Pseudo AnyEnum)
>           False,
>         CatCreateFunction FunName "enum_last" [Pseudo AnyEnum]
>           (Pseudo AnyEnum)
>           False,
>         CatCreateFunction FunName "enum_le"
>           [Pseudo AnyEnum, Pseudo AnyEnum]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "enum_lt"
>           [Pseudo AnyEnum, Pseudo AnyEnum]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "enum_ne"
>           [Pseudo AnyEnum, Pseudo AnyEnum]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "enum_out" [Pseudo AnyEnum]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "enum_range" [Pseudo AnyEnum]
>           (Pseudo AnyArray)
>           False,
>         CatCreateFunction FunName "enum_range"
>           [Pseudo AnyEnum, Pseudo AnyEnum]
>           (Pseudo AnyArray)
>           False,
>         CatCreateFunction FunName "enum_recv"
>           [Pseudo Cstring, ScalarType "oid"]
>           (Pseudo AnyEnum)
>           False,
>         CatCreateFunction FunName "enum_send" [Pseudo AnyEnum]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "enum_smaller"
>           [Pseudo AnyEnum, Pseudo AnyEnum]
>           (Pseudo AnyEnum)
>           False,
>         CatCreateFunction FunName "eqjoinsel"
>           [Pseudo Internal, ScalarType "oid", Pseudo Internal,
>            ScalarType "int2", Pseudo Internal]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "eqsel"
>           [Pseudo Internal, ScalarType "oid", Pseudo Internal,
>            ScalarType "int4"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "euc_cn_to_mic"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "euc_cn_to_utf8"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "euc_jis_2004_to_shift_jis_2004"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "euc_jis_2004_to_utf8"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "euc_jp_to_mic"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "euc_jp_to_sjis"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "euc_jp_to_utf8"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "euc_kr_to_mic"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "euc_kr_to_utf8"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "euc_tw_to_big5"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "euc_tw_to_mic"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "euc_tw_to_utf8"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "exp" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "exp" [ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "factorial" [ScalarType "int8"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "family" [ScalarType "inet"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "fdw_handler_in" [Pseudo Cstring]
>           (Pseudo FdwHandler)
>           False,
>         CatCreateFunction FunName "fdw_handler_out" [Pseudo FdwHandler]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "float4" [ScalarType "int8"]
>           (ScalarType "float4")
>           False,
>         CatCreateFunction FunName "float4" [ScalarType "int2"]
>           (ScalarType "float4")
>           False,
>         CatCreateFunction FunName "float4" [ScalarType "int4"]
>           (ScalarType "float4")
>           False,
>         CatCreateFunction FunName "float4" [ScalarType "float8"]
>           (ScalarType "float4")
>           False,
>         CatCreateFunction FunName "float4" [ScalarType "numeric"]
>           (ScalarType "float4")
>           False,
>         CatCreateFunction FunName "float48div"
>           [ScalarType "float4", ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "float48eq"
>           [ScalarType "float4", ScalarType "float8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "float48ge"
>           [ScalarType "float4", ScalarType "float8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "float48gt"
>           [ScalarType "float4", ScalarType "float8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "float48le"
>           [ScalarType "float4", ScalarType "float8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "float48lt"
>           [ScalarType "float4", ScalarType "float8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "float48mi"
>           [ScalarType "float4", ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "float48mul"
>           [ScalarType "float4", ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "float48ne"
>           [ScalarType "float4", ScalarType "float8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "float48pl"
>           [ScalarType "float4", ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "float4_accum"
>           [ArrayType (ScalarType "float8"), ScalarType "float4"]
>           (ArrayType (ScalarType "float8"))
>           False,
>         CatCreateFunction FunName "float4abs" [ScalarType "float4"]
>           (ScalarType "float4")
>           False,
>         CatCreateFunction FunName "float4div"
>           [ScalarType "float4", ScalarType "float4"]
>           (ScalarType "float4")
>           False,
>         CatCreateFunction FunName "float4eq"
>           [ScalarType "float4", ScalarType "float4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "float4ge"
>           [ScalarType "float4", ScalarType "float4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "float4gt"
>           [ScalarType "float4", ScalarType "float4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "float4in" [Pseudo Cstring]
>           (ScalarType "float4")
>           False,
>         CatCreateFunction FunName "float4larger"
>           [ScalarType "float4", ScalarType "float4"]
>           (ScalarType "float4")
>           False,
>         CatCreateFunction FunName "float4le"
>           [ScalarType "float4", ScalarType "float4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "float4lt"
>           [ScalarType "float4", ScalarType "float4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "float4mi"
>           [ScalarType "float4", ScalarType "float4"]
>           (ScalarType "float4")
>           False,
>         CatCreateFunction FunName "float4mul"
>           [ScalarType "float4", ScalarType "float4"]
>           (ScalarType "float4")
>           False,
>         CatCreateFunction FunName "float4ne"
>           [ScalarType "float4", ScalarType "float4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "float4out" [ScalarType "float4"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "float4pl"
>           [ScalarType "float4", ScalarType "float4"]
>           (ScalarType "float4")
>           False,
>         CatCreateFunction FunName "float4recv" [Pseudo Internal]
>           (ScalarType "float4")
>           False,
>         CatCreateFunction FunName "float4send" [ScalarType "float4"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "float4smaller"
>           [ScalarType "float4", ScalarType "float4"]
>           (ScalarType "float4")
>           False,
>         CatCreateFunction FunName "float4um" [ScalarType "float4"]
>           (ScalarType "float4")
>           False,
>         CatCreateFunction FunName "float4up" [ScalarType "float4"]
>           (ScalarType "float4")
>           False,
>         CatCreateFunction FunName "float8" [ScalarType "int8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "float8" [ScalarType "int2"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "float8" [ScalarType "int4"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "float8" [ScalarType "float4"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "float8" [ScalarType "numeric"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "float84div"
>           [ScalarType "float8", ScalarType "float4"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "float84eq"
>           [ScalarType "float8", ScalarType "float4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "float84ge"
>           [ScalarType "float8", ScalarType "float4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "float84gt"
>           [ScalarType "float8", ScalarType "float4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "float84le"
>           [ScalarType "float8", ScalarType "float4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "float84lt"
>           [ScalarType "float8", ScalarType "float4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "float84mi"
>           [ScalarType "float8", ScalarType "float4"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "float84mul"
>           [ScalarType "float8", ScalarType "float4"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "float84ne"
>           [ScalarType "float8", ScalarType "float4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "float84pl"
>           [ScalarType "float8", ScalarType "float4"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "float8_accum"
>           [ArrayType (ScalarType "float8"), ScalarType "float8"]
>           (ArrayType (ScalarType "float8"))
>           False,
>         CatCreateFunction FunName "float8_avg"
>           [ArrayType (ScalarType "float8")]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "float8_corr"
>           [ArrayType (ScalarType "float8")]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "float8_covar_pop"
>           [ArrayType (ScalarType "float8")]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "float8_covar_samp"
>           [ArrayType (ScalarType "float8")]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "float8_regr_accum"
>           [ArrayType (ScalarType "float8"), ScalarType "float8",
>            ScalarType "float8"]
>           (ArrayType (ScalarType "float8"))
>           False,
>         CatCreateFunction FunName "float8_regr_avgx"
>           [ArrayType (ScalarType "float8")]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "float8_regr_avgy"
>           [ArrayType (ScalarType "float8")]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "float8_regr_intercept"
>           [ArrayType (ScalarType "float8")]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "float8_regr_r2"
>           [ArrayType (ScalarType "float8")]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "float8_regr_slope"
>           [ArrayType (ScalarType "float8")]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "float8_regr_sxx"
>           [ArrayType (ScalarType "float8")]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "float8_regr_sxy"
>           [ArrayType (ScalarType "float8")]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "float8_regr_syy"
>           [ArrayType (ScalarType "float8")]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "float8_stddev_pop"
>           [ArrayType (ScalarType "float8")]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "float8_stddev_samp"
>           [ArrayType (ScalarType "float8")]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "float8_var_pop"
>           [ArrayType (ScalarType "float8")]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "float8_var_samp"
>           [ArrayType (ScalarType "float8")]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "float8abs" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "float8div"
>           [ScalarType "float8", ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "float8eq"
>           [ScalarType "float8", ScalarType "float8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "float8ge"
>           [ScalarType "float8", ScalarType "float8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "float8gt"
>           [ScalarType "float8", ScalarType "float8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "float8in" [Pseudo Cstring]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "float8larger"
>           [ScalarType "float8", ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "float8le"
>           [ScalarType "float8", ScalarType "float8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "float8lt"
>           [ScalarType "float8", ScalarType "float8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "float8mi"
>           [ScalarType "float8", ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "float8mul"
>           [ScalarType "float8", ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "float8ne"
>           [ScalarType "float8", ScalarType "float8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "float8out" [ScalarType "float8"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "float8pl"
>           [ScalarType "float8", ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "float8recv" [Pseudo Internal]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "float8send" [ScalarType "float8"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "float8smaller"
>           [ScalarType "float8", ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "float8um" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "float8up" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "floor" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "floor" [ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "flt4_mul_cash"
>           [ScalarType "float4", ScalarType "money"]
>           (ScalarType "money")
>           False,
>         CatCreateFunction FunName "flt8_mul_cash"
>           [ScalarType "float8", ScalarType "money"]
>           (ScalarType "money")
>           False,
>         CatCreateFunction FunName "fmgr_c_validator" [ScalarType "oid"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "fmgr_internal_validator"
>           [ScalarType "oid"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "fmgr_sql_validator" [ScalarType "oid"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "format" [ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "format_type"
>           [ScalarType "oid", ScalarType "int4"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "gb18030_to_utf8"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "gbk_to_utf8"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "generate_series"
>           [ScalarType "int8", ScalarType "int8"]
>           (SetOfType (ScalarType "int8"))
>           False,
>         CatCreateFunction FunName "generate_series"
>           [ScalarType "int4", ScalarType "int4"]
>           (SetOfType (ScalarType "int4"))
>           False,
>         CatCreateFunction FunName "generate_series"
>           [ScalarType "int8", ScalarType "int8", ScalarType "int8"]
>           (SetOfType (ScalarType "int8"))
>           False,
>         CatCreateFunction FunName "generate_series"
>           [ScalarType "int4", ScalarType "int4", ScalarType "int4"]
>           (SetOfType (ScalarType "int4"))
>           False,
>         CatCreateFunction FunName "generate_series"
>           [ScalarType "timestamp", ScalarType "timestamp",
>            ScalarType "interval"]
>           (SetOfType (ScalarType "timestamp"))
>           False,
>         CatCreateFunction FunName "generate_series"
>           [ScalarType "timestamptz", ScalarType "timestamptz",
>            ScalarType "interval"]
>           (SetOfType (ScalarType "timestamptz"))
>           False,
>         CatCreateFunction FunName "generate_subscripts"
>           [Pseudo AnyArray, ScalarType "int4"]
>           (SetOfType (ScalarType "int4"))
>           False,
>         CatCreateFunction FunName "generate_subscripts"
>           [Pseudo AnyArray, ScalarType "int4", ScalarType "bool"]
>           (SetOfType (ScalarType "int4"))
>           False,
>         CatCreateFunction FunName "get_bit"
>           [ScalarType "bytea", ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "get_bit"
>           [ScalarType "bit", ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "get_byte"
>           [ScalarType "bytea", ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "get_current_ts_config" []
>           (ScalarType "regconfig")
>           False,
>         CatCreateFunction FunName "getdatabaseencoding" []
>           (ScalarType "name")
>           False,
>         CatCreateFunction FunName "getpgusername" [] (ScalarType "name")
>           False,
>         CatCreateFunction FunName "gin_cmp_prefix"
>           [ScalarType "text", ScalarType "text", ScalarType "int2",
>            Pseudo Internal]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "gin_cmp_tslexeme"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "gin_extract_tsquery"
>           [ScalarType "tsquery", Pseudo Internal, ScalarType "int2",
>            Pseudo Internal, Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "gin_extract_tsquery"
>           [ScalarType "tsquery", Pseudo Internal, ScalarType "int2",
>            Pseudo Internal, Pseudo Internal, Pseudo Internal, Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "gin_extract_tsvector"
>           [ScalarType "tsvector", Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "gin_extract_tsvector"
>           [ScalarType "tsvector", Pseudo Internal, Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "gin_tsquery_consistent"
>           [Pseudo Internal, ScalarType "int2", ScalarType "tsquery",
>            ScalarType "int4", Pseudo Internal, Pseudo Internal]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "gin_tsquery_consistent"
>           [Pseudo Internal, ScalarType "int2", ScalarType "tsquery",
>            ScalarType "int4", Pseudo Internal, Pseudo Internal,
>            Pseudo Internal, Pseudo Internal]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "ginarrayconsistent"
>           [Pseudo Internal, ScalarType "int2", Pseudo AnyArray,
>            ScalarType "int4", Pseudo Internal, Pseudo Internal,
>            Pseudo Internal, Pseudo Internal]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "ginarrayextract"
>           [Pseudo AnyArray, Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "ginarrayextract"
>           [Pseudo AnyArray, Pseudo Internal, Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "ginbeginscan"
>           [Pseudo Internal, Pseudo Internal, Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "ginbuild"
>           [Pseudo Internal, Pseudo Internal, Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "ginbuildempty" [Pseudo Internal]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "ginbulkdelete"
>           [Pseudo Internal, Pseudo Internal, Pseudo Internal,
>            Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "gincostestimate"
>           [Pseudo Internal, Pseudo Internal, Pseudo Internal,
>            Pseudo Internal, Pseudo Internal, Pseudo Internal, Pseudo Internal,
>            Pseudo Internal, Pseudo Internal]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "ginendscan" [Pseudo Internal]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "gingetbitmap"
>           [Pseudo Internal, Pseudo Internal]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "gininsert"
>           [Pseudo Internal, Pseudo Internal, Pseudo Internal,
>            Pseudo Internal, Pseudo Internal, Pseudo Internal]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "ginmarkpos" [Pseudo Internal]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "ginoptions"
>           [ArrayType (ScalarType "text"), ScalarType "bool"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "ginqueryarrayextract"
>           [Pseudo AnyArray, Pseudo Internal, ScalarType "int2",
>            Pseudo Internal, Pseudo Internal, Pseudo Internal, Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "ginrescan"
>           [Pseudo Internal, Pseudo Internal, Pseudo Internal,
>            Pseudo Internal, Pseudo Internal]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "ginrestrpos" [Pseudo Internal]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "ginvacuumcleanup"
>           [Pseudo Internal, Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "gist_box_compress" [Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "gist_box_consistent"
>           [Pseudo Internal, ScalarType "box", ScalarType "int4",
>            ScalarType "oid", Pseudo Internal]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "gist_box_decompress" [Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "gist_box_penalty"
>           [Pseudo Internal, Pseudo Internal, Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "gist_box_picksplit"
>           [Pseudo Internal, Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "gist_box_same"
>           [ScalarType "box", ScalarType "box", Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "gist_box_union"
>           [Pseudo Internal, Pseudo Internal]
>           (ScalarType "box")
>           False,
>         CatCreateFunction FunName "gist_circle_compress" [Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "gist_circle_consistent"
>           [Pseudo Internal, ScalarType "circle", ScalarType "int4",
>            ScalarType "oid", Pseudo Internal]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "gist_point_compress" [Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "gist_point_consistent"
>           [Pseudo Internal, ScalarType "point", ScalarType "int4",
>            ScalarType "oid", Pseudo Internal]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "gist_point_distance"
>           [Pseudo Internal, ScalarType "point", ScalarType "int4",
>            ScalarType "oid"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "gist_poly_compress" [Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "gist_poly_consistent"
>           [Pseudo Internal, ScalarType "polygon", ScalarType "int4",
>            ScalarType "oid", Pseudo Internal]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "gistbeginscan"
>           [Pseudo Internal, Pseudo Internal, Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "gistbuild"
>           [Pseudo Internal, Pseudo Internal, Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "gistbuildempty" [Pseudo Internal]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "gistbulkdelete"
>           [Pseudo Internal, Pseudo Internal, Pseudo Internal,
>            Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "gistcostestimate"
>           [Pseudo Internal, Pseudo Internal, Pseudo Internal,
>            Pseudo Internal, Pseudo Internal, Pseudo Internal, Pseudo Internal,
>            Pseudo Internal, Pseudo Internal]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "gistendscan" [Pseudo Internal]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "gistgetbitmap"
>           [Pseudo Internal, Pseudo Internal]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "gistgettuple"
>           [Pseudo Internal, Pseudo Internal]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "gistinsert"
>           [Pseudo Internal, Pseudo Internal, Pseudo Internal,
>            Pseudo Internal, Pseudo Internal, Pseudo Internal]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "gistmarkpos" [Pseudo Internal]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "gistoptions"
>           [ArrayType (ScalarType "text"), ScalarType "bool"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "gistrescan"
>           [Pseudo Internal, Pseudo Internal, Pseudo Internal,
>            Pseudo Internal, Pseudo Internal]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "gistrestrpos" [Pseudo Internal]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "gistvacuumcleanup"
>           [Pseudo Internal, Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "gtsquery_compress" [Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "gtsquery_consistent"
>           [Pseudo Internal, Pseudo Internal, ScalarType "int4",
>            ScalarType "oid", Pseudo Internal]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "gtsquery_decompress" [Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "gtsquery_penalty"
>           [Pseudo Internal, Pseudo Internal, Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "gtsquery_picksplit"
>           [Pseudo Internal, Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "gtsquery_same"
>           [ScalarType "int8", ScalarType "int8", Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "gtsquery_union"
>           [Pseudo Internal, Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "gtsvector_compress" [Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "gtsvector_consistent"
>           [Pseudo Internal, ScalarType "gtsvector", ScalarType "int4",
>            ScalarType "oid", Pseudo Internal]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "gtsvector_decompress" [Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "gtsvector_penalty"
>           [Pseudo Internal, Pseudo Internal, Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "gtsvector_picksplit"
>           [Pseudo Internal, Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "gtsvector_same"
>           [ScalarType "gtsvector", ScalarType "gtsvector", Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "gtsvector_union"
>           [Pseudo Internal, Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "gtsvectorin" [Pseudo Cstring]
>           (ScalarType "gtsvector")
>           False,
>         CatCreateFunction FunName "gtsvectorout" [ScalarType "gtsvector"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "has_any_column_privilege"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_any_column_privilege"
>           [ScalarType "oid", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_any_column_privilege"
>           [ScalarType "name", ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_any_column_privilege"
>           [ScalarType "name", ScalarType "oid", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_any_column_privilege"
>           [ScalarType "oid", ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_any_column_privilege"
>           [ScalarType "oid", ScalarType "oid", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_column_privilege"
>           [ScalarType "text", ScalarType "int2", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_column_privilege"
>           [ScalarType "text", ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_column_privilege"
>           [ScalarType "oid", ScalarType "int2", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_column_privilege"
>           [ScalarType "oid", ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_column_privilege"
>           [ScalarType "name", ScalarType "text", ScalarType "int2",
>            ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_column_privilege"
>           [ScalarType "name", ScalarType "text", ScalarType "text",
>            ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_column_privilege"
>           [ScalarType "name", ScalarType "oid", ScalarType "int2",
>            ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_column_privilege"
>           [ScalarType "name", ScalarType "oid", ScalarType "text",
>            ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_column_privilege"
>           [ScalarType "oid", ScalarType "text", ScalarType "int2",
>            ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_column_privilege"
>           [ScalarType "oid", ScalarType "text", ScalarType "text",
>            ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_column_privilege"
>           [ScalarType "oid", ScalarType "oid", ScalarType "int2",
>            ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_column_privilege"
>           [ScalarType "oid", ScalarType "oid", ScalarType "text",
>            ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_database_privilege"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_database_privilege"
>           [ScalarType "oid", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_database_privilege"
>           [ScalarType "name", ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_database_privilege"
>           [ScalarType "name", ScalarType "oid", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_database_privilege"
>           [ScalarType "oid", ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_database_privilege"
>           [ScalarType "oid", ScalarType "oid", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_foreign_data_wrapper_privilege"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_foreign_data_wrapper_privilege"
>           [ScalarType "oid", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_foreign_data_wrapper_privilege"
>           [ScalarType "name", ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_foreign_data_wrapper_privilege"
>           [ScalarType "name", ScalarType "oid", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_foreign_data_wrapper_privilege"
>           [ScalarType "oid", ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_foreign_data_wrapper_privilege"
>           [ScalarType "oid", ScalarType "oid", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_function_privilege"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_function_privilege"
>           [ScalarType "oid", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_function_privilege"
>           [ScalarType "name", ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_function_privilege"
>           [ScalarType "name", ScalarType "oid", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_function_privilege"
>           [ScalarType "oid", ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_function_privilege"
>           [ScalarType "oid", ScalarType "oid", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_language_privilege"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_language_privilege"
>           [ScalarType "oid", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_language_privilege"
>           [ScalarType "name", ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_language_privilege"
>           [ScalarType "name", ScalarType "oid", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_language_privilege"
>           [ScalarType "oid", ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_language_privilege"
>           [ScalarType "oid", ScalarType "oid", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_schema_privilege"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_schema_privilege"
>           [ScalarType "oid", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_schema_privilege"
>           [ScalarType "name", ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_schema_privilege"
>           [ScalarType "name", ScalarType "oid", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_schema_privilege"
>           [ScalarType "oid", ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_schema_privilege"
>           [ScalarType "oid", ScalarType "oid", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_sequence_privilege"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_sequence_privilege"
>           [ScalarType "oid", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_sequence_privilege"
>           [ScalarType "name", ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_sequence_privilege"
>           [ScalarType "name", ScalarType "oid", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_sequence_privilege"
>           [ScalarType "oid", ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_sequence_privilege"
>           [ScalarType "oid", ScalarType "oid", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_server_privilege"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_server_privilege"
>           [ScalarType "oid", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_server_privilege"
>           [ScalarType "name", ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_server_privilege"
>           [ScalarType "name", ScalarType "oid", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_server_privilege"
>           [ScalarType "oid", ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_server_privilege"
>           [ScalarType "oid", ScalarType "oid", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_table_privilege"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_table_privilege"
>           [ScalarType "oid", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_table_privilege"
>           [ScalarType "name", ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_table_privilege"
>           [ScalarType "name", ScalarType "oid", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_table_privilege"
>           [ScalarType "oid", ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_table_privilege"
>           [ScalarType "oid", ScalarType "oid", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_tablespace_privilege"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_tablespace_privilege"
>           [ScalarType "oid", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_tablespace_privilege"
>           [ScalarType "name", ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_tablespace_privilege"
>           [ScalarType "name", ScalarType "oid", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_tablespace_privilege"
>           [ScalarType "oid", ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "has_tablespace_privilege"
>           [ScalarType "oid", ScalarType "oid", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "hash_aclitem" [ScalarType "aclitem"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "hash_array" [Pseudo AnyArray]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "hash_numeric" [ScalarType "numeric"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "hashbeginscan"
>           [Pseudo Internal, Pseudo Internal, Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "hashbpchar" [ScalarType "bpchar"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "hashbuild"
>           [Pseudo Internal, Pseudo Internal, Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "hashbuildempty" [Pseudo Internal]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "hashbulkdelete"
>           [Pseudo Internal, Pseudo Internal, Pseudo Internal,
>            Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "hashchar" [ScalarType "char"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "hashcostestimate"
>           [Pseudo Internal, Pseudo Internal, Pseudo Internal,
>            Pseudo Internal, Pseudo Internal, Pseudo Internal, Pseudo Internal,
>            Pseudo Internal, Pseudo Internal]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "hashendscan" [Pseudo Internal]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "hashenum" [Pseudo AnyEnum]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "hashfloat4" [ScalarType "float4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "hashfloat8" [ScalarType "float8"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "hashgetbitmap"
>           [Pseudo Internal, Pseudo Internal]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "hashgettuple"
>           [Pseudo Internal, Pseudo Internal]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "hashinet" [ScalarType "inet"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "hashinsert"
>           [Pseudo Internal, Pseudo Internal, Pseudo Internal,
>            Pseudo Internal, Pseudo Internal, Pseudo Internal]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "hashint2" [ScalarType "int2"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "hashint2vector"
>           [ScalarType "int2vector"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "hashint4" [ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "hashint8" [ScalarType "int8"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "hashmacaddr" [ScalarType "macaddr"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "hashmarkpos" [Pseudo Internal]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "hashname" [ScalarType "name"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "hashoid" [ScalarType "oid"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "hashoidvector" [ScalarType "oidvector"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "hashoptions"
>           [ArrayType (ScalarType "text"), ScalarType "bool"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "hashrescan"
>           [Pseudo Internal, Pseudo Internal, Pseudo Internal,
>            Pseudo Internal, Pseudo Internal]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "hashrestrpos" [Pseudo Internal]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "hashtext" [ScalarType "text"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "hashvacuumcleanup"
>           [Pseudo Internal, Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "hashvarlena" [Pseudo Internal]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "height" [ScalarType "box"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "host" [ScalarType "inet"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "hostmask" [ScalarType "inet"]
>           (ScalarType "inet")
>           False,
>         CatCreateFunction FunName "iclikejoinsel"
>           [Pseudo Internal, ScalarType "oid", Pseudo Internal,
>            ScalarType "int2", Pseudo Internal]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "iclikesel"
>           [Pseudo Internal, ScalarType "oid", Pseudo Internal,
>            ScalarType "int4"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "icnlikejoinsel"
>           [Pseudo Internal, ScalarType "oid", Pseudo Internal,
>            ScalarType "int2", Pseudo Internal]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "icnlikesel"
>           [Pseudo Internal, ScalarType "oid", Pseudo Internal,
>            ScalarType "int4"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "icregexeqjoinsel"
>           [Pseudo Internal, ScalarType "oid", Pseudo Internal,
>            ScalarType "int2", Pseudo Internal]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "icregexeqsel"
>           [Pseudo Internal, ScalarType "oid", Pseudo Internal,
>            ScalarType "int4"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "icregexnejoinsel"
>           [Pseudo Internal, ScalarType "oid", Pseudo Internal,
>            ScalarType "int2", Pseudo Internal]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "icregexnesel"
>           [Pseudo Internal, ScalarType "oid", Pseudo Internal,
>            ScalarType "int4"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "inet_client_addr" [] (ScalarType "inet")
>           False,
>         CatCreateFunction FunName "inet_client_port" [] (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "inet_in" [Pseudo Cstring]
>           (ScalarType "inet")
>           False,
>         CatCreateFunction FunName "inet_out" [ScalarType "inet"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "inet_recv" [Pseudo Internal]
>           (ScalarType "inet")
>           False,
>         CatCreateFunction FunName "inet_send" [ScalarType "inet"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "inet_server_addr" [] (ScalarType "inet")
>           False,
>         CatCreateFunction FunName "inet_server_port" [] (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "inetand"
>           [ScalarType "inet", ScalarType "inet"]
>           (ScalarType "inet")
>           False,
>         CatCreateFunction FunName "inetmi"
>           [ScalarType "inet", ScalarType "inet"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "inetmi_int8"
>           [ScalarType "inet", ScalarType "int8"]
>           (ScalarType "inet")
>           False,
>         CatCreateFunction FunName "inetnot" [ScalarType "inet"]
>           (ScalarType "inet")
>           False,
>         CatCreateFunction FunName "inetor"
>           [ScalarType "inet", ScalarType "inet"]
>           (ScalarType "inet")
>           False,
>         CatCreateFunction FunName "inetpl"
>           [ScalarType "inet", ScalarType "int8"]
>           (ScalarType "inet")
>           False,
>         CatCreateFunction FunName "initcap" [ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "int2" [ScalarType "int8"]
>           (ScalarType "int2")
>           False,
>         CatCreateFunction FunName "int2" [ScalarType "int4"]
>           (ScalarType "int2")
>           False,
>         CatCreateFunction FunName "int2" [ScalarType "float4"]
>           (ScalarType "int2")
>           False,
>         CatCreateFunction FunName "int2" [ScalarType "float8"]
>           (ScalarType "int2")
>           False,
>         CatCreateFunction FunName "int2" [ScalarType "numeric"]
>           (ScalarType "int2")
>           False,
>         CatCreateFunction FunName "int24div"
>           [ScalarType "int2", ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "int24eq"
>           [ScalarType "int2", ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int24ge"
>           [ScalarType "int2", ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int24gt"
>           [ScalarType "int2", ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int24le"
>           [ScalarType "int2", ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int24lt"
>           [ScalarType "int2", ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int24mi"
>           [ScalarType "int2", ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "int24mul"
>           [ScalarType "int2", ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "int24ne"
>           [ScalarType "int2", ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int24pl"
>           [ScalarType "int2", ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "int28div"
>           [ScalarType "int2", ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int28eq"
>           [ScalarType "int2", ScalarType "int8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int28ge"
>           [ScalarType "int2", ScalarType "int8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int28gt"
>           [ScalarType "int2", ScalarType "int8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int28le"
>           [ScalarType "int2", ScalarType "int8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int28lt"
>           [ScalarType "int2", ScalarType "int8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int28mi"
>           [ScalarType "int2", ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int28mul"
>           [ScalarType "int2", ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int28ne"
>           [ScalarType "int2", ScalarType "int8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int28pl"
>           [ScalarType "int2", ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int2_accum"
>           [ArrayType (ScalarType "numeric"), ScalarType "int2"]
>           (ArrayType (ScalarType "numeric"))
>           False,
>         CatCreateFunction FunName "int2_avg_accum"
>           [ArrayType (ScalarType "int8"), ScalarType "int2"]
>           (ArrayType (ScalarType "int8"))
>           False,
>         CatCreateFunction FunName "int2_mul_cash"
>           [ScalarType "int2", ScalarType "money"]
>           (ScalarType "money")
>           False,
>         CatCreateFunction FunName "int2_sum"
>           [ScalarType "int8", ScalarType "int2"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int2abs" [ScalarType "int2"]
>           (ScalarType "int2")
>           False,
>         CatCreateFunction FunName "int2and"
>           [ScalarType "int2", ScalarType "int2"]
>           (ScalarType "int2")
>           False,
>         CatCreateFunction FunName "int2div"
>           [ScalarType "int2", ScalarType "int2"]
>           (ScalarType "int2")
>           False,
>         CatCreateFunction FunName "int2eq"
>           [ScalarType "int2", ScalarType "int2"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int2ge"
>           [ScalarType "int2", ScalarType "int2"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int2gt"
>           [ScalarType "int2", ScalarType "int2"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int2in" [Pseudo Cstring]
>           (ScalarType "int2")
>           False,
>         CatCreateFunction FunName "int2larger"
>           [ScalarType "int2", ScalarType "int2"]
>           (ScalarType "int2")
>           False,
>         CatCreateFunction FunName "int2le"
>           [ScalarType "int2", ScalarType "int2"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int2lt"
>           [ScalarType "int2", ScalarType "int2"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int2mi"
>           [ScalarType "int2", ScalarType "int2"]
>           (ScalarType "int2")
>           False,
>         CatCreateFunction FunName "int2mod"
>           [ScalarType "int2", ScalarType "int2"]
>           (ScalarType "int2")
>           False,
>         CatCreateFunction FunName "int2mul"
>           [ScalarType "int2", ScalarType "int2"]
>           (ScalarType "int2")
>           False,
>         CatCreateFunction FunName "int2ne"
>           [ScalarType "int2", ScalarType "int2"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int2not" [ScalarType "int2"]
>           (ScalarType "int2")
>           False,
>         CatCreateFunction FunName "int2or"
>           [ScalarType "int2", ScalarType "int2"]
>           (ScalarType "int2")
>           False,
>         CatCreateFunction FunName "int2out" [ScalarType "int2"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "int2pl"
>           [ScalarType "int2", ScalarType "int2"]
>           (ScalarType "int2")
>           False,
>         CatCreateFunction FunName "int2recv" [Pseudo Internal]
>           (ScalarType "int2")
>           False,
>         CatCreateFunction FunName "int2send" [ScalarType "int2"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "int2shl"
>           [ScalarType "int2", ScalarType "int4"]
>           (ScalarType "int2")
>           False,
>         CatCreateFunction FunName "int2shr"
>           [ScalarType "int2", ScalarType "int4"]
>           (ScalarType "int2")
>           False,
>         CatCreateFunction FunName "int2smaller"
>           [ScalarType "int2", ScalarType "int2"]
>           (ScalarType "int2")
>           False,
>         CatCreateFunction FunName "int2um" [ScalarType "int2"]
>           (ScalarType "int2")
>           False,
>         CatCreateFunction FunName "int2up" [ScalarType "int2"]
>           (ScalarType "int2")
>           False,
>         CatCreateFunction FunName "int2vectoreq"
>           [ScalarType "int2vector", ScalarType "int2vector"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int2vectorin" [Pseudo Cstring]
>           (ScalarType "int2vector")
>           False,
>         CatCreateFunction FunName "int2vectorout" [ScalarType "int2vector"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "int2vectorrecv" [Pseudo Internal]
>           (ScalarType "int2vector")
>           False,
>         CatCreateFunction FunName "int2vectorsend"
>           [ScalarType "int2vector"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "int2xor"
>           [ScalarType "int2", ScalarType "int2"]
>           (ScalarType "int2")
>           False,
>         CatCreateFunction FunName "int4" [ScalarType "bool"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "int4" [ScalarType "char"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "int4" [ScalarType "int8"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "int4" [ScalarType "int2"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "int4" [ScalarType "float4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "int4" [ScalarType "float8"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "int4" [ScalarType "bit"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "int4" [ScalarType "numeric"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "int42div"
>           [ScalarType "int4", ScalarType "int2"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "int42eq"
>           [ScalarType "int4", ScalarType "int2"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int42ge"
>           [ScalarType "int4", ScalarType "int2"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int42gt"
>           [ScalarType "int4", ScalarType "int2"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int42le"
>           [ScalarType "int4", ScalarType "int2"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int42lt"
>           [ScalarType "int4", ScalarType "int2"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int42mi"
>           [ScalarType "int4", ScalarType "int2"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "int42mul"
>           [ScalarType "int4", ScalarType "int2"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "int42ne"
>           [ScalarType "int4", ScalarType "int2"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int42pl"
>           [ScalarType "int4", ScalarType "int2"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "int48div"
>           [ScalarType "int4", ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int48eq"
>           [ScalarType "int4", ScalarType "int8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int48ge"
>           [ScalarType "int4", ScalarType "int8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int48gt"
>           [ScalarType "int4", ScalarType "int8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int48le"
>           [ScalarType "int4", ScalarType "int8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int48lt"
>           [ScalarType "int4", ScalarType "int8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int48mi"
>           [ScalarType "int4", ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int48mul"
>           [ScalarType "int4", ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int48ne"
>           [ScalarType "int4", ScalarType "int8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int48pl"
>           [ScalarType "int4", ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int4_accum"
>           [ArrayType (ScalarType "numeric"), ScalarType "int4"]
>           (ArrayType (ScalarType "numeric"))
>           False,
>         CatCreateFunction FunName "int4_avg_accum"
>           [ArrayType (ScalarType "int8"), ScalarType "int4"]
>           (ArrayType (ScalarType "int8"))
>           False,
>         CatCreateFunction FunName "int4_mul_cash"
>           [ScalarType "int4", ScalarType "money"]
>           (ScalarType "money")
>           False,
>         CatCreateFunction FunName "int4_sum"
>           [ScalarType "int8", ScalarType "int4"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int4abs" [ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "int4and"
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "int4div"
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "int4eq"
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int4ge"
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int4gt"
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int4in" [Pseudo Cstring]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "int4inc" [ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "int4larger"
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "int4le"
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int4lt"
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int4mi"
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "int4mod"
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "int4mul"
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "int4ne"
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int4not" [ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "int4or"
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "int4out" [ScalarType "int4"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "int4pl"
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "int4recv" [Pseudo Internal]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "int4send" [ScalarType "int4"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "int4shl"
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "int4shr"
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "int4smaller"
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "int4um" [ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "int4up" [ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "int4xor"
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "int8" [ScalarType "int2"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int8" [ScalarType "int4"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int8" [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int8" [ScalarType "float4"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int8" [ScalarType "float8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int8" [ScalarType "bit"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int8" [ScalarType "numeric"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int82div"
>           [ScalarType "int8", ScalarType "int2"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int82eq"
>           [ScalarType "int8", ScalarType "int2"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int82ge"
>           [ScalarType "int8", ScalarType "int2"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int82gt"
>           [ScalarType "int8", ScalarType "int2"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int82le"
>           [ScalarType "int8", ScalarType "int2"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int82lt"
>           [ScalarType "int8", ScalarType "int2"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int82mi"
>           [ScalarType "int8", ScalarType "int2"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int82mul"
>           [ScalarType "int8", ScalarType "int2"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int82ne"
>           [ScalarType "int8", ScalarType "int2"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int82pl"
>           [ScalarType "int8", ScalarType "int2"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int84div"
>           [ScalarType "int8", ScalarType "int4"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int84eq"
>           [ScalarType "int8", ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int84ge"
>           [ScalarType "int8", ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int84gt"
>           [ScalarType "int8", ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int84le"
>           [ScalarType "int8", ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int84lt"
>           [ScalarType "int8", ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int84mi"
>           [ScalarType "int8", ScalarType "int4"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int84mul"
>           [ScalarType "int8", ScalarType "int4"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int84ne"
>           [ScalarType "int8", ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int84pl"
>           [ScalarType "int8", ScalarType "int4"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int8_accum"
>           [ArrayType (ScalarType "numeric"), ScalarType "int8"]
>           (ArrayType (ScalarType "numeric"))
>           False,
>         CatCreateFunction FunName "int8_avg"
>           [ArrayType (ScalarType "int8")]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "int8_avg_accum"
>           [ArrayType (ScalarType "numeric"), ScalarType "int8"]
>           (ArrayType (ScalarType "numeric"))
>           False,
>         CatCreateFunction FunName "int8_sum"
>           [ScalarType "numeric", ScalarType "int8"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "int8abs" [ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int8and"
>           [ScalarType "int8", ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int8div"
>           [ScalarType "int8", ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int8eq"
>           [ScalarType "int8", ScalarType "int8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int8ge"
>           [ScalarType "int8", ScalarType "int8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int8gt"
>           [ScalarType "int8", ScalarType "int8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int8in" [Pseudo Cstring]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int8inc" [ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int8inc_any"
>           [ScalarType "int8", Pseudo Any]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int8inc_float8_float8"
>           [ScalarType "int8", ScalarType "float8", ScalarType "float8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int8larger"
>           [ScalarType "int8", ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int8le"
>           [ScalarType "int8", ScalarType "int8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int8lt"
>           [ScalarType "int8", ScalarType "int8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int8mi"
>           [ScalarType "int8", ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int8mod"
>           [ScalarType "int8", ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int8mul"
>           [ScalarType "int8", ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int8ne"
>           [ScalarType "int8", ScalarType "int8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "int8not" [ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int8or"
>           [ScalarType "int8", ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int8out" [ScalarType "int8"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "int8pl"
>           [ScalarType "int8", ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int8pl_inet"
>           [ScalarType "int8", ScalarType "inet"]
>           (ScalarType "inet")
>           False,
>         CatCreateFunction FunName "int8recv" [Pseudo Internal]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int8send" [ScalarType "int8"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "int8shl"
>           [ScalarType "int8", ScalarType "int4"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int8shr"
>           [ScalarType "int8", ScalarType "int4"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int8smaller"
>           [ScalarType "int8", ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int8um" [ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int8up" [ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "int8xor"
>           [ScalarType "int8", ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "integer_pl_date"
>           [ScalarType "int4", ScalarType "date"]
>           (ScalarType "date")
>           False,
>         CatCreateFunction FunName "inter_lb"
>           [ScalarType "line", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "inter_sb"
>           [ScalarType "lseg", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "inter_sl"
>           [ScalarType "lseg", ScalarType "line"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "internal_in" [Pseudo Cstring]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "internal_out" [Pseudo Internal]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "interval" [ScalarType "reltime"]
>           (ScalarType "interval")
>           False,
>         CatCreateFunction FunName "interval" [ScalarType "time"]
>           (ScalarType "interval")
>           False,
>         CatCreateFunction FunName "interval"
>           [ScalarType "interval", ScalarType "int4"]
>           (ScalarType "interval")
>           False,
>         CatCreateFunction FunName "interval_accum"
>           [ArrayType (ScalarType "interval"), ScalarType "interval"]
>           (ArrayType (ScalarType "interval"))
>           False,
>         CatCreateFunction FunName "interval_avg"
>           [ArrayType (ScalarType "interval")]
>           (ScalarType "interval")
>           False,
>         CatCreateFunction FunName "interval_cmp"
>           [ScalarType "interval", ScalarType "interval"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "interval_div"
>           [ScalarType "interval", ScalarType "float8"]
>           (ScalarType "interval")
>           False,
>         CatCreateFunction FunName "interval_eq"
>           [ScalarType "interval", ScalarType "interval"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "interval_ge"
>           [ScalarType "interval", ScalarType "interval"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "interval_gt"
>           [ScalarType "interval", ScalarType "interval"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "interval_hash" [ScalarType "interval"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "interval_in"
>           [Pseudo Cstring, ScalarType "oid", ScalarType "int4"]
>           (ScalarType "interval")
>           False,
>         CatCreateFunction FunName "interval_larger"
>           [ScalarType "interval", ScalarType "interval"]
>           (ScalarType "interval")
>           False,
>         CatCreateFunction FunName "interval_le"
>           [ScalarType "interval", ScalarType "interval"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "interval_lt"
>           [ScalarType "interval", ScalarType "interval"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "interval_mi"
>           [ScalarType "interval", ScalarType "interval"]
>           (ScalarType "interval")
>           False,
>         CatCreateFunction FunName "interval_mul"
>           [ScalarType "interval", ScalarType "float8"]
>           (ScalarType "interval")
>           False,
>         CatCreateFunction FunName "interval_ne"
>           [ScalarType "interval", ScalarType "interval"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "interval_out" [ScalarType "interval"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "interval_pl"
>           [ScalarType "interval", ScalarType "interval"]
>           (ScalarType "interval")
>           False,
>         CatCreateFunction FunName "interval_pl_date"
>           [ScalarType "interval", ScalarType "date"]
>           (ScalarType "timestamp")
>           False,
>         CatCreateFunction FunName "interval_pl_time"
>           [ScalarType "interval", ScalarType "time"]
>           (ScalarType "time")
>           False,
>         CatCreateFunction FunName "interval_pl_timestamp"
>           [ScalarType "interval", ScalarType "timestamp"]
>           (ScalarType "timestamp")
>           False,
>         CatCreateFunction FunName "interval_pl_timestamptz"
>           [ScalarType "interval", ScalarType "timestamptz"]
>           (ScalarType "timestamptz")
>           False,
>         CatCreateFunction FunName "interval_pl_timetz"
>           [ScalarType "interval", ScalarType "timetz"]
>           (ScalarType "timetz")
>           False,
>         CatCreateFunction FunName "interval_recv"
>           [Pseudo Internal, ScalarType "oid", ScalarType "int4"]
>           (ScalarType "interval")
>           False,
>         CatCreateFunction FunName "interval_send" [ScalarType "interval"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "interval_smaller"
>           [ScalarType "interval", ScalarType "interval"]
>           (ScalarType "interval")
>           False,
>         CatCreateFunction FunName "interval_um" [ScalarType "interval"]
>           (ScalarType "interval")
>           False,
>         CatCreateFunction FunName "intervaltypmodin"
>           [ArrayType (Pseudo Cstring)]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "intervaltypmodout" [ScalarType "int4"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "intinterval"
>           [ScalarType "abstime", ScalarType "tinterval"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "isclosed" [ScalarType "path"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "isfinite" [ScalarType "abstime"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "isfinite" [ScalarType "date"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "isfinite" [ScalarType "timestamp"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "isfinite" [ScalarType "timestamptz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "isfinite" [ScalarType "interval"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "ishorizontal" [ScalarType "lseg"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "ishorizontal" [ScalarType "line"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "ishorizontal"
>           [ScalarType "point", ScalarType "point"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "iso8859_1_to_utf8"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "iso8859_to_utf8"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "iso_to_koi8r"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "iso_to_mic"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "iso_to_win1251"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "iso_to_win866"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "isopen" [ScalarType "path"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "isparallel"
>           [ScalarType "lseg", ScalarType "lseg"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "isparallel"
>           [ScalarType "line", ScalarType "line"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "isperp"
>           [ScalarType "lseg", ScalarType "lseg"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "isperp"
>           [ScalarType "line", ScalarType "line"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "isvertical" [ScalarType "lseg"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "isvertical" [ScalarType "line"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "isvertical"
>           [ScalarType "point", ScalarType "point"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "johab_to_utf8"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "justify_days" [ScalarType "interval"]
>           (ScalarType "interval")
>           False,
>         CatCreateFunction FunName "justify_hours" [ScalarType "interval"]
>           (ScalarType "interval")
>           False,
>         CatCreateFunction FunName "justify_interval"
>           [ScalarType "interval"]
>           (ScalarType "interval")
>           False,
>         CatCreateFunction FunName "koi8r_to_iso"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "koi8r_to_mic"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "koi8r_to_utf8"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "koi8r_to_win1251"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "koi8r_to_win866"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "koi8u_to_utf8"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "language_handler_in" [Pseudo Cstring]
>           (Pseudo LanguageHandler)
>           False,
>         CatCreateFunction FunName "language_handler_out"
>           [Pseudo LanguageHandler]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "lastval" [] (ScalarType "int8") False,
>         CatCreateFunction FunName "latin1_to_mic"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "latin2_to_mic"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "latin2_to_win1250"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "latin3_to_mic"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "latin4_to_mic"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "left"
>           [ScalarType "text", ScalarType "int4"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "length" [ScalarType "bytea"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "length" [ScalarType "text"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "length" [ScalarType "lseg"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "length" [ScalarType "path"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "length" [ScalarType "bpchar"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "length" [ScalarType "bit"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "length" [ScalarType "tsvector"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "length"
>           [ScalarType "bytea", ScalarType "name"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "like"
>           [ScalarType "bytea", ScalarType "bytea"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "like"
>           [ScalarType "name", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "like"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "like_escape"
>           [ScalarType "bytea", ScalarType "bytea"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "like_escape"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "likejoinsel"
>           [Pseudo Internal, ScalarType "oid", Pseudo Internal,
>            ScalarType "int2", Pseudo Internal]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "likesel"
>           [Pseudo Internal, ScalarType "oid", Pseudo Internal,
>            ScalarType "int4"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "line"
>           [ScalarType "point", ScalarType "point"]
>           (ScalarType "line")
>           False,
>         CatCreateFunction FunName "line_distance"
>           [ScalarType "line", ScalarType "line"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "line_eq"
>           [ScalarType "line", ScalarType "line"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "line_horizontal" [ScalarType "line"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "line_in" [Pseudo Cstring]
>           (ScalarType "line")
>           False,
>         CatCreateFunction FunName "line_interpt"
>           [ScalarType "line", ScalarType "line"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunName "line_intersect"
>           [ScalarType "line", ScalarType "line"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "line_out" [ScalarType "line"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "line_parallel"
>           [ScalarType "line", ScalarType "line"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "line_perp"
>           [ScalarType "line", ScalarType "line"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "line_recv" [Pseudo Internal]
>           (ScalarType "line")
>           False,
>         CatCreateFunction FunName "line_send" [ScalarType "line"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "line_vertical" [ScalarType "line"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "ln" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "ln" [ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "lo_close" [ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "lo_creat" [ScalarType "int4"]
>           (ScalarType "oid")
>           False,
>         CatCreateFunction FunName "lo_create" [ScalarType "oid"]
>           (ScalarType "oid")
>           False,
>         CatCreateFunction FunName "lo_export"
>           [ScalarType "oid", ScalarType "text"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "lo_import" [ScalarType "text"]
>           (ScalarType "oid")
>           False,
>         CatCreateFunction FunName "lo_import"
>           [ScalarType "text", ScalarType "oid"]
>           (ScalarType "oid")
>           False,
>         CatCreateFunction FunName "lo_lseek"
>           [ScalarType "int4", ScalarType "int4", ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "lo_open"
>           [ScalarType "oid", ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "lo_tell" [ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "lo_truncate"
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "lo_unlink" [ScalarType "oid"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "log" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "log" [ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "log"
>           [ScalarType "numeric", ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "loread"
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "lower" [ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "lowrite"
>           [ScalarType "int4", ScalarType "bytea"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "lpad"
>           [ScalarType "text", ScalarType "int4"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "lpad"
>           [ScalarType "text", ScalarType "int4", ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "lseg" [ScalarType "box"]
>           (ScalarType "lseg")
>           False,
>         CatCreateFunction FunName "lseg"
>           [ScalarType "point", ScalarType "point"]
>           (ScalarType "lseg")
>           False,
>         CatCreateFunction FunName "lseg_center" [ScalarType "lseg"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunName "lseg_distance"
>           [ScalarType "lseg", ScalarType "lseg"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "lseg_eq"
>           [ScalarType "lseg", ScalarType "lseg"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "lseg_ge"
>           [ScalarType "lseg", ScalarType "lseg"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "lseg_gt"
>           [ScalarType "lseg", ScalarType "lseg"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "lseg_horizontal" [ScalarType "lseg"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "lseg_in" [Pseudo Cstring]
>           (ScalarType "lseg")
>           False,
>         CatCreateFunction FunName "lseg_interpt"
>           [ScalarType "lseg", ScalarType "lseg"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunName "lseg_intersect"
>           [ScalarType "lseg", ScalarType "lseg"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "lseg_le"
>           [ScalarType "lseg", ScalarType "lseg"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "lseg_length" [ScalarType "lseg"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "lseg_lt"
>           [ScalarType "lseg", ScalarType "lseg"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "lseg_ne"
>           [ScalarType "lseg", ScalarType "lseg"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "lseg_out" [ScalarType "lseg"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "lseg_parallel"
>           [ScalarType "lseg", ScalarType "lseg"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "lseg_perp"
>           [ScalarType "lseg", ScalarType "lseg"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "lseg_recv" [Pseudo Internal]
>           (ScalarType "lseg")
>           False,
>         CatCreateFunction FunName "lseg_send" [ScalarType "lseg"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "lseg_vertical" [ScalarType "lseg"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "ltrim" [ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "ltrim"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "macaddr_cmp"
>           [ScalarType "macaddr", ScalarType "macaddr"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "macaddr_eq"
>           [ScalarType "macaddr", ScalarType "macaddr"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "macaddr_ge"
>           [ScalarType "macaddr", ScalarType "macaddr"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "macaddr_gt"
>           [ScalarType "macaddr", ScalarType "macaddr"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "macaddr_in" [Pseudo Cstring]
>           (ScalarType "macaddr")
>           False,
>         CatCreateFunction FunName "macaddr_le"
>           [ScalarType "macaddr", ScalarType "macaddr"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "macaddr_lt"
>           [ScalarType "macaddr", ScalarType "macaddr"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "macaddr_ne"
>           [ScalarType "macaddr", ScalarType "macaddr"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "macaddr_out" [ScalarType "macaddr"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "macaddr_recv" [Pseudo Internal]
>           (ScalarType "macaddr")
>           False,
>         CatCreateFunction FunName "macaddr_send" [ScalarType "macaddr"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "makeaclitem"
>           [ScalarType "oid", ScalarType "oid", ScalarType "text",
>            ScalarType "bool"]
>           (ScalarType "aclitem")
>           False,
>         CatCreateFunction FunName "masklen" [ScalarType "inet"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "md5" [ScalarType "bytea"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "md5" [ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "mic_to_ascii"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "mic_to_big5"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "mic_to_euc_cn"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "mic_to_euc_jp"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "mic_to_euc_kr"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "mic_to_euc_tw"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "mic_to_iso"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "mic_to_koi8r"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "mic_to_latin1"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "mic_to_latin2"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "mic_to_latin3"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "mic_to_latin4"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "mic_to_sjis"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "mic_to_win1250"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "mic_to_win1251"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "mic_to_win866"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "mktinterval"
>           [ScalarType "abstime", ScalarType "abstime"]
>           (ScalarType "tinterval")
>           False,
>         CatCreateFunction FunName "mod"
>           [ScalarType "int8", ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "mod"
>           [ScalarType "int2", ScalarType "int2"]
>           (ScalarType "int2")
>           False,
>         CatCreateFunction FunName "mod"
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "mod"
>           [ScalarType "numeric", ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "money" [ScalarType "int8"]
>           (ScalarType "money")
>           False,
>         CatCreateFunction FunName "money" [ScalarType "int4"]
>           (ScalarType "money")
>           False,
>         CatCreateFunction FunName "money" [ScalarType "numeric"]
>           (ScalarType "money")
>           False,
>         CatCreateFunction FunName "mul_d_interval"
>           [ScalarType "float8", ScalarType "interval"]
>           (ScalarType "interval")
>           False,
>         CatCreateFunction FunName "name" [ScalarType "text"]
>           (ScalarType "name")
>           False,
>         CatCreateFunction FunName "name" [ScalarType "bpchar"]
>           (ScalarType "name")
>           False,
>         CatCreateFunction FunName "name" [ScalarType "varchar"]
>           (ScalarType "name")
>           False,
>         CatCreateFunction FunName "nameeq"
>           [ScalarType "name", ScalarType "name"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "namege"
>           [ScalarType "name", ScalarType "name"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "namegt"
>           [ScalarType "name", ScalarType "name"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "nameiclike"
>           [ScalarType "name", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "nameicnlike"
>           [ScalarType "name", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "nameicregexeq"
>           [ScalarType "name", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "nameicregexne"
>           [ScalarType "name", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "namein" [Pseudo Cstring]
>           (ScalarType "name")
>           False,
>         CatCreateFunction FunName "namele"
>           [ScalarType "name", ScalarType "name"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "namelike"
>           [ScalarType "name", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "namelt"
>           [ScalarType "name", ScalarType "name"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "namene"
>           [ScalarType "name", ScalarType "name"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "namenlike"
>           [ScalarType "name", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "nameout" [ScalarType "name"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "namerecv" [Pseudo Internal]
>           (ScalarType "name")
>           False,
>         CatCreateFunction FunName "nameregexeq"
>           [ScalarType "name", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "nameregexne"
>           [ScalarType "name", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "namesend" [ScalarType "name"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "neqjoinsel"
>           [Pseudo Internal, ScalarType "oid", Pseudo Internal,
>            ScalarType "int2", Pseudo Internal]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "neqsel"
>           [Pseudo Internal, ScalarType "oid", Pseudo Internal,
>            ScalarType "int4"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "netmask" [ScalarType "inet"]
>           (ScalarType "inet")
>           False,
>         CatCreateFunction FunName "network" [ScalarType "inet"]
>           (ScalarType "cidr")
>           False,
>         CatCreateFunction FunName "network_cmp"
>           [ScalarType "inet", ScalarType "inet"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "network_eq"
>           [ScalarType "inet", ScalarType "inet"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "network_ge"
>           [ScalarType "inet", ScalarType "inet"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "network_gt"
>           [ScalarType "inet", ScalarType "inet"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "network_le"
>           [ScalarType "inet", ScalarType "inet"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "network_lt"
>           [ScalarType "inet", ScalarType "inet"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "network_ne"
>           [ScalarType "inet", ScalarType "inet"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "network_sub"
>           [ScalarType "inet", ScalarType "inet"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "network_subeq"
>           [ScalarType "inet", ScalarType "inet"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "network_sup"
>           [ScalarType "inet", ScalarType "inet"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "network_supeq"
>           [ScalarType "inet", ScalarType "inet"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "nextval" [ScalarType "regclass"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "nlikejoinsel"
>           [Pseudo Internal, ScalarType "oid", Pseudo Internal,
>            ScalarType "int2", Pseudo Internal]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "nlikesel"
>           [Pseudo Internal, ScalarType "oid", Pseudo Internal,
>            ScalarType "int4"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "notlike"
>           [ScalarType "bytea", ScalarType "bytea"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "notlike"
>           [ScalarType "name", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "notlike"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "now" [] (ScalarType "timestamptz")
>           False,
>         CatCreateFunction FunName "npoints" [ScalarType "path"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "npoints" [ScalarType "polygon"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "numeric" [ScalarType "int8"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "numeric" [ScalarType "int2"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "numeric" [ScalarType "int4"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "numeric" [ScalarType "float4"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "numeric" [ScalarType "float8"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "numeric" [ScalarType "money"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "numeric"
>           [ScalarType "numeric", ScalarType "int4"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "numeric_abs" [ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "numeric_accum"
>           [ArrayType (ScalarType "numeric"), ScalarType "numeric"]
>           (ArrayType (ScalarType "numeric"))
>           False,
>         CatCreateFunction FunName "numeric_add"
>           [ScalarType "numeric", ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "numeric_avg"
>           [ArrayType (ScalarType "numeric")]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "numeric_avg_accum"
>           [ArrayType (ScalarType "numeric"), ScalarType "numeric"]
>           (ArrayType (ScalarType "numeric"))
>           False,
>         CatCreateFunction FunName "numeric_cmp"
>           [ScalarType "numeric", ScalarType "numeric"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "numeric_div"
>           [ScalarType "numeric", ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "numeric_div_trunc"
>           [ScalarType "numeric", ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "numeric_eq"
>           [ScalarType "numeric", ScalarType "numeric"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "numeric_exp" [ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "numeric_fac" [ScalarType "int8"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "numeric_ge"
>           [ScalarType "numeric", ScalarType "numeric"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "numeric_gt"
>           [ScalarType "numeric", ScalarType "numeric"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "numeric_in"
>           [Pseudo Cstring, ScalarType "oid", ScalarType "int4"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "numeric_inc" [ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "numeric_larger"
>           [ScalarType "numeric", ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "numeric_le"
>           [ScalarType "numeric", ScalarType "numeric"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "numeric_ln" [ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "numeric_log"
>           [ScalarType "numeric", ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "numeric_lt"
>           [ScalarType "numeric", ScalarType "numeric"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "numeric_mod"
>           [ScalarType "numeric", ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "numeric_mul"
>           [ScalarType "numeric", ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "numeric_ne"
>           [ScalarType "numeric", ScalarType "numeric"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "numeric_out" [ScalarType "numeric"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "numeric_power"
>           [ScalarType "numeric", ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "numeric_recv"
>           [Pseudo Internal, ScalarType "oid", ScalarType "int4"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "numeric_send" [ScalarType "numeric"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "numeric_smaller"
>           [ScalarType "numeric", ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "numeric_sqrt" [ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "numeric_stddev_pop"
>           [ArrayType (ScalarType "numeric")]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "numeric_stddev_samp"
>           [ArrayType (ScalarType "numeric")]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "numeric_sub"
>           [ScalarType "numeric", ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "numeric_uminus" [ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "numeric_uplus" [ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "numeric_var_pop"
>           [ArrayType (ScalarType "numeric")]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "numeric_var_samp"
>           [ArrayType (ScalarType "numeric")]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "numerictypmodin"
>           [ArrayType (Pseudo Cstring)]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "numerictypmodout" [ScalarType "int4"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "numnode" [ScalarType "tsquery"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "obj_description" [ScalarType "oid"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "obj_description"
>           [ScalarType "oid", ScalarType "name"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "octet_length" [ScalarType "bytea"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "octet_length" [ScalarType "text"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "octet_length" [ScalarType "bpchar"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "octet_length" [ScalarType "bit"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "oid" [ScalarType "int8"]
>           (ScalarType "oid")
>           False,
>         CatCreateFunction FunName "oideq"
>           [ScalarType "oid", ScalarType "oid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "oidge"
>           [ScalarType "oid", ScalarType "oid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "oidgt"
>           [ScalarType "oid", ScalarType "oid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "oidin" [Pseudo Cstring]
>           (ScalarType "oid")
>           False,
>         CatCreateFunction FunName "oidlarger"
>           [ScalarType "oid", ScalarType "oid"]
>           (ScalarType "oid")
>           False,
>         CatCreateFunction FunName "oidle"
>           [ScalarType "oid", ScalarType "oid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "oidlt"
>           [ScalarType "oid", ScalarType "oid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "oidne"
>           [ScalarType "oid", ScalarType "oid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "oidout" [ScalarType "oid"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "oidrecv" [Pseudo Internal]
>           (ScalarType "oid")
>           False,
>         CatCreateFunction FunName "oidsend" [ScalarType "oid"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "oidsmaller"
>           [ScalarType "oid", ScalarType "oid"]
>           (ScalarType "oid")
>           False,
>         CatCreateFunction FunName "oidvectoreq"
>           [ScalarType "oidvector", ScalarType "oidvector"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "oidvectorge"
>           [ScalarType "oidvector", ScalarType "oidvector"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "oidvectorgt"
>           [ScalarType "oidvector", ScalarType "oidvector"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "oidvectorin" [Pseudo Cstring]
>           (ScalarType "oidvector")
>           False,
>         CatCreateFunction FunName "oidvectorle"
>           [ScalarType "oidvector", ScalarType "oidvector"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "oidvectorlt"
>           [ScalarType "oidvector", ScalarType "oidvector"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "oidvectorne"
>           [ScalarType "oidvector", ScalarType "oidvector"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "oidvectorout" [ScalarType "oidvector"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "oidvectorrecv" [Pseudo Internal]
>           (ScalarType "oidvector")
>           False,
>         CatCreateFunction FunName "oidvectorsend" [ScalarType "oidvector"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "oidvectortypes" [ScalarType "oidvector"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "on_pb"
>           [ScalarType "point", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "on_pl"
>           [ScalarType "point", ScalarType "line"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "on_ppath"
>           [ScalarType "point", ScalarType "path"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "on_ps"
>           [ScalarType "point", ScalarType "lseg"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "on_sb"
>           [ScalarType "lseg", ScalarType "box"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "on_sl"
>           [ScalarType "lseg", ScalarType "line"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "opaque_in" [Pseudo Cstring]
>           (Pseudo Opaque)
>           False,
>         CatCreateFunction FunName "opaque_out" [Pseudo Opaque]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "overlaps"
>           [ScalarType "time", ScalarType "time", ScalarType "time",
>            ScalarType "time"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "overlaps"
>           [ScalarType "time", ScalarType "time", ScalarType "time",
>            ScalarType "interval"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "overlaps"
>           [ScalarType "time", ScalarType "interval", ScalarType "time",
>            ScalarType "time"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "overlaps"
>           [ScalarType "time", ScalarType "interval", ScalarType "time",
>            ScalarType "interval"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "overlaps"
>           [ScalarType "timestamp", ScalarType "timestamp",
>            ScalarType "timestamp", ScalarType "timestamp"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "overlaps"
>           [ScalarType "timestamp", ScalarType "timestamp",
>            ScalarType "timestamp", ScalarType "interval"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "overlaps"
>           [ScalarType "timestamp", ScalarType "interval",
>            ScalarType "timestamp", ScalarType "timestamp"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "overlaps"
>           [ScalarType "timestamp", ScalarType "interval",
>            ScalarType "timestamp", ScalarType "interval"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "overlaps"
>           [ScalarType "timestamptz", ScalarType "timestamptz",
>            ScalarType "timestamptz", ScalarType "timestamptz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "overlaps"
>           [ScalarType "timestamptz", ScalarType "timestamptz",
>            ScalarType "timestamptz", ScalarType "interval"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "overlaps"
>           [ScalarType "timestamptz", ScalarType "interval",
>            ScalarType "timestamptz", ScalarType "timestamptz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "overlaps"
>           [ScalarType "timestamptz", ScalarType "interval",
>            ScalarType "timestamptz", ScalarType "interval"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "overlaps"
>           [ScalarType "timetz", ScalarType "timetz", ScalarType "timetz",
>            ScalarType "timetz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "overlay"
>           [ScalarType "bytea", ScalarType "bytea", ScalarType "int4"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "overlay"
>           [ScalarType "text", ScalarType "text", ScalarType "int4"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "overlay"
>           [ScalarType "bit", ScalarType "bit", ScalarType "int4"]
>           (ScalarType "bit")
>           False,
>         CatCreateFunction FunName "overlay"
>           [ScalarType "bytea", ScalarType "bytea", ScalarType "int4",
>            ScalarType "int4"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "overlay"
>           [ScalarType "text", ScalarType "text", ScalarType "int4",
>            ScalarType "int4"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "overlay"
>           [ScalarType "bit", ScalarType "bit", ScalarType "int4",
>            ScalarType "int4"]
>           (ScalarType "bit")
>           False,
>         CatCreateFunction FunName "path" [ScalarType "polygon"]
>           (ScalarType "path")
>           False,
>         CatCreateFunction FunName "path_add"
>           [ScalarType "path", ScalarType "path"]
>           (ScalarType "path")
>           False,
>         CatCreateFunction FunName "path_add_pt"
>           [ScalarType "path", ScalarType "point"]
>           (ScalarType "path")
>           False,
>         CatCreateFunction FunName "path_center" [ScalarType "path"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunName "path_contain_pt"
>           [ScalarType "path", ScalarType "point"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "path_distance"
>           [ScalarType "path", ScalarType "path"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "path_div_pt"
>           [ScalarType "path", ScalarType "point"]
>           (ScalarType "path")
>           False,
>         CatCreateFunction FunName "path_in" [Pseudo Cstring]
>           (ScalarType "path")
>           False,
>         CatCreateFunction FunName "path_inter"
>           [ScalarType "path", ScalarType "path"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "path_length" [ScalarType "path"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "path_mul_pt"
>           [ScalarType "path", ScalarType "point"]
>           (ScalarType "path")
>           False,
>         CatCreateFunction FunName "path_n_eq"
>           [ScalarType "path", ScalarType "path"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "path_n_ge"
>           [ScalarType "path", ScalarType "path"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "path_n_gt"
>           [ScalarType "path", ScalarType "path"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "path_n_le"
>           [ScalarType "path", ScalarType "path"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "path_n_lt"
>           [ScalarType "path", ScalarType "path"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "path_npoints" [ScalarType "path"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "path_out" [ScalarType "path"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "path_recv" [Pseudo Internal]
>           (ScalarType "path")
>           False,
>         CatCreateFunction FunName "path_send" [ScalarType "path"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "path_sub_pt"
>           [ScalarType "path", ScalarType "point"]
>           (ScalarType "path")
>           False,
>         CatCreateFunction FunName "pclose" [ScalarType "path"]
>           (ScalarType "path")
>           False,
>         CatCreateFunction FunName "pg_advisory_lock" [ScalarType "int8"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "pg_advisory_lock"
>           [ScalarType "int4", ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "pg_advisory_lock_shared"
>           [ScalarType "int8"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "pg_advisory_lock_shared"
>           [ScalarType "int4", ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "pg_advisory_unlock" [ScalarType "int8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "pg_advisory_unlock"
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "pg_advisory_unlock_all" [] (Pseudo Void)
>           False,
>         CatCreateFunction FunName "pg_advisory_unlock_shared"
>           [ScalarType "int8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "pg_advisory_unlock_shared"
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "pg_advisory_xact_lock"
>           [ScalarType "int8"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "pg_advisory_xact_lock"
>           [ScalarType "int4", ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "pg_advisory_xact_lock_shared"
>           [ScalarType "int8"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "pg_advisory_xact_lock_shared"
>           [ScalarType "int4", ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "pg_available_extension_versions" []
>           (SetOfType (Pseudo Record))
>           False,
>         CatCreateFunction FunName "pg_available_extensions" []
>           (SetOfType (Pseudo Record))
>           False,
>         CatCreateFunction FunName "pg_backend_pid" [] (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "pg_cancel_backend" [ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "pg_char_to_encoding" [ScalarType "name"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "pg_client_encoding" []
>           (ScalarType "name")
>           False,
>         CatCreateFunction FunName "pg_collation_is_visible"
>           [ScalarType "oid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "pg_column_size" [Pseudo Any]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "pg_conf_load_time" []
>           (ScalarType "timestamptz")
>           False,
>         CatCreateFunction FunName "pg_conversion_is_visible"
>           [ScalarType "oid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "pg_create_restore_point"
>           [ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "pg_current_xlog_insert_location" []
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "pg_current_xlog_location" []
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "pg_cursor" []
>           (SetOfType (Pseudo Record))
>           False,
>         CatCreateFunction FunName "pg_database_size" [ScalarType "name"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_database_size" [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_describe_object"
>           [ScalarType "oid", ScalarType "oid", ScalarType "int4"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "pg_encoding_max_length"
>           [ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "pg_encoding_to_char" [ScalarType "int4"]
>           (ScalarType "name")
>           False,
>         CatCreateFunction FunName "pg_extension_config_dump"
>           [ScalarType "regclass", ScalarType "text"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "pg_extension_update_paths"
>           [ScalarType "name"]
>           (SetOfType (Pseudo Record))
>           False,
>         CatCreateFunction FunName "pg_function_is_visible"
>           [ScalarType "oid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "pg_get_constraintdef" [ScalarType "oid"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "pg_get_constraintdef"
>           [ScalarType "oid", ScalarType "bool"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "pg_get_expr"
>           [ScalarType "pg_node_tree", ScalarType "oid"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "pg_get_expr"
>           [ScalarType "pg_node_tree", ScalarType "oid", ScalarType "bool"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "pg_get_function_arguments"
>           [ScalarType "oid"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "pg_get_function_identity_arguments"
>           [ScalarType "oid"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "pg_get_function_result"
>           [ScalarType "oid"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "pg_get_functiondef" [ScalarType "oid"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "pg_get_indexdef" [ScalarType "oid"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "pg_get_indexdef"
>           [ScalarType "oid", ScalarType "int4", ScalarType "bool"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "pg_get_keywords" []
>           (SetOfType (Pseudo Record))
>           False,
>         CatCreateFunction FunName "pg_get_ruledef" [ScalarType "oid"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "pg_get_ruledef"
>           [ScalarType "oid", ScalarType "bool"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "pg_get_serial_sequence"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "pg_get_triggerdef" [ScalarType "oid"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "pg_get_triggerdef"
>           [ScalarType "oid", ScalarType "bool"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "pg_get_userbyid" [ScalarType "oid"]
>           (ScalarType "name")
>           False,
>         CatCreateFunction FunName "pg_get_viewdef" [ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "pg_get_viewdef" [ScalarType "oid"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "pg_get_viewdef"
>           [ScalarType "text", ScalarType "bool"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "pg_get_viewdef"
>           [ScalarType "oid", ScalarType "bool"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "pg_has_role"
>           [ScalarType "name", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "pg_has_role"
>           [ScalarType "oid", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "pg_has_role"
>           [ScalarType "name", ScalarType "name", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "pg_has_role"
>           [ScalarType "name", ScalarType "oid", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "pg_has_role"
>           [ScalarType "oid", ScalarType "name", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "pg_has_role"
>           [ScalarType "oid", ScalarType "oid", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "pg_indexes_size" [ScalarType "regclass"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_is_in_recovery" []
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "pg_is_other_temp_schema"
>           [ScalarType "oid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "pg_is_xlog_replay_paused" []
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "pg_last_xact_replay_timestamp" []
>           (ScalarType "timestamptz")
>           False,
>         CatCreateFunction FunName "pg_last_xlog_receive_location" []
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "pg_last_xlog_replay_location" []
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "pg_listening_channels" []
>           (SetOfType (ScalarType "text"))
>           False,
>         CatCreateFunction FunName "pg_lock_status" []
>           (SetOfType (Pseudo Record))
>           False,
>         CatCreateFunction FunName "pg_ls_dir" [ScalarType "text"]
>           (SetOfType (ScalarType "text"))
>           False,
>         CatCreateFunction FunName "pg_my_temp_schema" [] (ScalarType "oid")
>           False,
>         CatCreateFunction FunName "pg_node_tree_in" [Pseudo Cstring]
>           (ScalarType "pg_node_tree")
>           False,
>         CatCreateFunction FunName "pg_node_tree_out"
>           [ScalarType "pg_node_tree"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "pg_node_tree_recv" [Pseudo Internal]
>           (ScalarType "pg_node_tree")
>           False,
>         CatCreateFunction FunName "pg_node_tree_send"
>           [ScalarType "pg_node_tree"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "pg_notify"
>           [ScalarType "text", ScalarType "text"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "pg_opclass_is_visible"
>           [ScalarType "oid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "pg_operator_is_visible"
>           [ScalarType "oid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "pg_options_to_table"
>           [ArrayType (ScalarType "text")]
>           (SetOfType (Pseudo Record))
>           False,
>         CatCreateFunction FunName "pg_postmaster_start_time" []
>           (ScalarType "timestamptz")
>           False,
>         CatCreateFunction FunName "pg_prepared_statement" []
>           (SetOfType (Pseudo Record))
>           False,
>         CatCreateFunction FunName "pg_prepared_xact" []
>           (SetOfType (Pseudo Record))
>           False,
>         CatCreateFunction FunName "pg_read_binary_file" [ScalarType "text"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "pg_read_binary_file"
>           [ScalarType "text", ScalarType "int8", ScalarType "int8"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "pg_read_file" [ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "pg_read_file"
>           [ScalarType "text", ScalarType "int8", ScalarType "int8"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "pg_relation_filenode"
>           [ScalarType "regclass"]
>           (ScalarType "oid")
>           False,
>         CatCreateFunction FunName "pg_relation_filepath"
>           [ScalarType "regclass"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "pg_relation_size"
>           [ScalarType "regclass"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_relation_size"
>           [ScalarType "regclass", ScalarType "text"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_reload_conf" [] (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "pg_rotate_logfile" []
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "pg_sequence_parameters"
>           [ScalarType "oid"]
>           (Pseudo Record)
>           False,
>         CatCreateFunction FunName "pg_show_all_settings" []
>           (SetOfType (Pseudo Record))
>           False,
>         CatCreateFunction FunName "pg_size_pretty" [ScalarType "int8"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "pg_sleep" [ScalarType "float8"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "pg_start_backup"
>           [ScalarType "text", ScalarType "bool"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "pg_stat_clear_snapshot" [] (Pseudo Void)
>           False,
>         CatCreateFunction FunName "pg_stat_file" [ScalarType "text"]
>           (Pseudo Record)
>           False,
>         CatCreateFunction FunName "pg_stat_get_activity"
>           [ScalarType "int4"]
>           (SetOfType (Pseudo Record))
>           False,
>         CatCreateFunction FunName "pg_stat_get_analyze_count"
>           [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_autoanalyze_count"
>           [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_autovacuum_count"
>           [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_backend_activity"
>           [ScalarType "int4"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "pg_stat_get_backend_activity_start"
>           [ScalarType "int4"]
>           (ScalarType "timestamptz")
>           False,
>         CatCreateFunction FunName "pg_stat_get_backend_client_addr"
>           [ScalarType "int4"]
>           (ScalarType "inet")
>           False,
>         CatCreateFunction FunName "pg_stat_get_backend_client_port"
>           [ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "pg_stat_get_backend_dbid"
>           [ScalarType "int4"]
>           (ScalarType "oid")
>           False,
>         CatCreateFunction FunName "pg_stat_get_backend_idset" []
>           (SetOfType (ScalarType "int4"))
>           False,
>         CatCreateFunction FunName "pg_stat_get_backend_pid"
>           [ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "pg_stat_get_backend_start"
>           [ScalarType "int4"]
>           (ScalarType "timestamptz")
>           False,
>         CatCreateFunction FunName "pg_stat_get_backend_userid"
>           [ScalarType "int4"]
>           (ScalarType "oid")
>           False,
>         CatCreateFunction FunName "pg_stat_get_backend_waiting"
>           [ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "pg_stat_get_backend_xact_start"
>           [ScalarType "int4"]
>           (ScalarType "timestamptz")
>           False,
>         CatCreateFunction FunName
>           "pg_stat_get_bgwriter_buf_written_checkpoints"
>           []
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_bgwriter_buf_written_clean"
>           []
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_bgwriter_maxwritten_clean"
>           []
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName
>           "pg_stat_get_bgwriter_requested_checkpoints"
>           []
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_bgwriter_stat_reset_time" []
>           (ScalarType "timestamptz")
>           False,
>         CatCreateFunction FunName "pg_stat_get_bgwriter_timed_checkpoints"
>           []
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_blocks_fetched"
>           [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_blocks_hit"
>           [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_buf_alloc" []
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_buf_fsync_backend" []
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_buf_written_backend" []
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_db_blocks_fetched"
>           [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_db_blocks_hit"
>           [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_db_conflict_all"
>           [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_db_conflict_bufferpin"
>           [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_db_conflict_lock"
>           [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_db_conflict_snapshot"
>           [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName
>           "pg_stat_get_db_conflict_startup_deadlock"
>           [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_db_conflict_tablespace"
>           [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_db_numbackends"
>           [ScalarType "oid"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "pg_stat_get_db_stat_reset_time"
>           [ScalarType "oid"]
>           (ScalarType "timestamptz")
>           False,
>         CatCreateFunction FunName "pg_stat_get_db_tuples_deleted"
>           [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_db_tuples_fetched"
>           [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_db_tuples_inserted"
>           [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_db_tuples_returned"
>           [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_db_tuples_updated"
>           [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_db_xact_commit"
>           [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_db_xact_rollback"
>           [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_dead_tuples"
>           [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_function_calls"
>           [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_function_self_time"
>           [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_function_time"
>           [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_last_analyze_time"
>           [ScalarType "oid"]
>           (ScalarType "timestamptz")
>           False,
>         CatCreateFunction FunName "pg_stat_get_last_autoanalyze_time"
>           [ScalarType "oid"]
>           (ScalarType "timestamptz")
>           False,
>         CatCreateFunction FunName "pg_stat_get_last_autovacuum_time"
>           [ScalarType "oid"]
>           (ScalarType "timestamptz")
>           False,
>         CatCreateFunction FunName "pg_stat_get_last_vacuum_time"
>           [ScalarType "oid"]
>           (ScalarType "timestamptz")
>           False,
>         CatCreateFunction FunName "pg_stat_get_live_tuples"
>           [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_numscans" [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_tuples_deleted"
>           [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_tuples_fetched"
>           [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_tuples_hot_updated"
>           [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_tuples_inserted"
>           [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_tuples_returned"
>           [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_tuples_updated"
>           [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_vacuum_count"
>           [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_wal_senders" []
>           (SetOfType (Pseudo Record))
>           False,
>         CatCreateFunction FunName "pg_stat_get_xact_blocks_fetched"
>           [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_xact_blocks_hit"
>           [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_xact_function_calls"
>           [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_xact_function_self_time"
>           [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_xact_function_time"
>           [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_xact_numscans"
>           [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_xact_tuples_deleted"
>           [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_xact_tuples_fetched"
>           [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_xact_tuples_hot_updated"
>           [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_xact_tuples_inserted"
>           [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_xact_tuples_returned"
>           [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_get_xact_tuples_updated"
>           [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_stat_reset" [] (Pseudo Void) False,
>         CatCreateFunction FunName "pg_stat_reset_shared"
>           [ScalarType "text"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "pg_stat_reset_single_function_counters"
>           [ScalarType "oid"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "pg_stat_reset_single_table_counters"
>           [ScalarType "oid"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "pg_stop_backup" [] (ScalarType "text")
>           False,
>         CatCreateFunction FunName "pg_switch_xlog" [] (ScalarType "text")
>           False,
>         CatCreateFunction FunName "pg_table_is_visible" [ScalarType "oid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "pg_table_size" [ScalarType "regclass"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_tablespace_databases"
>           [ScalarType "oid"]
>           (SetOfType (ScalarType "oid"))
>           False,
>         CatCreateFunction FunName "pg_tablespace_size" [ScalarType "name"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_tablespace_size" [ScalarType "oid"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_terminate_backend"
>           [ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "pg_timezone_abbrevs" []
>           (SetOfType (Pseudo Record))
>           False,
>         CatCreateFunction FunName "pg_timezone_names" []
>           (SetOfType (Pseudo Record))
>           False,
>         CatCreateFunction FunName "pg_total_relation_size"
>           [ScalarType "regclass"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "pg_try_advisory_lock"
>           [ScalarType "int8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "pg_try_advisory_lock"
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "pg_try_advisory_lock_shared"
>           [ScalarType "int8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "pg_try_advisory_lock_shared"
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "pg_try_advisory_xact_lock"
>           [ScalarType "int8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "pg_try_advisory_xact_lock"
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "pg_try_advisory_xact_lock_shared"
>           [ScalarType "int8"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "pg_try_advisory_xact_lock_shared"
>           [ScalarType "int4", ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "pg_ts_config_is_visible"
>           [ScalarType "oid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "pg_ts_dict_is_visible"
>           [ScalarType "oid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "pg_ts_parser_is_visible"
>           [ScalarType "oid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "pg_ts_template_is_visible"
>           [ScalarType "oid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "pg_type_is_visible" [ScalarType "oid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "pg_typeof" [Pseudo Any]
>           (ScalarType "regtype")
>           False,
>         CatCreateFunction FunName "pg_xlog_replay_pause" [] (Pseudo Void)
>           False,
>         CatCreateFunction FunName "pg_xlog_replay_resume" [] (Pseudo Void)
>           False,
>         CatCreateFunction FunName "pg_xlogfile_name" [ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "pg_xlogfile_name_offset"
>           [ScalarType "text"]
>           (Pseudo Record)
>           False,
>         CatCreateFunction FunName "pi" [] (ScalarType "float8") False,
>         CatCreateFunction FunName "plainto_tsquery" [ScalarType "text"]
>           (ScalarType "tsquery")
>           False,
>         CatCreateFunction FunName "plainto_tsquery"
>           [ScalarType "regconfig", ScalarType "text"]
>           (ScalarType "tsquery")
>           False,
>         CatCreateFunction FunName "plpgsql_call_handler" []
>           (Pseudo LanguageHandler)
>           False,
>         CatCreateFunction FunName "plpgsql_inline_handler"
>           [Pseudo Internal]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "plpgsql_validator" [ScalarType "oid"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "point" [ScalarType "lseg"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunName "point" [ScalarType "path"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunName "point" [ScalarType "box"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunName "point" [ScalarType "polygon"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunName "point" [ScalarType "circle"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunName "point"
>           [ScalarType "float8", ScalarType "float8"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunName "point_above"
>           [ScalarType "point", ScalarType "point"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "point_add"
>           [ScalarType "point", ScalarType "point"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunName "point_below"
>           [ScalarType "point", ScalarType "point"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "point_distance"
>           [ScalarType "point", ScalarType "point"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "point_div"
>           [ScalarType "point", ScalarType "point"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunName "point_eq"
>           [ScalarType "point", ScalarType "point"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "point_horiz"
>           [ScalarType "point", ScalarType "point"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "point_in" [Pseudo Cstring]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunName "point_left"
>           [ScalarType "point", ScalarType "point"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "point_mul"
>           [ScalarType "point", ScalarType "point"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunName "point_ne"
>           [ScalarType "point", ScalarType "point"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "point_out" [ScalarType "point"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "point_recv" [Pseudo Internal]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunName "point_right"
>           [ScalarType "point", ScalarType "point"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "point_send" [ScalarType "point"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "point_sub"
>           [ScalarType "point", ScalarType "point"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunName "point_vert"
>           [ScalarType "point", ScalarType "point"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "poly_above"
>           [ScalarType "polygon", ScalarType "polygon"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "poly_below"
>           [ScalarType "polygon", ScalarType "polygon"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "poly_center" [ScalarType "polygon"]
>           (ScalarType "point")
>           False,
>         CatCreateFunction FunName "poly_contain"
>           [ScalarType "polygon", ScalarType "polygon"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "poly_contain_pt"
>           [ScalarType "polygon", ScalarType "point"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "poly_contained"
>           [ScalarType "polygon", ScalarType "polygon"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "poly_distance"
>           [ScalarType "polygon", ScalarType "polygon"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "poly_in" [Pseudo Cstring]
>           (ScalarType "polygon")
>           False,
>         CatCreateFunction FunName "poly_left"
>           [ScalarType "polygon", ScalarType "polygon"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "poly_npoints" [ScalarType "polygon"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "poly_out" [ScalarType "polygon"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "poly_overabove"
>           [ScalarType "polygon", ScalarType "polygon"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "poly_overbelow"
>           [ScalarType "polygon", ScalarType "polygon"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "poly_overlap"
>           [ScalarType "polygon", ScalarType "polygon"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "poly_overleft"
>           [ScalarType "polygon", ScalarType "polygon"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "poly_overright"
>           [ScalarType "polygon", ScalarType "polygon"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "poly_recv" [Pseudo Internal]
>           (ScalarType "polygon")
>           False,
>         CatCreateFunction FunName "poly_right"
>           [ScalarType "polygon", ScalarType "polygon"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "poly_same"
>           [ScalarType "polygon", ScalarType "polygon"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "poly_send" [ScalarType "polygon"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "polygon" [ScalarType "path"]
>           (ScalarType "polygon")
>           False,
>         CatCreateFunction FunName "polygon" [ScalarType "box"]
>           (ScalarType "polygon")
>           False,
>         CatCreateFunction FunName "polygon" [ScalarType "circle"]
>           (ScalarType "polygon")
>           False,
>         CatCreateFunction FunName "polygon"
>           [ScalarType "int4", ScalarType "circle"]
>           (ScalarType "polygon")
>           False,
>         CatCreateFunction FunName "popen" [ScalarType "path"]
>           (ScalarType "path")
>           False,
>         CatCreateFunction FunName "position"
>           [ScalarType "bytea", ScalarType "bytea"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "position"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "position"
>           [ScalarType "bit", ScalarType "bit"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "positionjoinsel"
>           [Pseudo Internal, ScalarType "oid", Pseudo Internal,
>            ScalarType "int2", Pseudo Internal]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "positionsel"
>           [Pseudo Internal, ScalarType "oid", Pseudo Internal,
>            ScalarType "int4"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "postgresql_fdw_validator"
>           [ArrayType (ScalarType "text"), ScalarType "oid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "pow"
>           [ScalarType "float8", ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "pow"
>           [ScalarType "numeric", ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "power"
>           [ScalarType "float8", ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "power"
>           [ScalarType "numeric", ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "prsd_end" [Pseudo Internal]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "prsd_headline"
>           [Pseudo Internal, Pseudo Internal, ScalarType "tsquery"]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "prsd_lextype" [Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "prsd_nexttoken"
>           [Pseudo Internal, Pseudo Internal, Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "prsd_start"
>           [Pseudo Internal, ScalarType "int4"]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "pt_contained_circle"
>           [ScalarType "point", ScalarType "circle"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "pt_contained_poly"
>           [ScalarType "point", ScalarType "polygon"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "query_to_xml"
>           [ScalarType "text", ScalarType "bool", ScalarType "bool",
>            ScalarType "text"]
>           (ScalarType "xml")
>           False,
>         CatCreateFunction FunName "query_to_xml_and_xmlschema"
>           [ScalarType "text", ScalarType "bool", ScalarType "bool",
>            ScalarType "text"]
>           (ScalarType "xml")
>           False,
>         CatCreateFunction FunName "query_to_xmlschema"
>           [ScalarType "text", ScalarType "bool", ScalarType "bool",
>            ScalarType "text"]
>           (ScalarType "xml")
>           False,
>         CatCreateFunction FunName "querytree" [ScalarType "tsquery"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "quote_ident" [ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "quote_literal" [ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "quote_literal" [Pseudo AnyElement]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "quote_nullable" [ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "quote_nullable" [Pseudo AnyElement]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "radians" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "radius" [ScalarType "circle"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "random" [] (ScalarType "float8") False,
>         CatCreateFunction FunName "record_eq"
>           [Pseudo Record, Pseudo Record]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "record_ge"
>           [Pseudo Record, Pseudo Record]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "record_gt"
>           [Pseudo Record, Pseudo Record]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "record_in"
>           [Pseudo Cstring, ScalarType "oid", ScalarType "int4"]
>           (Pseudo Record)
>           False,
>         CatCreateFunction FunName "record_le"
>           [Pseudo Record, Pseudo Record]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "record_lt"
>           [Pseudo Record, Pseudo Record]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "record_ne"
>           [Pseudo Record, Pseudo Record]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "record_out" [Pseudo Record]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "record_recv"
>           [Pseudo Internal, ScalarType "oid", ScalarType "int4"]
>           (Pseudo Record)
>           False,
>         CatCreateFunction FunName "record_send" [Pseudo Record]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "regclass" [ScalarType "text"]
>           (ScalarType "regclass")
>           False,
>         CatCreateFunction FunName "regclassin" [Pseudo Cstring]
>           (ScalarType "regclass")
>           False,
>         CatCreateFunction FunName "regclassout" [ScalarType "regclass"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "regclassrecv" [Pseudo Internal]
>           (ScalarType "regclass")
>           False,
>         CatCreateFunction FunName "regclasssend" [ScalarType "regclass"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "regconfigin" [Pseudo Cstring]
>           (ScalarType "regconfig")
>           False,
>         CatCreateFunction FunName "regconfigout" [ScalarType "regconfig"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "regconfigrecv" [Pseudo Internal]
>           (ScalarType "regconfig")
>           False,
>         CatCreateFunction FunName "regconfigsend" [ScalarType "regconfig"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "regdictionaryin" [Pseudo Cstring]
>           (ScalarType "regdictionary")
>           False,
>         CatCreateFunction FunName "regdictionaryout"
>           [ScalarType "regdictionary"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "regdictionaryrecv" [Pseudo Internal]
>           (ScalarType "regdictionary")
>           False,
>         CatCreateFunction FunName "regdictionarysend"
>           [ScalarType "regdictionary"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "regexeqjoinsel"
>           [Pseudo Internal, ScalarType "oid", Pseudo Internal,
>            ScalarType "int2", Pseudo Internal]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "regexeqsel"
>           [Pseudo Internal, ScalarType "oid", Pseudo Internal,
>            ScalarType "int4"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "regexnejoinsel"
>           [Pseudo Internal, ScalarType "oid", Pseudo Internal,
>            ScalarType "int2", Pseudo Internal]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "regexnesel"
>           [Pseudo Internal, ScalarType "oid", Pseudo Internal,
>            ScalarType "int4"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "regexp_matches"
>           [ScalarType "text", ScalarType "text"]
>           (SetOfType (ArrayType (ScalarType "text")))
>           False,
>         CatCreateFunction FunName "regexp_matches"
>           [ScalarType "text", ScalarType "text", ScalarType "text"]
>           (SetOfType (ArrayType (ScalarType "text")))
>           False,
>         CatCreateFunction FunName "regexp_replace"
>           [ScalarType "text", ScalarType "text", ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "regexp_replace"
>           [ScalarType "text", ScalarType "text", ScalarType "text",
>            ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "regexp_split_to_array"
>           [ScalarType "text", ScalarType "text"]
>           (ArrayType (ScalarType "text"))
>           False,
>         CatCreateFunction FunName "regexp_split_to_array"
>           [ScalarType "text", ScalarType "text", ScalarType "text"]
>           (ArrayType (ScalarType "text"))
>           False,
>         CatCreateFunction FunName "regexp_split_to_table"
>           [ScalarType "text", ScalarType "text"]
>           (SetOfType (ScalarType "text"))
>           False,
>         CatCreateFunction FunName "regexp_split_to_table"
>           [ScalarType "text", ScalarType "text", ScalarType "text"]
>           (SetOfType (ScalarType "text"))
>           False,
>         CatCreateFunction FunName "regoperatorin" [Pseudo Cstring]
>           (ScalarType "regoperator")
>           False,
>         CatCreateFunction FunName "regoperatorout"
>           [ScalarType "regoperator"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "regoperatorrecv" [Pseudo Internal]
>           (ScalarType "regoperator")
>           False,
>         CatCreateFunction FunName "regoperatorsend"
>           [ScalarType "regoperator"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "regoperin" [Pseudo Cstring]
>           (ScalarType "regoper")
>           False,
>         CatCreateFunction FunName "regoperout" [ScalarType "regoper"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "regoperrecv" [Pseudo Internal]
>           (ScalarType "regoper")
>           False,
>         CatCreateFunction FunName "regopersend" [ScalarType "regoper"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "regprocedurein" [Pseudo Cstring]
>           (ScalarType "regprocedure")
>           False,
>         CatCreateFunction FunName "regprocedureout"
>           [ScalarType "regprocedure"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "regprocedurerecv" [Pseudo Internal]
>           (ScalarType "regprocedure")
>           False,
>         CatCreateFunction FunName "regproceduresend"
>           [ScalarType "regprocedure"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "regprocin" [Pseudo Cstring]
>           (ScalarType "regproc")
>           False,
>         CatCreateFunction FunName "regprocout" [ScalarType "regproc"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "regprocrecv" [Pseudo Internal]
>           (ScalarType "regproc")
>           False,
>         CatCreateFunction FunName "regprocsend" [ScalarType "regproc"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "regtypein" [Pseudo Cstring]
>           (ScalarType "regtype")
>           False,
>         CatCreateFunction FunName "regtypeout" [ScalarType "regtype"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "regtyperecv" [Pseudo Internal]
>           (ScalarType "regtype")
>           False,
>         CatCreateFunction FunName "regtypesend" [ScalarType "regtype"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "reltime" [ScalarType "interval"]
>           (ScalarType "reltime")
>           False,
>         CatCreateFunction FunName "reltimeeq"
>           [ScalarType "reltime", ScalarType "reltime"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "reltimege"
>           [ScalarType "reltime", ScalarType "reltime"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "reltimegt"
>           [ScalarType "reltime", ScalarType "reltime"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "reltimein" [Pseudo Cstring]
>           (ScalarType "reltime")
>           False,
>         CatCreateFunction FunName "reltimele"
>           [ScalarType "reltime", ScalarType "reltime"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "reltimelt"
>           [ScalarType "reltime", ScalarType "reltime"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "reltimene"
>           [ScalarType "reltime", ScalarType "reltime"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "reltimeout" [ScalarType "reltime"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "reltimerecv" [Pseudo Internal]
>           (ScalarType "reltime")
>           False,
>         CatCreateFunction FunName "reltimesend" [ScalarType "reltime"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "repeat"
>           [ScalarType "text", ScalarType "int4"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "replace"
>           [ScalarType "text", ScalarType "text", ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "reverse" [ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "right"
>           [ScalarType "text", ScalarType "int4"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "round" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "round" [ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "round"
>           [ScalarType "numeric", ScalarType "int4"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "rpad"
>           [ScalarType "text", ScalarType "int4"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "rpad"
>           [ScalarType "text", ScalarType "int4", ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "rtrim" [ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "rtrim"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "scalargtjoinsel"
>           [Pseudo Internal, ScalarType "oid", Pseudo Internal,
>            ScalarType "int2", Pseudo Internal]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "scalargtsel"
>           [Pseudo Internal, ScalarType "oid", Pseudo Internal,
>            ScalarType "int4"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "scalarltjoinsel"
>           [Pseudo Internal, ScalarType "oid", Pseudo Internal,
>            ScalarType "int2", Pseudo Internal]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "scalarltsel"
>           [Pseudo Internal, ScalarType "oid", Pseudo Internal,
>            ScalarType "int4"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "schema_to_xml"
>           [ScalarType "name", ScalarType "bool", ScalarType "bool",
>            ScalarType "text"]
>           (ScalarType "xml")
>           False,
>         CatCreateFunction FunName "schema_to_xml_and_xmlschema"
>           [ScalarType "name", ScalarType "bool", ScalarType "bool",
>            ScalarType "text"]
>           (ScalarType "xml")
>           False,
>         CatCreateFunction FunName "schema_to_xmlschema"
>           [ScalarType "name", ScalarType "bool", ScalarType "bool",
>            ScalarType "text"]
>           (ScalarType "xml")
>           False,
>         CatCreateFunction FunName "session_user" [] (ScalarType "name")
>           False,
>         CatCreateFunction FunName "set_bit"
>           [ScalarType "bytea", ScalarType "int4", ScalarType "int4"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "set_bit"
>           [ScalarType "bit", ScalarType "int4", ScalarType "int4"]
>           (ScalarType "bit")
>           False,
>         CatCreateFunction FunName "set_byte"
>           [ScalarType "bytea", ScalarType "int4", ScalarType "int4"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "set_config"
>           [ScalarType "text", ScalarType "text", ScalarType "bool"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "set_masklen"
>           [ScalarType "cidr", ScalarType "int4"]
>           (ScalarType "cidr")
>           False,
>         CatCreateFunction FunName "set_masklen"
>           [ScalarType "inet", ScalarType "int4"]
>           (ScalarType "inet")
>           False,
>         CatCreateFunction FunName "setseed" [ScalarType "float8"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "setval"
>           [ScalarType "regclass", ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "setval"
>           [ScalarType "regclass", ScalarType "int8", ScalarType "bool"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "setweight"
>           [ScalarType "tsvector", ScalarType "char"]
>           (ScalarType "tsvector")
>           False,
>         CatCreateFunction FunName "shell_in" [Pseudo Cstring]
>           (Pseudo Opaque)
>           False,
>         CatCreateFunction FunName "shell_out" [Pseudo Opaque]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "shift_jis_2004_to_euc_jis_2004"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "shift_jis_2004_to_utf8"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "shobj_description"
>           [ScalarType "oid", ScalarType "name"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "sign" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "sign" [ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "similar_escape"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "sin" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "sjis_to_euc_jp"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "sjis_to_mic"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "sjis_to_utf8"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "slope"
>           [ScalarType "point", ScalarType "point"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "smgreq"
>           [ScalarType "smgr", ScalarType "smgr"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "smgrin" [Pseudo Cstring]
>           (ScalarType "smgr")
>           False,
>         CatCreateFunction FunName "smgrne"
>           [ScalarType "smgr", ScalarType "smgr"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "smgrout" [ScalarType "smgr"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "split_part"
>           [ScalarType "text", ScalarType "text", ScalarType "int4"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "sqrt" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "sqrt" [ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "statement_timestamp" []
>           (ScalarType "timestamptz")
>           False,
>         CatCreateFunction FunName "string_agg_finalfn" [Pseudo Internal]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "string_agg_transfn"
>           [Pseudo Internal, ScalarType "text", ScalarType "text"]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "string_to_array"
>           [ScalarType "text", ScalarType "text"]
>           (ArrayType (ScalarType "text"))
>           False,
>         CatCreateFunction FunName "string_to_array"
>           [ScalarType "text", ScalarType "text", ScalarType "text"]
>           (ArrayType (ScalarType "text"))
>           False,
>         CatCreateFunction FunName "strip" [ScalarType "tsvector"]
>           (ScalarType "tsvector")
>           False,
>         CatCreateFunction FunName "strpos"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "substr"
>           [ScalarType "bytea", ScalarType "int4"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "substr"
>           [ScalarType "text", ScalarType "int4"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "substr"
>           [ScalarType "bytea", ScalarType "int4", ScalarType "int4"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "substr"
>           [ScalarType "text", ScalarType "int4", ScalarType "int4"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "substring"
>           [ScalarType "bytea", ScalarType "int4"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "substring"
>           [ScalarType "text", ScalarType "int4"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "substring"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "substring"
>           [ScalarType "bit", ScalarType "int4"]
>           (ScalarType "bit")
>           False,
>         CatCreateFunction FunName "substring"
>           [ScalarType "bytea", ScalarType "int4", ScalarType "int4"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "substring"
>           [ScalarType "text", ScalarType "int4", ScalarType "int4"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "substring"
>           [ScalarType "text", ScalarType "text", ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "substring"
>           [ScalarType "bit", ScalarType "int4", ScalarType "int4"]
>           (ScalarType "bit")
>           False,
>         CatCreateFunction FunName "suppress_redundant_updates_trigger" []
>           (Pseudo Trigger)
>           False,
>         CatCreateFunction FunName "table_to_xml"
>           [ScalarType "regclass", ScalarType "bool", ScalarType "bool",
>            ScalarType "text"]
>           (ScalarType "xml")
>           False,
>         CatCreateFunction FunName "table_to_xml_and_xmlschema"
>           [ScalarType "regclass", ScalarType "bool", ScalarType "bool",
>            ScalarType "text"]
>           (ScalarType "xml")
>           False,
>         CatCreateFunction FunName "table_to_xmlschema"
>           [ScalarType "regclass", ScalarType "bool", ScalarType "bool",
>            ScalarType "text"]
>           (ScalarType "xml")
>           False,
>         CatCreateFunction FunName "tan" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "text" [ScalarType "bool"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "text" [ScalarType "char"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "text" [ScalarType "name"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "text" [ScalarType "xml"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "text" [ScalarType "inet"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "text" [ScalarType "bpchar"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "text_ge"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "text_gt"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "text_larger"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "text_le"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "text_lt"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "text_pattern_ge"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "text_pattern_gt"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "text_pattern_le"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "text_pattern_lt"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "text_smaller"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "textanycat"
>           [ScalarType "text", Pseudo AnyNonArray]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "textcat"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "texteq"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "texticlike"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "texticnlike"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "texticregexeq"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "texticregexne"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "textin" [Pseudo Cstring]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "textlen" [ScalarType "text"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "textlike"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "textne"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "textnlike"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "textout" [ScalarType "text"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "textrecv" [Pseudo Internal]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "textregexeq"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "textregexne"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "textsend" [ScalarType "text"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "thesaurus_init" [Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "thesaurus_lexize"
>           [Pseudo Internal, Pseudo Internal, Pseudo Internal,
>            Pseudo Internal]
>           (Pseudo Internal)
>           False,
>         CatCreateFunction FunName "tideq"
>           [ScalarType "tid", ScalarType "tid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "tidge"
>           [ScalarType "tid", ScalarType "tid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "tidgt"
>           [ScalarType "tid", ScalarType "tid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "tidin" [Pseudo Cstring]
>           (ScalarType "tid")
>           False,
>         CatCreateFunction FunName "tidlarger"
>           [ScalarType "tid", ScalarType "tid"]
>           (ScalarType "tid")
>           False,
>         CatCreateFunction FunName "tidle"
>           [ScalarType "tid", ScalarType "tid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "tidlt"
>           [ScalarType "tid", ScalarType "tid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "tidne"
>           [ScalarType "tid", ScalarType "tid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "tidout" [ScalarType "tid"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "tidrecv" [Pseudo Internal]
>           (ScalarType "tid")
>           False,
>         CatCreateFunction FunName "tidsend" [ScalarType "tid"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "tidsmaller"
>           [ScalarType "tid", ScalarType "tid"]
>           (ScalarType "tid")
>           False,
>         CatCreateFunction FunName "time" [ScalarType "abstime"]
>           (ScalarType "time")
>           False,
>         CatCreateFunction FunName "time" [ScalarType "timestamp"]
>           (ScalarType "time")
>           False,
>         CatCreateFunction FunName "time" [ScalarType "timestamptz"]
>           (ScalarType "time")
>           False,
>         CatCreateFunction FunName "time" [ScalarType "interval"]
>           (ScalarType "time")
>           False,
>         CatCreateFunction FunName "time" [ScalarType "timetz"]
>           (ScalarType "time")
>           False,
>         CatCreateFunction FunName "time"
>           [ScalarType "time", ScalarType "int4"]
>           (ScalarType "time")
>           False,
>         CatCreateFunction FunName "time_cmp"
>           [ScalarType "time", ScalarType "time"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "time_eq"
>           [ScalarType "time", ScalarType "time"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "time_ge"
>           [ScalarType "time", ScalarType "time"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "time_gt"
>           [ScalarType "time", ScalarType "time"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "time_hash" [ScalarType "time"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "time_in"
>           [Pseudo Cstring, ScalarType "oid", ScalarType "int4"]
>           (ScalarType "time")
>           False,
>         CatCreateFunction FunName "time_larger"
>           [ScalarType "time", ScalarType "time"]
>           (ScalarType "time")
>           False,
>         CatCreateFunction FunName "time_le"
>           [ScalarType "time", ScalarType "time"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "time_lt"
>           [ScalarType "time", ScalarType "time"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "time_mi_interval"
>           [ScalarType "time", ScalarType "interval"]
>           (ScalarType "time")
>           False,
>         CatCreateFunction FunName "time_mi_time"
>           [ScalarType "time", ScalarType "time"]
>           (ScalarType "interval")
>           False,
>         CatCreateFunction FunName "time_ne"
>           [ScalarType "time", ScalarType "time"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "time_out" [ScalarType "time"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "time_pl_interval"
>           [ScalarType "time", ScalarType "interval"]
>           (ScalarType "time")
>           False,
>         CatCreateFunction FunName "time_recv"
>           [Pseudo Internal, ScalarType "oid", ScalarType "int4"]
>           (ScalarType "time")
>           False,
>         CatCreateFunction FunName "time_send" [ScalarType "time"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "time_smaller"
>           [ScalarType "time", ScalarType "time"]
>           (ScalarType "time")
>           False,
>         CatCreateFunction FunName "timedate_pl"
>           [ScalarType "time", ScalarType "date"]
>           (ScalarType "timestamp")
>           False,
>         CatCreateFunction FunName "timemi"
>           [ScalarType "abstime", ScalarType "reltime"]
>           (ScalarType "abstime")
>           False,
>         CatCreateFunction FunName "timenow" [] (ScalarType "abstime")
>           False,
>         CatCreateFunction FunName "timeofday" [] (ScalarType "text") False,
>         CatCreateFunction FunName "timepl"
>           [ScalarType "abstime", ScalarType "reltime"]
>           (ScalarType "abstime")
>           False,
>         CatCreateFunction FunName "timestamp" [ScalarType "abstime"]
>           (ScalarType "timestamp")
>           False,
>         CatCreateFunction FunName "timestamp" [ScalarType "date"]
>           (ScalarType "timestamp")
>           False,
>         CatCreateFunction FunName "timestamp" [ScalarType "timestamptz"]
>           (ScalarType "timestamp")
>           False,
>         CatCreateFunction FunName "timestamp"
>           [ScalarType "date", ScalarType "time"]
>           (ScalarType "timestamp")
>           False,
>         CatCreateFunction FunName "timestamp"
>           [ScalarType "timestamp", ScalarType "int4"]
>           (ScalarType "timestamp")
>           False,
>         CatCreateFunction FunName "timestamp_cmp"
>           [ScalarType "timestamp", ScalarType "timestamp"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "timestamp_cmp_date"
>           [ScalarType "timestamp", ScalarType "date"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "timestamp_cmp_timestamptz"
>           [ScalarType "timestamp", ScalarType "timestamptz"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "timestamp_eq"
>           [ScalarType "timestamp", ScalarType "timestamp"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "timestamp_eq_date"
>           [ScalarType "timestamp", ScalarType "date"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "timestamp_eq_timestamptz"
>           [ScalarType "timestamp", ScalarType "timestamptz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "timestamp_ge"
>           [ScalarType "timestamp", ScalarType "timestamp"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "timestamp_ge_date"
>           [ScalarType "timestamp", ScalarType "date"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "timestamp_ge_timestamptz"
>           [ScalarType "timestamp", ScalarType "timestamptz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "timestamp_gt"
>           [ScalarType "timestamp", ScalarType "timestamp"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "timestamp_gt_date"
>           [ScalarType "timestamp", ScalarType "date"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "timestamp_gt_timestamptz"
>           [ScalarType "timestamp", ScalarType "timestamptz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "timestamp_hash" [ScalarType "timestamp"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "timestamp_in"
>           [Pseudo Cstring, ScalarType "oid", ScalarType "int4"]
>           (ScalarType "timestamp")
>           False,
>         CatCreateFunction FunName "timestamp_larger"
>           [ScalarType "timestamp", ScalarType "timestamp"]
>           (ScalarType "timestamp")
>           False,
>         CatCreateFunction FunName "timestamp_le"
>           [ScalarType "timestamp", ScalarType "timestamp"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "timestamp_le_date"
>           [ScalarType "timestamp", ScalarType "date"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "timestamp_le_timestamptz"
>           [ScalarType "timestamp", ScalarType "timestamptz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "timestamp_lt"
>           [ScalarType "timestamp", ScalarType "timestamp"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "timestamp_lt_date"
>           [ScalarType "timestamp", ScalarType "date"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "timestamp_lt_timestamptz"
>           [ScalarType "timestamp", ScalarType "timestamptz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "timestamp_mi"
>           [ScalarType "timestamp", ScalarType "timestamp"]
>           (ScalarType "interval")
>           False,
>         CatCreateFunction FunName "timestamp_mi_interval"
>           [ScalarType "timestamp", ScalarType "interval"]
>           (ScalarType "timestamp")
>           False,
>         CatCreateFunction FunName "timestamp_ne"
>           [ScalarType "timestamp", ScalarType "timestamp"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "timestamp_ne_date"
>           [ScalarType "timestamp", ScalarType "date"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "timestamp_ne_timestamptz"
>           [ScalarType "timestamp", ScalarType "timestamptz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "timestamp_out" [ScalarType "timestamp"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "timestamp_pl_interval"
>           [ScalarType "timestamp", ScalarType "interval"]
>           (ScalarType "timestamp")
>           False,
>         CatCreateFunction FunName "timestamp_recv"
>           [Pseudo Internal, ScalarType "oid", ScalarType "int4"]
>           (ScalarType "timestamp")
>           False,
>         CatCreateFunction FunName "timestamp_send" [ScalarType "timestamp"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "timestamp_smaller"
>           [ScalarType "timestamp", ScalarType "timestamp"]
>           (ScalarType "timestamp")
>           False,
>         CatCreateFunction FunName "timestamptypmodin"
>           [ArrayType (Pseudo Cstring)]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "timestamptypmodout" [ScalarType "int4"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "timestamptz" [ScalarType "abstime"]
>           (ScalarType "timestamptz")
>           False,
>         CatCreateFunction FunName "timestamptz" [ScalarType "date"]
>           (ScalarType "timestamptz")
>           False,
>         CatCreateFunction FunName "timestamptz" [ScalarType "timestamp"]
>           (ScalarType "timestamptz")
>           False,
>         CatCreateFunction FunName "timestamptz"
>           [ScalarType "date", ScalarType "time"]
>           (ScalarType "timestamptz")
>           False,
>         CatCreateFunction FunName "timestamptz"
>           [ScalarType "date", ScalarType "timetz"]
>           (ScalarType "timestamptz")
>           False,
>         CatCreateFunction FunName "timestamptz"
>           [ScalarType "timestamptz", ScalarType "int4"]
>           (ScalarType "timestamptz")
>           False,
>         CatCreateFunction FunName "timestamptz_cmp"
>           [ScalarType "timestamptz", ScalarType "timestamptz"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "timestamptz_cmp_date"
>           [ScalarType "timestamptz", ScalarType "date"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "timestamptz_cmp_timestamp"
>           [ScalarType "timestamptz", ScalarType "timestamp"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "timestamptz_eq"
>           [ScalarType "timestamptz", ScalarType "timestamptz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "timestamptz_eq_date"
>           [ScalarType "timestamptz", ScalarType "date"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "timestamptz_eq_timestamp"
>           [ScalarType "timestamptz", ScalarType "timestamp"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "timestamptz_ge"
>           [ScalarType "timestamptz", ScalarType "timestamptz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "timestamptz_ge_date"
>           [ScalarType "timestamptz", ScalarType "date"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "timestamptz_ge_timestamp"
>           [ScalarType "timestamptz", ScalarType "timestamp"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "timestamptz_gt"
>           [ScalarType "timestamptz", ScalarType "timestamptz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "timestamptz_gt_date"
>           [ScalarType "timestamptz", ScalarType "date"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "timestamptz_gt_timestamp"
>           [ScalarType "timestamptz", ScalarType "timestamp"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "timestamptz_in"
>           [Pseudo Cstring, ScalarType "oid", ScalarType "int4"]
>           (ScalarType "timestamptz")
>           False,
>         CatCreateFunction FunName "timestamptz_larger"
>           [ScalarType "timestamptz", ScalarType "timestamptz"]
>           (ScalarType "timestamptz")
>           False,
>         CatCreateFunction FunName "timestamptz_le"
>           [ScalarType "timestamptz", ScalarType "timestamptz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "timestamptz_le_date"
>           [ScalarType "timestamptz", ScalarType "date"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "timestamptz_le_timestamp"
>           [ScalarType "timestamptz", ScalarType "timestamp"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "timestamptz_lt"
>           [ScalarType "timestamptz", ScalarType "timestamptz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "timestamptz_lt_date"
>           [ScalarType "timestamptz", ScalarType "date"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "timestamptz_lt_timestamp"
>           [ScalarType "timestamptz", ScalarType "timestamp"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "timestamptz_mi"
>           [ScalarType "timestamptz", ScalarType "timestamptz"]
>           (ScalarType "interval")
>           False,
>         CatCreateFunction FunName "timestamptz_mi_interval"
>           [ScalarType "timestamptz", ScalarType "interval"]
>           (ScalarType "timestamptz")
>           False,
>         CatCreateFunction FunName "timestamptz_ne"
>           [ScalarType "timestamptz", ScalarType "timestamptz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "timestamptz_ne_date"
>           [ScalarType "timestamptz", ScalarType "date"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "timestamptz_ne_timestamp"
>           [ScalarType "timestamptz", ScalarType "timestamp"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "timestamptz_out"
>           [ScalarType "timestamptz"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "timestamptz_pl_interval"
>           [ScalarType "timestamptz", ScalarType "interval"]
>           (ScalarType "timestamptz")
>           False,
>         CatCreateFunction FunName "timestamptz_recv"
>           [Pseudo Internal, ScalarType "oid", ScalarType "int4"]
>           (ScalarType "timestamptz")
>           False,
>         CatCreateFunction FunName "timestamptz_send"
>           [ScalarType "timestamptz"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "timestamptz_smaller"
>           [ScalarType "timestamptz", ScalarType "timestamptz"]
>           (ScalarType "timestamptz")
>           False,
>         CatCreateFunction FunName "timestamptztypmodin"
>           [ArrayType (Pseudo Cstring)]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "timestamptztypmodout"
>           [ScalarType "int4"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "timetypmodin"
>           [ArrayType (Pseudo Cstring)]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "timetypmodout" [ScalarType "int4"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "timetz" [ScalarType "time"]
>           (ScalarType "timetz")
>           False,
>         CatCreateFunction FunName "timetz" [ScalarType "timestamptz"]
>           (ScalarType "timetz")
>           False,
>         CatCreateFunction FunName "timetz"
>           [ScalarType "timetz", ScalarType "int4"]
>           (ScalarType "timetz")
>           False,
>         CatCreateFunction FunName "timetz_cmp"
>           [ScalarType "timetz", ScalarType "timetz"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "timetz_eq"
>           [ScalarType "timetz", ScalarType "timetz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "timetz_ge"
>           [ScalarType "timetz", ScalarType "timetz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "timetz_gt"
>           [ScalarType "timetz", ScalarType "timetz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "timetz_hash" [ScalarType "timetz"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "timetz_in"
>           [Pseudo Cstring, ScalarType "oid", ScalarType "int4"]
>           (ScalarType "timetz")
>           False,
>         CatCreateFunction FunName "timetz_larger"
>           [ScalarType "timetz", ScalarType "timetz"]
>           (ScalarType "timetz")
>           False,
>         CatCreateFunction FunName "timetz_le"
>           [ScalarType "timetz", ScalarType "timetz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "timetz_lt"
>           [ScalarType "timetz", ScalarType "timetz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "timetz_mi_interval"
>           [ScalarType "timetz", ScalarType "interval"]
>           (ScalarType "timetz")
>           False,
>         CatCreateFunction FunName "timetz_ne"
>           [ScalarType "timetz", ScalarType "timetz"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "timetz_out" [ScalarType "timetz"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "timetz_pl_interval"
>           [ScalarType "timetz", ScalarType "interval"]
>           (ScalarType "timetz")
>           False,
>         CatCreateFunction FunName "timetz_recv"
>           [Pseudo Internal, ScalarType "oid", ScalarType "int4"]
>           (ScalarType "timetz")
>           False,
>         CatCreateFunction FunName "timetz_send" [ScalarType "timetz"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "timetz_smaller"
>           [ScalarType "timetz", ScalarType "timetz"]
>           (ScalarType "timetz")
>           False,
>         CatCreateFunction FunName "timetzdate_pl"
>           [ScalarType "timetz", ScalarType "date"]
>           (ScalarType "timestamptz")
>           False,
>         CatCreateFunction FunName "timetztypmodin"
>           [ArrayType (Pseudo Cstring)]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "timetztypmodout" [ScalarType "int4"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "timezone"
>           [ScalarType "text", ScalarType "timestamp"]
>           (ScalarType "timestamptz")
>           False,
>         CatCreateFunction FunName "timezone"
>           [ScalarType "text", ScalarType "timestamptz"]
>           (ScalarType "timestamp")
>           False,
>         CatCreateFunction FunName "timezone"
>           [ScalarType "text", ScalarType "timetz"]
>           (ScalarType "timetz")
>           False,
>         CatCreateFunction FunName "timezone"
>           [ScalarType "interval", ScalarType "timestamp"]
>           (ScalarType "timestamptz")
>           False,
>         CatCreateFunction FunName "timezone"
>           [ScalarType "interval", ScalarType "timestamptz"]
>           (ScalarType "timestamp")
>           False,
>         CatCreateFunction FunName "timezone"
>           [ScalarType "interval", ScalarType "timetz"]
>           (ScalarType "timetz")
>           False,
>         CatCreateFunction FunName "tinterval"
>           [ScalarType "abstime", ScalarType "abstime"]
>           (ScalarType "tinterval")
>           False,
>         CatCreateFunction FunName "tintervalct"
>           [ScalarType "tinterval", ScalarType "tinterval"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "tintervalend" [ScalarType "tinterval"]
>           (ScalarType "abstime")
>           False,
>         CatCreateFunction FunName "tintervaleq"
>           [ScalarType "tinterval", ScalarType "tinterval"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "tintervalge"
>           [ScalarType "tinterval", ScalarType "tinterval"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "tintervalgt"
>           [ScalarType "tinterval", ScalarType "tinterval"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "tintervalin" [Pseudo Cstring]
>           (ScalarType "tinterval")
>           False,
>         CatCreateFunction FunName "tintervalle"
>           [ScalarType "tinterval", ScalarType "tinterval"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "tintervalleneq"
>           [ScalarType "tinterval", ScalarType "reltime"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "tintervallenge"
>           [ScalarType "tinterval", ScalarType "reltime"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "tintervallengt"
>           [ScalarType "tinterval", ScalarType "reltime"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "tintervallenle"
>           [ScalarType "tinterval", ScalarType "reltime"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "tintervallenlt"
>           [ScalarType "tinterval", ScalarType "reltime"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "tintervallenne"
>           [ScalarType "tinterval", ScalarType "reltime"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "tintervallt"
>           [ScalarType "tinterval", ScalarType "tinterval"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "tintervalne"
>           [ScalarType "tinterval", ScalarType "tinterval"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "tintervalout" [ScalarType "tinterval"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "tintervalov"
>           [ScalarType "tinterval", ScalarType "tinterval"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "tintervalrecv" [Pseudo Internal]
>           (ScalarType "tinterval")
>           False,
>         CatCreateFunction FunName "tintervalrel" [ScalarType "tinterval"]
>           (ScalarType "reltime")
>           False,
>         CatCreateFunction FunName "tintervalsame"
>           [ScalarType "tinterval", ScalarType "tinterval"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "tintervalsend" [ScalarType "tinterval"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "tintervalstart" [ScalarType "tinterval"]
>           (ScalarType "abstime")
>           False,
>         CatCreateFunction FunName "to_ascii" [ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "to_ascii"
>           [ScalarType "text", ScalarType "name"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "to_ascii"
>           [ScalarType "text", ScalarType "int4"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "to_char"
>           [ScalarType "int8", ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "to_char"
>           [ScalarType "int4", ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "to_char"
>           [ScalarType "float4", ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "to_char"
>           [ScalarType "float8", ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "to_char"
>           [ScalarType "timestamp", ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "to_char"
>           [ScalarType "timestamptz", ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "to_char"
>           [ScalarType "interval", ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "to_char"
>           [ScalarType "numeric", ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "to_date"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "date")
>           False,
>         CatCreateFunction FunName "to_hex" [ScalarType "int8"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "to_hex" [ScalarType "int4"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "to_number"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "to_timestamp" [ScalarType "float8"]
>           (ScalarType "timestamptz")
>           False,
>         CatCreateFunction FunName "to_timestamp"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "timestamptz")
>           False,
>         CatCreateFunction FunName "to_tsquery" [ScalarType "text"]
>           (ScalarType "tsquery")
>           False,
>         CatCreateFunction FunName "to_tsquery"
>           [ScalarType "regconfig", ScalarType "text"]
>           (ScalarType "tsquery")
>           False,
>         CatCreateFunction FunName "to_tsvector" [ScalarType "text"]
>           (ScalarType "tsvector")
>           False,
>         CatCreateFunction FunName "to_tsvector"
>           [ScalarType "regconfig", ScalarType "text"]
>           (ScalarType "tsvector")
>           False,
>         CatCreateFunction FunName "transaction_timestamp" []
>           (ScalarType "timestamptz")
>           False,
>         CatCreateFunction FunName "translate"
>           [ScalarType "text", ScalarType "text", ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "trigger_in" [Pseudo Cstring]
>           (Pseudo Trigger)
>           False,
>         CatCreateFunction FunName "trigger_out" [Pseudo Trigger]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "trunc" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "trunc" [ScalarType "macaddr"]
>           (ScalarType "macaddr")
>           False,
>         CatCreateFunction FunName "trunc" [ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "trunc"
>           [ScalarType "numeric", ScalarType "int4"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunName "ts_debug" [ScalarType "text"]
>           (SetOfType (Pseudo Record))
>           False,
>         CatCreateFunction FunName "ts_debug"
>           [ScalarType "regconfig", ScalarType "text"]
>           (SetOfType (Pseudo Record))
>           False,
>         CatCreateFunction FunName "ts_headline"
>           [ScalarType "text", ScalarType "tsquery"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "ts_headline"
>           [ScalarType "text", ScalarType "tsquery", ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "ts_headline"
>           [ScalarType "regconfig", ScalarType "text", ScalarType "tsquery"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "ts_headline"
>           [ScalarType "regconfig", ScalarType "text", ScalarType "tsquery",
>            ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "ts_lexize"
>           [ScalarType "regdictionary", ScalarType "text"]
>           (ArrayType (ScalarType "text"))
>           False,
>         CatCreateFunction FunName "ts_match_qv"
>           [ScalarType "tsquery", ScalarType "tsvector"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "ts_match_tq"
>           [ScalarType "text", ScalarType "tsquery"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "ts_match_tt"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "ts_match_vq"
>           [ScalarType "tsvector", ScalarType "tsquery"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "ts_parse"
>           [ScalarType "text", ScalarType "text"]
>           (SetOfType (Pseudo Record))
>           False,
>         CatCreateFunction FunName "ts_parse"
>           [ScalarType "oid", ScalarType "text"]
>           (SetOfType (Pseudo Record))
>           False,
>         CatCreateFunction FunName "ts_rank"
>           [ScalarType "tsvector", ScalarType "tsquery"]
>           (ScalarType "float4")
>           False,
>         CatCreateFunction FunName "ts_rank"
>           [ArrayType (ScalarType "float4"), ScalarType "tsvector",
>            ScalarType "tsquery"]
>           (ScalarType "float4")
>           False,
>         CatCreateFunction FunName "ts_rank"
>           [ScalarType "tsvector", ScalarType "tsquery", ScalarType "int4"]
>           (ScalarType "float4")
>           False,
>         CatCreateFunction FunName "ts_rank"
>           [ArrayType (ScalarType "float4"), ScalarType "tsvector",
>            ScalarType "tsquery", ScalarType "int4"]
>           (ScalarType "float4")
>           False,
>         CatCreateFunction FunName "ts_rank_cd"
>           [ScalarType "tsvector", ScalarType "tsquery"]
>           (ScalarType "float4")
>           False,
>         CatCreateFunction FunName "ts_rank_cd"
>           [ArrayType (ScalarType "float4"), ScalarType "tsvector",
>            ScalarType "tsquery"]
>           (ScalarType "float4")
>           False,
>         CatCreateFunction FunName "ts_rank_cd"
>           [ScalarType "tsvector", ScalarType "tsquery", ScalarType "int4"]
>           (ScalarType "float4")
>           False,
>         CatCreateFunction FunName "ts_rank_cd"
>           [ArrayType (ScalarType "float4"), ScalarType "tsvector",
>            ScalarType "tsquery", ScalarType "int4"]
>           (ScalarType "float4")
>           False,
>         CatCreateFunction FunName "ts_rewrite"
>           [ScalarType "tsquery", ScalarType "text"]
>           (ScalarType "tsquery")
>           False,
>         CatCreateFunction FunName "ts_rewrite"
>           [ScalarType "tsquery", ScalarType "tsquery", ScalarType "tsquery"]
>           (ScalarType "tsquery")
>           False,
>         CatCreateFunction FunName "ts_stat" [ScalarType "text"]
>           (SetOfType (Pseudo Record))
>           False,
>         CatCreateFunction FunName "ts_stat"
>           [ScalarType "text", ScalarType "text"]
>           (SetOfType (Pseudo Record))
>           False,
>         CatCreateFunction FunName "ts_token_type" [ScalarType "text"]
>           (SetOfType (Pseudo Record))
>           False,
>         CatCreateFunction FunName "ts_token_type" [ScalarType "oid"]
>           (SetOfType (Pseudo Record))
>           False,
>         CatCreateFunction FunName "ts_typanalyze" [Pseudo Internal]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "tsmatchjoinsel"
>           [Pseudo Internal, ScalarType "oid", Pseudo Internal,
>            ScalarType "int2", Pseudo Internal]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "tsmatchsel"
>           [Pseudo Internal, ScalarType "oid", Pseudo Internal,
>            ScalarType "int4"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "tsq_mcontained"
>           [ScalarType "tsquery", ScalarType "tsquery"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "tsq_mcontains"
>           [ScalarType "tsquery", ScalarType "tsquery"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "tsquery_and"
>           [ScalarType "tsquery", ScalarType "tsquery"]
>           (ScalarType "tsquery")
>           False,
>         CatCreateFunction FunName "tsquery_cmp"
>           [ScalarType "tsquery", ScalarType "tsquery"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "tsquery_eq"
>           [ScalarType "tsquery", ScalarType "tsquery"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "tsquery_ge"
>           [ScalarType "tsquery", ScalarType "tsquery"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "tsquery_gt"
>           [ScalarType "tsquery", ScalarType "tsquery"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "tsquery_le"
>           [ScalarType "tsquery", ScalarType "tsquery"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "tsquery_lt"
>           [ScalarType "tsquery", ScalarType "tsquery"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "tsquery_ne"
>           [ScalarType "tsquery", ScalarType "tsquery"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "tsquery_not" [ScalarType "tsquery"]
>           (ScalarType "tsquery")
>           False,
>         CatCreateFunction FunName "tsquery_or"
>           [ScalarType "tsquery", ScalarType "tsquery"]
>           (ScalarType "tsquery")
>           False,
>         CatCreateFunction FunName "tsqueryin" [Pseudo Cstring]
>           (ScalarType "tsquery")
>           False,
>         CatCreateFunction FunName "tsqueryout" [ScalarType "tsquery"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "tsqueryrecv" [Pseudo Internal]
>           (ScalarType "tsquery")
>           False,
>         CatCreateFunction FunName "tsquerysend" [ScalarType "tsquery"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "tsvector_cmp"
>           [ScalarType "tsvector", ScalarType "tsvector"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "tsvector_concat"
>           [ScalarType "tsvector", ScalarType "tsvector"]
>           (ScalarType "tsvector")
>           False,
>         CatCreateFunction FunName "tsvector_eq"
>           [ScalarType "tsvector", ScalarType "tsvector"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "tsvector_ge"
>           [ScalarType "tsvector", ScalarType "tsvector"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "tsvector_gt"
>           [ScalarType "tsvector", ScalarType "tsvector"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "tsvector_le"
>           [ScalarType "tsvector", ScalarType "tsvector"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "tsvector_lt"
>           [ScalarType "tsvector", ScalarType "tsvector"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "tsvector_ne"
>           [ScalarType "tsvector", ScalarType "tsvector"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "tsvector_update_trigger" []
>           (Pseudo Trigger)
>           False,
>         CatCreateFunction FunName "tsvector_update_trigger_column" []
>           (Pseudo Trigger)
>           False,
>         CatCreateFunction FunName "tsvectorin" [Pseudo Cstring]
>           (ScalarType "tsvector")
>           False,
>         CatCreateFunction FunName "tsvectorout" [ScalarType "tsvector"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "tsvectorrecv" [Pseudo Internal]
>           (ScalarType "tsvector")
>           False,
>         CatCreateFunction FunName "tsvectorsend" [ScalarType "tsvector"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "txid_current" [] (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "txid_current_snapshot" []
>           (ScalarType "txid_snapshot")
>           False,
>         CatCreateFunction FunName "txid_snapshot_in" [Pseudo Cstring]
>           (ScalarType "txid_snapshot")
>           False,
>         CatCreateFunction FunName "txid_snapshot_out"
>           [ScalarType "txid_snapshot"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "txid_snapshot_recv" [Pseudo Internal]
>           (ScalarType "txid_snapshot")
>           False,
>         CatCreateFunction FunName "txid_snapshot_send"
>           [ScalarType "txid_snapshot"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "txid_snapshot_xip"
>           [ScalarType "txid_snapshot"]
>           (SetOfType (ScalarType "int8"))
>           False,
>         CatCreateFunction FunName "txid_snapshot_xmax"
>           [ScalarType "txid_snapshot"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "txid_snapshot_xmin"
>           [ScalarType "txid_snapshot"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunName "txid_visible_in_snapshot"
>           [ScalarType "int8", ScalarType "txid_snapshot"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "uhc_to_utf8"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "unique_key_recheck" [] (Pseudo Trigger)
>           False,
>         CatCreateFunction FunName "unknownin" [Pseudo Cstring]
>           (ScalarType "unknown")
>           False,
>         CatCreateFunction FunName "unknownout" [ScalarType "unknown"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "unknownrecv" [Pseudo Internal]
>           (ScalarType "unknown")
>           False,
>         CatCreateFunction FunName "unknownsend" [ScalarType "unknown"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "unnest" [Pseudo AnyArray]
>           (SetOfType (Pseudo AnyElement))
>           False,
>         CatCreateFunction FunName "upper" [ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunName "utf8_to_ascii"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "utf8_to_big5"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "utf8_to_euc_cn"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "utf8_to_euc_jis_2004"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "utf8_to_euc_jp"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "utf8_to_euc_kr"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "utf8_to_euc_tw"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "utf8_to_gb18030"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "utf8_to_gbk"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "utf8_to_iso8859"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "utf8_to_iso8859_1"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "utf8_to_johab"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "utf8_to_koi8r"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "utf8_to_koi8u"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "utf8_to_shift_jis_2004"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "utf8_to_sjis"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "utf8_to_uhc"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "utf8_to_win"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "uuid_cmp"
>           [ScalarType "uuid", ScalarType "uuid"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "uuid_eq"
>           [ScalarType "uuid", ScalarType "uuid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "uuid_ge"
>           [ScalarType "uuid", ScalarType "uuid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "uuid_gt"
>           [ScalarType "uuid", ScalarType "uuid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "uuid_hash" [ScalarType "uuid"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "uuid_in" [Pseudo Cstring]
>           (ScalarType "uuid")
>           False,
>         CatCreateFunction FunName "uuid_le"
>           [ScalarType "uuid", ScalarType "uuid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "uuid_lt"
>           [ScalarType "uuid", ScalarType "uuid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "uuid_ne"
>           [ScalarType "uuid", ScalarType "uuid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "uuid_out" [ScalarType "uuid"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "uuid_recv" [Pseudo Internal]
>           (ScalarType "uuid")
>           False,
>         CatCreateFunction FunName "uuid_send" [ScalarType "uuid"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "varbit"
>           [ScalarType "varbit", ScalarType "int4", ScalarType "bool"]
>           (ScalarType "varbit")
>           False,
>         CatCreateFunction FunName "varbit_in"
>           [Pseudo Cstring, ScalarType "oid", ScalarType "int4"]
>           (ScalarType "varbit")
>           False,
>         CatCreateFunction FunName "varbit_out" [ScalarType "varbit"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "varbit_recv"
>           [Pseudo Internal, ScalarType "oid", ScalarType "int4"]
>           (ScalarType "varbit")
>           False,
>         CatCreateFunction FunName "varbit_send" [ScalarType "varbit"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "varbitcmp"
>           [ScalarType "varbit", ScalarType "varbit"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "varbiteq"
>           [ScalarType "varbit", ScalarType "varbit"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "varbitge"
>           [ScalarType "varbit", ScalarType "varbit"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "varbitgt"
>           [ScalarType "varbit", ScalarType "varbit"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "varbitle"
>           [ScalarType "varbit", ScalarType "varbit"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "varbitlt"
>           [ScalarType "varbit", ScalarType "varbit"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "varbitne"
>           [ScalarType "varbit", ScalarType "varbit"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "varbittypmodin"
>           [ArrayType (Pseudo Cstring)]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "varbittypmodout" [ScalarType "int4"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "varchar" [ScalarType "name"]
>           (ScalarType "varchar")
>           False,
>         CatCreateFunction FunName "varchar"
>           [ScalarType "varchar", ScalarType "int4", ScalarType "bool"]
>           (ScalarType "varchar")
>           False,
>         CatCreateFunction FunName "varcharin"
>           [Pseudo Cstring, ScalarType "oid", ScalarType "int4"]
>           (ScalarType "varchar")
>           False,
>         CatCreateFunction FunName "varcharout" [ScalarType "varchar"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "varcharrecv"
>           [Pseudo Internal, ScalarType "oid", ScalarType "int4"]
>           (ScalarType "varchar")
>           False,
>         CatCreateFunction FunName "varcharsend" [ScalarType "varchar"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "varchartypmodin"
>           [ArrayType (Pseudo Cstring)]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "varchartypmodout" [ScalarType "int4"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "version" [] (ScalarType "text") False,
>         CatCreateFunction FunName "void_in" [Pseudo Cstring] (Pseudo Void)
>           False,
>         CatCreateFunction FunName "void_out" [Pseudo Void] (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "void_recv" [Pseudo Internal]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "void_send" [Pseudo Void]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "width" [ScalarType "box"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunName "width_bucket"
>           [ScalarType "float8", ScalarType "float8", ScalarType "float8",
>            ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "width_bucket"
>           [ScalarType "numeric", ScalarType "numeric", ScalarType "numeric",
>            ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunName "win1250_to_latin2"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "win1250_to_mic"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "win1251_to_iso"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "win1251_to_koi8r"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "win1251_to_mic"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "win1251_to_win866"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "win866_to_iso"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "win866_to_koi8r"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "win866_to_mic"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "win866_to_win1251"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "win_to_utf8"
>           [ScalarType "int4", ScalarType "int4", Pseudo Cstring,
>            Pseudo Internal, ScalarType "int4"]
>           (Pseudo Void)
>           False,
>         CatCreateFunction FunName "xideq"
>           [ScalarType "xid", ScalarType "xid"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "xideqint4"
>           [ScalarType "xid", ScalarType "int4"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "xidin" [Pseudo Cstring]
>           (ScalarType "xid")
>           False,
>         CatCreateFunction FunName "xidout" [ScalarType "xid"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "xidrecv" [Pseudo Internal]
>           (ScalarType "xid")
>           False,
>         CatCreateFunction FunName "xidsend" [ScalarType "xid"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "xml" [ScalarType "text"]
>           (ScalarType "xml")
>           False,
>         CatCreateFunction FunName "xml_in" [Pseudo Cstring]
>           (ScalarType "xml")
>           False,
>         CatCreateFunction FunName "xml_is_well_formed" [ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "xml_is_well_formed_content"
>           [ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "xml_is_well_formed_document"
>           [ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "xml_out" [ScalarType "xml"]
>           (Pseudo Cstring)
>           False,
>         CatCreateFunction FunName "xml_recv" [Pseudo Internal]
>           (ScalarType "xml")
>           False,
>         CatCreateFunction FunName "xml_send" [ScalarType "xml"]
>           (ScalarType "bytea")
>           False,
>         CatCreateFunction FunName "xmlcomment" [ScalarType "text"]
>           (ScalarType "xml")
>           False,
>         CatCreateFunction FunName "xmlconcat2"
>           [ScalarType "xml", ScalarType "xml"]
>           (ScalarType "xml")
>           False,
>         CatCreateFunction FunName "xmlexists"
>           [ScalarType "text", ScalarType "xml"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "xmlvalidate"
>           [ScalarType "xml", ScalarType "text"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "xpath"
>           [ScalarType "text", ScalarType "xml"]
>           (ArrayType (ScalarType "xml"))
>           False,
>         CatCreateFunction FunName "xpath"
>           [ScalarType "text", ScalarType "xml",
>            ArrayType (ScalarType "text")]
>           (ArrayType (ScalarType "xml"))
>           False,
>         CatCreateFunction FunName "xpath_exists"
>           [ScalarType "text", ScalarType "xml"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunName "xpath_exists"
>           [ScalarType "text", ScalarType "xml",
>            ArrayType (ScalarType "text")]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunAgg "array_agg" [Pseudo AnyElement]
>           (Pseudo AnyArray)
>           False,
>         CatCreateFunction FunAgg "avg" [ScalarType "int8"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunAgg "avg" [ScalarType "int2"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunAgg "avg" [ScalarType "int4"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunAgg "avg" [ScalarType "float4"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunAgg "avg" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunAgg "avg" [ScalarType "interval"]
>           (ScalarType "interval")
>           False,
>         CatCreateFunction FunAgg "avg" [ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunAgg "bit_and" [ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunAgg "bit_and" [ScalarType "int2"]
>           (ScalarType "int2")
>           False,
>         CatCreateFunction FunAgg "bit_and" [ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunAgg "bit_and" [ScalarType "bit"]
>           (ScalarType "bit")
>           False,
>         CatCreateFunction FunAgg "bit_or" [ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunAgg "bit_or" [ScalarType "int2"]
>           (ScalarType "int2")
>           False,
>         CatCreateFunction FunAgg "bit_or" [ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunAgg "bit_or" [ScalarType "bit"]
>           (ScalarType "bit")
>           False,
>         CatCreateFunction FunAgg "bool_and" [ScalarType "bool"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunAgg "bool_or" [ScalarType "bool"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunAgg "corr"
>           [ScalarType "float8", ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunAgg "count" [] (ScalarType "int8") False,
>         CatCreateFunction FunAgg "count" [Pseudo Any] (ScalarType "int8")
>           False,
>         CatCreateFunction FunAgg "covar_pop"
>           [ScalarType "float8", ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunAgg "covar_samp"
>           [ScalarType "float8", ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunAgg "every" [ScalarType "bool"]
>           (ScalarType "bool")
>           False,
>         CatCreateFunction FunAgg "max" [ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunAgg "max" [ScalarType "int2"]
>           (ScalarType "int2")
>           False,
>         CatCreateFunction FunAgg "max" [ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunAgg "max" [ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunAgg "max" [ScalarType "oid"]
>           (ScalarType "oid")
>           False,
>         CatCreateFunction FunAgg "max" [ScalarType "tid"]
>           (ScalarType "tid")
>           False,
>         CatCreateFunction FunAgg "max" [ScalarType "float4"]
>           (ScalarType "float4")
>           False,
>         CatCreateFunction FunAgg "max" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunAgg "max" [ScalarType "abstime"]
>           (ScalarType "abstime")
>           False,
>         CatCreateFunction FunAgg "max" [ScalarType "money"]
>           (ScalarType "money")
>           False,
>         CatCreateFunction FunAgg "max" [ScalarType "bpchar"]
>           (ScalarType "bpchar")
>           False,
>         CatCreateFunction FunAgg "max" [ScalarType "date"]
>           (ScalarType "date")
>           False,
>         CatCreateFunction FunAgg "max" [ScalarType "time"]
>           (ScalarType "time")
>           False,
>         CatCreateFunction FunAgg "max" [ScalarType "timestamp"]
>           (ScalarType "timestamp")
>           False,
>         CatCreateFunction FunAgg "max" [ScalarType "timestamptz"]
>           (ScalarType "timestamptz")
>           False,
>         CatCreateFunction FunAgg "max" [ScalarType "interval"]
>           (ScalarType "interval")
>           False,
>         CatCreateFunction FunAgg "max" [ScalarType "timetz"]
>           (ScalarType "timetz")
>           False,
>         CatCreateFunction FunAgg "max" [ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunAgg "max" [Pseudo AnyArray] (Pseudo AnyArray)
>           False,
>         CatCreateFunction FunAgg "max" [Pseudo AnyEnum] (Pseudo AnyEnum)
>           False,
>         CatCreateFunction FunAgg "min" [ScalarType "int8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunAgg "min" [ScalarType "int2"]
>           (ScalarType "int2")
>           False,
>         CatCreateFunction FunAgg "min" [ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunAgg "min" [ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunAgg "min" [ScalarType "oid"]
>           (ScalarType "oid")
>           False,
>         CatCreateFunction FunAgg "min" [ScalarType "tid"]
>           (ScalarType "tid")
>           False,
>         CatCreateFunction FunAgg "min" [ScalarType "float4"]
>           (ScalarType "float4")
>           False,
>         CatCreateFunction FunAgg "min" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunAgg "min" [ScalarType "abstime"]
>           (ScalarType "abstime")
>           False,
>         CatCreateFunction FunAgg "min" [ScalarType "money"]
>           (ScalarType "money")
>           False,
>         CatCreateFunction FunAgg "min" [ScalarType "bpchar"]
>           (ScalarType "bpchar")
>           False,
>         CatCreateFunction FunAgg "min" [ScalarType "date"]
>           (ScalarType "date")
>           False,
>         CatCreateFunction FunAgg "min" [ScalarType "time"]
>           (ScalarType "time")
>           False,
>         CatCreateFunction FunAgg "min" [ScalarType "timestamp"]
>           (ScalarType "timestamp")
>           False,
>         CatCreateFunction FunAgg "min" [ScalarType "timestamptz"]
>           (ScalarType "timestamptz")
>           False,
>         CatCreateFunction FunAgg "min" [ScalarType "interval"]
>           (ScalarType "interval")
>           False,
>         CatCreateFunction FunAgg "min" [ScalarType "timetz"]
>           (ScalarType "timetz")
>           False,
>         CatCreateFunction FunAgg "min" [ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunAgg "min" [Pseudo AnyArray] (Pseudo AnyArray)
>           False,
>         CatCreateFunction FunAgg "min" [Pseudo AnyEnum] (Pseudo AnyEnum)
>           False,
>         CatCreateFunction FunAgg "regr_avgx"
>           [ScalarType "float8", ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunAgg "regr_avgy"
>           [ScalarType "float8", ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunAgg "regr_count"
>           [ScalarType "float8", ScalarType "float8"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunAgg "regr_intercept"
>           [ScalarType "float8", ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunAgg "regr_r2"
>           [ScalarType "float8", ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunAgg "regr_slope"
>           [ScalarType "float8", ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunAgg "regr_sxx"
>           [ScalarType "float8", ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunAgg "regr_sxy"
>           [ScalarType "float8", ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunAgg "regr_syy"
>           [ScalarType "float8", ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunAgg "stddev" [ScalarType "int8"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunAgg "stddev" [ScalarType "int2"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunAgg "stddev" [ScalarType "int4"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunAgg "stddev" [ScalarType "float4"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunAgg "stddev" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunAgg "stddev" [ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunAgg "stddev_pop" [ScalarType "int8"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunAgg "stddev_pop" [ScalarType "int2"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunAgg "stddev_pop" [ScalarType "int4"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunAgg "stddev_pop" [ScalarType "float4"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunAgg "stddev_pop" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunAgg "stddev_pop" [ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunAgg "stddev_samp" [ScalarType "int8"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunAgg "stddev_samp" [ScalarType "int2"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunAgg "stddev_samp" [ScalarType "int4"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunAgg "stddev_samp" [ScalarType "float4"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunAgg "stddev_samp" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunAgg "stddev_samp" [ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunAgg "string_agg"
>           [ScalarType "text", ScalarType "text"]
>           (ScalarType "text")
>           False,
>         CatCreateFunction FunAgg "sum" [ScalarType "int8"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunAgg "sum" [ScalarType "int2"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunAgg "sum" [ScalarType "int4"]
>           (ScalarType "int8")
>           False,
>         CatCreateFunction FunAgg "sum" [ScalarType "float4"]
>           (ScalarType "float4")
>           False,
>         CatCreateFunction FunAgg "sum" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunAgg "sum" [ScalarType "money"]
>           (ScalarType "money")
>           False,
>         CatCreateFunction FunAgg "sum" [ScalarType "interval"]
>           (ScalarType "interval")
>           False,
>         CatCreateFunction FunAgg "sum" [ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunAgg "var_pop" [ScalarType "int8"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunAgg "var_pop" [ScalarType "int2"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunAgg "var_pop" [ScalarType "int4"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunAgg "var_pop" [ScalarType "float4"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunAgg "var_pop" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunAgg "var_pop" [ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunAgg "var_samp" [ScalarType "int8"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunAgg "var_samp" [ScalarType "int2"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunAgg "var_samp" [ScalarType "int4"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunAgg "var_samp" [ScalarType "float4"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunAgg "var_samp" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunAgg "var_samp" [ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunAgg "variance" [ScalarType "int8"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunAgg "variance" [ScalarType "int2"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunAgg "variance" [ScalarType "int4"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunAgg "variance" [ScalarType "float4"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunAgg "variance" [ScalarType "float8"]
>           (ScalarType "float8")
>           False,
>         CatCreateFunction FunAgg "variance" [ScalarType "numeric"]
>           (ScalarType "numeric")
>           False,
>         CatCreateFunction FunAgg "xmlagg" [ScalarType "xml"]
>           (ScalarType "xml")
>           False,
>         CatCreateFunction FunWindow "cume_dist" [] (ScalarType "float8")
>           False,
>         CatCreateFunction FunWindow "dense_rank" [] (ScalarType "int8")
>           False,
>         CatCreateFunction FunWindow "first_value" [Pseudo AnyElement]
>           (Pseudo AnyElement)
>           False,
>         CatCreateFunction FunWindow "lag" [Pseudo AnyElement]
>           (Pseudo AnyElement)
>           False,
>         CatCreateFunction FunWindow "lag"
>           [Pseudo AnyElement, ScalarType "int4"]
>           (Pseudo AnyElement)
>           False,
>         CatCreateFunction FunWindow "lag"
>           [Pseudo AnyElement, ScalarType "int4", Pseudo AnyElement]
>           (Pseudo AnyElement)
>           False,
>         CatCreateFunction FunWindow "last_value" [Pseudo AnyElement]
>           (Pseudo AnyElement)
>           False,
>         CatCreateFunction FunWindow "lead" [Pseudo AnyElement]
>           (Pseudo AnyElement)
>           False,
>         CatCreateFunction FunWindow "lead"
>           [Pseudo AnyElement, ScalarType "int4"]
>           (Pseudo AnyElement)
>           False,
>         CatCreateFunction FunWindow "lead"
>           [Pseudo AnyElement, ScalarType "int4", Pseudo AnyElement]
>           (Pseudo AnyElement)
>           False,
>         CatCreateFunction FunWindow "nth_value"
>           [Pseudo AnyElement, ScalarType "int4"]
>           (Pseudo AnyElement)
>           False,
>         CatCreateFunction FunWindow "ntile" [ScalarType "int4"]
>           (ScalarType "int4")
>           False,
>         CatCreateFunction FunWindow "percent_rank" [] (ScalarType "float8")
>           False,
>         CatCreateFunction FunWindow "rank" [] (ScalarType "int8") False,
>         CatCreateFunction FunWindow "row_number" [] (ScalarType "int8")
>           False,
>         CatCreateTable "pg_aggregate"
>           [("aggfnoid", ScalarType "regproc"),
>            ("aggtransfn", ScalarType "regproc"),
>            ("aggfinalfn", ScalarType "regproc"),
>            ("aggsortop", ScalarType "oid"),
>            ("aggtranstype", ScalarType "oid"),
>            ("agginitval", ScalarType "text")]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_am"
>           [("amname", ScalarType "name"),
>            ("amstrategies", ScalarType "int2"),
>            ("amsupport", ScalarType "int2"),
>            ("amcanorder", ScalarType "bool"),
>            ("amcanorderbyop", ScalarType "bool"),
>            ("amcanbackward", ScalarType "bool"),
>            ("amcanunique", ScalarType "bool"),
>            ("amcanmulticol", ScalarType "bool"),
>            ("amoptionalkey", ScalarType "bool"),
>            ("amsearchnulls", ScalarType "bool"),
>            ("amstorage", ScalarType "bool"),
>            ("amclusterable", ScalarType "bool"),
>            ("ampredlocks", ScalarType "bool"),
>            ("amkeytype", ScalarType "oid"),
>            ("aminsert", ScalarType "regproc"),
>            ("ambeginscan", ScalarType "regproc"),
>            ("amgettuple", ScalarType "regproc"),
>            ("amgetbitmap", ScalarType "regproc"),
>            ("amrescan", ScalarType "regproc"),
>            ("amendscan", ScalarType "regproc"),
>            ("ammarkpos", ScalarType "regproc"),
>            ("amrestrpos", ScalarType "regproc"),
>            ("ambuild", ScalarType "regproc"),
>            ("ambuildempty", ScalarType "regproc"),
>            ("ambulkdelete", ScalarType "regproc"),
>            ("amvacuumcleanup", ScalarType "regproc"),
>            ("amcostestimate", ScalarType "regproc"),
>            ("amoptions", ScalarType "regproc")]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("oid", ScalarType "oid"),
>            ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_amop"
>           [("amopfamily", ScalarType "oid"),
>            ("amoplefttype", ScalarType "oid"),
>            ("amoprighttype", ScalarType "oid"),
>            ("amopstrategy", ScalarType "int2"),
>            ("amoppurpose", ScalarType "char"), ("amopopr", ScalarType "oid"),
>            ("amopmethod", ScalarType "oid"),
>            ("amopsortfamily", ScalarType "oid")]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("oid", ScalarType "oid"),
>            ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_amproc"
>           [("amprocfamily", ScalarType "oid"),
>            ("amproclefttype", ScalarType "oid"),
>            ("amprocrighttype", ScalarType "oid"),
>            ("amprocnum", ScalarType "int2"), ("amproc", ScalarType "regproc")]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("oid", ScalarType "oid"),
>            ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_attrdef"
>           [("adrelid", ScalarType "oid"), ("adnum", ScalarType "int2"),
>            ("adbin", ScalarType "pg_node_tree"), ("adsrc", ScalarType "text")]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("oid", ScalarType "oid"),
>            ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_attribute"
>           [("attrelid", ScalarType "oid"), ("attname", ScalarType "name"),
>            ("atttypid", ScalarType "oid"),
>            ("attstattarget", ScalarType "int4"),
>            ("attlen", ScalarType "int2"), ("attnum", ScalarType "int2"),
>            ("attndims", ScalarType "int4"),
>            ("attcacheoff", ScalarType "int4"),
>            ("atttypmod", ScalarType "int4"), ("attbyval", ScalarType "bool"),
>            ("attstorage", ScalarType "char"), ("attalign", ScalarType "char"),
>            ("attnotnull", ScalarType "bool"),
>            ("atthasdef", ScalarType "bool"),
>            ("attisdropped", ScalarType "bool"),
>            ("attislocal", ScalarType "bool"),
>            ("attinhcount", ScalarType "int4"),
>            ("attcollation", ScalarType "oid"),
>            ("attacl", ArrayType (ScalarType "aclitem")),
>            ("attoptions", ArrayType (ScalarType "text"))]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_auth_members"
>           [("roleid", ScalarType "oid"), ("member", ScalarType "oid"),
>            ("grantor", ScalarType "oid"), ("admin_option", ScalarType "bool")]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_authid"
>           [("rolname", ScalarType "name"), ("rolsuper", ScalarType "bool"),
>            ("rolinherit", ScalarType "bool"),
>            ("rolcreaterole", ScalarType "bool"),
>            ("rolcreatedb", ScalarType "bool"),
>            ("rolcatupdate", ScalarType "bool"),
>            ("rolcanlogin", ScalarType "bool"),
>            ("rolreplication", ScalarType "bool"),
>            ("rolconnlimit", ScalarType "int4"),
>            ("rolpassword", ScalarType "text"),
>            ("rolvaliduntil", ScalarType "timestamptz")]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("oid", ScalarType "oid"),
>            ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_cast"
>           [("castsource", ScalarType "oid"),
>            ("casttarget", ScalarType "oid"), ("castfunc", ScalarType "oid"),
>            ("castcontext", ScalarType "char"),
>            ("castmethod", ScalarType "char")]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("oid", ScalarType "oid"),
>            ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_class"
>           [("relname", ScalarType "name"),
>            ("relnamespace", ScalarType "oid"), ("reltype", ScalarType "oid"),
>            ("reloftype", ScalarType "oid"), ("relowner", ScalarType "oid"),
>            ("relam", ScalarType "oid"), ("relfilenode", ScalarType "oid"),
>            ("reltablespace", ScalarType "oid"),
>            ("relpages", ScalarType "int4"),
>            ("reltuples", ScalarType "float4"),
>            ("reltoastrelid", ScalarType "oid"),
>            ("reltoastidxid", ScalarType "oid"),
>            ("relhasindex", ScalarType "bool"),
>            ("relisshared", ScalarType "bool"),
>            ("relpersistence", ScalarType "char"),
>            ("relkind", ScalarType "char"), ("relnatts", ScalarType "int2"),
>            ("relchecks", ScalarType "int2"),
>            ("relhasoids", ScalarType "bool"),
>            ("relhaspkey", ScalarType "bool"),
>            ("relhasrules", ScalarType "bool"),
>            ("relhastriggers", ScalarType "bool"),
>            ("relhassubclass", ScalarType "bool"),
>            ("relfrozenxid", ScalarType "xid"),
>            ("relacl", ArrayType (ScalarType "aclitem")),
>            ("reloptions", ArrayType (ScalarType "text"))]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("oid", ScalarType "oid"),
>            ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_collation"
>           [("collname", ScalarType "name"),
>            ("collnamespace", ScalarType "oid"),
>            ("collowner", ScalarType "oid"),
>            ("collencoding", ScalarType "int4"),
>            ("collcollate", ScalarType "name"),
>            ("collctype", ScalarType "name")]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("oid", ScalarType "oid"),
>            ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_constraint"
>           [("conname", ScalarType "name"),
>            ("connamespace", ScalarType "oid"), ("contype", ScalarType "char"),
>            ("condeferrable", ScalarType "bool"),
>            ("condeferred", ScalarType "bool"),
>            ("convalidated", ScalarType "bool"),
>            ("conrelid", ScalarType "oid"), ("contypid", ScalarType "oid"),
>            ("conindid", ScalarType "oid"), ("confrelid", ScalarType "oid"),
>            ("confupdtype", ScalarType "char"),
>            ("confdeltype", ScalarType "char"),
>            ("confmatchtype", ScalarType "char"),
>            ("conislocal", ScalarType "bool"),
>            ("coninhcount", ScalarType "int4"),
>            ("conkey", ArrayType (ScalarType "int2")),
>            ("confkey", ArrayType (ScalarType "int2")),
>            ("conpfeqop", ArrayType (ScalarType "oid")),
>            ("conppeqop", ArrayType (ScalarType "oid")),
>            ("conffeqop", ArrayType (ScalarType "oid")),
>            ("conexclop", ArrayType (ScalarType "oid")),
>            ("conbin", ScalarType "pg_node_tree"),
>            ("consrc", ScalarType "text")]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("oid", ScalarType "oid"),
>            ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_conversion"
>           [("conname", ScalarType "name"),
>            ("connamespace", ScalarType "oid"), ("conowner", ScalarType "oid"),
>            ("conforencoding", ScalarType "int4"),
>            ("contoencoding", ScalarType "int4"),
>            ("conproc", ScalarType "regproc"),
>            ("condefault", ScalarType "bool")]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("oid", ScalarType "oid"),
>            ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_database"
>           [("datname", ScalarType "name"), ("datdba", ScalarType "oid"),
>            ("encoding", ScalarType "int4"), ("datcollate", ScalarType "name"),
>            ("datctype", ScalarType "name"),
>            ("datistemplate", ScalarType "bool"),
>            ("datallowconn", ScalarType "bool"),
>            ("datconnlimit", ScalarType "int4"),
>            ("datlastsysoid", ScalarType "oid"),
>            ("datfrozenxid", ScalarType "xid"),
>            ("dattablespace", ScalarType "oid"),
>            ("datacl", ArrayType (ScalarType "aclitem"))]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("oid", ScalarType "oid"),
>            ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_db_role_setting"
>           [("setdatabase", ScalarType "oid"), ("setrole", ScalarType "oid"),
>            ("setconfig", ArrayType (ScalarType "text"))]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_default_acl"
>           [("defaclrole", ScalarType "oid"),
>            ("defaclnamespace", ScalarType "oid"),
>            ("defaclobjtype", ScalarType "char"),
>            ("defaclacl", ArrayType (ScalarType "aclitem"))]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("oid", ScalarType "oid"),
>            ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_depend"
>           [("classid", ScalarType "oid"), ("objid", ScalarType "oid"),
>            ("objsubid", ScalarType "int4"), ("refclassid", ScalarType "oid"),
>            ("refobjid", ScalarType "oid"), ("refobjsubid", ScalarType "int4"),
>            ("deptype", ScalarType "char")]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_description"
>           [("objoid", ScalarType "oid"), ("classoid", ScalarType "oid"),
>            ("objsubid", ScalarType "int4"),
>            ("description", ScalarType "text")]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_enum"
>           [("enumtypid", ScalarType "oid"),
>            ("enumsortorder", ScalarType "float4"),
>            ("enumlabel", ScalarType "name")]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("oid", ScalarType "oid"),
>            ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_extension"
>           [("extname", ScalarType "name"), ("extowner", ScalarType "oid"),
>            ("extnamespace", ScalarType "oid"),
>            ("extrelocatable", ScalarType "bool"),
>            ("extversion", ScalarType "text"),
>            ("extconfig", ArrayType (ScalarType "oid")),
>            ("extcondition", ArrayType (ScalarType "text"))]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("oid", ScalarType "oid"),
>            ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_foreign_data_wrapper"
>           [("fdwname", ScalarType "name"), ("fdwowner", ScalarType "oid"),
>            ("fdwhandler", ScalarType "oid"),
>            ("fdwvalidator", ScalarType "oid"),
>            ("fdwacl", ArrayType (ScalarType "aclitem")),
>            ("fdwoptions", ArrayType (ScalarType "text"))]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("oid", ScalarType "oid"),
>            ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_foreign_server"
>           [("srvname", ScalarType "name"), ("srvowner", ScalarType "oid"),
>            ("srvfdw", ScalarType "oid"), ("srvtype", ScalarType "text"),
>            ("srvversion", ScalarType "text"),
>            ("srvacl", ArrayType (ScalarType "aclitem")),
>            ("srvoptions", ArrayType (ScalarType "text"))]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("oid", ScalarType "oid"),
>            ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_foreign_table"
>           [("ftrelid", ScalarType "oid"), ("ftserver", ScalarType "oid"),
>            ("ftoptions", ArrayType (ScalarType "text"))]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_index"
>           [("indexrelid", ScalarType "oid"), ("indrelid", ScalarType "oid"),
>            ("indnatts", ScalarType "int2"),
>            ("indisunique", ScalarType "bool"),
>            ("indisprimary", ScalarType "bool"),
>            ("indisexclusion", ScalarType "bool"),
>            ("indimmediate", ScalarType "bool"),
>            ("indisclustered", ScalarType "bool"),
>            ("indisvalid", ScalarType "bool"),
>            ("indcheckxmin", ScalarType "bool"),
>            ("indisready", ScalarType "bool"),
>            ("indkey", ScalarType "int2vector"),
>            ("indcollation", ScalarType "oidvector"),
>            ("indclass", ScalarType "oidvector"),
>            ("indoption", ScalarType "int2vector"),
>            ("indexprs", ScalarType "pg_node_tree"),
>            ("indpred", ScalarType "pg_node_tree")]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_inherits"
>           [("inhrelid", ScalarType "oid"), ("inhparent", ScalarType "oid"),
>            ("inhseqno", ScalarType "int4")]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_language"
>           [("lanname", ScalarType "name"), ("lanowner", ScalarType "oid"),
>            ("lanispl", ScalarType "bool"),
>            ("lanpltrusted", ScalarType "bool"),
>            ("lanplcallfoid", ScalarType "oid"),
>            ("laninline", ScalarType "oid"),
>            ("lanvalidator", ScalarType "oid"),
>            ("lanacl", ArrayType (ScalarType "aclitem"))]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("oid", ScalarType "oid"),
>            ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_largeobject"
>           [("loid", ScalarType "oid"), ("pageno", ScalarType "int4"),
>            ("data", ScalarType "bytea")]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_largeobject_metadata"
>           [("lomowner", ScalarType "oid"),
>            ("lomacl", ArrayType (ScalarType "aclitem"))]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("oid", ScalarType "oid"),
>            ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_namespace"
>           [("nspname", ScalarType "name"), ("nspowner", ScalarType "oid"),
>            ("nspacl", ArrayType (ScalarType "aclitem"))]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("oid", ScalarType "oid"),
>            ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_opclass"
>           [("opcmethod", ScalarType "oid"), ("opcname", ScalarType "name"),
>            ("opcnamespace", ScalarType "oid"), ("opcowner", ScalarType "oid"),
>            ("opcfamily", ScalarType "oid"), ("opcintype", ScalarType "oid"),
>            ("opcdefault", ScalarType "bool"),
>            ("opckeytype", ScalarType "oid")]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("oid", ScalarType "oid"),
>            ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_operator"
>           [("oprname", ScalarType "name"),
>            ("oprnamespace", ScalarType "oid"), ("oprowner", ScalarType "oid"),
>            ("oprkind", ScalarType "char"), ("oprcanmerge", ScalarType "bool"),
>            ("oprcanhash", ScalarType "bool"), ("oprleft", ScalarType "oid"),
>            ("oprright", ScalarType "oid"), ("oprresult", ScalarType "oid"),
>            ("oprcom", ScalarType "oid"), ("oprnegate", ScalarType "oid"),
>            ("oprcode", ScalarType "regproc"),
>            ("oprrest", ScalarType "regproc"),
>            ("oprjoin", ScalarType "regproc")]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("oid", ScalarType "oid"),
>            ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_opfamily"
>           [("opfmethod", ScalarType "oid"), ("opfname", ScalarType "name"),
>            ("opfnamespace", ScalarType "oid"), ("opfowner", ScalarType "oid")]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("oid", ScalarType "oid"),
>            ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_pltemplate"
>           [("tmplname", ScalarType "name"),
>            ("tmpltrusted", ScalarType "bool"),
>            ("tmpldbacreate", ScalarType "bool"),
>            ("tmplhandler", ScalarType "text"),
>            ("tmplinline", ScalarType "text"),
>            ("tmplvalidator", ScalarType "text"),
>            ("tmpllibrary", ScalarType "text"),
>            ("tmplacl", ArrayType (ScalarType "aclitem"))]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_proc"
>           [("proname", ScalarType "name"),
>            ("pronamespace", ScalarType "oid"), ("proowner", ScalarType "oid"),
>            ("prolang", ScalarType "oid"), ("procost", ScalarType "float4"),
>            ("prorows", ScalarType "float4"),
>            ("provariadic", ScalarType "oid"), ("proisagg", ScalarType "bool"),
>            ("proiswindow", ScalarType "bool"),
>            ("prosecdef", ScalarType "bool"),
>            ("proisstrict", ScalarType "bool"),
>            ("proretset", ScalarType "bool"),
>            ("provolatile", ScalarType "char"),
>            ("pronargs", ScalarType "int2"),
>            ("pronargdefaults", ScalarType "int2"),
>            ("prorettype", ScalarType "oid"),
>            ("proargtypes", ScalarType "oidvector"),
>            ("proallargtypes", ArrayType (ScalarType "oid")),
>            ("proargmodes", ArrayType (ScalarType "char")),
>            ("proargnames", ArrayType (ScalarType "text")),
>            ("proargdefaults", ScalarType "pg_node_tree"),
>            ("prosrc", ScalarType "text"), ("probin", ScalarType "text"),
>            ("proconfig", ArrayType (ScalarType "text")),
>            ("proacl", ArrayType (ScalarType "aclitem"))]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("oid", ScalarType "oid"),
>            ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_rewrite"
>           [("rulename", ScalarType "name"), ("ev_class", ScalarType "oid"),
>            ("ev_attr", ScalarType "int2"), ("ev_type", ScalarType "char"),
>            ("ev_enabled", ScalarType "char"),
>            ("is_instead", ScalarType "bool"),
>            ("ev_qual", ScalarType "pg_node_tree"),
>            ("ev_action", ScalarType "pg_node_tree")]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("oid", ScalarType "oid"),
>            ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_seclabel"
>           [("objoid", ScalarType "oid"), ("classoid", ScalarType "oid"),
>            ("objsubid", ScalarType "int4"), ("provider", ScalarType "text"),
>            ("label", ScalarType "text")]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_shdepend"
>           [("dbid", ScalarType "oid"), ("classid", ScalarType "oid"),
>            ("objid", ScalarType "oid"), ("objsubid", ScalarType "int4"),
>            ("refclassid", ScalarType "oid"), ("refobjid", ScalarType "oid"),
>            ("deptype", ScalarType "char")]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_shdescription"
>           [("objoid", ScalarType "oid"), ("classoid", ScalarType "oid"),
>            ("description", ScalarType "text")]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_statistic"
>           [("starelid", ScalarType "oid"), ("staattnum", ScalarType "int2"),
>            ("stainherit", ScalarType "bool"),
>            ("stanullfrac", ScalarType "float4"),
>            ("stawidth", ScalarType "int4"),
>            ("stadistinct", ScalarType "float4"),
>            ("stakind1", ScalarType "int2"), ("stakind2", ScalarType "int2"),
>            ("stakind3", ScalarType "int2"), ("stakind4", ScalarType "int2"),
>            ("staop1", ScalarType "oid"), ("staop2", ScalarType "oid"),
>            ("staop3", ScalarType "oid"), ("staop4", ScalarType "oid"),
>            ("stanumbers1", ArrayType (ScalarType "float4")),
>            ("stanumbers2", ArrayType (ScalarType "float4")),
>            ("stanumbers3", ArrayType (ScalarType "float4")),
>            ("stanumbers4", ArrayType (ScalarType "float4")),
>            ("stavalues1", Pseudo AnyArray), ("stavalues2", Pseudo AnyArray),
>            ("stavalues3", Pseudo AnyArray), ("stavalues4", Pseudo AnyArray)]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_tablespace"
>           [("spcname", ScalarType "name"), ("spcowner", ScalarType "oid"),
>            ("spclocation", ScalarType "text"),
>            ("spcacl", ArrayType (ScalarType "aclitem")),
>            ("spcoptions", ArrayType (ScalarType "text"))]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("oid", ScalarType "oid"),
>            ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_trigger"
>           [("tgrelid", ScalarType "oid"), ("tgname", ScalarType "name"),
>            ("tgfoid", ScalarType "oid"), ("tgtype", ScalarType "int2"),
>            ("tgenabled", ScalarType "char"),
>            ("tgisinternal", ScalarType "bool"),
>            ("tgconstrrelid", ScalarType "oid"),
>            ("tgconstrindid", ScalarType "oid"),
>            ("tgconstraint", ScalarType "oid"),
>            ("tgdeferrable", ScalarType "bool"),
>            ("tginitdeferred", ScalarType "bool"),
>            ("tgnargs", ScalarType "int2"),
>            ("tgattr", ScalarType "int2vector"),
>            ("tgargs", ScalarType "bytea"),
>            ("tgqual", ScalarType "pg_node_tree")]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("oid", ScalarType "oid"),
>            ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_ts_config"
>           [("cfgname", ScalarType "name"),
>            ("cfgnamespace", ScalarType "oid"), ("cfgowner", ScalarType "oid"),
>            ("cfgparser", ScalarType "oid")]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("oid", ScalarType "oid"),
>            ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_ts_config_map"
>           [("mapcfg", ScalarType "oid"), ("maptokentype", ScalarType "int4"),
>            ("mapseqno", ScalarType "int4"), ("mapdict", ScalarType "oid")]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_ts_dict"
>           [("dictname", ScalarType "name"),
>            ("dictnamespace", ScalarType "oid"),
>            ("dictowner", ScalarType "oid"),
>            ("dicttemplate", ScalarType "oid"),
>            ("dictinitoption", ScalarType "text")]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("oid", ScalarType "oid"),
>            ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_ts_parser"
>           [("prsname", ScalarType "name"),
>            ("prsnamespace", ScalarType "oid"),
>            ("prsstart", ScalarType "regproc"),
>            ("prstoken", ScalarType "regproc"),
>            ("prsend", ScalarType "regproc"),
>            ("prsheadline", ScalarType "regproc"),
>            ("prslextype", ScalarType "regproc")]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("oid", ScalarType "oid"),
>            ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_ts_template"
>           [("tmplname", ScalarType "name"),
>            ("tmplnamespace", ScalarType "oid"),
>            ("tmplinit", ScalarType "regproc"),
>            ("tmpllexize", ScalarType "regproc")]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("oid", ScalarType "oid"),
>            ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_type"
>           [("typname", ScalarType "name"),
>            ("typnamespace", ScalarType "oid"), ("typowner", ScalarType "oid"),
>            ("typlen", ScalarType "int2"), ("typbyval", ScalarType "bool"),
>            ("typtype", ScalarType "char"), ("typcategory", ScalarType "char"),
>            ("typispreferred", ScalarType "bool"),
>            ("typisdefined", ScalarType "bool"),
>            ("typdelim", ScalarType "char"), ("typrelid", ScalarType "oid"),
>            ("typelem", ScalarType "oid"), ("typarray", ScalarType "oid"),
>            ("typinput", ScalarType "regproc"),
>            ("typoutput", ScalarType "regproc"),
>            ("typreceive", ScalarType "regproc"),
>            ("typsend", ScalarType "regproc"),
>            ("typmodin", ScalarType "regproc"),
>            ("typmodout", ScalarType "regproc"),
>            ("typanalyze", ScalarType "regproc"),
>            ("typalign", ScalarType "char"), ("typstorage", ScalarType "char"),
>            ("typnotnull", ScalarType "bool"),
>            ("typbasetype", ScalarType "oid"),
>            ("typtypmod", ScalarType "int4"), ("typndims", ScalarType "int4"),
>            ("typcollation", ScalarType "oid"),
>            ("typdefaultbin", ScalarType "pg_node_tree"),
>            ("typdefault", ScalarType "text")]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("oid", ScalarType "oid"),
>            ("ctid", ScalarType "tid")],
>         CatCreateTable "pg_user_mapping"
>           [("umuser", ScalarType "oid"), ("umserver", ScalarType "oid"),
>            ("umoptions", ArrayType (ScalarType "text"))]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("oid", ScalarType "oid"),
>            ("ctid", ScalarType "tid")],
>         CatCreateTable "information_schema.sql_features"
>           [("feature_id", DomainType "information_schema.character_data"),
>            ("feature_name", DomainType "information_schema.character_data"),
>            ("sub_feature_id", DomainType "information_schema.character_data"),
>            ("sub_feature_name",
>             DomainType "information_schema.character_data"),
>            ("is_supported", DomainType "information_schema.yes_or_no"),
>            ("is_verified_by", DomainType "information_schema.character_data"),
>            ("comments", DomainType "information_schema.character_data")]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("ctid", ScalarType "tid")],
>         CatCreateTable "information_schema.sql_implementation_info"
>           [("implementation_info_id",
>             DomainType "information_schema.character_data"),
>            ("implementation_info_name",
>             DomainType "information_schema.character_data"),
>            ("integer_value", DomainType "information_schema.cardinal_number"),
>            ("character_value",
>             DomainType "information_schema.character_data"),
>            ("comments", DomainType "information_schema.character_data")]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("ctid", ScalarType "tid")],
>         CatCreateTable "information_schema.sql_languages"
>           [("sql_language_source",
>             DomainType "information_schema.character_data"),
>            ("sql_language_year",
>             DomainType "information_schema.character_data"),
>            ("sql_language_conformance",
>             DomainType "information_schema.character_data"),
>            ("sql_language_integrity",
>             DomainType "information_schema.character_data"),
>            ("sql_language_implementation",
>             DomainType "information_schema.character_data"),
>            ("sql_language_binding_style",
>             DomainType "information_schema.character_data"),
>            ("sql_language_programming_language",
>             DomainType "information_schema.character_data")]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("ctid", ScalarType "tid")],
>         CatCreateTable "information_schema.sql_packages"
>           [("feature_id", DomainType "information_schema.character_data"),
>            ("feature_name", DomainType "information_schema.character_data"),
>            ("is_supported", DomainType "information_schema.yes_or_no"),
>            ("is_verified_by", DomainType "information_schema.character_data"),
>            ("comments", DomainType "information_schema.character_data")]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("ctid", ScalarType "tid")],
>         CatCreateTable "information_schema.sql_parts"
>           [("feature_id", DomainType "information_schema.character_data"),
>            ("feature_name", DomainType "information_schema.character_data"),
>            ("is_supported", DomainType "information_schema.yes_or_no"),
>            ("is_verified_by", DomainType "information_schema.character_data"),
>            ("comments", DomainType "information_schema.character_data")]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("ctid", ScalarType "tid")],
>         CatCreateTable "information_schema.sql_sizing"
>           [("sizing_id", DomainType "information_schema.cardinal_number"),
>            ("sizing_name", DomainType "information_schema.character_data"),
>            ("supported_value",
>             DomainType "information_schema.cardinal_number"),
>            ("comments", DomainType "information_schema.character_data")]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("ctid", ScalarType "tid")],
>         CatCreateTable "information_schema.sql_sizing_profiles"
>           [("sizing_id", DomainType "information_schema.cardinal_number"),
>            ("sizing_name", DomainType "information_schema.character_data"),
>            ("profile_id", DomainType "information_schema.character_data"),
>            ("required_value",
>             DomainType "information_schema.cardinal_number"),
>            ("comments", DomainType "information_schema.character_data")]
>           [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
>            ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
>            ("xmin", ScalarType "xid"), ("ctid", ScalarType "tid")],
>         CatCreateView "information_schema._pg_foreign_data_wrappers"
>           [("oid", ScalarType "oid"), ("fdwowner", ScalarType "oid"),
>            ("fdwoptions", ArrayType (ScalarType "text")),
>            ("foreign_data_wrapper_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("foreign_data_wrapper_name",
>             DomainType "information_schema.sql_identifier"),
>            ("authorization_identifier",
>             DomainType "information_schema.sql_identifier"),
>            ("foreign_data_wrapper_language",
>             DomainType "information_schema.character_data")],
>         CatCreateView "information_schema._pg_foreign_servers"
>           [("oid", ScalarType "oid"),
>            ("srvoptions", ArrayType (ScalarType "text")),
>            ("foreign_server_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("foreign_server_name",
>             DomainType "information_schema.sql_identifier"),
>            ("foreign_data_wrapper_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("foreign_data_wrapper_name",
>             DomainType "information_schema.sql_identifier"),
>            ("foreign_server_type",
>             DomainType "information_schema.character_data"),
>            ("foreign_server_version",
>             DomainType "information_schema.character_data"),
>            ("authorization_identifier",
>             DomainType "information_schema.sql_identifier")],
>         CatCreateView "information_schema._pg_foreign_tables"
>           [("foreign_table_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("foreign_table_schema", ScalarType "name"),
>            ("foreign_table_name", ScalarType "name"),
>            ("ftoptions", ArrayType (ScalarType "text")),
>            ("foreign_server_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("foreign_server_name",
>             DomainType "information_schema.sql_identifier"),
>            ("authorization_identifier",
>             DomainType "information_schema.sql_identifier")],
>         CatCreateView "information_schema._pg_user_mappings"
>           [("oid", ScalarType "oid"),
>            ("umoptions", ArrayType (ScalarType "text")),
>            ("umuser", ScalarType "oid"),
>            ("authorization_identifier",
>             DomainType "information_schema.sql_identifier"),
>            ("foreign_server_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("foreign_server_name",
>             DomainType "information_schema.sql_identifier"),
>            ("srvowner", DomainType "information_schema.sql_identifier")],
>         CatCreateView
>           "information_schema.administrable_role_authorizations"
>           [("grantee", DomainType "information_schema.sql_identifier"),
>            ("role_name", DomainType "information_schema.sql_identifier"),
>            ("is_grantable", DomainType "information_schema.yes_or_no")],
>         CatCreateView "information_schema.applicable_roles"
>           [("grantee", DomainType "information_schema.sql_identifier"),
>            ("role_name", DomainType "information_schema.sql_identifier"),
>            ("is_grantable", DomainType "information_schema.yes_or_no")],
>         CatCreateView "information_schema.attributes"
>           [("udt_catalog", DomainType "information_schema.sql_identifier"),
>            ("udt_schema", DomainType "information_schema.sql_identifier"),
>            ("udt_name", DomainType "information_schema.sql_identifier"),
>            ("attribute_name", DomainType "information_schema.sql_identifier"),
>            ("ordinal_position",
>             DomainType "information_schema.cardinal_number"),
>            ("attribute_default",
>             DomainType "information_schema.character_data"),
>            ("is_nullable", DomainType "information_schema.yes_or_no"),
>            ("data_type", DomainType "information_schema.character_data"),
>            ("character_maximum_length",
>             DomainType "information_schema.cardinal_number"),
>            ("character_octet_length",
>             DomainType "information_schema.cardinal_number"),
>            ("character_set_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("character_set_schema",
>             DomainType "information_schema.sql_identifier"),
>            ("character_set_name",
>             DomainType "information_schema.sql_identifier"),
>            ("collation_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("collation_schema",
>             DomainType "information_schema.sql_identifier"),
>            ("collation_name", DomainType "information_schema.sql_identifier"),
>            ("numeric_precision",
>             DomainType "information_schema.cardinal_number"),
>            ("numeric_precision_radix",
>             DomainType "information_schema.cardinal_number"),
>            ("numeric_scale", DomainType "information_schema.cardinal_number"),
>            ("datetime_precision",
>             DomainType "information_schema.cardinal_number"),
>            ("interval_type", DomainType "information_schema.character_data"),
>            ("interval_precision",
>             DomainType "information_schema.character_data"),
>            ("attribute_udt_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("attribute_udt_schema",
>             DomainType "information_schema.sql_identifier"),
>            ("attribute_udt_name",
>             DomainType "information_schema.sql_identifier"),
>            ("scope_catalog", DomainType "information_schema.sql_identifier"),
>            ("scope_schema", DomainType "information_schema.sql_identifier"),
>            ("scope_name", DomainType "information_schema.sql_identifier"),
>            ("maximum_cardinality",
>             DomainType "information_schema.cardinal_number"),
>            ("dtd_identifier", DomainType "information_schema.sql_identifier"),
>            ("is_derived_reference_attribute",
>             DomainType "information_schema.yes_or_no")],
>         CatCreateView "information_schema.character_sets"
>           [("character_set_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("character_set_schema",
>             DomainType "information_schema.sql_identifier"),
>            ("character_set_name",
>             DomainType "information_schema.sql_identifier"),
>            ("character_repertoire",
>             DomainType "information_schema.sql_identifier"),
>            ("form_of_use", DomainType "information_schema.sql_identifier"),
>            ("default_collate_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("default_collate_schema",
>             DomainType "information_schema.sql_identifier"),
>            ("default_collate_name",
>             DomainType "information_schema.sql_identifier")],
>         CatCreateView "information_schema.check_constraint_routine_usage"
>           [("constraint_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("constraint_schema",
>             DomainType "information_schema.sql_identifier"),
>            ("constraint_name",
>             DomainType "information_schema.sql_identifier"),
>            ("specific_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("specific_schema",
>             DomainType "information_schema.sql_identifier"),
>            ("specific_name", DomainType "information_schema.sql_identifier")],
>         CatCreateView "information_schema.check_constraints"
>           [("constraint_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("constraint_schema",
>             DomainType "information_schema.sql_identifier"),
>            ("constraint_name",
>             DomainType "information_schema.sql_identifier"),
>            ("check_clause", DomainType "information_schema.character_data")],
>         CatCreateView
>           "information_schema.collation_character_set_applicability"
>           [("collation_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("collation_schema",
>             DomainType "information_schema.sql_identifier"),
>            ("collation_name", DomainType "information_schema.sql_identifier"),
>            ("character_set_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("character_set_schema",
>             DomainType "information_schema.sql_identifier"),
>            ("character_set_name",
>             DomainType "information_schema.sql_identifier")],
>         CatCreateView "information_schema.collations"
>           [("collation_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("collation_schema",
>             DomainType "information_schema.sql_identifier"),
>            ("collation_name", DomainType "information_schema.sql_identifier"),
>            ("pad_attribute", DomainType "information_schema.character_data")],
>         CatCreateView "information_schema.column_domain_usage"
>           [("domain_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("domain_schema", DomainType "information_schema.sql_identifier"),
>            ("domain_name", DomainType "information_schema.sql_identifier"),
>            ("table_catalog", DomainType "information_schema.sql_identifier"),
>            ("table_schema", DomainType "information_schema.sql_identifier"),
>            ("table_name", DomainType "information_schema.sql_identifier"),
>            ("column_name", DomainType "information_schema.sql_identifier")],
>         CatCreateView "information_schema.column_privileges"
>           [("grantor", DomainType "information_schema.sql_identifier"),
>            ("grantee", DomainType "information_schema.sql_identifier"),
>            ("table_catalog", DomainType "information_schema.sql_identifier"),
>            ("table_schema", DomainType "information_schema.sql_identifier"),
>            ("table_name", DomainType "information_schema.sql_identifier"),
>            ("column_name", DomainType "information_schema.sql_identifier"),
>            ("privilege_type", DomainType "information_schema.character_data"),
>            ("is_grantable", DomainType "information_schema.yes_or_no")],
>         CatCreateView "information_schema.column_udt_usage"
>           [("udt_catalog", DomainType "information_schema.sql_identifier"),
>            ("udt_schema", DomainType "information_schema.sql_identifier"),
>            ("udt_name", DomainType "information_schema.sql_identifier"),
>            ("table_catalog", DomainType "information_schema.sql_identifier"),
>            ("table_schema", DomainType "information_schema.sql_identifier"),
>            ("table_name", DomainType "information_schema.sql_identifier"),
>            ("column_name", DomainType "information_schema.sql_identifier")],
>         CatCreateView "information_schema.columns"
>           [("table_catalog", DomainType "information_schema.sql_identifier"),
>            ("table_schema", DomainType "information_schema.sql_identifier"),
>            ("table_name", DomainType "information_schema.sql_identifier"),
>            ("column_name", DomainType "information_schema.sql_identifier"),
>            ("ordinal_position",
>             DomainType "information_schema.cardinal_number"),
>            ("column_default", DomainType "information_schema.character_data"),
>            ("is_nullable", DomainType "information_schema.yes_or_no"),
>            ("data_type", DomainType "information_schema.character_data"),
>            ("character_maximum_length",
>             DomainType "information_schema.cardinal_number"),
>            ("character_octet_length",
>             DomainType "information_schema.cardinal_number"),
>            ("numeric_precision",
>             DomainType "information_schema.cardinal_number"),
>            ("numeric_precision_radix",
>             DomainType "information_schema.cardinal_number"),
>            ("numeric_scale", DomainType "information_schema.cardinal_number"),
>            ("datetime_precision",
>             DomainType "information_schema.cardinal_number"),
>            ("interval_type", DomainType "information_schema.character_data"),
>            ("interval_precision",
>             DomainType "information_schema.character_data"),
>            ("character_set_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("character_set_schema",
>             DomainType "information_schema.sql_identifier"),
>            ("character_set_name",
>             DomainType "information_schema.sql_identifier"),
>            ("collation_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("collation_schema",
>             DomainType "information_schema.sql_identifier"),
>            ("collation_name", DomainType "information_schema.sql_identifier"),
>            ("domain_catalog", DomainType "information_schema.sql_identifier"),
>            ("domain_schema", DomainType "information_schema.sql_identifier"),
>            ("domain_name", DomainType "information_schema.sql_identifier"),
>            ("udt_catalog", DomainType "information_schema.sql_identifier"),
>            ("udt_schema", DomainType "information_schema.sql_identifier"),
>            ("udt_name", DomainType "information_schema.sql_identifier"),
>            ("scope_catalog", DomainType "information_schema.sql_identifier"),
>            ("scope_schema", DomainType "information_schema.sql_identifier"),
>            ("scope_name", DomainType "information_schema.sql_identifier"),
>            ("maximum_cardinality",
>             DomainType "information_schema.cardinal_number"),
>            ("dtd_identifier", DomainType "information_schema.sql_identifier"),
>            ("is_self_referencing", DomainType "information_schema.yes_or_no"),
>            ("is_identity", DomainType "information_schema.yes_or_no"),
>            ("identity_generation",
>             DomainType "information_schema.character_data"),
>            ("identity_start", DomainType "information_schema.character_data"),
>            ("identity_increment",
>             DomainType "information_schema.character_data"),
>            ("identity_maximum",
>             DomainType "information_schema.character_data"),
>            ("identity_minimum",
>             DomainType "information_schema.character_data"),
>            ("identity_cycle", DomainType "information_schema.yes_or_no"),
>            ("is_generated", DomainType "information_schema.character_data"),
>            ("generation_expression",
>             DomainType "information_schema.character_data"),
>            ("is_updatable", DomainType "information_schema.yes_or_no")],
>         CatCreateView "information_schema.constraint_column_usage"
>           [("table_catalog", DomainType "information_schema.sql_identifier"),
>            ("table_schema", DomainType "information_schema.sql_identifier"),
>            ("table_name", DomainType "information_schema.sql_identifier"),
>            ("column_name", DomainType "information_schema.sql_identifier"),
>            ("constraint_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("constraint_schema",
>             DomainType "information_schema.sql_identifier"),
>            ("constraint_name",
>             DomainType "information_schema.sql_identifier")],
>         CatCreateView "information_schema.constraint_table_usage"
>           [("table_catalog", DomainType "information_schema.sql_identifier"),
>            ("table_schema", DomainType "information_schema.sql_identifier"),
>            ("table_name", DomainType "information_schema.sql_identifier"),
>            ("constraint_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("constraint_schema",
>             DomainType "information_schema.sql_identifier"),
>            ("constraint_name",
>             DomainType "information_schema.sql_identifier")],
>         CatCreateView "information_schema.data_type_privileges"
>           [("object_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("object_schema", DomainType "information_schema.sql_identifier"),
>            ("object_name", DomainType "information_schema.sql_identifier"),
>            ("object_type", DomainType "information_schema.character_data"),
>            ("dtd_identifier",
>             DomainType "information_schema.sql_identifier")],
>         CatCreateView "information_schema.domain_constraints"
>           [("constraint_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("constraint_schema",
>             DomainType "information_schema.sql_identifier"),
>            ("constraint_name",
>             DomainType "information_schema.sql_identifier"),
>            ("domain_catalog", DomainType "information_schema.sql_identifier"),
>            ("domain_schema", DomainType "information_schema.sql_identifier"),
>            ("domain_name", DomainType "information_schema.sql_identifier"),
>            ("is_deferrable", DomainType "information_schema.yes_or_no"),
>            ("initially_deferred", DomainType "information_schema.yes_or_no")],
>         CatCreateView "information_schema.domain_udt_usage"
>           [("udt_catalog", DomainType "information_schema.sql_identifier"),
>            ("udt_schema", DomainType "information_schema.sql_identifier"),
>            ("udt_name", DomainType "information_schema.sql_identifier"),
>            ("domain_catalog", DomainType "information_schema.sql_identifier"),
>            ("domain_schema", DomainType "information_schema.sql_identifier"),
>            ("domain_name", DomainType "information_schema.sql_identifier")],
>         CatCreateView "information_schema.domains"
>           [("domain_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("domain_schema", DomainType "information_schema.sql_identifier"),
>            ("domain_name", DomainType "information_schema.sql_identifier"),
>            ("data_type", DomainType "information_schema.character_data"),
>            ("character_maximum_length",
>             DomainType "information_schema.cardinal_number"),
>            ("character_octet_length",
>             DomainType "information_schema.cardinal_number"),
>            ("character_set_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("character_set_schema",
>             DomainType "information_schema.sql_identifier"),
>            ("character_set_name",
>             DomainType "information_schema.sql_identifier"),
>            ("collation_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("collation_schema",
>             DomainType "information_schema.sql_identifier"),
>            ("collation_name", DomainType "information_schema.sql_identifier"),
>            ("numeric_precision",
>             DomainType "information_schema.cardinal_number"),
>            ("numeric_precision_radix",
>             DomainType "information_schema.cardinal_number"),
>            ("numeric_scale", DomainType "information_schema.cardinal_number"),
>            ("datetime_precision",
>             DomainType "information_schema.cardinal_number"),
>            ("interval_type", DomainType "information_schema.character_data"),
>            ("interval_precision",
>             DomainType "information_schema.character_data"),
>            ("domain_default", DomainType "information_schema.character_data"),
>            ("udt_catalog", DomainType "information_schema.sql_identifier"),
>            ("udt_schema", DomainType "information_schema.sql_identifier"),
>            ("udt_name", DomainType "information_schema.sql_identifier"),
>            ("scope_catalog", DomainType "information_schema.sql_identifier"),
>            ("scope_schema", DomainType "information_schema.sql_identifier"),
>            ("scope_name", DomainType "information_schema.sql_identifier"),
>            ("maximum_cardinality",
>             DomainType "information_schema.cardinal_number"),
>            ("dtd_identifier",
>             DomainType "information_schema.sql_identifier")],
>         CatCreateView "information_schema.element_types"
>           [("object_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("object_schema", DomainType "information_schema.sql_identifier"),
>            ("object_name", DomainType "information_schema.sql_identifier"),
>            ("object_type", DomainType "information_schema.character_data"),
>            ("collection_type_identifier",
>             DomainType "information_schema.sql_identifier"),
>            ("data_type", DomainType "information_schema.character_data"),
>            ("character_maximum_length",
>             DomainType "information_schema.cardinal_number"),
>            ("character_octet_length",
>             DomainType "information_schema.cardinal_number"),
>            ("character_set_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("character_set_schema",
>             DomainType "information_schema.sql_identifier"),
>            ("character_set_name",
>             DomainType "information_schema.sql_identifier"),
>            ("collation_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("collation_schema",
>             DomainType "information_schema.sql_identifier"),
>            ("collation_name", DomainType "information_schema.sql_identifier"),
>            ("numeric_precision",
>             DomainType "information_schema.cardinal_number"),
>            ("numeric_precision_radix",
>             DomainType "information_schema.cardinal_number"),
>            ("numeric_scale", DomainType "information_schema.cardinal_number"),
>            ("datetime_precision",
>             DomainType "information_schema.cardinal_number"),
>            ("interval_type", DomainType "information_schema.character_data"),
>            ("interval_precision",
>             DomainType "information_schema.character_data"),
>            ("domain_default", DomainType "information_schema.character_data"),
>            ("udt_catalog", DomainType "information_schema.sql_identifier"),
>            ("udt_schema", DomainType "information_schema.sql_identifier"),
>            ("udt_name", DomainType "information_schema.sql_identifier"),
>            ("scope_catalog", DomainType "information_schema.sql_identifier"),
>            ("scope_schema", DomainType "information_schema.sql_identifier"),
>            ("scope_name", DomainType "information_schema.sql_identifier"),
>            ("maximum_cardinality",
>             DomainType "information_schema.cardinal_number"),
>            ("dtd_identifier",
>             DomainType "information_schema.sql_identifier")],
>         CatCreateView "information_schema.enabled_roles"
>           [("role_name", DomainType "information_schema.sql_identifier")],
>         CatCreateView "information_schema.foreign_data_wrapper_options"
>           [("foreign_data_wrapper_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("foreign_data_wrapper_name",
>             DomainType "information_schema.sql_identifier"),
>            ("option_name", DomainType "information_schema.sql_identifier"),
>            ("option_value", DomainType "information_schema.character_data")],
>         CatCreateView "information_schema.foreign_data_wrappers"
>           [("foreign_data_wrapper_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("foreign_data_wrapper_name",
>             DomainType "information_schema.sql_identifier"),
>            ("authorization_identifier",
>             DomainType "information_schema.sql_identifier"),
>            ("library_name", DomainType "information_schema.character_data"),
>            ("foreign_data_wrapper_language",
>             DomainType "information_schema.character_data")],
>         CatCreateView "information_schema.foreign_server_options"
>           [("foreign_server_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("foreign_server_name",
>             DomainType "information_schema.sql_identifier"),
>            ("option_name", DomainType "information_schema.sql_identifier"),
>            ("option_value", DomainType "information_schema.character_data")],
>         CatCreateView "information_schema.foreign_servers"
>           [("foreign_server_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("foreign_server_name",
>             DomainType "information_schema.sql_identifier"),
>            ("foreign_data_wrapper_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("foreign_data_wrapper_name",
>             DomainType "information_schema.sql_identifier"),
>            ("foreign_server_type",
>             DomainType "information_schema.character_data"),
>            ("foreign_server_version",
>             DomainType "information_schema.character_data"),
>            ("authorization_identifier",
>             DomainType "information_schema.sql_identifier")],
>         CatCreateView "information_schema.foreign_table_options"
>           [("foreign_table_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("foreign_table_schema", ScalarType "name"),
>            ("foreign_table_name", ScalarType "name"),
>            ("option_name", DomainType "information_schema.sql_identifier"),
>            ("option_value", DomainType "information_schema.character_data")],
>         CatCreateView "information_schema.foreign_tables"
>           [("foreign_table_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("foreign_table_schema", ScalarType "name"),
>            ("foreign_table_name", ScalarType "name"),
>            ("foreign_server_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("foreign_server_name",
>             DomainType "information_schema.sql_identifier")],
>         CatCreateView "information_schema.information_schema_catalog_name"
>           [("catalog_name", DomainType "information_schema.sql_identifier")],
>         CatCreateView "information_schema.key_column_usage"
>           [("constraint_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("constraint_schema",
>             DomainType "information_schema.sql_identifier"),
>            ("constraint_name",
>             DomainType "information_schema.sql_identifier"),
>            ("table_catalog", DomainType "information_schema.sql_identifier"),
>            ("table_schema", DomainType "information_schema.sql_identifier"),
>            ("table_name", DomainType "information_schema.sql_identifier"),
>            ("column_name", DomainType "information_schema.sql_identifier"),
>            ("ordinal_position",
>             DomainType "information_schema.cardinal_number"),
>            ("position_in_unique_constraint",
>             DomainType "information_schema.cardinal_number")],
>         CatCreateView "information_schema.parameters"
>           [("specific_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("specific_schema",
>             DomainType "information_schema.sql_identifier"),
>            ("specific_name", DomainType "information_schema.sql_identifier"),
>            ("ordinal_position",
>             DomainType "information_schema.cardinal_number"),
>            ("parameter_mode", DomainType "information_schema.character_data"),
>            ("is_result", DomainType "information_schema.yes_or_no"),
>            ("as_locator", DomainType "information_schema.yes_or_no"),
>            ("parameter_name", DomainType "information_schema.sql_identifier"),
>            ("data_type", DomainType "information_schema.character_data"),
>            ("character_maximum_length",
>             DomainType "information_schema.cardinal_number"),
>            ("character_octet_length",
>             DomainType "information_schema.cardinal_number"),
>            ("character_set_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("character_set_schema",
>             DomainType "information_schema.sql_identifier"),
>            ("character_set_name",
>             DomainType "information_schema.sql_identifier"),
>            ("collation_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("collation_schema",
>             DomainType "information_schema.sql_identifier"),
>            ("collation_name", DomainType "information_schema.sql_identifier"),
>            ("numeric_precision",
>             DomainType "information_schema.cardinal_number"),
>            ("numeric_precision_radix",
>             DomainType "information_schema.cardinal_number"),
>            ("numeric_scale", DomainType "information_schema.cardinal_number"),
>            ("datetime_precision",
>             DomainType "information_schema.cardinal_number"),
>            ("interval_type", DomainType "information_schema.character_data"),
>            ("interval_precision",
>             DomainType "information_schema.character_data"),
>            ("udt_catalog", DomainType "information_schema.sql_identifier"),
>            ("udt_schema", DomainType "information_schema.sql_identifier"),
>            ("udt_name", DomainType "information_schema.sql_identifier"),
>            ("scope_catalog", DomainType "information_schema.sql_identifier"),
>            ("scope_schema", DomainType "information_schema.sql_identifier"),
>            ("scope_name", DomainType "information_schema.sql_identifier"),
>            ("maximum_cardinality",
>             DomainType "information_schema.cardinal_number"),
>            ("dtd_identifier",
>             DomainType "information_schema.sql_identifier")],
>         CatCreateView "pg_available_extension_versions"
>           [("name", ScalarType "name"), ("version", ScalarType "text"),
>            ("installed", ScalarType "bool"), ("superuser", ScalarType "bool"),
>            ("relocatable", ScalarType "bool"), ("schema", ScalarType "name"),
>            ("requires", ArrayType (ScalarType "name")),
>            ("comment", ScalarType "text")],
>         CatCreateView "pg_available_extensions"
>           [("name", ScalarType "name"),
>            ("default_version", ScalarType "text"),
>            ("installed_version", ScalarType "text"),
>            ("comment", ScalarType "text")],
>         CatCreateView "pg_cursors"
>           [("name", ScalarType "text"), ("statement", ScalarType "text"),
>            ("is_holdable", ScalarType "bool"),
>            ("is_binary", ScalarType "bool"),
>            ("is_scrollable", ScalarType "bool"),
>            ("creation_time", ScalarType "timestamptz")],
>         CatCreateView "pg_group"
>           [("groname", ScalarType "name"), ("grosysid", ScalarType "oid"),
>            ("grolist", ArrayType (ScalarType "oid"))],
>         CatCreateView "pg_indexes"
>           [("schemaname", ScalarType "name"),
>            ("tablename", ScalarType "name"), ("indexname", ScalarType "name"),
>            ("tablespace", ScalarType "name"),
>            ("indexdef", ScalarType "text")],
>         CatCreateView "pg_locks"
>           [("locktype", ScalarType "text"), ("database", ScalarType "oid"),
>            ("relation", ScalarType "oid"), ("page", ScalarType "int4"),
>            ("tuple", ScalarType "int2"), ("virtualxid", ScalarType "text"),
>            ("transactionid", ScalarType "xid"), ("classid", ScalarType "oid"),
>            ("objid", ScalarType "oid"), ("objsubid", ScalarType "int2"),
>            ("virtualtransaction", ScalarType "text"),
>            ("pid", ScalarType "int4"), ("mode", ScalarType "text"),
>            ("granted", ScalarType "bool")],
>         CatCreateView "pg_prepared_statements"
>           [("name", ScalarType "text"), ("statement", ScalarType "text"),
>            ("prepare_time", ScalarType "timestamptz"),
>            ("parameter_types", ArrayType (ScalarType "regtype")),
>            ("from_sql", ScalarType "bool")],
>         CatCreateView "pg_prepared_xacts"
>           [("transaction", ScalarType "xid"), ("gid", ScalarType "text"),
>            ("prepared", ScalarType "timestamptz"),
>            ("owner", ScalarType "name"), ("database", ScalarType "name")],
>         CatCreateView "pg_roles"
>           [("rolname", ScalarType "name"), ("rolsuper", ScalarType "bool"),
>            ("rolinherit", ScalarType "bool"),
>            ("rolcreaterole", ScalarType "bool"),
>            ("rolcreatedb", ScalarType "bool"),
>            ("rolcatupdate", ScalarType "bool"),
>            ("rolcanlogin", ScalarType "bool"),
>            ("rolreplication", ScalarType "bool"),
>            ("rolconnlimit", ScalarType "int4"),
>            ("rolpassword", ScalarType "text"),
>            ("rolvaliduntil", ScalarType "timestamptz"),
>            ("rolconfig", ArrayType (ScalarType "text")),
>            ("oid", ScalarType "oid")],
>         CatCreateView "pg_rules"
>           [("schemaname", ScalarType "name"),
>            ("tablename", ScalarType "name"), ("rulename", ScalarType "name"),
>            ("definition", ScalarType "text")],
>         CatCreateView "pg_seclabels"
>           [("objoid", ScalarType "oid"), ("classoid", ScalarType "oid"),
>            ("objsubid", ScalarType "int4"), ("objtype", ScalarType "text"),
>            ("objnamespace", ScalarType "oid"), ("objname", ScalarType "text"),
>            ("provider", ScalarType "text"), ("label", ScalarType "text")],
>         CatCreateView "pg_settings"
>           [("name", ScalarType "text"), ("setting", ScalarType "text"),
>            ("unit", ScalarType "text"), ("category", ScalarType "text"),
>            ("short_desc", ScalarType "text"),
>            ("extra_desc", ScalarType "text"), ("context", ScalarType "text"),
>            ("vartype", ScalarType "text"), ("source", ScalarType "text"),
>            ("min_val", ScalarType "text"), ("max_val", ScalarType "text"),
>            ("enumvals", ArrayType (ScalarType "text")),
>            ("boot_val", ScalarType "text"), ("reset_val", ScalarType "text"),
>            ("sourcefile", ScalarType "text"),
>            ("sourceline", ScalarType "int4")],
>         CatCreateView "pg_shadow"
>           [("usename", ScalarType "name"), ("usesysid", ScalarType "oid"),
>            ("usecreatedb", ScalarType "bool"),
>            ("usesuper", ScalarType "bool"), ("usecatupd", ScalarType "bool"),
>            ("userepl", ScalarType "bool"), ("passwd", ScalarType "text"),
>            ("valuntil", ScalarType "abstime"),
>            ("useconfig", ArrayType (ScalarType "text"))],
>         CatCreateView "pg_stat_activity"
>           [("datid", ScalarType "oid"), ("datname", ScalarType "name"),
>            ("procpid", ScalarType "int4"), ("usesysid", ScalarType "oid"),
>            ("usename", ScalarType "name"),
>            ("application_name", ScalarType "text"),
>            ("client_addr", ScalarType "inet"),
>            ("client_hostname", ScalarType "text"),
>            ("client_port", ScalarType "int4"),
>            ("backend_start", ScalarType "timestamptz"),
>            ("xact_start", ScalarType "timestamptz"),
>            ("query_start", ScalarType "timestamptz"),
>            ("waiting", ScalarType "bool"),
>            ("current_query", ScalarType "text")],
>         CatCreateView "pg_stat_all_indexes"
>           [("relid", ScalarType "oid"), ("indexrelid", ScalarType "oid"),
>            ("schemaname", ScalarType "name"), ("relname", ScalarType "name"),
>            ("indexrelname", ScalarType "name"),
>            ("idx_scan", ScalarType "int8"),
>            ("idx_tup_read", ScalarType "int8"),
>            ("idx_tup_fetch", ScalarType "int8")],
>         CatCreateView "pg_stat_all_tables"
>           [("relid", ScalarType "oid"), ("schemaname", ScalarType "name"),
>            ("relname", ScalarType "name"), ("seq_scan", ScalarType "int8"),
>            ("seq_tup_read", ScalarType "int8"),
>            ("idx_scan", ScalarType "int8"),
>            ("idx_tup_fetch", ScalarType "int8"),
>            ("n_tup_ins", ScalarType "int8"), ("n_tup_upd", ScalarType "int8"),
>            ("n_tup_del", ScalarType "int8"),
>            ("n_tup_hot_upd", ScalarType "int8"),
>            ("n_live_tup", ScalarType "int8"),
>            ("n_dead_tup", ScalarType "int8"),
>            ("last_vacuum", ScalarType "timestamptz"),
>            ("last_autovacuum", ScalarType "timestamptz"),
>            ("last_analyze", ScalarType "timestamptz"),
>            ("last_autoanalyze", ScalarType "timestamptz"),
>            ("vacuum_count", ScalarType "int8"),
>            ("autovacuum_count", ScalarType "int8"),
>            ("analyze_count", ScalarType "int8"),
>            ("autoanalyze_count", ScalarType "int8")],
>         CatCreateView "pg_stat_bgwriter"
>           [("checkpoints_timed", ScalarType "int8"),
>            ("checkpoints_req", ScalarType "int8"),
>            ("buffers_checkpoint", ScalarType "int8"),
>            ("buffers_clean", ScalarType "int8"),
>            ("maxwritten_clean", ScalarType "int8"),
>            ("buffers_backend", ScalarType "int8"),
>            ("buffers_backend_fsync", ScalarType "int8"),
>            ("buffers_alloc", ScalarType "int8"),
>            ("stats_reset", ScalarType "timestamptz")],
>         CatCreateView "pg_stat_database"
>           [("datid", ScalarType "oid"), ("datname", ScalarType "name"),
>            ("numbackends", ScalarType "int4"),
>            ("xact_commit", ScalarType "int8"),
>            ("xact_rollback", ScalarType "int8"),
>            ("blks_read", ScalarType "int8"), ("blks_hit", ScalarType "int8"),
>            ("tup_returned", ScalarType "int8"),
>            ("tup_fetched", ScalarType "int8"),
>            ("tup_inserted", ScalarType "int8"),
>            ("tup_updated", ScalarType "int8"),
>            ("tup_deleted", ScalarType "int8"),
>            ("conflicts", ScalarType "int8"),
>            ("stats_reset", ScalarType "timestamptz")],
>         CatCreateView "pg_stat_database_conflicts"
>           [("datid", ScalarType "oid"), ("datname", ScalarType "name"),
>            ("confl_tablespace", ScalarType "int8"),
>            ("confl_lock", ScalarType "int8"),
>            ("confl_snapshot", ScalarType "int8"),
>            ("confl_bufferpin", ScalarType "int8"),
>            ("confl_deadlock", ScalarType "int8")],
>         CatCreateView "pg_stat_replication"
>           [("procpid", ScalarType "int4"), ("usesysid", ScalarType "oid"),
>            ("usename", ScalarType "name"),
>            ("application_name", ScalarType "text"),
>            ("client_addr", ScalarType "inet"),
>            ("client_hostname", ScalarType "text"),
>            ("client_port", ScalarType "int4"),
>            ("backend_start", ScalarType "timestamptz"),
>            ("state", ScalarType "text"), ("sent_location", ScalarType "text"),
>            ("write_location", ScalarType "text"),
>            ("flush_location", ScalarType "text"),
>            ("replay_location", ScalarType "text"),
>            ("sync_priority", ScalarType "int4"),
>            ("sync_state", ScalarType "text")],
>         CatCreateView "pg_stat_sys_indexes"
>           [("relid", ScalarType "oid"), ("indexrelid", ScalarType "oid"),
>            ("schemaname", ScalarType "name"), ("relname", ScalarType "name"),
>            ("indexrelname", ScalarType "name"),
>            ("idx_scan", ScalarType "int8"),
>            ("idx_tup_read", ScalarType "int8"),
>            ("idx_tup_fetch", ScalarType "int8")],
>         CatCreateView "pg_stat_sys_tables"
>           [("relid", ScalarType "oid"), ("schemaname", ScalarType "name"),
>            ("relname", ScalarType "name"), ("seq_scan", ScalarType "int8"),
>            ("seq_tup_read", ScalarType "int8"),
>            ("idx_scan", ScalarType "int8"),
>            ("idx_tup_fetch", ScalarType "int8"),
>            ("n_tup_ins", ScalarType "int8"), ("n_tup_upd", ScalarType "int8"),
>            ("n_tup_del", ScalarType "int8"),
>            ("n_tup_hot_upd", ScalarType "int8"),
>            ("n_live_tup", ScalarType "int8"),
>            ("n_dead_tup", ScalarType "int8"),
>            ("last_vacuum", ScalarType "timestamptz"),
>            ("last_autovacuum", ScalarType "timestamptz"),
>            ("last_analyze", ScalarType "timestamptz"),
>            ("last_autoanalyze", ScalarType "timestamptz"),
>            ("vacuum_count", ScalarType "int8"),
>            ("autovacuum_count", ScalarType "int8"),
>            ("analyze_count", ScalarType "int8"),
>            ("autoanalyze_count", ScalarType "int8")],
>         CatCreateView "pg_stat_user_functions"
>           [("funcid", ScalarType "oid"), ("schemaname", ScalarType "name"),
>            ("funcname", ScalarType "name"), ("calls", ScalarType "int8"),
>            ("total_time", ScalarType "int8"),
>            ("self_time", ScalarType "int8")],
>         CatCreateView "pg_stat_user_indexes"
>           [("relid", ScalarType "oid"), ("indexrelid", ScalarType "oid"),
>            ("schemaname", ScalarType "name"), ("relname", ScalarType "name"),
>            ("indexrelname", ScalarType "name"),
>            ("idx_scan", ScalarType "int8"),
>            ("idx_tup_read", ScalarType "int8"),
>            ("idx_tup_fetch", ScalarType "int8")],
>         CatCreateView "pg_stat_user_tables"
>           [("relid", ScalarType "oid"), ("schemaname", ScalarType "name"),
>            ("relname", ScalarType "name"), ("seq_scan", ScalarType "int8"),
>            ("seq_tup_read", ScalarType "int8"),
>            ("idx_scan", ScalarType "int8"),
>            ("idx_tup_fetch", ScalarType "int8"),
>            ("n_tup_ins", ScalarType "int8"), ("n_tup_upd", ScalarType "int8"),
>            ("n_tup_del", ScalarType "int8"),
>            ("n_tup_hot_upd", ScalarType "int8"),
>            ("n_live_tup", ScalarType "int8"),
>            ("n_dead_tup", ScalarType "int8"),
>            ("last_vacuum", ScalarType "timestamptz"),
>            ("last_autovacuum", ScalarType "timestamptz"),
>            ("last_analyze", ScalarType "timestamptz"),
>            ("last_autoanalyze", ScalarType "timestamptz"),
>            ("vacuum_count", ScalarType "int8"),
>            ("autovacuum_count", ScalarType "int8"),
>            ("analyze_count", ScalarType "int8"),
>            ("autoanalyze_count", ScalarType "int8")],
>         CatCreateView "pg_stat_xact_all_tables"
>           [("relid", ScalarType "oid"), ("schemaname", ScalarType "name"),
>            ("relname", ScalarType "name"), ("seq_scan", ScalarType "int8"),
>            ("seq_tup_read", ScalarType "int8"),
>            ("idx_scan", ScalarType "int8"),
>            ("idx_tup_fetch", ScalarType "int8"),
>            ("n_tup_ins", ScalarType "int8"), ("n_tup_upd", ScalarType "int8"),
>            ("n_tup_del", ScalarType "int8"),
>            ("n_tup_hot_upd", ScalarType "int8")],
>         CatCreateView "pg_stat_xact_sys_tables"
>           [("relid", ScalarType "oid"), ("schemaname", ScalarType "name"),
>            ("relname", ScalarType "name"), ("seq_scan", ScalarType "int8"),
>            ("seq_tup_read", ScalarType "int8"),
>            ("idx_scan", ScalarType "int8"),
>            ("idx_tup_fetch", ScalarType "int8"),
>            ("n_tup_ins", ScalarType "int8"), ("n_tup_upd", ScalarType "int8"),
>            ("n_tup_del", ScalarType "int8"),
>            ("n_tup_hot_upd", ScalarType "int8")],
>         CatCreateView "pg_stat_xact_user_functions"
>           [("funcid", ScalarType "oid"), ("schemaname", ScalarType "name"),
>            ("funcname", ScalarType "name"), ("calls", ScalarType "int8"),
>            ("total_time", ScalarType "int8"),
>            ("self_time", ScalarType "int8")],
>         CatCreateView "pg_stat_xact_user_tables"
>           [("relid", ScalarType "oid"), ("schemaname", ScalarType "name"),
>            ("relname", ScalarType "name"), ("seq_scan", ScalarType "int8"),
>            ("seq_tup_read", ScalarType "int8"),
>            ("idx_scan", ScalarType "int8"),
>            ("idx_tup_fetch", ScalarType "int8"),
>            ("n_tup_ins", ScalarType "int8"), ("n_tup_upd", ScalarType "int8"),
>            ("n_tup_del", ScalarType "int8"),
>            ("n_tup_hot_upd", ScalarType "int8")],
>         CatCreateView "pg_statio_all_indexes"
>           [("relid", ScalarType "oid"), ("indexrelid", ScalarType "oid"),
>            ("schemaname", ScalarType "name"), ("relname", ScalarType "name"),
>            ("indexrelname", ScalarType "name"),
>            ("idx_blks_read", ScalarType "int8"),
>            ("idx_blks_hit", ScalarType "int8")],
>         CatCreateView "pg_statio_all_sequences"
>           [("relid", ScalarType "oid"), ("schemaname", ScalarType "name"),
>            ("relname", ScalarType "name"), ("blks_read", ScalarType "int8"),
>            ("blks_hit", ScalarType "int8")],
>         CatCreateView "pg_statio_all_tables"
>           [("relid", ScalarType "oid"), ("schemaname", ScalarType "name"),
>            ("relname", ScalarType "name"),
>            ("heap_blks_read", ScalarType "int8"),
>            ("heap_blks_hit", ScalarType "int8"),
>            ("idx_blks_read", ScalarType "int8"),
>            ("idx_blks_hit", ScalarType "int8"),
>            ("toast_blks_read", ScalarType "int8"),
>            ("toast_blks_hit", ScalarType "int8"),
>            ("tidx_blks_read", ScalarType "int8"),
>            ("tidx_blks_hit", ScalarType "int8")],
>         CatCreateView "pg_statio_sys_indexes"
>           [("relid", ScalarType "oid"), ("indexrelid", ScalarType "oid"),
>            ("schemaname", ScalarType "name"), ("relname", ScalarType "name"),
>            ("indexrelname", ScalarType "name"),
>            ("idx_blks_read", ScalarType "int8"),
>            ("idx_blks_hit", ScalarType "int8")],
>         CatCreateView "pg_statio_sys_sequences"
>           [("relid", ScalarType "oid"), ("schemaname", ScalarType "name"),
>            ("relname", ScalarType "name"), ("blks_read", ScalarType "int8"),
>            ("blks_hit", ScalarType "int8")],
>         CatCreateView "pg_statio_sys_tables"
>           [("relid", ScalarType "oid"), ("schemaname", ScalarType "name"),
>            ("relname", ScalarType "name"),
>            ("heap_blks_read", ScalarType "int8"),
>            ("heap_blks_hit", ScalarType "int8"),
>            ("idx_blks_read", ScalarType "int8"),
>            ("idx_blks_hit", ScalarType "int8"),
>            ("toast_blks_read", ScalarType "int8"),
>            ("toast_blks_hit", ScalarType "int8"),
>            ("tidx_blks_read", ScalarType "int8"),
>            ("tidx_blks_hit", ScalarType "int8")],
>         CatCreateView "pg_statio_user_indexes"
>           [("relid", ScalarType "oid"), ("indexrelid", ScalarType "oid"),
>            ("schemaname", ScalarType "name"), ("relname", ScalarType "name"),
>            ("indexrelname", ScalarType "name"),
>            ("idx_blks_read", ScalarType "int8"),
>            ("idx_blks_hit", ScalarType "int8")],
>         CatCreateView "pg_statio_user_sequences"
>           [("relid", ScalarType "oid"), ("schemaname", ScalarType "name"),
>            ("relname", ScalarType "name"), ("blks_read", ScalarType "int8"),
>            ("blks_hit", ScalarType "int8")],
>         CatCreateView "pg_statio_user_tables"
>           [("relid", ScalarType "oid"), ("schemaname", ScalarType "name"),
>            ("relname", ScalarType "name"),
>            ("heap_blks_read", ScalarType "int8"),
>            ("heap_blks_hit", ScalarType "int8"),
>            ("idx_blks_read", ScalarType "int8"),
>            ("idx_blks_hit", ScalarType "int8"),
>            ("toast_blks_read", ScalarType "int8"),
>            ("toast_blks_hit", ScalarType "int8"),
>            ("tidx_blks_read", ScalarType "int8"),
>            ("tidx_blks_hit", ScalarType "int8")],
>         CatCreateView "pg_stats"
>           [("schemaname", ScalarType "name"),
>            ("tablename", ScalarType "name"), ("attname", ScalarType "name"),
>            ("inherited", ScalarType "bool"),
>            ("null_frac", ScalarType "float4"),
>            ("avg_width", ScalarType "int4"),
>            ("n_distinct", ScalarType "float4"),
>            ("most_common_vals", Pseudo AnyArray),
>            ("most_common_freqs", ArrayType (ScalarType "float4")),
>            ("histogram_bounds", Pseudo AnyArray),
>            ("correlation", ScalarType "float4")],
>         CatCreateView "pg_tables"
>           [("schemaname", ScalarType "name"),
>            ("tablename", ScalarType "name"),
>            ("tableowner", ScalarType "name"),
>            ("tablespace", ScalarType "name"),
>            ("hasindexes", ScalarType "bool"), ("hasrules", ScalarType "bool"),
>            ("hastriggers", ScalarType "bool")],
>         CatCreateView "pg_timezone_abbrevs"
>           [("abbrev", ScalarType "text"),
>            ("utc_offset", ScalarType "interval"),
>            ("is_dst", ScalarType "bool")],
>         CatCreateView "pg_timezone_names"
>           [("name", ScalarType "text"), ("abbrev", ScalarType "text"),
>            ("utc_offset", ScalarType "interval"),
>            ("is_dst", ScalarType "bool")],
>         CatCreateView "pg_user"
>           [("usename", ScalarType "name"), ("usesysid", ScalarType "oid"),
>            ("usecreatedb", ScalarType "bool"),
>            ("usesuper", ScalarType "bool"), ("usecatupd", ScalarType "bool"),
>            ("userepl", ScalarType "bool"), ("passwd", ScalarType "text"),
>            ("valuntil", ScalarType "abstime"),
>            ("useconfig", ArrayType (ScalarType "text"))],
>         CatCreateView "pg_user_mappings"
>           [("umid", ScalarType "oid"), ("srvid", ScalarType "oid"),
>            ("srvname", ScalarType "name"), ("umuser", ScalarType "oid"),
>            ("usename", ScalarType "name"),
>            ("umoptions", ArrayType (ScalarType "text"))],
>         CatCreateView "pg_views"
>           [("schemaname", ScalarType "name"),
>            ("viewname", ScalarType "name"), ("viewowner", ScalarType "name"),
>            ("definition", ScalarType "text")],
>         CatCreateView "information_schema.referential_constraints"
>           [("constraint_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("constraint_schema",
>             DomainType "information_schema.sql_identifier"),
>            ("constraint_name",
>             DomainType "information_schema.sql_identifier"),
>            ("unique_constraint_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("unique_constraint_schema",
>             DomainType "information_schema.sql_identifier"),
>            ("unique_constraint_name",
>             DomainType "information_schema.sql_identifier"),
>            ("match_option", DomainType "information_schema.character_data"),
>            ("update_rule", DomainType "information_schema.character_data"),
>            ("delete_rule", DomainType "information_schema.character_data")],
>         CatCreateView "information_schema.role_column_grants"
>           [("grantor", DomainType "information_schema.sql_identifier"),
>            ("grantee", DomainType "information_schema.sql_identifier"),
>            ("table_catalog", DomainType "information_schema.sql_identifier"),
>            ("table_schema", DomainType "information_schema.sql_identifier"),
>            ("table_name", DomainType "information_schema.sql_identifier"),
>            ("column_name", DomainType "information_schema.sql_identifier"),
>            ("privilege_type", DomainType "information_schema.character_data"),
>            ("is_grantable", DomainType "information_schema.yes_or_no")],
>         CatCreateView "information_schema.role_routine_grants"
>           [("grantor", DomainType "information_schema.sql_identifier"),
>            ("grantee", DomainType "information_schema.sql_identifier"),
>            ("specific_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("specific_schema",
>             DomainType "information_schema.sql_identifier"),
>            ("specific_name", DomainType "information_schema.sql_identifier"),
>            ("routine_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("routine_schema", DomainType "information_schema.sql_identifier"),
>            ("routine_name", DomainType "information_schema.sql_identifier"),
>            ("privilege_type", DomainType "information_schema.character_data"),
>            ("is_grantable", DomainType "information_schema.yes_or_no")],
>         CatCreateView "information_schema.role_table_grants"
>           [("grantor", DomainType "information_schema.sql_identifier"),
>            ("grantee", DomainType "information_schema.sql_identifier"),
>            ("table_catalog", DomainType "information_schema.sql_identifier"),
>            ("table_schema", DomainType "information_schema.sql_identifier"),
>            ("table_name", DomainType "information_schema.sql_identifier"),
>            ("privilege_type", DomainType "information_schema.character_data"),
>            ("is_grantable", DomainType "information_schema.yes_or_no"),
>            ("with_hierarchy", DomainType "information_schema.yes_or_no")],
>         CatCreateView "information_schema.role_usage_grants"
>           [("grantor", DomainType "information_schema.sql_identifier"),
>            ("grantee", DomainType "information_schema.sql_identifier"),
>            ("object_catalog", DomainType "information_schema.sql_identifier"),
>            ("object_schema", DomainType "information_schema.sql_identifier"),
>            ("object_name", DomainType "information_schema.sql_identifier"),
>            ("object_type", DomainType "information_schema.character_data"),
>            ("privilege_type", DomainType "information_schema.character_data"),
>            ("is_grantable", DomainType "information_schema.yes_or_no")],
>         CatCreateView "information_schema.routine_privileges"
>           [("grantor", DomainType "information_schema.sql_identifier"),
>            ("grantee", DomainType "information_schema.sql_identifier"),
>            ("specific_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("specific_schema",
>             DomainType "information_schema.sql_identifier"),
>            ("specific_name", DomainType "information_schema.sql_identifier"),
>            ("routine_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("routine_schema", DomainType "information_schema.sql_identifier"),
>            ("routine_name", DomainType "information_schema.sql_identifier"),
>            ("privilege_type", DomainType "information_schema.character_data"),
>            ("is_grantable", DomainType "information_schema.yes_or_no")],
>         CatCreateView "information_schema.routines"
>           [("specific_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("specific_schema",
>             DomainType "information_schema.sql_identifier"),
>            ("specific_name", DomainType "information_schema.sql_identifier"),
>            ("routine_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("routine_schema", DomainType "information_schema.sql_identifier"),
>            ("routine_name", DomainType "information_schema.sql_identifier"),
>            ("routine_type", DomainType "information_schema.character_data"),
>            ("module_catalog", DomainType "information_schema.sql_identifier"),
>            ("module_schema", DomainType "information_schema.sql_identifier"),
>            ("module_name", DomainType "information_schema.sql_identifier"),
>            ("udt_catalog", DomainType "information_schema.sql_identifier"),
>            ("udt_schema", DomainType "information_schema.sql_identifier"),
>            ("udt_name", DomainType "information_schema.sql_identifier"),
>            ("data_type", DomainType "information_schema.character_data"),
>            ("character_maximum_length",
>             DomainType "information_schema.cardinal_number"),
>            ("character_octet_length",
>             DomainType "information_schema.cardinal_number"),
>            ("character_set_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("character_set_schema",
>             DomainType "information_schema.sql_identifier"),
>            ("character_set_name",
>             DomainType "information_schema.sql_identifier"),
>            ("collation_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("collation_schema",
>             DomainType "information_schema.sql_identifier"),
>            ("collation_name", DomainType "information_schema.sql_identifier"),
>            ("numeric_precision",
>             DomainType "information_schema.cardinal_number"),
>            ("numeric_precision_radix",
>             DomainType "information_schema.cardinal_number"),
>            ("numeric_scale", DomainType "information_schema.cardinal_number"),
>            ("datetime_precision",
>             DomainType "information_schema.cardinal_number"),
>            ("interval_type", DomainType "information_schema.character_data"),
>            ("interval_precision",
>             DomainType "information_schema.character_data"),
>            ("type_udt_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("type_udt_schema",
>             DomainType "information_schema.sql_identifier"),
>            ("type_udt_name", DomainType "information_schema.sql_identifier"),
>            ("scope_catalog", DomainType "information_schema.sql_identifier"),
>            ("scope_schema", DomainType "information_schema.sql_identifier"),
>            ("scope_name", DomainType "information_schema.sql_identifier"),
>            ("maximum_cardinality",
>             DomainType "information_schema.cardinal_number"),
>            ("dtd_identifier", DomainType "information_schema.sql_identifier"),
>            ("routine_body", DomainType "information_schema.character_data"),
>            ("routine_definition",
>             DomainType "information_schema.character_data"),
>            ("external_name", DomainType "information_schema.character_data"),
>            ("external_language",
>             DomainType "information_schema.character_data"),
>            ("parameter_style",
>             DomainType "information_schema.character_data"),
>            ("is_deterministic", DomainType "information_schema.yes_or_no"),
>            ("sql_data_access",
>             DomainType "information_schema.character_data"),
>            ("is_null_call", DomainType "information_schema.yes_or_no"),
>            ("sql_path", DomainType "information_schema.character_data"),
>            ("schema_level_routine",
>             DomainType "information_schema.yes_or_no"),
>            ("max_dynamic_result_sets",
>             DomainType "information_schema.cardinal_number"),
>            ("is_user_defined_cast",
>             DomainType "information_schema.yes_or_no"),
>            ("is_implicitly_invocable",
>             DomainType "information_schema.yes_or_no"),
>            ("security_type", DomainType "information_schema.character_data"),
>            ("to_sql_specific_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("to_sql_specific_schema",
>             DomainType "information_schema.sql_identifier"),
>            ("to_sql_specific_name",
>             DomainType "information_schema.sql_identifier"),
>            ("as_locator", DomainType "information_schema.yes_or_no"),
>            ("created", DomainType "information_schema.time_stamp"),
>            ("last_altered", DomainType "information_schema.time_stamp"),
>            ("new_savepoint_level", DomainType "information_schema.yes_or_no"),
>            ("is_udt_dependent", DomainType "information_schema.yes_or_no"),
>            ("result_cast_from_data_type",
>             DomainType "information_schema.character_data"),
>            ("result_cast_as_locator",
>             DomainType "information_schema.yes_or_no"),
>            ("result_cast_char_max_length",
>             DomainType "information_schema.cardinal_number"),
>            ("result_cast_char_octet_length",
>             DomainType "information_schema.cardinal_number"),
>            ("result_cast_char_set_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("result_cast_char_set_schema",
>             DomainType "information_schema.sql_identifier"),
>            ("result_cast_character_set_name",
>             DomainType "information_schema.sql_identifier"),
>            ("result_cast_collation_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("result_cast_collation_schema",
>             DomainType "information_schema.sql_identifier"),
>            ("result_cast_collation_name",
>             DomainType "information_schema.sql_identifier"),
>            ("result_cast_numeric_precision",
>             DomainType "information_schema.cardinal_number"),
>            ("result_cast_numeric_precision_radix",
>             DomainType "information_schema.cardinal_number"),
>            ("result_cast_numeric_scale",
>             DomainType "information_schema.cardinal_number"),
>            ("result_cast_datetime_precision",
>             DomainType "information_schema.cardinal_number"),
>            ("result_cast_interval_type",
>             DomainType "information_schema.character_data"),
>            ("result_cast_interval_precision",
>             DomainType "information_schema.character_data"),
>            ("result_cast_type_udt_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("result_cast_type_udt_schema",
>             DomainType "information_schema.sql_identifier"),
>            ("result_cast_type_udt_name",
>             DomainType "information_schema.sql_identifier"),
>            ("result_cast_scope_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("result_cast_scope_schema",
>             DomainType "information_schema.sql_identifier"),
>            ("result_cast_scope_name",
>             DomainType "information_schema.sql_identifier"),
>            ("result_cast_maximum_cardinality",
>             DomainType "information_schema.cardinal_number"),
>            ("result_cast_dtd_identifier",
>             DomainType "information_schema.sql_identifier")],
>         CatCreateView "information_schema.schemata"
>           [("catalog_name", DomainType "information_schema.sql_identifier"),
>            ("schema_name", DomainType "information_schema.sql_identifier"),
>            ("schema_owner", DomainType "information_schema.sql_identifier"),
>            ("default_character_set_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("default_character_set_schema",
>             DomainType "information_schema.sql_identifier"),
>            ("default_character_set_name",
>             DomainType "information_schema.sql_identifier"),
>            ("sql_path", DomainType "information_schema.character_data")],
>         CatCreateView "information_schema.sequences"
>           [("sequence_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("sequence_schema",
>             DomainType "information_schema.sql_identifier"),
>            ("sequence_name", DomainType "information_schema.sql_identifier"),
>            ("data_type", DomainType "information_schema.character_data"),
>            ("numeric_precision",
>             DomainType "information_schema.cardinal_number"),
>            ("numeric_precision_radix",
>             DomainType "information_schema.cardinal_number"),
>            ("numeric_scale", DomainType "information_schema.cardinal_number"),
>            ("start_value", DomainType "information_schema.character_data"),
>            ("minimum_value", DomainType "information_schema.character_data"),
>            ("maximum_value", DomainType "information_schema.character_data"),
>            ("increment", DomainType "information_schema.character_data"),
>            ("cycle_option", DomainType "information_schema.yes_or_no")],
>         CatCreateView "information_schema.table_constraints"
>           [("constraint_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("constraint_schema",
>             DomainType "information_schema.sql_identifier"),
>            ("constraint_name",
>             DomainType "information_schema.sql_identifier"),
>            ("table_catalog", DomainType "information_schema.sql_identifier"),
>            ("table_schema", DomainType "information_schema.sql_identifier"),
>            ("table_name", DomainType "information_schema.sql_identifier"),
>            ("constraint_type",
>             DomainType "information_schema.character_data"),
>            ("is_deferrable", DomainType "information_schema.yes_or_no"),
>            ("initially_deferred", DomainType "information_schema.yes_or_no")],
>         CatCreateView "information_schema.table_privileges"
>           [("grantor", DomainType "information_schema.sql_identifier"),
>            ("grantee", DomainType "information_schema.sql_identifier"),
>            ("table_catalog", DomainType "information_schema.sql_identifier"),
>            ("table_schema", DomainType "information_schema.sql_identifier"),
>            ("table_name", DomainType "information_schema.sql_identifier"),
>            ("privilege_type", DomainType "information_schema.character_data"),
>            ("is_grantable", DomainType "information_schema.yes_or_no"),
>            ("with_hierarchy", DomainType "information_schema.yes_or_no")],
>         CatCreateView "information_schema.tables"
>           [("table_catalog", DomainType "information_schema.sql_identifier"),
>            ("table_schema", DomainType "information_schema.sql_identifier"),
>            ("table_name", DomainType "information_schema.sql_identifier"),
>            ("table_type", DomainType "information_schema.character_data"),
>            ("self_referencing_column_name",
>             DomainType "information_schema.sql_identifier"),
>            ("reference_generation",
>             DomainType "information_schema.character_data"),
>            ("user_defined_type_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("user_defined_type_schema",
>             DomainType "information_schema.sql_identifier"),
>            ("user_defined_type_name",
>             DomainType "information_schema.sql_identifier"),
>            ("is_insertable_into", DomainType "information_schema.yes_or_no"),
>            ("is_typed", DomainType "information_schema.yes_or_no"),
>            ("commit_action", DomainType "information_schema.character_data")],
>         CatCreateView "information_schema.triggered_update_columns"
>           [("trigger_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("trigger_schema", DomainType "information_schema.sql_identifier"),
>            ("trigger_name", DomainType "information_schema.sql_identifier"),
>            ("event_object_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("event_object_schema",
>             DomainType "information_schema.sql_identifier"),
>            ("event_object_table",
>             DomainType "information_schema.sql_identifier"),
>            ("event_object_column",
>             DomainType "information_schema.sql_identifier")],
>         CatCreateView "information_schema.triggers"
>           [("trigger_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("trigger_schema", DomainType "information_schema.sql_identifier"),
>            ("trigger_name", DomainType "information_schema.sql_identifier"),
>            ("event_manipulation",
>             DomainType "information_schema.character_data"),
>            ("event_object_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("event_object_schema",
>             DomainType "information_schema.sql_identifier"),
>            ("event_object_table",
>             DomainType "information_schema.sql_identifier"),
>            ("action_order", DomainType "information_schema.cardinal_number"),
>            ("action_condition",
>             DomainType "information_schema.character_data"),
>            ("action_statement",
>             DomainType "information_schema.character_data"),
>            ("action_orientation",
>             DomainType "information_schema.character_data"),
>            ("action_timing", DomainType "information_schema.character_data"),
>            ("action_reference_old_table",
>             DomainType "information_schema.sql_identifier"),
>            ("action_reference_new_table",
>             DomainType "information_schema.sql_identifier"),
>            ("action_reference_old_row",
>             DomainType "information_schema.sql_identifier"),
>            ("action_reference_new_row",
>             DomainType "information_schema.sql_identifier"),
>            ("created", DomainType "information_schema.time_stamp")],
>         CatCreateView "information_schema.usage_privileges"
>           [("grantor", DomainType "information_schema.sql_identifier"),
>            ("grantee", DomainType "information_schema.sql_identifier"),
>            ("object_catalog", DomainType "information_schema.sql_identifier"),
>            ("object_schema", DomainType "information_schema.sql_identifier"),
>            ("object_name", DomainType "information_schema.sql_identifier"),
>            ("object_type", DomainType "information_schema.character_data"),
>            ("privilege_type", DomainType "information_schema.character_data"),
>            ("is_grantable", DomainType "information_schema.yes_or_no")],
>         CatCreateView "information_schema.user_mapping_options"
>           [("authorization_identifier",
>             DomainType "information_schema.sql_identifier"),
>            ("foreign_server_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("foreign_server_name",
>             DomainType "information_schema.sql_identifier"),
>            ("option_name", DomainType "information_schema.sql_identifier"),
>            ("option_value", DomainType "information_schema.character_data")],
>         CatCreateView "information_schema.user_mappings"
>           [("authorization_identifier",
>             DomainType "information_schema.sql_identifier"),
>            ("foreign_server_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("foreign_server_name",
>             DomainType "information_schema.sql_identifier")],
>         CatCreateView "information_schema.view_column_usage"
>           [("view_catalog", DomainType "information_schema.sql_identifier"),
>            ("view_schema", DomainType "information_schema.sql_identifier"),
>            ("view_name", DomainType "information_schema.sql_identifier"),
>            ("table_catalog", DomainType "information_schema.sql_identifier"),
>            ("table_schema", DomainType "information_schema.sql_identifier"),
>            ("table_name", DomainType "information_schema.sql_identifier"),
>            ("column_name", DomainType "information_schema.sql_identifier")],
>         CatCreateView "information_schema.view_routine_usage"
>           [("table_catalog", DomainType "information_schema.sql_identifier"),
>            ("table_schema", DomainType "information_schema.sql_identifier"),
>            ("table_name", DomainType "information_schema.sql_identifier"),
>            ("specific_catalog",
>             DomainType "information_schema.sql_identifier"),
>            ("specific_schema",
>             DomainType "information_schema.sql_identifier"),
>            ("specific_name", DomainType "information_schema.sql_identifier")],
>         CatCreateView "information_schema.view_table_usage"
>           [("view_catalog", DomainType "information_schema.sql_identifier"),
>            ("view_schema", DomainType "information_schema.sql_identifier"),
>            ("view_name", DomainType "information_schema.sql_identifier"),
>            ("table_catalog", DomainType "information_schema.sql_identifier"),
>            ("table_schema", DomainType "information_schema.sql_identifier"),
>            ("table_name", DomainType "information_schema.sql_identifier")],
>         CatCreateView "information_schema.views"
>           [("table_catalog", DomainType "information_schema.sql_identifier"),
>            ("table_schema", DomainType "information_schema.sql_identifier"),
>            ("table_name", DomainType "information_schema.sql_identifier"),
>            ("view_definition",
>             DomainType "information_schema.character_data"),
>            ("check_option", DomainType "information_schema.character_data"),
>            ("is_updatable", DomainType "information_schema.yes_or_no"),
>            ("is_insertable_into", DomainType "information_schema.yes_or_no"),
>            ("is_trigger_updatable",
>             DomainType "information_schema.yes_or_no"),
>            ("is_trigger_deletable",
>             DomainType "information_schema.yes_or_no"),
>            ("is_trigger_insertable_into",
>             DomainType "information_schema.yes_or_no")]]

