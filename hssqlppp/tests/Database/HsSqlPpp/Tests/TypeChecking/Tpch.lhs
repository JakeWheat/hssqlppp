
Tests using the tpch queries. Just tests the result type at the
moment.

> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.TypeChecking.Tpch
>     (tpch) where
> import Database.HsSqlPpp.Internals.TypesInternal
> import Database.HsSqlPpp.Types
> import Database.HsSqlPpp.Tests.TpchData
> import Database.HsSqlPpp.Tests.TypeChecking.Utils
> import qualified Data.Text.Lazy as L

> tpch :: Item
> tpch =
>   Group "tpch" $
>         -- FIXME: 15 has cte which isn't implemented yet
>         let (s,e) = splitAt 14 t
>         in s ++ drop 1 e
>   where
>     t = zipWith (\(_n,s) t' -> QueryExpr tpchCatalog (L.pack s)
>                               (Right $ {-Pseudo $ SetOfType $ -}CompositeType t'))
>          tpchQueries
>          [-- q1
>           [("l_returnflag", mkTypeExtra typeChar)
>           ,("l_linestatus", mkTypeExtra typeChar)
>           ,("sum_qty", mkTypeExtra typeNumeric)
>           ,("sum_base_price", mkTypeExtra typeNumeric)
>           ,("sum_disc_price", mkTypeExtra typeNumeric)
>           ,("sum_charge", mkTypeExtra typeNumeric)
>           ,("avg_qty", mkTypeExtra typeNumeric)
>           ,("avg_price", mkTypeExtra typeNumeric)
>           ,("avg_disc", mkTypeExtra typeNumeric)
>           ,("count_order", mkTypeExtraNN typeBigInt)]
>          ,--q2
>           [("s_acctbal", mkTypeExtra typeNumeric)
>           ,("s_name", mkTypeExtra typeChar)
>           ,("n_name", mkTypeExtra typeChar)
>           ,("p_partkey", mkTypeExtra typeInt)
>           ,("p_mfgr", mkTypeExtra typeChar)
>           ,("s_address", mkTypeExtra typeVarChar)
>           ,("s_phone", mkTypeExtra typeChar)
>           ,("s_comment", mkTypeExtra typeVarChar)]
>          ,--q3
>           [("l_orderkey", mkTypeExtra typeInt)
>           ,("revenue", mkTypeExtra typeNumeric)
>           ,("o_orderdate", mkTypeExtra typeDate)
>           ,("o_shippriority", mkTypeExtra typeInt)]
>          ,--q4
>           [("o_orderpriority", mkTypeExtra typeChar)
>           ,("order_count", mkTypeExtraNN typeBigInt)]
>          ,--q5
>           [("n_name", mkTypeExtra typeChar)
>           ,("revenue", mkTypeExtra typeNumeric)]
>          ,--q6
>           [("revenue", mkTypeExtra typeNumeric)]
>          ,--q7
>           [("supp_nation", mkTypeExtra typeChar)
>           ,("cust_nation", mkTypeExtra typeChar)
>           ,("l_year", mkTypeExtra typeFloat8)
>           ,("revenue", mkTypeExtra typeNumeric)]
>          ,--q8
>           [("o_year", mkTypeExtra typeFloat8)
>           ,("mkt_share", mkTypeExtra typeNumeric)]
>          ,--q9
>           [("nation", mkTypeExtra typeChar)
>           ,("o_year", mkTypeExtra typeFloat8)
>           ,("sum_profit", mkTypeExtra typeNumeric)]
>          ,--q10
>           [("c_custkey", mkTypeExtra typeInt)
>           ,("c_name", mkTypeExtra typeChar)
>           ,("revenue", mkTypeExtra typeNumeric)
>           ,("c_acctbal", mkTypeExtra typeNumeric)
>           ,("n_name", mkTypeExtra typeChar)
>           ,("c_address", mkTypeExtra typeVarChar)
>           ,("c_phone", mkTypeExtra typeChar)
>           ,("c_comment", mkTypeExtra typeVarChar)]
>          ,--q11
>           [("ps_partkey", mkTypeExtra typeInt)
>           ,("value", mkTypeExtra typeNumeric)]
>          ,--q12
>           [("l_shipmode", mkTypeExtra typeChar)
>           ,("high_line_count", mkTypeExtraNN typeBigInt)
>           ,("low_line_count", mkTypeExtraNN typeBigInt)]
>          ,--q13
>           [("c_count", mkTypeExtraNN typeBigInt)
>           ,("custdist", mkTypeExtraNN typeBigInt)]
>          ,--q14
>           [("promo_revenue", mkTypeExtra typeNumeric)]
>          ,--q15
>           [("s_suppkey", mkTypeExtra typeInt)
>           ,("s_name", mkTypeExtra typeChar)
>           ,("s_address", mkTypeExtra typeVarChar)
>           ,("s_phone", mkTypeExtra typeChar)
>           ,("total_revenue", mkTypeExtra typeNumeric)]
>          ,--q16
>           [("p_brand", mkTypeExtra typeChar)
>           ,("p_type", mkTypeExtra typeVarChar)
>           ,("p_size", mkTypeExtra typeInt)
>           ,("supplier_cnt", mkTypeExtraNN typeBigInt)]
>          ,--q17
>           [("avg_yearly", mkTypeExtra typeNumeric)]
>          ,--q18
>           [("c_name", mkTypeExtra typeChar)
>           ,("c_custkey", mkTypeExtra typeInt)
>           ,("o_orderkey", mkTypeExtra typeInt)
>           ,("o_orderdate", mkTypeExtra typeDate)
>           ,("o_totalprice", mkTypeExtra typeNumeric)
>           ,("sum", mkTypeExtra typeNumeric)]
>          ,--q19
>           [("revenue", mkTypeExtra typeNumeric)]
>          ,--q20
>           [("s_name", mkTypeExtra typeChar)
>           ,("s_address", mkTypeExtra typeVarChar)]
>          ,--q21
>           [("s_name", mkTypeExtra typeChar)
>           ,("numwait", mkTypeExtraNN typeBigInt)]
>          ,--q22
>           [("cntrycode", mkTypeExtra $ ScalarType "char")
>           ,("numcust", mkTypeExtraNN typeBigInt)
>           ,("totacctbal", mkTypeExtra typeNumeric)]
>          ]
