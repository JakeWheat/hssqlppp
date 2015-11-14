
This is the catalog for the odbc scalar functions.

https://msdn.microsoft.com/en-us/library/ms711813(v=vs.85).aspx

TODO: this needs finishing

> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Dialects.OdbcCatalog
>      (odbcCatalog,odbcConvertTypes,odbcIntervalTypes) where
>
> --import Database.HsSqlPpp.Internals.Catalog.CatalogInternal
> import Database.HsSqlPpp.Internals.Catalog.CatalogTypes

todo: refactor this so we only have the names in this file and get rid
of the leakage of Type into here.

> import Database.HsSqlPpp.Internals.TypesInternal
> --import Database.HsSqlPpp.Internals.TypesInternal

todo: we have to add odbc on top of different dialects, do some tests
then figure out how to handle e.g. the canonical type names being
different in different dialects.

> typeBigInt,typeFloat8,typeChar,typeNumeric,typeFloat4,
>   typeSmallInt,typeInt,typeDate,typeTimestamp,typeVarChar :: Type
> typeBigInt = ScalarType "int8"
> typeFloat8 = ScalarType "float8"
> typeChar = ScalarType "char"
> typeNumeric = ScalarType "numeric"
> typeFloat4 = ScalarType "float4"
> typeSmallInt = ScalarType "int2"
> typeInt = ScalarType "int4"
> typeDate = ScalarType "date"
> typeTimestamp = ScalarType "timestamp"
> typeVarChar = ScalarType "varchar"

> -- | The odbc catalog contains the odbc specific functions
> -- if you want to type check sql with odbc specific functions, you
> -- can add these catalog updates to your catalog and then it should
> -- work
> odbcCatalog :: [CatalogUpdate]
> odbcCatalog = [

string functions

ASCII(string_exp) (ODBC 1.0)
Returns the ASCII code value of the leftmost character of string_exp as an integer.

>         CatCreateFunction "!odbc-ascii" ["text"] False "int4"
>        ,CatCreateFunction "!odbc-ascii" ["char"] False "int4"
>        ,CatCreateFunction "!odbc-ascii" ["varchar"] False "int4"

BIT_LENGTH(string_exp) (ODBC 3.0)
Returns the length in bits of the string expression.
Does not work only for string data types, therefore will not implicitly convert string_exp to string but instead will return the (internal) size of whatever datatype it is given.

>        ,CatCreateFunction "!odbc-bit_length" ["text"] False "int4"
>        ,CatCreateFunction "!odbc-bit_length" ["char"] False "int4"
>        ,CatCreateFunction "!odbc-bit_length" ["varchar"] False "int4"

CHAR(code) (ODBC 1.0)
Returns the character that has the ASCII code value specified by code. The value of code should be between 0 and 255; otherwise, the return value is data source–dependent.

>        ,CatCreateFunction "!odbc-char" ["int4"] False "char"

CHAR_LENGTH(string_exp) (ODBC 3.0)
Returns the length in characters of the string expression, if the string expression is of a character data type; otherwise, returns the length in bytes of the string expression (the smallest integer not less than the number of bits divided by 8). (This function is the same as the CHARACTER_LENGTH function.)

>        ,CatCreateFunction "!odbc-char_length" ["text"] False "int4"
>        ,CatCreateFunction "!odbc-char_length" ["char"] False "int4"
>        ,CatCreateFunction "!odbc-char_length" ["varchar"] False "int4"

CHARACTER_LENGTH(string_exp) (ODBC 3.0)
Returns the length in characters of the string expression, if the string expression is of a character data type; otherwise, returns the length in bytes of the string expression (the smallest integer not less tha the number of bits divided by 8). (This function is the same as the CHAR_LENGTH function.)

>        ,CatCreateFunction "!odbc-character_length" ["text"] False "int4"
>        ,CatCreateFunction "!odbc-character_length" ["char"] False "int4"
>        ,CatCreateFunction "!odbc-character_length" ["varchar"] False "int4"

CONCAT(string_exp1,string_exp2) (ODBC 1.0)
Returns a character string that is the result of concatenating string_exp2 to string_exp1. The resulting string is DBMS-dependent. For example, if the column represented by string_exp1 contained a NULL value, DB2 would return NULL but SQL Server would return the non-NULL string.

concat not in pg

>       ,CatCreateFunction "!odbc-concat" ["text","text"] False "text"
>       ,CatCreateFunction "!odbc-concat" ["char","char"] False "char"
>       ,CatCreateFunction "!odbc-concat" ["varchar","varchar"] False "varchar"

DIFFERENCE(string_exp1,string_exp2) (ODBC 2.0)
Returns an integer value that indicates the difference between the values returned by the SOUNDEX function for string_exp1 and string_exp2.

difference not in pg

>       ,CatCreateFunction "!odbc-difference" ["text","text"] False "int4"
>       ,CatCreateFunction "!odbc-difference" ["char","char"] False "int4"
>       ,CatCreateFunction "!odbc-difference" ["varchar","varchar"] False "int4"

INSERT(string_exp1, start, length, string_exp2) (ODBC 1.0)
Returns a character string where length characters have been deleted from string_exp1, beginning at start, and where string_exp2 has been inserted into string_exp, beginning at start.

insert not in pg

>       ,CatCreateFunction "!odbc-insert" ["text","int4","int4","text"] False "text"
>       ,CatCreateFunction "!odbc-insert" ["char","int4","int4","char"] False "char"
>       ,CatCreateFunction "!odbc-insert" ["varchar","int4","int4","varchar"] False "varchar"

lcase not in pg

LCASE(string_exp) (ODBC 1.0)
Returns a string equal to that in string_exp, with all uppercase characters converted to lowercase.

>       ,CatCreateFunction "!odbc-lcase" ["text"] False "text"
>       ,CatCreateFunction "!odbc-lcase" ["char"] False "char"
>       ,CatCreateFunction "!odbc-lcase" ["varchar"] False "varchar"

left is opposite in pg

LEFT(string_exp, count) (ODBC 1.0)
Returns the leftmost count characters of string_exp.

>       ,CatCreateFunction "!odbc-left" ["text","int4"] False "text"
>       ,CatCreateFunction "!odbc-left" ["char","int4"] False "char"
>       ,CatCreateFunction "!odbc-left" ["varchar","int4"] False "varchar"

LENGTH(string_exp) (ODBC 1.0)
Returns the number of characters in string_exp, excluding trailing blanks.
LENGTH only accepts strings. Therefore will implicitly convert string_exp to a string, and return the length of this string (not the internal size of the datatype).

TODO: we don't support the custom implicit conversion to string unless
there is a standard implicit conversion usable.

>       ,CatCreateFunction "!odbc-length" ["text"] False "text"
>       ,CatCreateFunction "!odbc-length" ["char"] False "char"
>       ,CatCreateFunction "!odbc-length" ["varchar"] False "varchar"


LOCATE(string_exp1, string_exp2[, start])  (ODBC 1.0)
Returns the starting position of the first occurrence of string_exp1 within string_exp2. The search for the first occurrence of string_exp1 begins with the first character position in string_exp2 unless the optional argument, start, is specified. If start is specified, the search begins with the character position indicated by the value of start. The first character position in string_exp2 is indicated by the value 1. If string_exp1 is not found within string_exp2, the value 0 is returned.
If an application can call the LOCATE scalar function with the string_exp1, string_exp2, and start arguments, the driver returns SQL_FN_STR_LOCATE when SQLGetInfo is called with an Option of SQL_STRING_FUNCTIONS. If the application can call the LOCATE scalar function with only the string_exp1 and string_exp2 arguments, the driver returns SQL_FN_STR_LOCATE_2 when SQLGetInfo is called with an Option of SQL_STRING_FUNCTIONS. Drivers that support calling the LOCATE function with either two or three arguments return both SQL_FN_STR_LOCATE and SQL_FN_STR_LOCATE_2.

>       ,CatCreateFunction "!odbc-locate" ["text","text"] False "int4"
>       ,CatCreateFunction "!odbc-locate" ["text","text","int4"] False "int4"
>       ,CatCreateFunction "!odbc-locate" ["char","char"] False "int4"
>       ,CatCreateFunction "!odbc-locate" ["char","char","int4"] False "int4"
>       ,CatCreateFunction "!odbc-locate" ["varchar","varchar"] False "int4"
>       ,CatCreateFunction "!odbc-locate" ["varchar","varchar","int4"] False "int4"

LTRIM(string_exp) (ODBC 1.0)
Returns the characters of string_exp, with leading blanks removed.

>       ,CatCreateFunction "!odbc-ltrim" ["text"] False "text"
>       ,CatCreateFunction "!odbc-ltrim" ["char"] False "char"
>       ,CatCreateFunction "!odbc-ltrim" ["varchar"] False "varchar"


OCTET_LENGTH(string_exp) (ODBC 3.0)
Returns the length in bytes of the string expression. The result is the smallest integer not less than the number of bits divided by 8.
Does not work only for string data types, therefore will not implicitly convert string_exp to string but instead will return the (internal) size of whatever datatype it is given.

>       ,CatCreateFunction "!odbc-octet_length" ["text"] False "int4"
>       ,CatCreateFunction "!odbc-octet_length" ["char"] False "int4"
>       ,CatCreateFunction "!odbc-octet_length" ["varchar"] False "int4"

POSITION(character_exp IN character_exp) (ODBC 3.0)
Returns the position of the first character expression in the second character expression. The result is an exact numeric with an implementation-defined precision and a scale of 0.

we don't support position for now since we this is a ansi sql syntax
which we also don't currently support

REPEAT(string_exp, count) (ODBC 1.0)
Returns a character string composed of string_exp repeated count times.

>       ,CatCreateFunction "!odbc-repeat" ["text","int4"] False "text"
>       ,CatCreateFunction "!odbc-repeat" ["char","int4"] False "char"
>       ,CatCreateFunction "!odbc-repeat" ["varchar","int4"] False "varchar"

REPLACE(string_exp1, string_exp2, string_exp3) (ODBC 1.0)
Search string_exp1 foroccurrences of string_exp2, and replace with string_exp3.

>       ,CatCreateFunction "!odbc-replace" ["text","text","text"] False "text"
>       ,CatCreateFunction "!odbc-replace" ["char","char","char"] False "char"
>       ,CatCreateFunction "!odbc-replace" ["varchar","varchar","varchar"] False "varchar"

RIGHT(string_exp, count) (ODBC 1.0)
Returns the rightmost count characters of string_exp.

>       ,CatCreateFunction "!odbc-right" ["text","int4"] False "text"
>       ,CatCreateFunction "!odbc-right" ["char","int4"] False "char"
>       ,CatCreateFunction "!odbc-right" ["varchar","int4"] False "varchar"


RTRIM(string_exp) (ODBC 1.0)
Returns the characters of string_exp with trailing blanks removed.

>       ,CatCreateFunction "!odbc-rtrim" ["text"] False "text"
>       ,CatCreateFunction "!odbc-rtrim" ["char"] False "char"
>       ,CatCreateFunction "!odbc-rtrim" ["varchar"] False "varchar"

SOUNDEX(string_exp) (ODBC 2.0)
Returns a data source–dependent character string representing the sound of the words in string_exp. For example, SQL Server returns a 4-digit SOUNDEX code; Oracle returns a phonetic representation of each word.

>       ,CatCreateFunction "!odbc-soundex" ["text"] False "text"
>       ,CatCreateFunction "!odbc-soundex" ["char"] False "char"
>       ,CatCreateFunction "!odbc-soundex" ["varchar"] False "varchar"


SPACE(count) (ODBC 2.0)
Returns a character string consisting of count spaces.

>       ,CatCreateFunction "!odbc-space" ["int4"] False "char"

SUBSTRING(string_exp, start, length) (ODBC 1.0)
Returns a character string that is derived from string_exp, beginning at the character position specified by start for length characters.

>       ,CatCreateFunction "!odbc-substring" ["text","int4","int4"] False "text"
>       ,CatCreateFunction "!odbc-substring" ["char","int4","int4"] False "char"
>       ,CatCreateFunction "!odbc-substring" ["varchar","int4","int4"] False "varchar"

UCASE(string_exp) (ODBC 1.0)
Returns a string equal to that in string_exp, with all lowercase characters converted to uppercase.

>       ,CatCreateFunction "!odbc-ucase" ["text"] False "text"
>       ,CatCreateFunction "!odbc-ucase" ["char"] False "char"
>       ,CatCreateFunction "!odbc-ucase" ["varchar"] False "varchar"

numeric functions

>       ]

ABS(numeric_exp) (ODBC 1.0)
Returns the absolute value of numeric_exp.

>        ++ [CatCreateFunction "!odbc-abs" [n] False n  | n <- numTypes]

ACOS(float_exp) (ODBC 1.0)
Returns the arccosine of float_exp as an angle, expressed in radians.

>        ++ [CatCreateFunction "!odbc-acos" [n] False n  | n <- floatTypes]

ASIN(float_exp) (ODBC 1.0)
Returns the arcsine of float_exp as an angle, expressed in radians.

>        ++ [CatCreateFunction "!odbc-asin" [n] False n  | n <- floatTypes]

ATAN(float_exp) (ODBC 1.0)
Returns the arctangent of float_exp as an angle, expressed in radians.

>        ++ [CatCreateFunction "!odbc-atan" [n] False n  | n <- floatTypes]

ATAN2(float_exp1, float_exp2) (ODBC 2.0)
Returns the arctangent of the x and y coordinates, specified by float_exp1 and float_exp2, respectively, as an angle, expressed in radians.

>        ++ [CatCreateFunction "!odbc-atan2" [n] False n  | n <- floatTypes]

CEILING(numeric_exp) (ODBC 1.0)
Returns the smallest integer greater than or equal to numeric_exp. The return value is of the same data type as the input parameter.

>        ++ [CatCreateFunction "!odbc-ceiling" [n] False n  | n <- numTypes]

COS(float_exp) (ODBC 1.0)
Returns the cosine of float_exp, where float_exp is an angle expressed in radians.

>        ++ [CatCreateFunction "!odbc-cos" [n] False n  | n <- floatTypes]

COT(float_exp) (ODBC 1.0)
Returns the cotangent of float_exp, where float_exp is an angle expressed in radians.

>        ++ [CatCreateFunction "!odbc-cot" [n] False n  | n <- floatTypes]

DEGREES(numeric_exp) (ODBC 2.0)
Returns the number of degrees converted from numeric_exp radians.

>        ++ [CatCreateFunction "!odbc-degrees" [n] False n  | n <- numTypes]

EXP(float_exp) (ODBC 1.0)
Returns the exponential value of float_exp.

>        ++ [CatCreateFunction "!odbc-exp" [n] False n  | n <- floatTypes]

FLOOR(numeric_exp) (ODBC 1.0)
Returns the largest integer less than or equal to numeric_exp. The return value is of the same data type as the input parameter.

>        ++ [CatCreateFunction "!odbc-floor" [n] False n  | n <- numTypes]

LOG(float_exp) (ODBC 1.0)
Returns the natural logarithm of float_exp.

>        ++ [CatCreateFunction "!odbc-log" [n] False n  | n <- floatTypes]

LOG10(float_exp) (ODBC 2.0)
Returns the base 10 logarithm of float_exp.

>        ++ [CatCreateFunction "!odbc-log10" [n] False n  | n <- floatTypes]

MOD(integer_exp1, integer_exp2) (ODBC 1.0)
Returns the remainder (modulus) of integer_exp1 divided by integer_exp2.

>        ++ [CatCreateFunction "!odbc-mod" [n] False n  | n <- intTypes]

PI( ) (ODBC 1.0)
Returns the constant value of pi as a floating-point value.

>        ++ [CatCreateFunction "!odbc-pi" [] False "float8"]

POWER(numeric_exp, integer_exp) (ODBC 2.0)
Returns the value of numeric_exp to the power of integer_exp.

>        ++ [CatCreateFunction "!odbc-power" [n,m] False n | n <- numTypes, m <- intTypes]

RADIANS(numeric_exp) (ODBC 2.0)
Returns the number of radians converted from numeric_exp degrees.

>        ++ [CatCreateFunction "!odbc-radians" [n] False n | n <- numTypes]

RAND([integer_exp]) (ODBC 1.0)
Returns a random floating-point value using integer_exp as the optional seed value.

>        ++ [CatCreateFunction "!odbc-rand" [n] False n | n <- intTypes]

ROUND(numeric_exp, integer_exp) (ODBC 2.0)
Returns numeric_exp rounded to integer_exp places right of the decimal point. If integer_exp is negative, numeric_exp is rounded to |integer_exp| places to the left of the decimal point.

>        ++ [CatCreateFunction "!odbc-round" [n,m] False n | n <- numTypes, m <- intTypes]

SIGN(numeric_exp) (ODBC 1.0)
Returns an indicator of the sign of numeric_exp. If numeric_exp is less than zero, –1 is returned. If numeric_exp equals zero, 0 is returned. If numeric_exp is greater than zero, 1 is returned.

>        ++ [CatCreateFunction "!odbc-sign" [n] False n  | n <- numTypes]

SIN(float_exp) (ODBC 1.0)
Returns the sine of float_exp, where float_exp is an angle expressed in radians.

>        ++ [CatCreateFunction "!odbc-sin" [n] False n  | n <- floatTypes]

SQRT(float_exp) (ODBC 1.0)
Returns the square root of float_exp.

>        ++ [CatCreateFunction "!odbc-sqrt" [n] False n  | n <- floatTypes]

TAN(float_exp) (ODBC 1.0)
Returns the tangent of float_exp, where float_exp is an angle expressed in radians.

>        ++ [CatCreateFunction "!odbc-tan" [n] False n  | n <- floatTypes]

TRUNCATE(numeric_exp, integer_exp) (ODBC 2.0)
Returns numeric_exp truncated to integer_exp places right of the decimal point. If integer_exp is negative, numeric_exp is truncated to |integer_exp| places to the left of the decimal point.

>        ++ [CatCreateFunction "!odbc-log10" [n,m] False n  | n <- numTypes, m <- intTypes]

>        ++ [

time

CURRENT_DATE( ) (ODBC 3.0)
Returns the current date.

>        CatCreateFunction "!odbc-current_date" [] False "date"

CURRENT_TIME[(time-precision)] (ODBC 3.0)
Returns the current local time. The time-precision argument determines the seconds precision of the returned value.

>       ,CatCreateFunction "!odbc-current_time" [] False "time"

CURRENT_TIMESTAMP [(timestamp-precision)] (ODBC 3.0)
Returns the current local date and local time as a timestamp value. The timestamp-precision argument determines the seconds precision of the returned timestamp.

>       ,CatCreateFunction "!odbc-current_timestamp" [] False "timestamp"

CURDATE( ) (ODBC 1.0)
Returns the current date.

>       ,CatCreateFunction "!odbc-curdate" [] False "date"

CURTIME( ) (ODBC 1.0)
Returns the current local time.

>       ,CatCreateFunction "!odbc-curtime" [] False "time"]

DAYNAME(date_exp) (ODBC 2.0)
Returns a character string containing the data source–specific name of the day (for example, Sunday through Saturday or Sun. through Sat. for a data source that uses English, or Sonntag through Samstag for a data source that uses German) for the day portion of date_exp.

>       ++ [CatCreateFunction "!odbc-dayname" [d] False "varchar" | d <- dateTypes]

DAYOFMONTH(date_exp) (ODBC 1.0)
Returns the day of the month based on the month field in date_exp as an integer value in the range of 1–31.

>       ++ [CatCreateFunction "!odbc-dayofmonth" [d] False "int4" | d <- dateTypes]

DAYOFWEEK(date_exp) (ODBC 1.0)
Returns the day of the week based on the week field in date_exp as an integer value in the range of 1–7, where 1 represents Sunday.

>       ++ [CatCreateFunction "!odbc-dayofweek" [d] False "int4" | d <- dateTypes]

DAYOFYEAR(date_exp) (ODBC 1.0)
Returns the day of the year based on the year field in date_exp as an integer value in the range of 1–366.

>       ++ [CatCreateFunction "!odbc-dayofyear" [d] False "int4" | d <- dateTypes]

EXTRACT(extract-field FROM extract-source) (ODBC 3.0)
Returns the extract-field portion of the extract-source. The extract-source argument is a datetime or interval expression. The extract-field argument can be one of the following keywords:
YEAR MONTH DAY HOUR MINUTE SECOND
The precision of the returned value is implementation-defined. The scale is 0 unless SECOND is specified, in which case the scale is not less than the fractional seconds precision of the extract-source field.

This is a special case and handled purely in the typechecker

HOUR(time_exp) (ODBC 1.0)
Returns the hour based on the hour field in time_exp as an integer value in the range of 0–23.

>       ++ [CatCreateFunction "!odbc-hour" [d] False "int4" | d <- timeTypes]

MINUTE(time_exp) (ODBC 1.0)
Returns the minute based on the minute field in time_exp as an integer value in the range of 0–59.

>       ++ [CatCreateFunction "!odbc-minute" [d] False "int4" | d <- timeTypes]

MONTH(date_exp) (ODBC 1.0)
Returns the month based on the month field in date_exp as an integer value in the range of 1–12.

>       ++ [CatCreateFunction "!odbc-month" [d] False "int4" | d <- dateTypes]

MONTHNAME(date_exp) (ODBC 2.0)
Returns a character string containing the data source–specific name of the month (for example, January through December or Jan. through Dec. for a data source that uses English, or Januar through Dezember for a data source that uses German) for the month portion of date_exp.

>       ++ [CatCreateFunction "!odbc-monthname" [d] False "varchar" | d <- dateTypes]

NOW( ) (ODBC 1.0)
Returns current date and time as a timestamp value.

>       ++ [CatCreateFunction "!odbc-now" [] False "timestamp"]

QUARTER(date_exp) (ODBC 1.0)
Returns the quarter in date_exp as an integer value in the range of 1–4, where 1 represents January 1 through March 31.

>       ++ [CatCreateFunction "!odbc-quarter" [d] False "int4" | d <- dateTypes]

SECOND(time_exp) (ODBC 1.0)
Returns the second based on the second field in time_exp as an integer value in the range of 0–59.

>       ++ [CatCreateFunction "!odbc-second" [d] False "int4" | d <- timeTypes]

TIMESTAMPADD(interval, integer_exp, timestamp_exp) (ODBC 2.0)
Returns the timestamp calculated by adding integer_exp intervals of type interval to timestamp_exp. Valid values of interval are the following keywords:
SQL_TSI_FRAC_SECOND
SQL_TSI_SECOND
SQL_TSI_MINUTE
SQL_TSI_HOUR
SQL_TSI_DAY
SQL_TSI_WEEK
SQL_TSI_MONTH
SQL_TSI_QUARTER
SQL_TSI_YEAR
where fractional seconds are expressed in billionths of a second. For example, the following SQL statement returns the name of each employee and his or her one-year anniversary date:
SELECT NAME, {fn TIMESTAMPADD(SQL_TSI_YEAR, 1, HIRE_DATE)} FROM EMPLOYEES
If timestamp_exp is a time value and interval specifies days, weeks, months, quarters, or years, the date portion of timestamp_exp is set to the current date before calculating the resulting timestamp.
If timestamp_exp is a date value and interval specifies fractional seconds, seconds, minutes, or hours, the time portion of timestamp_exp is set to 0 before calculating the resulting timestamp.
An application determines which intervals a data source supports by calling SQLGetInfo with the SQL_TIMEDATE_ADD_INTERVALS option.

>        ++ [CatCreateFunction "!odbc-timestampadd" ["int4","int4","date"] False "date"
>       ,CatCreateFunction "!odbc-timestampadd" ["int4","int4","time"] False "time"
>       ,CatCreateFunction "!odbc-timestampadd" ["int4","int4","timestamp"] False "timestamp"

TIMESTAMPDIFF(interval, timestamp_exp1, timestamp_exp2) (ODBC 2.0)
Returns the integer number of intervals of type interval by which timestamp_exp2 is greater than timestamp_exp1. Valid values of interval are the following keywords:
SQL_TSI_FRAC_SECOND
SQL_TSI_SECOND
SQL_TSI_MINUTE
SQL_TSI_HOUR
SQL_TSI_DAY
SQL_TSI_WEEK
SQL_TSI_MONTH
SQL_TSI_QUARTER
SQL_TSI_YEAR
where fractional seconds are expressed in billionths of a second. For example, the following SQL statement returns the name of each employee and the number of years he or she has been employed:
SELECT NAME, {fn TIMESTAMPDIFF(SQL_TSI_YEAR, {fn CURDATE()}, HIRE_DATE)} FROM EMPLOYEES
If either timestamp expression is a time value and interval specifies days, weeks, months, quarters, or years, the date portion of that timestamp is set to the current date before calculating the difference between the timestamps.
If either timestamp expression is a date value and interval specifies fractional seconds, seconds, minutes, or hours, the time portion of that timestamp is set to 0 before calculating the difference between the timestamps.
An application determines which intervals a data source supports by calling SQLGetInfo with the SQL_TIMEDATE_DIFF_INTERVALS option.

>       ,CatCreateFunction "!odbc-timestampdiff" ["int4","time","time"] False "int4"
>       ,CatCreateFunction "!odbc-timestampdiff" ["int4","date","date"] False "int4"
>       ,CatCreateFunction "!odbc-timestampdiff" ["int4","timestamp","timestamp"] False "int4"]


WEEK(date_exp) (ODBC 1.0)
Returns the week of the year based on the week field in date_exp as an integer value in the range of 1–53.

>       ++ [CatCreateFunction "!odbc-week" [d] False "int4" | d <- dateTypes]

YEAR(date_exp) (ODBC 1.0)
Returns the year based on the year field in date_exp as an integer value. The range is data source–dependent.

>       ++ [CatCreateFunction "!odbc-year" [d] False "int4" | d <- dateTypes]

system

DATABASE( ) (ODBC 1.0)
Returns the name of the database corresponding to the connection handle. (The name of the database is also available by calling SQLGetConnectOption with the SQL_CURRENT_QUALIFIER connection option.)

>       ++ [CatCreateFunction "!odbc-database" [] False "varchar"]

IFNULL(exp,value) (ODBC 1.0)
If exp is null, value is returned. If exp is not null, exp is returned. The possible data type or types of value must be compatible with the data type of exp.

>       ++ [CatCreateFunction "!odbc-ifnull" [t,t] False t | t <- allTypes]

USER( ) (ODBC 1.0)
Returns the user name in the DBMS. (The user name is also available by way of SQLGetInfo by specifying the information type: SQL_USER_NAME.) This can be different than the login name.

>       ++ [CatCreateFunction "!odbc-user" [] False "varchar"]

conversion function

CONVERT(value_exp, data_type)
The function returns the value specified by value_exp converted to the specified data_type, where data_type is one of the following keywords:
SQL_BIGINT
SQL_INTERVAL_HOUR_TO_MINUTE
SQL_BINARY
SQL_INTERVAL_HOUR_TO_SECOND
SQL_BIT
SQL_INTERVAL_MINUTE_TO_SECOND
SQL_CHAR
SQL_LONGVARBINARY
SQL_DECIMAL
SQL_LONGVARCHAR
SQL_DOUBLE
SQL_NUMERIC
SQL_FLOAT
SQL_REAL
SQL_GUID
SQL_SMALLINT
SQL_INTEGER
SQL_DATE
SQL_INTERVAL_MONTH
SQL_TIME
SQL_INTERVAL_YEAR
SQL_TIMESTAMP
SQL_INTERVAL_YEAR_TO_MONTH
SQL_TINYINT
SQL_INTERVAL_DAY
SQL_VARBINARY
SQL_INTERVAL_HOUR
SQL_VARCHAR
SQL_INTERVAL_MINUTE
SQL_WCHAR
SQL_INTERVAL_SECOND
SQL_WLONGVARCHAR
SQL_INTERVAL_DAY_TO_HOUR
SQL_WVARCHAR
SQL_INTERVAL_DAY_TO_MINUTE
 
SQL_INTERVAL_DAY_TO_SECOND


>    ++ [CatCreateFunction "!odbc-convert" [t,"int4"] False t
>       | t <- ["int2","int4","int8", "float4", "float8", "numeric"
>              ,"date","time","timestamp","char","varchar","text"
>              ,"bool"]
>       ]
>    where
>      floatTypes = ["float4","float8"]
>      numTypes = ["int2","int4","int8", "float4", "float8", "numeric"]
>      -- if a numeric with a fraction part is used, then maybe
>      -- it is a runtime error, or maybe the typechecker can understand this
>      -- most of the time and give a compile error, or maybe
>      -- it should cast it?
>      intTypes = ["int2","int4","int8", "numeric"]
>      timeTypes = ["time","timestamp"]
>      dateTypes = ["date","timestamp"]
>      allTypes = ["int2","int4","int8", "float4", "float8", "numeric"
>                 ,"date","time","timestamp","char","varchar","text"
>                 ,"bool"]

TODO : all the weird ones are commented out. I couldn't find a clear
reference on what the correspondence between the convert enum
identifiers and sql types are. The interval ones are the most likely
to be needed out of the commented out ones. Maybe they return smallint
or int or something.

> odbcConvertTypes :: [(String,Type)]
> odbcConvertTypes = [("sql_bigint", typeBigInt)
>                    ,("sql_float", typeFloat8)
>                    --,("sql_interval_hour_to_minute", typeFloat8)
>                    --,("sql_binary", $ ScalarType "unknown")
>                    --,("sql_interval_hour_to_second", typeFloat8)
>                    --,("sql_bit", $ ScalarType "unknown")
>                    --,("sql_interval_minute_to_second", typeFloat8)
>                    ,("sql_char", typeChar)
>                    --,("sql_longvarbinary", $ ScalarType "unknown")
>                    ,("sql_decimal", typeNumeric)
>                    --,("sql_longvarchar", typeVarChar)
>                    ,("sql_double", typeFloat8)
>                    ,("sql_numeric", typeNumeric)
>                    ,("sql_float", typeFloat8)
>                    ,("sql_real", typeFloat4)
>                    --,("sql_guid", $ ScalarType "unknown")
>                    ,("sql_smallint", typeSmallInt)
>                    ,("sql_integer", typeInt)
>                    ,("sql_date", typeDate)
>                    --,("sql_interval_month", typeFloat8)
>                    ,("sql_time", ScalarType "time")
>                    --,("sql_interval_year", typeFloat8)
>                    ,("sql_timestamp", typeTimestamp)
>                    --,("sql_interval_year_to_month", typeFloat8)
>                    --,("sql_tinyint", $ ScalarType "unknown")
>                    --,("sql_interval_day", typeFloat8)
>                    --,("sql_varbinary", $ ScalarType "unknown")
>                    --,("sql_interval_hour", typeFloat8)
>                    ,("sql_varchar", typeVarChar)
>                    --,("sql_interval_minute", typeFloat8)
>                    --,("sql_wchar", $ ScalarType "unknown")
>                    --,("sql_interval_second", typeFloat8)
>                    --,("sql_wlongvarchar", typeVarChar)
>                    --,("sql_interval_day_to_hour", typeFloat8)
>                    --,("sql_wvarchar", $ ScalarType "unknown")
>                    --,("sql_interval_day_to_minute", typeFloat8)
>                    --,("sql_interval_day_to_second", typeFloat8)
>                    ]

> odbcIntervalTypes :: [String]
> odbcIntervalTypes = ["sql_tsi_frac_second"
>                     ,"sql_tsi_second"
>                     ,"sql_tsi_minute"
>                     ,"sql_tsi_hour"
>                     ,"sql_tsi_day"
>                     ,"sql_tsi_week"
>                     ,"sql_tsi_month"
>                     ,"sql_tsi_quarter"
>                     ,"sql_tsi_year"]
