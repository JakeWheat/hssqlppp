
{-

simple wrapper around libpq, focused on supporting copy from stdin/ to
stdout

-}

{-# LANGUAGE ForeignFunctionInterface #-}
module Database.HsSqlPpp.Utils.PQ where

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/time.h>
#include "libpq-fe.h"

import Foreign
import Database.HsSqlPpp.Utils.C2HS

{#enum ConnStatusType {} deriving (Eq,Show)#}
{#enum ExecStatusType {} deriving (Eq,Show)#}

{#pointer *PGconn as ^ newtype#}

{#pointer *PGresult as ^ newtype#}

makePGConn :: Ptr () -> PGconn
makePGConn p = PGconn (castPtr p)

{#fun unsafe PQconnectdb as pqConnectdb
   {withCString* `String'} -> `PGconn' id #}

{#fun unsafe PQfinish as pqFinish
   {id `PGconn'} -> `()' #}

{#fun unsafe PQstatus as pqStatus
   {id `PGconn'} -> `ConnStatusType' cToEnum#}

{#fun unsafe PQerrorMessage as pqErrorMessage
   {id `PGconn'} -> `String' peekCString* #}

{#fun unsafe PQexec as pqExec
   {id `PGconn'
   ,withCString* `String'
   } -> `PGresult' id #}

{#fun unsafe PQclear as pqClear
   {id `PGresult'} -> `()' #}

{#fun unsafe PQresultStatus as pqResultStatus
   {id `PGresult'} -> `ExecStatusType' cToEnum #}

{#fun unsafe PQresStatus as pqResStatus
   {cFromEnum `ExecStatusType'} -> `String' peekCString* #}

{#fun unsafe PQresultErrorMessage as pqResultErrorMessage
   {id `PGresult'} -> `String' peekCString* #}


{# fun unsafe PQputCopyData as pqPutCopyData
   {id `PGconn'
   ,withCString* `String'
   ,`Int'} -> `Int' #}

{# fun unsafe PQputCopyEnd as pqPutCopyEnd
   {id `PGconn'
   ,withMCString* `Maybe String'} -> `Int' #}

withMCString :: Maybe String -> (CString -> IO a) -> IO a
withMCString m f = do
  case m of
    Nothing -> f nullPtr
    Just s -> withCString s f

{# fun unsafe PQgetCopyData as pqGetCopyData
   {id `PGconn'
   ,id `Ptr (Ptr CChar)'
   ,`Int'} -> `Int' #}

{# fun unsafe PQgetResult as pqGetResult
   {id `PGconn'} -> `Maybe PGresult' mPGresult #}

mPGresult :: PGresult -> Maybe PGresult
mPGresult p@(PGresult x) = if x == nullPtr
                 then Nothing
                 else Just p

{# fun unsafe PQfreemem as pqFreemem
   {id `Ptr ()'} -> `()' #}

