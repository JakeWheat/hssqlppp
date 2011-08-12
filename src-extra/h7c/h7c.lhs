
compiler library and command line access/ build tool

change the typecheckdb example to use this library
use the cmd to generate defaultTemplate1Catalog

milestones:

command line: parses sql, outputs parse errors or optionally ast if
successful

run typecheck on sql, display type errors

typecheck options: against saved schema, pg dump, pg database, sql
  source

load sql into database with optional type check

display catalog

use sql extensions - how should this work?:

  create a build file: this contains
    a list of sql filenames to use
    a list of extension names (fully qualified function names)
    a search path for the implementation of these modules

then can have the above options with sql extensions
plus ability out output the transformed sql as text

generate documentation