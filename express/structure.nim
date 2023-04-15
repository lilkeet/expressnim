
{.experimental: "strictFuncs".}

import
  std/[options, tables],
  ../baseTypes

type
  ExpressObject = ref object of RootObj
    ## Only universe (parent of root) Schema has nil as its parent.
    parent* {.cursor.}: ExpressObject
    remark*: Option[SimpleString]

  Scope = ref object of ExpressObject
    children*: Table[SimpleString, Schema]
    population*: Table[SimpleString, EntityAsScope]
    functions*: Table[SimpleString, Function]
    procedures*: Table[SimpleString, Procedure]
    constants*: Table[SimpleString, Constant]


  Schema* = ref object of Scope
    kind*: SchemaKind


  SchemaKind* = enum
    skUniverse, skRoot, skPrimary, skSupport

  EntityAsScope* = ref object of Scope
  Function* = ref object of Scope
  Procedure* = ref object of Scope
  Constant*[T] = ref object of ExpressObject
    value*: T
