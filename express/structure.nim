
{.experimental: "strictFuncs".}

type
  ExpressObject = ref object of RootObj
    parent = ExpressObject

  Schema = ref object of ExpressObject
    kind: SchemaKind
    population: seq[Schema]
    functions: seq[Function]
    procedures: seq[Procedure]

  SchemaKind = enum
    skRoot, skPrimary, skSupport

  Function = ref object of ExpressObject
  Procedure = ref object of ExpressObject
