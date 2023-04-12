
{.experimental: "strictFuncs".}

type
  ExpressObject = ref object of RootObj
    parent = ExpressObject

  Schema = ref object of ExpressObject
    kind: SchemaKind
    children: seq[Schema]
    population: seq[Entity]
    functions: seq[Function]
    procedures: seq[Procedure]

  SchemaKind = enum
    skRoot, skPrimary, skSupport

  Entity = ref object of ExpressObject
  Function = ref object of ExpressObject
  Procedure = ref object of ExpressObject
