
import types

{.experimental: "strictFuncs".}

type
  ExpressNodeKind* = enum
    empty,
    binaryLit, stringLit, realLit, intLit,
    ident

  ExpressNode* = object
    case kind: ExpressNodeKind
    of empty: discard
    of binaryLit: binaryVal: Binary
    of stringLit: stringVal: String
    of realLit: realVal: Real
    of intLit: intVal: int
    of ident: name: string


func ident*(s: string): ExpressNode = ExpressNode(kind: ident, name: s)
