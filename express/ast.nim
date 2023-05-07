
import types

{.experimental: "strictFuncs".}

type
  ExpressNodeKind* = enum
    empty, binaryLit, stringLit

  ExpressNode* = ref object
    case kind: ExpressNodeKind
    of empty: discard
    of binaryLit: bin: Binary
    of encodedStringLit: encodedStr: EncodedString


