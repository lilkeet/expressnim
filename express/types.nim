
{.experimental: "strictFuncs".}


import std/[options, tables, unicode, sets]

#### Simple data types
type
  Simple = Number or Logical or String or Binary

  Number = Real or int

  Real = object
    value: float
    precision: Positive ## Significant digits.

  Logical = enum
    logFalse, logUnknown, logTrue

  String = object of RootObj
    fixed: bool
    maxWidth: Option[Positive]
  EncodedString =  object of String
    runes: seq[Rune] ## Runes must be in `{'\x20'..'\x7E',
                     ##                    '\xA0'..Rune(0x10FFFE)}.
                     ## Cannot be empty!
  SimpleString = object of String
    value: string ## Chars must be in `ExpressChars` from `parse.nim`.

  Bit = bool
  Binary = object
    value: seq[Bit] ## Cannot be empty!
    fixed: bool
    maxWidth: Option[Positive]

#### Aggregation data types
type
  Aggregate = object of RootObj
    ## `assert bounds.a < bounds.b`
    bounds: HSlice[Natural, Positive]

  List[T] = object of Aggregate
    value: seq[T]
    headIndex: int
    mayContainDuplicates: bool ## As in two of the same instance.

  Array[T] = object of Aggregate
    case mayContainIndeterminate: bool
    of true: maybeValue: seq[Option[T]]
    of false: value: seq[T]
    mayContainDuplicates: bool ## As in two of the same instance.

  Bag[T] = object of Aggregate
    ## Can contain multiple of the same instance.
    value: CountTable[T]

  Set[T] = object of Aggregate
    ## Cannnot contain multiple of the same instance.
    value: HashSet[T]
    size: Option[Natural]

func `:=:`[T](l, r: T): bool =
  ## Instance equality operator.
  l == r
template `:<>:`[T](l, r: T): bool =
  ## Instance inequality operator.
  not (l :=: r)


#### Named data types
type
  Named = Entity or DefinedType

  Entity = object of RootObj

  DefinedType = object
    ## Like a type alias.
    value: string

#### Constructed data types
type
  Constructed = Enumeration or Select

  Enumeration = object
    value: OrderedSet[string]
    extensible: bool
    case extending: bool
    of true: base: string
    of false: discard

  Select = object
    value: HashSet[string] ## Cannot be empty if not extensible.
    case extensible: bool
    of true: onlyGenericEntities: bool ## Only contains Entities and/or
                                        ## Selects of Entities.
    of false: discard

    case extending: bool
    of true: base: string
    of false: discard


#### Type organizing
type
  Generic = Simple or Aggregate or Named or Constructed

  Instantiable = Simple or Aggregate or Named

  Parameter = Instantiable or Generic  ## Passable to a funcs/procs
  Underlying = Simple or Aggregate or DefinedType or Constructed
  # Named already exists

#### Arithmatic Operators
