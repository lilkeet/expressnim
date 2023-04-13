
import std/[options, tables, unicode]

type
  Number* = Real or int

  Real* = object
    value*: float
    precision*: Positive

  Logical* = enum
    logFalse, logUnknown, logTrue

  ## Only allows `{'\x20'..'\x7E', '\xA0'..Rune(0x10FFFE)}`.
  ## Cannot have an empty string!
  String* = object
    value*: seq[Rune]
    fixed*: bool
    maxWidth*: Option[Positive]

func width*(s: String): Positive = s.value.len
func `[]`*(s: String; index: Natural): Rune = s.value[index]

type
  Aggregate* = List or Array or Bag or Set

  List*[T] = object
    value*: seq[T]
    headIndex*: int
    lowerBound*: Natural ## Smallest possible len.
    upperBound*: Option[Positive] ## Biggest possible len.
    mayContainDuplicates*: bool ## Duplicates as in pointers to the same obj.

  Array*[T] = object
    value*: array[Natural, Option[T]]
    startIndex*: int
    mayContainDuplicates*: bool ## Duplicates as in pointers to the same obj.
    mayContainIndeterminate*: bool

  Bag*[T] = object
    value*: CountTable[T]
    lowerBound*: Natural ## Smallest possible size.
    upperBound*: Option[Positive] ## Biggest possible size.

  Set*[T] = object
    value*: Table[T]
    size*: Option[Natural]


type
  Bit* = bool
  ## Cannot have an empty Binary!
  Binary* = object
    value*: seq[Bit]
    fixed*: bool
    maxWidth*: Option[Positive]

func width*(b: Binary): Positive = b.value.len
func `[]`*(b: Binary; index: Natural): Bit = b.value[index]
