
import std/[options, tables]

type
  Number* = float or int

  Logical* = enum
    logTrue, logFalse, logUnknown

type
  List*[T] = seq[T]

  Array*[T] = seq[Option[T]]

  Bag*[T] = CountTable[T]


const BitLimit = 32
type
  Bit* = bool
  Binary* = array[BitLimit, Bit]
