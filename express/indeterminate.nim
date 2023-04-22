
##[
This module implements types which encapsulate an Indeterminate value.
A value of type `?T` either contains a value `x` (represented as
`some(x)`) or is empty (`none(T)`).
This can be useful when you have a value that can be present or not.

Most operators are generically built-in via concepts to match any combination
of Indeterminate and determined values, as long as there is already an operator
for that type.
]##

runnableExamples:
  func `/`(l, r: int): float = l.float / r.float

  block one:
    let example = some(5) / some(10)
    assert example.isSome
    assert example is ?float
    assert get (example == 0.5)

  block two:
    let example = some(5) / 10
    assert example.isSome
    assert get (example == some(0.5))

  block three:
    let example = 5 / none(int)
    try:
      echo (get example)
      assert false, "Cannot unpack an Indeterminate value!"
    except UnpackDefect:
      assert true
      break three

import std/sugar
import std/options except `==`

{.experimental: "strictFuncs".}

type Indeterminate*[T] = Option[T]
  ## An optional type that may or may not contain a value of type `T`.
  ## When `T` is a a pointer type (`ptr`, `pointer`, `ref` or `proc`),
  ## `none(T)` is represented as `nil`.

template `?`*(t: untyped): typedesc =
  ## Shorthand for `Indeterminate[t]`.
  Indeterminate[t]

func indeterminate*[T](x: T): ?T =
  ## Returns an indeterminate form of x.
  option x
func `??`*[T](x: T): ?T =
  ## Returns an indeterminate form of x.
  indeterminate x

# Nim's concepts seem to bind before templates, so each of these must be typed
# out explicately.
type
  HasPlus = concept x
    x isnot Indeterminate
    type Resultant = typeOf(x + x)
  HasMinus = concept x
    x isnot Indeterminate
    type Resultant = typeOf(x - x)
  HasTimes = concept x
    x isnot Indeterminate
    type Resultant = typeOf(x * x)
  HasTwoAsterisks = concept x
    x isnot Indeterminate
    type Resultant = typeOf(x ** x)
  HasUpCarat = concept x
    x isnot Indeterminate
    type Resultant = typeOf(x ^ x)
  HasDiv = concept x
    x isnot Indeterminate
    type Resultant = typeOf(x div x)
  HasMod = concept x
    x isnot Indeterminate
    type Resultant = typeOf(x mod x)
  HasSlash = concept x
    x isnot Indeterminate
    type Resultant = typeOf(x / x)
  HasAnd = concept x
    x isnot Indeterminate
    type Resultant = typeOf(x and x)
  HasOr = concept x
    x isnot Indeterminate
    type Resultant = typeOf(x or x)
  HasXor = concept x
    x isnot Indeterminate
    type Resultant = typeOf(x xor x)
  HasEquals = concept x
    x isnot Indeterminate
    type Resultant = typeOf(x == x)
  HasLessThan = concept x
    x isnot Indeterminate
    type Resultant = typeOf(x < x)
  HasLessThanOrEqualTo = concept x
    x isnot Indeterminate
    type Resultant = typeOf(x <= x)

template generateOperatorFor(operator, conceptName: untyped): untyped =
  # Generates generic (concept-based) functions that match any type
  # `T or Indeterminate[T]` where there is already a proc `operator` for `T`.
  func `operator`*(l: ?conceptName;
                   r: conceptName): ?conceptName.Resultant =
    ##[Matches when the first argument is type `Indeterminate[T]` and the
    second argument is type `T`.]##
    l.map (left) => operator(left, r)

  func `operator`*(l: conceptName;
                   r: ?conceptName): ?conceptName.Resultant =
    ##[Matches when the first argument is type `T` and the
    second argument is type `Indeterminate[T]`.]##
    r.map (right) => operator(l, right)

  func `operator`*(l, r: ?conceptName): ?conceptName.Resultant =
    ##[Matches when the both arguments are of type `Indeterminate[T]`.]##
    flatten (l.map (left) => operator(left, r))

generateOperatorFor `+`, HasPlus
generateOperatorFor `-`, HasMinus
generateOperatorFor `*`, HasTimes
generateOperatorFor `**`, HasTwoAsterisks
generateOperatorFor `^`, HasUpCarat
generateOperatorFor `div`, HasDiv
generateOperatorFor `mod`, HasMod
generateOperatorFor `/`, HasSlash
generateOperatorFor `and`, HasAnd
generateOperatorFor `or`, HasOr
generateOperatorFor `xor`, HasXor
generateOperatorFor `==`, HasEquals
generateOperatorFor `<`, HasLessThan
generateOperatorFor `<=`, HasLessThanOrEqualTo


export get, some, none, UnpackDefect, options.`$`, map, flatMap, flatten,
       isSome, isNone
