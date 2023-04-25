
{.experimental: "strictFuncs".}

import
  std/[tables, unicode, sets, math, sequtils],
  indeterminate,
  ../utils/funcBlock
import std/options except `==`, `$`

#### Simple data types
type
  Simple = Number or Logical or String or Binary

  Number = Real or int

  Logical = ?bool

  Real = object
    value: float
    precision: Positive ## Significant digits.

  String = object of RootObj
    fixed: bool
    maxWidth: Option[Positive]
  EncodedString =  object of String
    value: seq[Rune] ## Runes must be in `{'\x20'..'\x7E',
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
    ## `assert (bounds.a < bounds.b) or (bounds.b.isNone)`
    bounds: HSlice[Natural, Indeterminate[Positive]]

  Array[T] = object of Aggregate
    ## Fixed size (so `bounds.b.isSome`).
    ## Can contain Indeterminate as an `OptionalArray`.
    value: seq[T]
    uniqueValues: bool ## As in two of the same instance.
  OptionalArray[T] = Array[Indeterminate[T]]

  List[T] = object of Aggregate
    ## Variable, sometimes limited size.
    ## Cannot contain Indeterminate.
    value: seq[T]
    uniqueValues: bool ## As in two of the same instance.

  Bag[T] = object of Aggregate
    ## Variable, sometimes limited size.
    ## Cannot contain Indeterminate.
    ## Can contain multiple of the same instance.
    value: CountTable[T]

  Set[T] = object of Aggregate
    ## Variable, sometimes limited size.
    ## Cannot contain Indeterminate.
    ## Cannnot contain multiple of the same instance.
    value: HashSet[T]

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

#### Basic utilities
const RealMaxPrecision = funcBlock(int):
  func decimalToBinary(decimal: BiggestFloat, n: int): tuple[integer,
                                                            fraction: Binary] =
    var integerPart = int decimal
    block convertIntegerPart:
      while integerPart > 0:
        let newDigit: Bit = if integerPart mod 2 == 0: off else: on
        result.integer.value = newDigit & result.integer.value
        integerPart = integerPart div 2
    block convertFractionalPart:
      var fractionalPart = decimal - BiggestFloat integerPart
      for _ in 1..n:
        fractionalPart *= 2
        if fractionalPart >= 1:
          result.fraction.value.add on
          fractionalPart -= 1
        else:
          result.fraction.value.add off

  func binaryToDecimal(binaryNumber: tuple[integer,
                                           fraction: Binary]): BiggestFloat =
    block convertIntegerPart:
      var powerOfTwo = 1.0
      for index in countdown(binaryNumber.integer.value.high, 0):
        if binaryNumber.integer.value[index] == on: result += powerOfTwo
        powerOfTwo *= 2
    block convertIntegerPart:
      var powerOfTwo = 0.5
      for bit in binaryNumber.fraction.value:
        if bit == on: result += powerOfTwo
        powerOfTwo *= 0.5

  const
    BitsOfMantissa = if sizeOf(float) == 8: 52
                     elif sizeOf(float) == 4: 23
                     else:
                       assert false, "Unimplemented size of float!"
                       0
    DecimalsThatAreNonTerminatingAsBinary: array[6, BiggestFloat] = [0.1, 0.2,
                                                                     0.3, 0.4,
                                                                     0.6, 0.9]
  var exponents: seq[int]
  for decimal in DecimalsThatAreNonTerminatingAsBinary:
    let
      nonTerminatingBinary = decimalToBinary(decimal, BitsOfMantissa)
      floatRepresention = binaryToDecimal(nonTerminatingBinary)
      error = decimal - floatRepresention
    exponents.add (int floor log10 error) * -1
  (min exponents) - 1

func toReal[T: Number|float](n: T): Real =
  when n is Real: n
  else: Real(value: float(n), precision: RealMaxPrecision)

func round(n: Real): Real =
  let
    exponent = floor log10 n.value
    mantissa = n.value * (float 10).pow(-exponent)
  result = Real(value: mantissa.round(n.precision - 1) *
                       (float 10).pow(exponent),
                precision: n.precision)

func sizeOf(a: Array|List|Bag|Set): Positive =
  when a is Array:
    case a.optionalValues
    of true: a.maybeValue.len
    of false: a.value.len
  else: a.value.len
func len(a: Array|List|Bag|Set): Positive = sizeOf a

func length(s: EncodedString|SimpleString): Positive =  s.value.len
func len(s: EncodedString|SimpleString): Positive = s.length



#### Arithmatic Operators
func `+`(n: Real): Real = n
func `-`(n: Real): Real =
  result = Real(value: -n.value, precision: n.precision)
func `+`[T: Number](n: Indeterminate[T]): Indeterminate[T] = n
func `-`[T: Number](n: Indeterminate[T]): Indeterminate[T] =
  if n.isNone: n
  else: some -n

template generateArithmaticFor(operator: untyped): untyped =
  func `operator`(l, r: Real): Real =
    result = Real(value: operator(l.value, r.value),
                  precision: min(l.precision, r.precision))
  func `operator`[T: Number; U: Number](l: T; r: U): Real =
    operator(toReal(l), toReal(r))

generateArithmaticFor `+`
generateArithmaticFor `-`
generateArithmaticFor `*`
generateArithmaticFor `/`


func `**`(base, exponent: int): int =
  ##[EXPRESS defines that this function always returns an integer, even when
  the exponent is negative. Why? Who knows. So this will round the result
  to the nearest integer and pretend that it is correct.]##
  let exponentIsNegative = exponent < 0
  result = if exponentIsNegative: int round pow(float(base), float exponent)
           else: base ^ exponent

func `**`(base, exponent: Real): Real =
  result = Real(value: pow(base.value, exponent.value),
                precision: min(base.precision, exponent.precision))
func `**`[T: Number; U: Number](l: T; r: U): Real = toReal(l) ** toReal(r)

func truncate(n: Real): int = int n.value

func `mod`[T: Number; U: Number](l: T; r: U): int =
  result = when l is int: l mod truncate r
           else:
             when r is int: (truncate l) mod r
             else: (truncate l) mod (truncate r)
  result = abs result
  let rightIsNegative = r < 0
  if rightIsNegative: result = result *  -1

func `div`[T: Number; U: Number](l: T; r: U): int =
  when l is int: l div truncate r
  else:
    when r is int: (truncate l) div r
    else: (truncate l) div (truncate r)

#### Relational operators
### Numerical comparisons
template generateRelationsFor(operator: untyped): untyped =
  func `operator`[T: Number; U: Number](l: T; r: U): bool =
    when l is Real and r is Real: operator(l.value, r.value)
    elif l is Real: operator(l.value, r)
    else: operator(l, r.value)

generateRelationsFor `==`
generateRelationsFor `<`
generateRelationsFor `<=`

template `<>`(l, r: untyped): untyped = l != r

### Sequence comparisons
func `<`(l, r: Rune): bool = l <% r
type
  Comparison = enum
    compEq, compGt, compLt
  ComparableSequence = concept x
    type Contained = typeOf(x.value[int])
    Contained == Contained is bool
    Contained < Contained is bool
    type LengthMeasure = typeOf(x.value.len)
    LengthMeasure == LengthMeasure is bool
    LengthMeasure < LengthMeasure is bool

func compare(a, b: ComparableSequence): Comparison =
  for (valueA, valueB) in zip(a.value, b.value):
    if valueA == valueB: continue
    elif valueA < valueB: return compLt
    else: return compGt
  let
    aLength = a.value.len
    bLength = b.value.len
  result = if aLength == bLength: compEq
           elif aLength > bLength: compGt
           else: compLt

func `==`(l, r: ComparableSequence): bool =
  case compare(l, r)
  of compEq: true
  else: false

func `<`(l, r: ComparableSequence): bool =
  case compare(l, r)
  of compLt: true
  else: false

func `<=`(l, r: ComparableSequence): bool =
  case compare(l, r)
  of compLt, compEq: true
  else: false
