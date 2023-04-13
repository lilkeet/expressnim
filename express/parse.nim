 
{.experimental: "strictFuncs".}

import std/[pegs]
import std/strutils except Whitespace

const
  TokenChars = {'\x21'..'\x7E'}

  HorizontalTab = '\x09'
  LineFeed = '\x0A'
  CarriageReturn = '\x0D'
  WhiteSpace = {' ', HorizontalTab, LineFeed, CarriageReturn} + Newlines

  SpecialChars = {'\x21'..'\x2F', '\x3A'..'\x3F', '\x40', '\x5B'..'\x5E',
                  '\x60', '\x7B'..'\x7E'}

  ## These are all the characters allowed within Express, excluding
  ## within a literal:
  ExpressChars = TokenChars + WhiteSpace

  ##[Remarks (comments) can either span multiple lines inbetween any two
  tokens as an `EmbeddedRemark` or follow a line as a `TailRemark`. A
  Remark can also be associated with any named item with a `RemarkTag`,
  like: `{*"thing_to_be_attached_to" blah blah blah*}`]##
  EmbeddedRemark = ["{.", ".}"]
  TailRemark = "--"
  RemarkTag = '"'

  Keywords = """abstract aggregate alias array
as bag based on begin
binary boolean by case
constant derive else end
end alias end case end constant end entity
end function end if end local end procedure
end repeat end rule end schema end subtype constraint
end type entity enumeration escape
extensible fixed for from
function generic generic entity if
integer inverse list local
logical number of oneof
optional otherwise procedure query
real renamed reference repeat
return rule schema select
set skip string subtype
subtype constraint supertype then to
total over type unique until
use var where while
with""".split peg"\s / \n"
  Operators = "and andor div in like mod not or xor".split ' '
  BuiltinConstants = "? self const e pi false true unknown".split ' '
  BuiltinFunctions = """abs acos asin atan
blength cos exists exp
format hibound hiindex length
lobound log log2 log10
loindex nvl odd rolesof
sin sizeof sqrt tan
typeof usedin value value in
value unique""".split peg"\s / \n"
  BuiltinProcedures = "insert remove".split ' '
  Symbols = """. , ; : * + - = % ’ \ / < > [ ] { } | e ( ) <= <> >= <* := ||
** -- (* *) :=: :<>:""".split peg"\s / \n"

  ## Binary literals follow this pattern: `%1001101010000`
  BinaryLiteral = '%'
  BitChars = {'0', '1'}

  ## Integer literals follow this pattern: `12345712`
  ## Signs are not part of literals, they are unary operators.
  IntegerChars = Digits

  ##[Real literals are composed of a mantissa and an optional exponent:
  `13258.65248e+65487`
  `5.E-2`
  `656.87999`
  The number of digits in the mantissa (ingoring leading zeros) is the amount
  of significant figures for that number.]##
  RealChars = Digits + {'e', 'E', '+', '-'}

  ##[Simple string literals consist of `ExpressChars`, contained within a pair
  of apostrophes `'` (double to escape).
  `'my friend''s dog`]##
  SimpleStringLiteral = '\''
  SimpleStringChars = ExpressChars

  ##[Encoded string literals consist of runes where each rune is 4 bytes long.
  First byte: SO/IEC 10646 group in which the character is defined.
  Second byte: SO/IEC 10646 plane in which the character is defined.
  Third byte: SO/IEC 10646 row in which the character is defined.
  Fourth byte: SO/IEC 10646 cell in which the character is defined.

  I.e., a `Rune` from `std/unicode`.

  `"00000041"` is `A` (more explicately, `['\0', '\0', '\0', '\x41']`, but we
                       skip over control the zero control chars).
  ]##
  EncodedStringLiteral = '"'
  EncodedStringChars = HexDigits

  ## Note: string literals will never contain a newline.

  LogicalLiterals = ["false", "true", "unknown"]
















