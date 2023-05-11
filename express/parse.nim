
import
  std/[unicode, parseutils, strutils, pegs],
  types, indeterminate

{.experimental: "strictFuncs".}


func parseBinaryLit*(input: string; output: var Binary; start = 0): Natural =
  ## Parses the literal form of a `Binary` and stores it in `output`.
  ## Returns the length parsed, or 0 if an error occurred.
  runnableExamples:
    import types
    block:
      let
        whiteSpace = "     "
        binText = "%100110110"
        s = whiteSpace & binText
      var bin = newBinary([])
      assert s.parseBinaryLit(bin, whiteSpace.len) == binText.len
      assert bin[0] == on and bin[binText.high - "%".len] == off
    block:
      let faulty = "101110"
      var bin = newBinary([])
      assert faulty.parseBinaryLit(bin) == 0
      assert bin.len == 0

  let atStartOfBinaryLit = input[start] == '%'
  if not atStartOfBinaryLit:
    discard "failure"
    return 0
  result.inc

  while start + result < input.len:
    case input[start + result]
    of '0': output.add off
    of '1': output.add on
    else:
      discard "reached end of binary literal"
      return
    result.inc

func parseStringLit*(input: string; output: var String; start = 0): Natural =
  ## Parses the literal form of a `String` (an EXPRESS string) and stores it
  ## in `output`.
  ## Returns the length parsed, or 0 if an error occurred.
  ## Works with both simple strings and encoded strings.
  runnableExamples:
    import types
    block:
      let
        prefix = "not\nrelevant"
        encodedExample = "\"00000041\""
        together = prefix & encodedExample
      var output: String
      assert together.parseStringLit(output, prefix.len) == encodedExample.len
      assert $output == "A"
    block:
      let encodedExample = "\"0000795E00006238\""
      var output: String
      assert encodedExample.parseStringLit(output) == encodedExample.len
      assert $output == "神戸"
    block:
      let faultyEncoded = "\"0000063\"" # Must be a multiple of 8 chars long.
      var output: String
      assert faultyEncoded.parseStringLit(output) == 0
      assert $output == ""
    block:
      let
        prefix = "ignore me123!\n"
        simpleExample = "'My cat''s are realy hungry on the 10th.'"
        together = prefix & simpleExample
      var output: String
      assert together.parseStringLit(output, prefix.len) == simpleExample.len
      assert $output == "My cat's are realy hungry on the 10th."
    block:
      let faultySimple = "'Cannot span multiple \n lines'"
      var output: String
      assert faultySimple.parseStringLit(output) == 0
      assert $output == "Cannot span multiple "

  var pos = start + 1

  template failure(): void = return 0
  template success(): void = return (pos - start) + 1

  const
    SimpleMarker = '\''
    EncodedMarker = '"'
  case input[start]
  of SimpleMarker:
    func add(container: var String; toAdd: char) =
      # Normally unicode doesn't directly match up with Nim's char set
      # (the asci chars), but EXPRESS only allows the first 126 in simple string
      # literals, which are the ones that do match up.
      container.add Rune(toAdd)

    while pos <= input.high:
      let current = input[pos]

      const
        ExpressCharacters = {'\x9', '\xA', '\xD', '\x20'..'\x7E'}
        Valid = ExpressCharacters - NewLines
        Invalid = AllChars - Valid
      case current
      of Valid - {SimpleMarker}: output.add current
      of SimpleMarker:
        let reachedEndOfInput = pos == input.high
        if reachedEndOfInput: success()

        let
          next = input[pos + 1]
          atEscapedMarker = next == SimpleMarker
        if atEscapedMarker:
          output.add SimpleMarker
          inc pos
        else: success()
      of Invalid: failure()
      inc pos

  of EncodedMarker:
    #[EXPRESS's encoded strings take the form of UTF-32, where each byte is
    represented as an asci hexadecimal pair. Since std/unicode's Runes are
    also UTF-32, we can just parse each sequence of 8 hexadecimal characters
    into an int32 and convert that into a Rune.]#
    while pos < input.high and input[pos + 1] != EncodedMarker:
      var current: int32
      const
        CharsPerByte = 2
        CharsPerRune = sizeOf(int32) * CharsPerByte
      let charsParsed: range[0..CharsPerRune] = input.parseHex(current,
                                                            start=pos,
                                                            maxLen=CharsPerRune)
      case charsParsed
      of CharsPerRune:
        output.add Rune(current)
        inc pos, CharsPerRune
      else: failure()
    success()
  else:
    # Not at beginning of either an encoded nor a simple string.
    failure()

#[let whiteSpace = peg"""whiteSpace <- (normalWhiteSpace / remark)+
#### Normal whitespace
horizontalTab <- \009
lineFeed <- \010
carriageReturn <- \013
normalWhiteSpace <- horizontalTab / lineFeed / carriageReturn / [ ] / \n

#### Remarks
# Remarks must be treated as whitespace, in order to meet the ISO's
# requirements to be an EXPRESS parser.
embeddedMarker <- [()*]
embeddedRemark <- "(*" (embeddedRemark / (!embeddedMarker expressChar))* "*)"
tailRemark <- "--" (!\n expressChar)* \n

remark <- embeddedRemark / tailRemark
"""]#


func parseRealLit*(input: string; output: var Real; start = 0): Natural =
  ## Parses the literal form of a `String` (an EXPRESS string) and stores it
  ## in `output`.
  ## Returns the length parsed, or 0 if an error occurred.
  block checkForErrors:
    # EXPRESS is a little more restrictive than Nim's parseFloat.
    let
      realLit = peg"\d+ ([.] \d* ([eE] [-+] \d+)?)?"
      followsExpressRealFormat = input.match(realLit, start)
    if not followsExpressRealFormat: return
  var tmp: float
  result = input.parseFloat(tmp, start)
  output = toReal tmp


func parseIntLit*(input: string; output: var int; start = 0): Natural =
  ## Parses the literal form of an `Integer` (an EXPRESS int) and stores it
  ## in `output`.
  ## Returns the length parsed, or 0 if an error occurred.
  result = input.parseInt(output, start)

func parseLogicalLit*(input: string; output: var Logical; start = 0): Natural =
  ## Parses the literal form of a `Logical` and stores it in `output`.
  ## Returns the length parsed, or 0 if an error occurred.
  ## A `Logical` could be thought of as an enum with values `false`, `unknown`,
  ## and `true`, in that order. For consistency, though, it is currently
  ## modeled as an `Indeterminate[bool]`.
  runnableExamples:
    import indeterminate, types
    let example = "fAlsE"
    var output: Logical
    assert example.parseLogicalLit(output) == example.len
    let test = output == ??(false)
    assert test.isSome
    assert (get test)

    assert output is Indeterminate[bool]
    assert output is ?bool

  type LogicalKind = enum
    lkFalse, lkUnknown, lkTrue
  template logArray(t: typedesc): typedesc = array[lkFalse..lkTrue, t]
  let matchers: logArray(Peg) = [peg" i'false' ", peg" i'unknown' ",
                                 peg" i'true' "]
  const
    MatchLens: logArray(Natural) = [Natural "false".len, "unknown".len,
                                    "true".len]
    Outs: logArray(Logical) = [some(false), none(bool), some(true)]
  for kind in lkFalse..lkTrue:
    let matchFound = input.match(matchers[kind], start)
    if matchFound:
      output = Outs[kind]
      return MatchLens[kind]


