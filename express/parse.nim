
import
  std/[pegs, unicode, parseutils],
  types,
  ../utils/[widthRestrictedSeq]

{.experimental: "strictFuncs".}


func parseBinaryLit*(input: string; output: var Binary; start = 0): Natural =
  ## Parses the literal form of a `Binary` and stores it in `output`.
  ## Returns the length parsed, or 0 if an error occurred.
  runnableExamples:
    import types, ../utils/widthRestrictedSeq
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

  while result < input.len:
    case input[start + result]
    of '0': output.add off
    of '1': output.add on
    else:
      discard "reached end of binary literal"
      return
    result.inc

proc parseStringLit*(input: string; output: var String; start = 0): Natural =
  ## Parses the literal form of a `String` (an EXPRESS string) and stores it
  ## in `output`.
  ## Returns the length parsed, or 0 if an error occurred.
  ## Works with both simple strings and encoded strings.
  runnableExamples:
    import types, ../utils/widthRestrictedSeq
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
      assert $output == ""
  let stringLit = peg"""stringLit <- encodedLit / simpleLit
      # Simple string literals:
      escapedApostrophe <- ['] {[']} # Only capture one per escaped.
      expressChar <- [!-~] # Every valid character in Express.
      notApostropheOrNewLine <- !(['] / \n) {expressChar}
      simpleLit <- ['] (escapedApostrophe / notApostropheOrNewLine)* [']

      # Encoded string literals:
      hexdigit <- \d / [a-fA-F]
      octet <- {hexdigit hexdigit}
      encodedChar <- octet octet octet octet
      encodedLit <- ["] encodedChar+ ["]"""

  type StringKind = enum
    simple, encoded
  var state: StringKind # The current style of String literal being parsed.

  # EXPRESS's encoded strings take the form of UTF-32, where each byte is
  # represented as an asci hexadecimal pair. Since std/unicode's Runes are
  # also UTF-32, we can just parse each hexadecimal pair, convert it to an int,
  # shift it over to the correct position, and `or` them together into a Rune.
  type
    Octet = byte # One hexadecimal pair
    EncodedChar = array[4, Octet] # One Rune
  func toRune(x: EncodedChar): Rune =
    result = Rune(0)
    assert sizeOf(Rune) == sizeOf(int32)
    var count = 0
    for index in countdown(x.len, 0):
      const BitsPerByte = 8
      let bitsToShift = count * BitsPerByte
      result = Rune((int32 result) or ((int32 x[index]) shl bitsToShift))
      count.inc

  var encodedState: tuple[rune: EncodedChar, index: range[0..EncodedChar.high]]
  var willBeAddedToOutput: String
  let myParse = stringLit.eventParser:
    pkNonTerminal:
      enter:
        # Update our state upon which type we are attempting to parse.
        state = case p.nt.name
                of "simpleLit": simple
                of "encodedLit": encoded
                else:
                  assert false, "mismatched peg nonterminal"
                  simple
    pkCapture:
      leave:
        let successful = length != -1
        if successful:
          case state
          of simple:
            # Normally unicode doesn't directly match up with Nim's char set
            # (the asci chars), but EXPRESS only allows the first 126, which
            # are the ones that do match up.
            willBeAddedToOutput.add Rune(input[start])
          of encoded:
            discard s.parseHex(encodedState.rune[encodedState.index], start, 2)
            let filledInLastOctet = encodedState.index == encodedState.rune.high
            if filledInLastOctet:
              encodedState.index = 0
              willBeAddedToOutput.add toRune(encodedState.rune)
            else: encodedState.index.inc
  const PegFailure = -1 # The input is not a simple nor an encoded string.
  result = myParse(input)
  case result
  of PegFailure: result = 0
  else: output &= willBeAddedToOutput
