
discard """
  action: "run"
  batchable: true
  joinable: true
  timeout: 5.0
  targets: "c cpp"
  valgrind: true
"""


import
  std/[math],
  ../express/[types, parse, indeterminate, ast]

block binaryLit:
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

block stringLit:
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


block realLit:
  block:
    let example = "1.E6"
    var output = real()
    assert example.parseRealLit(output) == example.len
    assert output == toReal (1 * 10^6)
  block:
    let
      prefix = "\n\n hehe\n\n"
      example = "3.5e-5"
      together = prefix & example
    var output = real()
    assert together.parseRealLit(output, prefix.len) == example.len
    assert output == toReal (3.5 * 10.0.pow(-5.0))
  block:
    let faultyExample = ".001"
    var output = real()
    assert faultyExample.parseRealLit(output) == 0

block logicalLit:
  block:
    let example = "fAlsE"
    var output: Logical
    assert example.parseLogicalLit(output) == example.len

    let test = output == ??(false)
    assert test.isSome
    assert (get test)

    assert output is Indeterminate[bool]
    assert output is ?bool

block idents:
  block:
    let
      prefix = "nono"
      example = "myIdent"
      together = prefix & example
    var output = ident("")
    assert together.parseIdent(output, prefix.len) == example.len
