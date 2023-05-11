
discard """
  action: "run"
  batchable: true
  joinable: true
  timeout: 5.0
  targets: "c cpp js"
  valgrind: true
"""


import ../express/[types, parse]

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
