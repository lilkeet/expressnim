 
##[Implements a `block` that acts like a `func`. That is, a `block` that
returns a value and has no side effects.]##

template funcBlock*(returning: typedesc; body: untyped): untyped =
  ## A specialization of the `block` keyword.
  ## Returns a value of type `returning`, and has no side effects.
  runnableExamples:
    proc myProc(): string =
      let a = funcBlock(int):
        # Each funcBlock has its own internal result variable.
        # This is easy to understand because a funcBlock cannot have
        # side effects.
        result = 0
        for c in '0' .. '9':
          result += 1
        assert result == 10

      # After exiting the funcBlock's scope, the old result variable is
      # forgotten and stored into the declared variable.
      assert result == ""
      assert a == 10
    assert myProc() == ""

  block:
    func insideFuncBlock(): returning =
      body
    insideFuncBlock()
