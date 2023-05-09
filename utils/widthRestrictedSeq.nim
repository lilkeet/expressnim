

{.experimental: "strictFuncs".}

type
  WidthRestrictedSeqKind* = enum
    unrestricted, max, fixed
  WidthRestrictedSeq*[T] = object
    value: seq[T]
    case kind: WidthRestrictedSeqKind
    of unrestricted, fixed: discard
    of max: maxWidth: Positive

  WRSK = WidthRestrictedSeqKind

func len*[T](container: WidthRestrictedSeq[T]): Natural =
  container.value.len

func high*[T](container: WidthRestrictedSeq[T]): int =
  (int container.len) - 1

func `[]`*[T](container: WidthRestrictedSeq[T];
              requested: int|BackwardsIndex): T =
  container.value[requested]

func `[]=`*[T](container: var WidthRestrictedSeq[T];
               requested: int|BackwardsIndex; newVal: T) =
  container[requested] = newVal

type InvalidWidthModification* = object of Defect

func add*[T](container: var WidthRestrictedSeq[T]; toAdd: T) =
  block errorChecking:
    case container.kind
    of unrestricted: discard
    of fixed: raise newException(InvalidWidthModification,
                                "Cannot add to a fixed container!")
    of max:
      let alreadyAtMax = container.len == container.maxWidth
      if alreadyAtMax: raise newException(InvalidWidthModification,
                                          "Cannot add to a full container!")
  container.value.add toAdd



func newWidthRestrictedSeq*[T](value: openarray[T]): WidthRestrictedSeq[T] =
  WidthRestrictedSeq[T](value: value.items.toSeq, kind: unrestricted)

func newWidthRestrictedSeq*[T](value: openarray[T]; fixed: bool;
                               size:Positive=value.len): WidthRestrictedSeq[T] =
  result = WidthRestrictedSeq[T](value: newSeq[T](size), kind: WRSK.fixed)
  for index, item in value:
    result.value[index] = item

func newWidthRestrictedSeq*[T](value: openarray[T];
                               maxWidth: Positive): WidthRestrictedSeq[T] =
  result = WidthRestrictedSeq[T](value: newSeqOfCap[T](maxWidth), kind: max,
                                 maxWidth: maxWidth)
  for index, item in value:
    result.value[index] = item

func `&`*[T](l: WidthRestrictedSeq[T]; r: T): WidthRestrictedSeq[T] =
  newWidthRestrictedSeq(l.value & r)

func `&`*[T](l: T; r: WidthRestrictedSeq[T]): WidthRestrictedSeq[T] =
  newWidthRestrictedSeq(l & r.value)

func `&`*[T](l, r: WidthRestrictedSeq[T]): WidthRestrictedSeq[T] =
  newWidthRestrictedSeq(l.value & r.value)


func `&=`*[T](l: var WidthRestrictedSeq[T]; r: WidthRestrictedSeq[T]) =
  block errorChecking:
    case l.kind
    of unrestricted: discard
    of fixed: raise newException(InvalidWidthModification,
                                "Cannot add to a fixed container!")
    of max:
      let willSurpassMax = l.len + r.len > l.maxWidth
      if willSurpassMax: raise newException(InvalidWidthModification,
                                            "Cannot add, will overflow!")
  l.value &= r.value

iterator items*[T](container: WidthRestrictedSeq[T]): T =
  for item in container.value:
    yield item
