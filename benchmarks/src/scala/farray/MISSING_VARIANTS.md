# Missing benchmark kind-variants

Every op below currently has a benchmark for exactly **one** element kind.
These are flagged, not created. The missing variants would round out kind coverage
(e.g. an Int-only op could gain a `Str` variant, and vice versa). The four kinds the
library specializes are Int / Long / Double / Str — most ops here cover just one of them.

Total single-kind ops: 55  (Int-only: 26, Str-only: 29)

## Int-only ops (26) — missing a Str (and Long/Double) variant

- `Aggregate` — only `AggregateIntBenchmark` (file `AggregateBenchmark.scala`)
- `AppendChain` — only `AppendChainIntBenchmark` (file `AppendChainBenchmark.scala`)
- `CombinationsPermutations` — only `CombinationsPermutationsIntBenchmark` (file `CombinationsPermutationsBenchmark.scala`)
- `ConcatN` — only `ConcatNIntBenchmark` (file `ConcatNBenchmark.scala`)
- `ConcatTree` — only `ConcatTreeIntBenchmark` (file `ConcatTreeBenchmark.scala`)
- `Convert` — only `ConvertIntBenchmark` (file `ConvertBenchmark.scala`)
- `Creation` — only `CreationIntBenchmark` (file `CreationBenchmark.scala`)
- `DeepConcat` — only `DeepConcatIntBenchmark` (file `DeepConcatBenchmark.scala`)
- `DropMap` — only `DropMapIntBenchmark` (file `DropMapBenchmark.scala`)
- `Flatten` — only `FlattenIntBenchmark` (file `FlattenBenchmark.scala`)
- `InitsTailsPatch` — only `InitsTailsPatchIntBenchmark` (file `InitsTailsPatchBenchmark.scala`)
- `MixedTree` — only `MixedTreeIntBenchmark` (file `MixedTreeBenchmark.scala`)
- `PadMap` — only `PadMapIntBenchmark` (file `PadMapBenchmark.scala`)
- `PrependChain` — only `PrependChainIntBenchmark` (file `PrependChainBenchmark.scala`)
- `RangeMap` — only `RangeMapIntBenchmark` (file `RangeMapBenchmark.scala`)
- `Reduce` — only `ReduceIntBenchmark` (file `ReduceBenchmark.scala`)
- `ReverseMap` — only `ReverseMapIntBenchmark` (file `ReverseMapBenchmark.scala`)
- `Scan` — only `ScanIntBenchmark` (file `ScanBenchmark.scala`)
- `SearchSlice` — only `SearchSliceIntBenchmark` (file `SearchSliceBenchmark.scala`)
- `SetOps` — only `SetOpsIntBenchmark` (file `SetOpsBenchmark.scala`)
- `Slicing` — only `SlicingIntBenchmark` (file `SlicingBenchmark.scala`)
- `SortAdaptive` — only `SortAdaptiveIntBenchmark` (file `SortAdaptiveBenchmark.scala`)
- `Transpose` — only `TransposeIntBenchmark` (file `TransposeBenchmark.scala`)
- `UpdateChain` — only `UpdateChainIntBenchmark` (file `UpdateChainBenchmark.scala`)
- `Updated4Map` — only `Updated4MapIntBenchmark` (file `Updated4MapBenchmark.scala`)
- `UpdatedMap` — only `UpdatedMapIntBenchmark` (file `UpdatedMapBenchmark.scala`)

## Str-only ops (29) — missing an Int (and Long/Double) variant

- `AppendConcatReverse` — only `AppendConcatReverseStrBenchmark` (file `AppendConcatReverseBenchmark.scala`)
- `AppendOne` — only `AppendOneStrBenchmark` (file `AppendOneBenchmark.scala`)
- `Apply` — only `ApplyStrBenchmark` (file `ApplyBenchmark.scala`)
- `Concat` — only `ConcatStrBenchmark` (file `ConcatBenchmark.scala`)
- `ConcatDropTake` — only `ConcatDropTakeStrBenchmark` (file `ConcatDropTakeBenchmark.scala`)
- `Contains` — only `ContainsStrBenchmark` (file `ContainsBenchmark.scala`)
- `Drop` — only `DropStrBenchmark` (file `DropBenchmark.scala`)
- `DropConcatFold` — only `DropConcatFoldStrBenchmark` (file `DropConcatFoldBenchmark.scala`)
- `DropTakeMap` — only `DropTakeMapStrBenchmark` (file `DropTakeMapBenchmark.scala`)
- `Filter` — only `FilterStrBenchmark` (file `FilterBenchmark.scala`)
- `FilterMapReverse` — only `FilterMapReverseStrBenchmark` (file `FilterMapReverseBenchmark.scala`)
- `FlatMapFilterTake` — only `FlatMapFilterTakeStrBenchmark` (file `FlatMapFilterTakeBenchmark.scala`)
- `Foreach` — only `ForeachStrBenchmark` (file `ForeachBenchmark.scala`)
- `Head` — only `HeadStrBenchmark` (file `HeadBenchmark.scala`)
- `Init` — only `InitStrBenchmark` (file `InitBenchmark.scala`)
- `Iterator` — only `IteratorStrBenchmark` (file `IteratorBenchmark.scala`)
- `Last` — only `LastStrBenchmark` (file `LastBenchmark.scala`)
- `LengthCompare` — only `LengthCompareStrBenchmark` (file `LengthCompareBenchmark.scala`)
- `MapFilterFold` — only `MapFilterFoldStrBenchmark` (file `MapFilterFoldBenchmark.scala`)
- `MapFlatMapFold` — only `MapFlatMapFoldStrBenchmark` (file `MapFlatMapFoldBenchmark.scala`)
- `NewOps` — only `NewOpsStrBenchmark` (file `NewOpsBenchmark.scala`)
- `PrependAppendMap` — only `PrependAppendMapStrBenchmark` (file `PrependAppendMapBenchmark.scala`)
- `PrependOne` — only `PrependOneStrBenchmark` (file `PrependOneBenchmark.scala`)
- `Reverse` — only `ReverseStrBenchmark` (file `ReverseBenchmark.scala`)
- `Slice` — only `SliceStrBenchmark` (file `SliceBenchmark.scala`)
- `Tail` — only `TailStrBenchmark` (file `TailBenchmark.scala`)
- `Take` — only `TakeStrBenchmark` (file `TakeBenchmark.scala`)
- `TakeDropFold` — only `TakeDropFoldStrBenchmark` (file `TakeDropFoldBenchmark.scala`)
- `Updated` — only `UpdatedStrBenchmark` (file `UpdatedBenchmark.scala`)

## Note on multi-kind ops (already covered, listed for completeness)

These ops have multiple kind variants and are NOT missing anything tracked here:
`FlatMap` (Int, Str), `FlatMapChain` (Int, Str), `FoldLeft` (Int, Long, Str),
`ListLike` / `ListLikeScaling` (Int, Str), `Map` (Int, Str), `MapConserve` (Int, Str),
`MapMega` (Int, Str), `ShortCircuit` (Int, Str), `Sort` (Int, Str),
`StructuralShowcase` (Int, Str), `Unzip` (Int, Str), `Zip` (Int, Str).
