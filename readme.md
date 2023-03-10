## `FArray` - Immutable `Array`-based sequence replacement for Scala 3

This is a covariant `Array` wrapper for Scala 3 projects which have the potential to make some codebases incredibly much faster if 
it used as the default immutable sequence data type.

### Performance on the JVM

Performance on the JVM is often death by a thousand paper cuts. If you have for instance a compiler-like codebase.
A lot of those cuts will come from manipulating sequences. 

Especially if you use scala collections, because they are slow in many ways which show up in a profiler:
- you ~always suffer virtual dispatch costs
- many combinators use lambdas, which still have a non-neglible runtime overhead
- the JVM needs to run constructors for the whole inheritance hierarchy on instantiation
- many combinators are implemented inefficiently
- some collection types like `List` have terrible memory locality

### `FArray` approach to better performance

- everything is stored in one `Array[AnyRef]`. Casts on the way out.
- hand-codes all combinators with `while` loops. it's the fastest implementation you'll get.
- all combinators which receive lambda arguments are `inline`d
- uses `System.arraycopy` when applicable, which is the fastest way of copying data
- memory locality is excellent

### Preliminary benchmark numbers

For running through the entire set of collection operations on a bunch of large and small sequences:
```
cold FArray: 7 ms
cold List: 22 ms
cold IArray: 38 ms
warm FArray: 2 ms
warm List: 9 ms
warm IArray: 8 ms
```

Benchmark code can be found [here](tests/src/scala/farray/Benchmark.scala).

Note that it's very early, if you are on friendly terms with `jmh` your contribution would be especially welcome!

### Help needed

- *verify* code. This library is *not* production-ready yet. Hopefully by copying existing tests for Scala collections.
- benchmark properly
- Many combinators can still be optimized
- cleanup code

### Limitations

#### Only works for `AnyRef`. 

If you need specialized primitives this is not (at least yet) the library for you.

You can absolutely create `FArray` with for instance `Int` by creating it with the boxed versions (`FArray[java.lang.Integer](0, 1, 2)`) 

#### Implies an additional allocation over just using `Array`

```scala
final class FArray[+A <: AnyRef](underlying: Array[AnyRef]):
```
This is done to provide `equals`, `hashCode` and `toString`. It's just not usable as a sequence replacement without these

### Contributing/building

See [contributing](./contributing.md)
