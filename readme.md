# FArray · FSet

**Immutable, unboxed collections for Scala 3 — the full API you already know, at the speed of a raw array.**

```scala
val xs: FArray[Int] = FArray(1, 2, 3, 4, 5)
xs.map(_ * 2).filter(_ > 4).foldLeft(0)(_ + _)       // a real int[] underneath — no Integer, ever

xs.fuse.map(_ * 2).filter(_ > 4).foldLeft(0)(_ + _)  // one word fuses the chain into a single loop
```

📖 **[Read the story →](https://oyvindberg.github.io/farray/)** — how FArray and FSet are built, why they're
fast, and the complete benchmark record against every serious competitor. Every chart is a hoverable JMH
measurement; every snippet is extracted from source that compiles. **The site is the documentation.**

## Regenerate the numbers

```bash
scripts/bench-run.sh                     # FArray JMH suite → docs/bench-results.json
scripts/setbench-run.sh                  # FSet JMH suite  → docs/set-bench-results.json
cd site && npm install && npm run dev    # the site, live, on your fresh numbers
```

Commit the refreshed JSON and push — CI rebuilds and redeploys the site automatically.

Contributions welcome — especially if you're handy with JMH. See [`contributing.md`](contributing.md).
