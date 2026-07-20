package farray

import java.nio.file.Files

/** Tiny golden-file harness. `check(name, actual)` compares against `tests/snapshots/<name>`, which is tracked in git. A missing golden — or "update mode" —
  * writes the file; otherwise a mismatch fails with a diff and drops `<name>.actual` next to it. Repo root is found by ascending to the dir holding
  * `bleep.yaml`, so it works regardless of the test runner's cwd.
  *
  * Update mode is signalled by the marker FILE `tests/snapshots/.update` (create it, run the suite, delete it). A file — not an env var — because bleep runs
  * the tests in a FORKED JVM that does NOT inherit the launching process's environment, so `UPDATE_SNAPSHOTS=1 bleep test` never reached this code; the forked
  * JVM does, however, see the same working tree, so a marker file is read reliably. A `-Dfarray.updateSnapshots=true` system property is also honoured for
  * callers that can forward JVM options.
  */
object Snapshots:
  private def repoRoot: java.io.File =
    var d = new java.io.File(".").getCanonicalFile
    while d != null && !java.io.File(d, "bleep.yaml").exists() do d = d.getParentFile
    if d == null then new java.io.File(".").getCanonicalFile else d

  private val dir = new java.io.File(repoRoot, "tests/snapshots")

  /** true when the golden files should be (re)written rather than compared — see the class comment. */
  private def updateMode: Boolean =
    java.io.File(dir, ".update").exists() || java.lang.Boolean.getBoolean("farray.updateSnapshots")

  /** strip compile-run-specific synthetic counters so the golden is stable across recompiles: `$proxy336`/`op$proxy24`/`xs$proxy258` → `$proxy`, `_$294` →
    * `_$`.
    */
  private def normalize(s: String): String =
    s.replaceAll("""\$proxy\d+""", java.util.regex.Matcher.quoteReplacement("$proxy"))
      .replaceAll("""_\$\d+""", java.util.regex.Matcher.quoteReplacement("_$"))

  def check(name: String, actual0: String): Unit =
    dir.mkdirs()
    val actual = normalize(actual0).strip + "\n"
    val f = new java.io.File(dir, name)
    if !f.exists() || updateMode then Files.writeString(f.toPath, actual)
    else
      val expected = Files.readString(f.toPath)
      if expected != actual then
        Files.writeString(new java.io.File(dir, name + ".actual").toPath, actual)
        org.junit.Assert.assertEquals(s"snapshot '$name' changed — wrote '$name.actual'; rerun with UPDATE_SNAPSHOTS=1 to accept", expected, actual)
