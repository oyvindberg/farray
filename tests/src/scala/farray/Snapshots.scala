package farray

import java.nio.file.Files

/** Tiny golden-file harness. `check(name, actual)` compares against `tests/snapshots/<name>`, which is tracked
 *  in git. First run (or `UPDATE_SNAPSHOTS=1`) writes the file; otherwise a mismatch fails with a diff and
 *  drops `<name>.actual` next to it. Repo root is found by ascending to the dir holding `bleep.yaml`, so it
 *  works regardless of the test runner's cwd. */
object Snapshots:
  private def repoRoot: java.io.File =
    var d = new java.io.File(".").getCanonicalFile
    while d != null && !java.io.File(d, "bleep.yaml").exists() do d = d.getParentFile
    if d == null then new java.io.File(".").getCanonicalFile else d

  private val dir = new java.io.File(repoRoot, "tests/snapshots")

  /** strip compile-run-specific synthetic counters so the golden is stable across recompiles:
   *  `$proxy336`/`op$proxy24`/`xs$proxy258` → `$proxy`, `_$294` → `_$`. */
  private def normalize(s: String): String =
    s.replaceAll("""\$proxy\d+""", java.util.regex.Matcher.quoteReplacement("$proxy"))
     .replaceAll("""_\$\d+""", java.util.regex.Matcher.quoteReplacement("_$"))

  def check(name: String, actual0: String): Unit =
    dir.mkdirs()
    val actual = normalize(actual0).strip + "\n"
    val f = new java.io.File(dir, name)
    if !f.exists() || sys.env.contains("UPDATE_SNAPSHOTS") then
      Files.writeString(f.toPath, actual)
    else
      val expected = Files.readString(f.toPath)
      if expected != actual then
        Files.writeString(new java.io.File(dir, name + ".actual").toPath, actual)
        org.junit.Assert.assertEquals(
          s"snapshot '$name' changed — wrote '$name.actual'; rerun with UPDATE_SNAPSHOTS=1 to accept",
          expected, actual)
