package farray.json

import org.junit.Test
import org.junit.Assert.*
import farray.Agg

/** Plan-DESCRIPTION tests: the macro emits a structured string describing the plan it built (which fields are
 *  scanned, decoded vs kept as lazy slices, the predicate/early-out set, whether the record is rebuilt). We
 *  assert on that STRUCTURE — the precise, stable way to test a macro. The value-only tests missed two real
 *  bugs (a terminal-read field defaulting to 0.0; a type-changing `map` inflating the live-set to all 20
 *  fields) because outputs were coincidentally right; these plan assertions fail loudly on both. */
class JsonPlanTest:
  private val b = Array.emptyByteArray
  private def fields(plan: String, key: String): Set[String] =
    s"$key=\\[([^\\]]*)\\]".r.findFirstMatchIn(plan).map(_.group(1)).filter(_.nonEmpty).map(_.split(",").toSet).getOrElse(Set.empty)
  private def flag(plan: String, key: String): Boolean = plan.contains(s"$key=true")

  // ── the over-scanning regression: a type-changing map must NOT make every field live ──────────────────
  @Test def mapAmount_scans_only_amount(): Unit =
    val p = Json.ndjson[Rec](b).stream.filter(_.amount > 1).map(_.amount).plan
    assertEquals(Set("amount"), fields(p, "live"))
    assertEquals(Set("amount"), fields(p, "decoded"))
    assertEquals(Set.empty, fields(p, "sliced"))
    assertFalse(flag(p, "rebuildsRecord"))

  // ── the `acc + 0.0` regression: a terminal reading a field with no map must mark it live ──────────────
  @Test def planFold_amount_scans_amount(): Unit =
    val p = Json.ndjson[Rec](b).stream.planFold((a: Double, r) => a + r.amount)
    assertEquals(Set("amount"), fields(p, "live"))
    assertTrue(p.contains("terminal=Fold"))
    assertFalse(flag(p, "rebuildsRecord"))

  @Test def planFold_multiField(): Unit =
    val p = Json.ndjson[Rec](b).stream.planFold((a: Double, r) => a + r.amount + r.age)
    assertEquals(Set("amount", "age"), fields(p, "live"))
    assertEquals(Set("amount", "age"), fields(p, "decoded"))

  // ── lazy string slice + early-out ─────────────────────────────────────────────────────────────────────
  @Test def filterMapCategory_slices_category_earlyOut(): Unit =
    val p = Json.ndjson[Rec](b).stream.filter(_.amount > 1).map(_.category).plan
    assertEquals(Set("amount", "category"), fields(p, "live"))
    assertEquals(Set("amount"), fields(p, "decoded"))     // amount decoded for the predicate
    assertEquals(Set("category"), fields(p, "sliced"))    // category kept as a lazy slice
    assertEquals(Set("amount"), fields(p, "predicate"))
    assertTrue("early-out: predicate field present, projection field later", flag(p, "earlyOut"))

  // ── no early-out when there's no projection-only field (sum reads only the predicate field) ──────────
  @Test def filterSumPredField_noEarlyOut(): Unit =
    val p = Json.ndjson[Rec](b).stream.filter(_.amount > 1).map(_.amount).plan
    assertFalse("nothing to skip past the predicate → no early-out", flag(p, "earlyOut"))

  // ── whole-record use scans all + rebuilds ────────────────────────────────────────────────────────────
  @Test def mapIdentity_scansAll_rebuilds(): Unit =
    val p = Json.ndjson[Rec](b).stream.map(r => r).plan
    assertEquals(20, fields(p, "live").size)
    assertTrue(flag(p, "rebuildsRecord"))

  @Test def planFold_method_rebuilds(): Unit =
    // a method call (not a field accessor) needs the whole record
    val p = Json.ndjson[Rec](b).stream.planFold((a: Int, r) => a + r.toString.length)
    assertTrue(flag(p, "rebuildsRecord"))
    assertEquals(20, fields(p, "live").size)

  // ── count reads nothing; filter+count reads only the filter field ─────────────────────────────────────
  @Test def filterCount_onlyFilterField(): Unit =
    val p = Json.ndjson[Rec](b).stream.filter(_.amount > 1).plan
    assertEquals(Set("amount"), fields(p, "live"))

  @Test def plainCount_noFields(): Unit =
    val p = Json.ndjson[Rec](b).stream.plan // no stages, no terminal lambda
    assertEquals(Set.empty, fields(p, "live"))
    assertFalse(flag(p, "rebuildsRecord"))

  // ── distinct field types: int/long/double decode, string slices ──────────────────────────────────────
  @Test def kinds_decoded_vs_sliced(): Unit =
    val p = Json.ndjson[Rec](b).stream.planFold((acc: String, r) => acc + r.id + r.amount + r.name + r.age)
    assertEquals(Set("id", "amount", "name", "age"), fields(p, "live"))
    assertEquals(Set("id", "amount", "age"), fields(p, "decoded")) // Long/Double/Int decode
    assertEquals(Set("name"), fields(p, "sliced"))                  // String slices

end JsonPlanTest
