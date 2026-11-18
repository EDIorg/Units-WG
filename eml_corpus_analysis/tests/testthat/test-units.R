fake_qudt_units <- function() {
  tibble::tribble(
    ~unit_uri, ~qudt_unit, ~qk_relation, ~qk, ~dimension_vector,
    "http://qudt.org/vocab/unit/GM-PER-M2-DAY", "GM-PER-M2-DAY", "hasQuantityKind",
    "MassPerAreaTime", "A0E0L-2I0M1H0T-1D0",
    "http://qudt.org/vocab/unit/GM-PER-M2", "GM-PER-M2", "hasQuantityKind",
    "MassPerArea", "A0E0L-2I0M1H0T0D0",
    "http://qudt.org/vocab/unit/MicroMOL-PER-M2-SEC", "MicroMOL-PER-M2-SEC", "hasQuantityKind",
    "PhotosyntheticPhotonFluxDensity", "A1E0L-2I0M0H0T-1D0"
  ) |>
    dplyr::mutate(unit_label = qudt_unit, multiplier = NA_character_, retrieved = "2026-11-18")
}

testthat::test_that("unit match keys group spelling variants", {
  keys <- unit_match_key(c(
    "gramsPerSquareMeterPerDay", "gramPerMeterSquaredPerDay", " GramsPerSquareMetrePerDay ",
    "micromolePerMilligramPerHour", "micromolePerHourPerMilligram", NA
  ))
  testthat::expect_equal(keys[1:3], rep("gram/day.metersquared", 3))
  testthat::expect_equal(keys[4], keys[5])
  testthat::expect_true(is.na(keys[6]))
  testthat::expect_equal(unit_match_key("inverseDay"), "/day")
  testthat::expect_equal(unit_match_key("cubicCentimeters"), "centimetercubed")
})

testthat::test_that("QUDT qnames map into the EML key space", {
  testthat::expect_equal(
    qname_match_key("GM-PER-M2-DAY"), unit_match_key("gramPerSquareMeterPerDay")
  )
  testthat::expect_equal(
    qname_match_key("MicroMOL-PER-HR-MilliGM"), unit_match_key("micromolePerMilligramPerHour")
  )
  testthat::expect_equal(qname_match_key("NUM"), "number")
})

testthat::test_that("unit map verifies RCode_JP URIs and falls back to match keys", {
  jp <- tibble::tribble(
    ~unit_key, ~jp_qudt_uri, ~jp_qname,
    "gramspersquaremeter", "http://qudt.org/vocab/unit/GM-PER-M2", "GM-PER-M2",
    "obsoleteunit", "http://qudt.org/vocab/unit/GONE", "GONE"
  )
  map <- build_unit_map(
    c("gramsPerSquareMeter", "gramsPerSquareMeterPerDay", "obsoleteUnit", "mysteryUnit"),
    jp, fake_qudt_units()
  )
  get <- \(key, col) map[[col]][map$unit_key == key]
  testthat::expect_equal(get("gramspersquaremeter", "mapping_source"), "rcode_jp_verified")
  testthat::expect_equal(get("gramspersquaremeterperday", "qudt_unit"), "GM-PER-M2-DAY")
  testthat::expect_equal(get("gramspersquaremeterperday", "mapping_source"), "match_key")
  testthat::expect_equal(get("obsoleteunit", "mapping_source"), "rcode_jp_not_in_qudt")
  testthat::expect_true(is.na(get("obsoleteunit", "qudt_unit")))
  testthat::expect_equal(get("mysteryunit", "mapping_source"), "unmapped")
  testthat::expect_equal(get("gramspersquaremeter", "dimension_vector"), "A0E0L-2I0M1H0T0D0")
})

testthat::test_that("annotated units group by QUDT unit, else by match key", {
  attrs <- tibble::tibble(
    package_id = "edi.1.1", attribute_id = c("a", "b", "c"),
    standard_unit = c("gramsPerSquareMeter", NA, NA),
    custom_unit = c(NA, "mysteryUnits", NA)
  )
  map <- build_unit_map(c("gramsPerSquareMeter", "mysteryUnits"), tibble::tibble(
    unit_key = character(), jp_qudt_uri = character(), jp_qname = character()
  ), fake_qudt_units())
  out <- annotate_units(attrs, map)
  testthat::expect_equal(out$unit_kind, c("standard", "custom", "none"))
  testthat::expect_equal(out$unit_group[1], "GM-PER-M2")
  testthat::expect_equal(out$unit_group[2], unit_match_key("mysteryUnits"))
  testthat::expect_equal(out$scope, rep("edi", 3))
})
