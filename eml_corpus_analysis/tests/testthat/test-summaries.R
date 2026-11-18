toy_units <- function() {
  tibble::tibble(
    attribute_id = paste0("x", 1:6),
    package_id = c("p1", "p2", "p3", "p1", "p2", "p4"),
    scope = c("s1", "s2", "s3", "s1", "s2", "s4"),
    attribute_name = c("anpp", "npp", "litter", "par", "co2_flux", "temp"),
    attribute_label = NA_character_,
    attribute_definition = c(
      "aboveground net primary production", "net primary productivity",
      "litterfall mass", "photosynthetically active radiation", "net CO2 flux", "air temperature"
    ),
    unit = "u", unit_kind = "standard", unit_key = "u",
    qudt_unit = c(
      "GM-PER-M2-YR", "GM-PER-M2-YR", "GM-PER-M2-YR",
      "MicroMOL-PER-M2-SEC", "MicroMOL-PER-M2-SEC", "DEG_C"
    ),
    unit_group = qudt_unit,
    dimension_vector = c("MPAT", "MPAT", "MPAT", "NPAT", "NPAT", "TEMP")
  )
}

toy_seeds <- function() {
  tibble::tribble(
    ~concept, ~label, ~include_pattern, ~exclude_pattern, ~qudt_qk_hint, ~notes,
    "npp", "Net primary production", "net primary|\\banpp\\b|\\bnpp\\b", NA, NA, NA,
    "par", "Photosynthetically active radiation", "photosynthetically active", NA,
    "photosynthetic", NA,
    "co2_flux", "CO2 flux", "co2[ _]?flux|co2 flux", "temperature", NA, NA
  )
}

testthat::test_that("concept seeds match names and definitions non-exclusively", {
  hits <- match_concepts(toy_units(), toy_seeds())
  testthat::expect_setequal(hits$attribute_id[hits$concept == "npp"], c("x1", "x2"))
  testthat::expect_equal(hits$attribute_id[hits$concept == "co2_flux"], "x5")
  testthat::expect_false("x6" %in% hits$attribute_id)
})

testthat::test_that("normalized entropy spans 0 to 1", {
  testthat::expect_equal(normalized_entropy(5), 0)
  testthat::expect_equal(normalized_entropy(c(3, 3, 3)), 1)
  testthat::expect_lt(normalized_entropy(c(9, 1)), 1)
})

testthat::test_that("polysemy counts concepts per unit group", {
  units <- toy_units()
  hits <- match_concepts(units, toy_seeds())
  poly <- summarise_polysemy(units, hits, min_datasets = 2)
  par_unit <- poly[poly$unit_group == "MicroMOL-PER-M2-SEC", ]
  testthat::expect_match(par_unit$top_concepts, "par \\(1\\)")
  testthat::expect_match(par_unit$top_concepts, "co2_flux \\(1\\)")
  testthat::expect_equal(par_unit$polysemy_index, 1)
})

testthat::test_that("concept summaries report units by dimension vector", {
  units <- toy_units()
  out <- summarise_concept_units(units, match_concepts(units, toy_seeds()))
  npp <- out$overview[out$overview$concept == "npp", ]
  testthat::expect_equal(npp$datasets, 2)
  testthat::expect_equal(npp$dimension_vectors, 1)
  testthat::expect_equal(nrow(out$by_dimension[out$by_dimension$concept == "npp", ]), 1)
})

testthat::test_that("top terms drop stop words and boilerplate", {
  testthat::expect_match(top_terms(c("soil respiration", "soil flux")), "^soil \\(2\\)")
  testthat::expect_false(grepl("measured", top_terms("the measured soil value")))
})
