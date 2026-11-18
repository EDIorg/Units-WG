toy_dimensions <- function() {
  tibble::tribble(
    ~concept, ~dimension_vector, ~attributes, ~datasets, ~scopes, ~units, ~top_units,
    "npp", "A0E0L-2I0M1H0T-1D0", 400, 120, 15, 6, "GM-PER-M2-YR (300)",
    "par", "A1E0L-2I0M0H0T-1D0", 300, 90, 12, 2, "MicroMOL-PER-M2-SEC (290)",
    "cover", "A0E0L0I0M0H0T0D1", 500, 200, 20, 1, "PERCENT (500)",
    "rare", "A0E0L-2I0M1H0T-1D0", 10, 4, 1, 1, "GM-PER-M2-YR (10)",
    "npp", "unmapped", 50, 30, 5, 3, "x (20)"
  )
}

toy_qks <- function() {
  tibble::tribble(
    ~qk, ~qk_label, ~dimension_vector, ~specialization_of, ~broader,
    "MassPerAreaTime", "Mass per Area Time", "A0E0L-2I0M1H0T-1D0", NA, NA,
    "PhotosyntheticPhotonFluxDensity", "Photosynthetic Photon Flux Density",
    "A1E0L-2I0M0H0T-1D0", NA, NA,
    "DimensionlessRatio", "Dimensionless Ratio", "A0E0L0I0M0H0T0D1", NA, NA
  )
}

toy_seeds <- function() {
  tibble::tibble(
    concept = c("npp", "par", "cover", "rare"),
    label = c("NPP", "PAR", "Cover", "Rare"),
    qudt_qk_hint = c(NA, "photosynthetic photon", NA, NA)
  )
}

toy_qudt_units <- function() {
  tibble::tribble(
    ~qudt_unit, ~qk_relation, ~qk,
    "GM-PER-M2-YR", "hasQuantityKind", "MassPerAreaTime",
    "MicroMOL-PER-M2-SEC", "hasQuantityKind", "PhotosyntheticPhotonFluxDensity"
  )
}

toy_concept_units <- function() {
  tibble::tribble(
    ~concept, ~unit_group, ~qudt_unit, ~dimension_vector, ~datasets, ~scopes,
    "npp", "GM-PER-M2-YR", "GM-PER-M2-YR", "A0E0L-2I0M1H0T-1D0", 100, 12,
    "npp", "KiloGM-PER-HA-YR", "KiloGM-PER-HA-YR", "A0E0L-2I0M1H0T-1D0", 2, 1,
    "par", "MicroMOL-PER-M2-SEC", "MicroMOL-PER-M2-SEC", "A1E0L-2I0M0H0T-1D0", 85, 11
  )
}

testthat::test_that("candidate status follows recurrence, existing QKs, and dimensionless rules", {
  parents <- suggest_parent_qks(toy_concept_units(), toy_qudt_units(), toy_qks())
  out <- assess_qk_candidates(toy_dimensions(), toy_seeds(), toy_qks(), parents,
    min_datasets = 20, min_scopes = 3
  )
  status <- purrr::set_names(out$status, out$concept)
  testthat::expect_false("unmapped" %in% out$dimension_vector)
  testthat::expect_equal(status[["npp"]], "candidate-new-qk")
  testthat::expect_equal(status[["par"]], "reuse-or-specialize-existing-qk")
  testthat::expect_equal(status[["cover"]], "dimensionless-needs-ratio-semantics")
  testthat::expect_equal(status[["rare"]], "below-threshold")
  testthat::expect_equal(
    out$existing_qk_matches[out$concept == "par"], "PhotosyntheticPhotonFluxDensity"
  )
  testthat::expect_equal(out$suggested_parent_qk[out$concept == "npp"], "MassPerAreaTime")
  testthat::expect_equal(out$concept[1], "npp")
  testthat::expect_equal(out$vector_share[out$concept == "npp"], 1)
})

testthat::test_that("parents must share the concept's dimension vector", {
  units <- toy_concept_units()
  units$dimension_vector[1] <- "A0E0L-2I0M1H0T0D0"
  parents <- suggest_parent_qks(units, toy_qudt_units(), toy_qks())
  testthat::expect_false(any(
    parents$concept == "npp" & parents$dimension_vector == "A0E0L-2I0M1H0T0D0"
  ))
})

testthat::test_that("attachments require corpus usage and flag existing links", {
  parents <- suggest_parent_qks(toy_concept_units(), toy_qudt_units(), toy_qks())
  candidates <- assess_qk_candidates(toy_dimensions(), toy_seeds(), toy_qks(), parents)
  out <- propose_unit_attachments(candidates, toy_concept_units(), toy_qudt_units(), 3)
  testthat::expect_false("KiloGM-PER-HA-YR" %in% out$qudt_unit)
  npp <- out[out$concept == "npp", ]
  testthat::expect_equal(npp$target_qk, "new: npp")
  testthat::expect_equal(npp$proposed_relation, "unitForQuantityKind")
  par <- out[out$concept == "par", ]
  testthat::expect_true(par$already_attached)
  testthat::expect_equal(par$proposed_relation, "none (present)")
})

testthat::test_that("shortlist evidence is a dataset union and validates names", {
  units_tbl <- tibble::tibble(
    attribute_id = c("a1", "a2", "a3", "a4"),
    package_id = c("p1", "p1", "p2", "p3"),
    scope = c("s1", "s1", "s2", "s3"),
    unit_group = c("GM-PER-M2-DAY", "GM-PER-M2-DAY", "GM-PER-M2-YR", "GM"),
    dimension_vector = c(rep("A0E0L-2I0M1H0T-1D0", 3), "A0E0L0I0M1H0T0D0")
  )
  hits <- tibble::tibble(
    attribute_id = c("a1", "a2", "a2", "a3", "a4"),
    concept = c("npp", "npp", "leaching", "leaching", "npp")
  )
  seeds <- tibble::tibble(concept = c("npp", "leaching"))
  qks <- tibble::tibble(qk = "MassPerAreaTime")
  shortlist <- tibble::tibble(
    proposal_id = "rate", qudt_qk = "MassPerAreaTime",
    concepts = "npp; leaching", dimension_vector = "A0E0L-2I0M1H0T-1D0"
  )
  out <- summarise_qk_shortlist(shortlist, units_tbl, hits, seeds, qks)
  testthat::expect_equal(out$attributes, 3)
  testthat::expect_equal(out$datasets, 2)
  bad_concept <- dplyr::mutate(shortlist, concepts = "npp;nope")
  testthat::expect_error(
    summarise_qk_shortlist(bad_concept, units_tbl, hits, seeds, qks), "nope"
  )
  bad_qk <- dplyr::mutate(shortlist, qudt_qk = "NotAQk")
  testthat::expect_error(summarise_qk_shortlist(bad_qk, units_tbl, hits, seeds, qks), "NotAQk")
})
