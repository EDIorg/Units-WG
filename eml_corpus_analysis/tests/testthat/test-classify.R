testthat::test_that("definition clustering is seeded and reproducible", {
  defs <- c(
    paste("soil respiration flux replicate", 1:40),
    paste("litterfall dry mass collected trap", 1:40),
    paste("nitrate uptake rate stream reach", 1:40)
  )
  units <- tibble::tibble(
    attribute_id = paste0("a", seq_along(defs)),
    package_id = paste0("p", seq_along(defs) %% 7),
    scope = paste0("s", seq_along(defs) %% 3),
    attribute_definition = defs,
    attribute_name = "x",
    unit = "gramsPerSquareMeterPerDay",
    unit_group = "GM-PER-M2-DAY",
    dimension_vector = "DV"
  )
  run <- \() cluster_definitions(units, min_definitions = 50, max_k = 6, seed = 1)
  first <- run()
  testthat::expect_identical(first, run())
  testthat::expect_gte(nrow(first$clusters), 2)
  testthat::expect_equal(nrow(first$assignments), length(defs))
  labels <- label_clusters_llm(first$clusters, list(enabled = FALSE), "unused")
  testthat::expect_true(all(labels$llm_status == "not-run"))
  audit <- draw_audit_sample(units, first$assignments, labels, per_cluster = 2, seed = 1)
  testthat::expect_equal(nrow(audit), 2 * nrow(first$clusters))
  testthat::expect_true(all(is.na(audit$reviewer_agrees)))
})

testthat::test_that("audit summaries ignore unreviewed rows", {
  path <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(tibble::tibble(
    cluster_id = c("DV/c01", "DV/c01", "DV/c02"),
    reviewer_agrees = c("yes", "no", NA)
  ), path, na = "")
  out <- summarise_audit(path)
  testthat::expect_equal(out$reviewed, 2)
  testthat::expect_equal(out$agreement, 0.5)
})
