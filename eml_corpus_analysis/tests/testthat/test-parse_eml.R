fixture <- function(name) testthat::test_path("fixtures", name)

testthat::test_that("EML 2.1.1 attributes, dataset fields, and custom units parse", {
  parsed <- parse_eml_file(fixture("eml-2.1.1-custom-unit.xml"))
  ds <- parsed$datasets
  testthat::expect_equal(ds$package_id, "knb-lter-xyz.12.3")
  testthat::expect_equal(ds$scope, "knb-lter-xyz")
  testthat::expect_equal(ds$eml_version, "eml-2.1.1")
  testthat::expect_equal(ds$keywords, "litterfall; productivity")
  testthat::expect_equal(ds$keyword_thesauri, "LTER Controlled Vocabulary")
  testthat::expect_equal(ds$begin_date, "2001-01-01")
  testthat::expect_equal(ds$west, -89.7)
  testthat::expect_false(ds$has_taxonomic_coverage)
  testthat::expect_true(is.na(ds$parse_error))
  testthat::expect_equal(nchar(ds$sha256), 64)
  testthat::expect_equal(ds$n_attributes, 3)

  attrs <- parsed$attributes
  testthat::expect_equal(attrs$attribute_id, paste0("knb-lter-xyz.12.3#e1.a", 1:3))
  testthat::expect_equal(attrs$attribute_definition[1], "litterfall dry mass flux")
  testthat::expect_equal(attrs$standard_unit, c("gramsPerSquareMeter", NA, NA))
  testthat::expect_equal(attrs$custom_unit, c(NA, "microgramPerGramPerHour", NA))
  testthat::expect_equal(attrs$measurement_scale, c("ratio", "ratio", "nominal"))
  testthat::expect_equal(attrs$number_type[1], "real")

  cu <- parsed$custom_units
  testthat::expect_equal(cu$unit_id, "microgramPerGramPerHour")
  testthat::expect_equal(cu$parent_si, "perSecond")
  testthat::expect_equal(cu$description, "micrograms per gram of dry soil per hour")
  testthat::expect_equal(nrow(parsed$annotations), 0)
})

testthat::test_that("EML 2.2.0 annotations resolve at dataset, attribute, and referenced levels", {
  parsed <- parse_eml_file(fixture("eml-2.2.0-annotated.xml"))
  testthat::expect_equal(parsed$datasets$eml_version, "eml-2.2.0")
  testthat::expect_true(parsed$datasets$has_taxonomic_coverage)
  ann <- parsed$annotations
  testthat::expect_equal(sort(ann$subject_level), c("attribute", "attribute", "dataset"))
  qudt <- dplyr::filter(ann, stringr::str_detect(value_uri, "qudt"))
  testthat::expect_equal(qudt$attribute_id, "edi.99.1#e1.a2")
  testthat::expect_equal(qudt$value_label, "degree Celsius")
  ecso <- dplyr::filter(ann, value_label == "Net Primary Productivity")
  testthat::expect_equal(ecso$attribute_id, "edi.99.1#e1.a1")
  testthat::expect_setequal(annotation_vocabulary(ann$value_uri), c("ENVO", "ECSO", "QUDT"))
})

testthat::test_that("unreadable files are reported, not fatal", {
  parsed <- parse_eml_file("<eml><unclosed>", file_label = "bad.xml")
  testthat::expect_false(is.na(parsed$datasets$parse_error))
  testthat::expect_equal(nrow(parsed$attributes), 0)
})

testthat::test_that("documents without entities return empty tables", {
  xml <- '<eml:eml xmlns:eml="https://eml.ecoinformatics.org/eml-2.2.0" packageId="edi.1.1">
    <dataset><title>No tables</title></dataset></eml:eml>'
  parsed <- parse_eml_file(xml, file_label = "empty.xml")
  testthat::expect_equal(parsed$datasets$n_entities, 0)
  testthat::expect_equal(nrow(parsed$attributes), 0)
  testthat::expect_equal(nrow(parsed$annotations), 0)
})

testthat::test_that("batches combine tables across files", {
  batch <- parse_eml_batch(
    c(fixture("eml-2.1.1-custom-unit.xml"), fixture("eml-2.2.0-annotated.xml"))
  )
  testthat::expect_equal(nrow(batch$datasets), 2)
  testthat::expect_equal(nrow(batch$attributes), 5)
  combined <- combine_parsed(list(batch, batch), "attributes")
  testthat::expect_equal(nrow(combined), 10)
})

testthat::test_that("scope is derived from package identifiers", {
  testthat::expect_equal(
    package_scope(c("knb-lter-ntl.1.52", "edi.99.1", "knb-lter-and.4027")),
    c("knb-lter-ntl", "edi", "knb-lter-and")
  )
})
