#' Parse one EML document into tidy tables
#'
#' Namespaces are stripped so the same XPath expressions work for EML
#' 2.0-2.2 (including `stmml:unitList` custom-unit definitions). Raw corpus
#' files are only read, never modified.
#'
#' @param path Path to an EML XML file, or literal XML text (used in tests).
#' @param file_label Identifier stored in the `file` column.
#' @param snippet_chars Maximum characters kept from abstracts and methods.
#' @return A named list of tibbles: `datasets` (one row), `attributes`,
#'   `custom_units`, and `annotations`. Unreadable documents return a
#'   `datasets` row with a non-missing `parse_error` and empty tables.
parse_eml_file <- function(path, file_label = basename(path), snippet_chars = 1000) {
  doc <- tryCatch(xml2::read_xml(path), error = function(e) e)
  file_info <- file_fingerprint(path)
  if (inherits(doc, "error")) {
    return(empty_parse(file_label, conditionMessage(doc), file_info))
  }
  xml2::xml_ns_strip(doc)
  root <- xml2::xml_root(doc)
  dataset <- xml2::xml_find_first(root, "./dataset")
  package_id <- xml2::xml_attr(root, "packageId")
  subject_id <- dplyr::coalesce(package_id, file_label)

  entities <- xml2::xml_find_all(dataset, "./*[attributeList]")
  entity_tables <- purrr::imap(as.list(entities), \(entity, i) {
    parse_entity(entity, i, subject_id)
  })
  attributes <- purrr::map(entity_tables, "attributes") |>
    purrr::list_rbind()
  attributes <- if (nrow(attributes) == 0) {
    empty_attributes()
  } else {
    dplyr::mutate(attributes, package_id = package_id, .before = 1)
  }

  list(
    datasets = parse_dataset(root, dataset, file_label, package_id, snippet_chars, file_info) |>
      dplyr::mutate(n_entities = length(entities), n_attributes = nrow(attributes)),
    attributes = attributes,
    custom_units = parse_custom_units(root, package_id),
    annotations = parse_annotations(root, dataset, entity_tables, package_id)
  )
}

#' Parse a batch of EML files and combine the tables
#'
#' @param paths Character vector of EML file paths.
#' @return A named list of combined tibbles, as from [parse_eml_file()].
parse_eml_batch <- function(paths) {
  parsed <- purrr::map(paths, parse_eml_file)
  table_names <- c("datasets", "attributes", "custom_units", "annotations")
  purrr::set_names(table_names) |>
    purrr::map(\(name) purrr::list_rbind(purrr::map(parsed, name)))
}

#' Combine one table from many parsed batches
#'
#' @param batches List of outputs from [parse_eml_batch()].
#' @param name Table name, such as `"attributes"`.
#' @return A single tibble.
combine_parsed <- function(batches, name) {
  purrr::list_rbind(purrr::map(batches, name))
}

#' Derive the EDI scope from a package identifier
#'
#' @param package_id Character vector such as `"knb-lter-ntl.1.2"`.
#' @return Character vector of scopes, such as `"knb-lter-ntl"`.
package_scope <- function(package_id) {
  stringr::str_remove(package_id, "\\.[0-9]+(\\.[0-9]+)?$")
}

#' List corpus EML files, optionally as a seeded sample
#'
#' @param corpus_root Directory containing EML XML files.
#' @param sample_n Number of files to sample; `0` lists the full corpus.
#' @param seed Random seed used when sampling.
#' @return Sorted character vector of file paths.
list_corpus_files <- function(corpus_root, sample_n = 0, seed = 20261112) {
  if (!nzchar(corpus_root) || !dir.exists(corpus_root)) {
    stop("Set EML_CORPUS_ROOT to the EML corpus directory.", call. = FALSE)
  }
  files <- sort(list.files(corpus_root, pattern = "\\.xml$", full.names = TRUE))
  if (sample_n > 0 && sample_n < length(files)) {
    set.seed(seed)
    files <- sort(sample(files, sample_n))
  }
  files
}

# ---- internal helpers --------------------------------------------------------

first_text <- function(node, xpath) {
  found <- xml2::xml_find_first(node, xpath)
  out <- xml2::xml_text(found)
  out[is.na(found) | !nzchar(out)] <- NA_character_
  stringr::str_squish(out)
}

file_fingerprint <- function(path) {
  is_file <- length(path) == 1 && !grepl("<", path, fixed = TRUE) && file.exists(path)
  if (!is_file) {
    return(list(sha256 = NA_character_, bytes = NA_real_))
  }
  list(sha256 = digest::digest(file = path, algo = "sha256"), bytes = file.size(path))
}

parse_dataset <- function(root, dataset, file_label, package_id, snippet_chars, file_info) {
  keywords <- xml2::xml_find_all(dataset, ".//keywordSet")
  keyword_text <- purrr::map_chr(as.list(keywords), \(set) {
    words <- xml2::xml_text(xml2::xml_find_all(set, "./keyword"))
    paste(stringr::str_squish(words), collapse = "; ")
  })
  thesauri <- purrr::map_chr(as.list(keywords), \(set) {
    dplyr::coalesce(first_text(set, "./keywordThesaurus"), "none")
  })
  coordinate <- function(name) {
    as.numeric(first_text(dataset, paste0(".//boundingCoordinates/", name)))
  }
  tibble::tibble(
    file = file_label,
    package_id = package_id,
    scope = package_scope(package_id),
    eml_version = stringr::str_extract(
      dplyr::coalesce(xml2::xml_attr(root, "schemaLocation"), ""), "eml-[0-9.]+"
    ),
    title = first_text(dataset, "./title"),
    keywords = paste(keyword_text[nzchar(keyword_text)], collapse = "; "),
    keyword_thesauri = paste(unique(thesauri), collapse = "; "),
    abstract = stringr::str_trunc(first_text(dataset, "./abstract"), snippet_chars),
    methods = stringr::str_trunc(first_text(dataset, "./methods"), snippet_chars),
    geographic_description = first_text(dataset, ".//geographicCoverage/geographicDescription"),
    west = coordinate("westBoundingCoordinate"),
    east = coordinate("eastBoundingCoordinate"),
    north = coordinate("northBoundingCoordinate"),
    south = coordinate("southBoundingCoordinate"),
    begin_date = dplyr::coalesce(
      first_text(dataset, ".//temporalCoverage//beginDate/calendarDate"),
      first_text(dataset, ".//temporalCoverage//singleDateTime/calendarDate")
    ),
    end_date = first_text(dataset, ".//temporalCoverage//endDate/calendarDate"),
    has_taxonomic_coverage = length(xml2::xml_find_all(dataset, ".//taxonomicCoverage")) > 0,
    sha256 = file_info$sha256,
    bytes = file_info$bytes,
    parse_error = NA_character_
  )
}

parse_entity <- function(entity, entity_index, subject_id) {
  attrs <- xml2::xml_find_all(entity, "./attributeList/attribute")
  if (length(attrs) == 0) {
    return(list(attributes = NULL, xml_ids = character(), nodes = attrs))
  }
  text_of <- \(xpath) first_text(attrs, xpath)
  scale <- xml2::xml_find_first(attrs, "./measurementScale/*")
  attributes <- tibble::tibble(
    attribute_id = sprintf("%s#e%d.a%d", subject_id, entity_index, seq_along(attrs)),
    entity_type = xml2::xml_name(entity),
    entity = first_text(entity, "./entityName"),
    attribute_name = text_of("./attributeName"),
    attribute_label = text_of("./attributeLabel"),
    attribute_definition = text_of("./attributeDefinition"),
    measurement_scale = ifelse(is.na(scale), NA_character_, xml2::xml_name(scale)),
    standard_unit = text_of(".//unit/standardUnit"),
    custom_unit = text_of(".//unit/customUnit"),
    precision = text_of(".//precision"),
    number_type = text_of(".//numberType"),
    n_annotations = lengths(purrr::map(as.list(attrs), \(a) {
      xml2::xml_find_all(a, "./annotation")
    }))
  )
  list(attributes = attributes, xml_ids = xml2::xml_attr(attrs, "id"), nodes = attrs)
}

annotation_rows <- function(nodes, subject_level, attribute_id = NA_character_) {
  if (length(nodes) == 0) {
    return(NULL)
  }
  property <- xml2::xml_find_first(nodes, "./propertyURI")
  value <- xml2::xml_find_first(nodes, "./valueURI")
  tibble::tibble(
    subject_level = subject_level,
    attribute_id = attribute_id,
    property_label = xml2::xml_attr(property, "label"),
    property_uri = stringr::str_squish(xml2::xml_text(property)),
    value_label = xml2::xml_attr(value, "label"),
    value_uri = stringr::str_squish(xml2::xml_text(value))
  )
}

parse_annotations <- function(root, dataset, entity_tables, package_id) {
  attribute_level <- purrr::map(entity_tables, \(tbl) {
    purrr::map2(as.list(tbl$nodes), tbl$attributes$attribute_id, \(node, id) {
      annotation_rows(xml2::xml_find_all(node, "./annotation"), "attribute", id)
    })
  }) |>
    purrr::list_flatten()

  # EML 2.2 also allows a top-level <annotations> block that points at
  # elements by id; resolve references to attribute ids where possible.
  id_lookup <- purrr::map(entity_tables, \(tbl) {
    if (is.null(tbl$attributes)) {
      return(NULL)
    }
    tibble::tibble(xml_id = tbl$xml_ids, attribute_id = tbl$attributes$attribute_id)
  }) |>
    purrr::list_rbind()
  referenced <- xml2::xml_find_all(root, "./annotations/annotation")
  referenced_rows <- NULL
  if (length(referenced) > 0) {
    refs <- xml2::xml_attr(referenced, "references")
    matched <- if (!is.null(id_lookup) && nrow(id_lookup) > 0) {
      id_lookup$attribute_id[match(refs, id_lookup$xml_id)]
    } else {
      rep(NA_character_, length(refs))
    }
    referenced_rows <- annotation_rows(referenced, "referenced", matched) |>
      dplyr::mutate(subject_level = ifelse(is.na(attribute_id), "referenced", "attribute"))
  }

  dplyr::bind_rows(
    annotation_rows(xml2::xml_find_all(dataset, "./annotation"), "dataset"),
    annotation_rows(xml2::xml_find_all(dataset, "./*[attributeList]/annotation"), "entity"),
    attribute_level,
    referenced_rows
  ) |>
    empty_if_null(empty_annotations()) |>
    dplyr::mutate(package_id = package_id, .before = 1)
}

parse_custom_units <- function(root, package_id) {
  # xml_ns_strip() only removes default namespaces; unitList is often
  # prefixed (stmml:), so match on local names.
  units <- xml2::xml_find_all(
    root, ".//additionalMetadata//*[local-name()='unitList']/*[local-name()='unit']"
  )
  if (length(units) == 0) {
    return(empty_custom_units())
  }
  attr_of <- \(name) xml2::xml_attr(units, name)
  tibble::tibble(
    package_id = package_id,
    unit_id = attr_of("id"),
    name = attr_of("name"),
    unit_type = attr_of("unitType"),
    parent_si = attr_of("parentSI"),
    multiplier_to_si = attr_of("multiplierToSI"),
    abbreviation = attr_of("abbreviation"),
    description = first_text(units, "./*[local-name()='description']")
  )
}

empty_if_null <- function(x, empty) {
  if (is.null(x) || nrow(x) == 0) empty else x
}

empty_attributes <- function() {
  tibble::tibble(
    package_id = character(), attribute_id = character(), entity_type = character(),
    entity = character(), attribute_name = character(), attribute_label = character(),
    attribute_definition = character(), measurement_scale = character(),
    standard_unit = character(), custom_unit = character(), precision = character(),
    number_type = character(), n_annotations = integer()
  )
}

empty_annotations <- function() {
  tibble::tibble(
    subject_level = character(), attribute_id = character(), property_label = character(),
    property_uri = character(), value_label = character(), value_uri = character()
  )
}

empty_custom_units <- function() {
  tibble::tibble(
    package_id = character(), unit_id = character(), name = character(),
    unit_type = character(), parent_si = character(), multiplier_to_si = character(),
    abbreviation = character(), description = character()
  )
}

empty_parse <- function(file_label, message, file_info) {
  list(
    datasets = tibble::tibble(
      file = file_label, sha256 = file_info$sha256, bytes = file_info$bytes,
      parse_error = message
    ),
    attributes = empty_attributes(),
    custom_units = empty_custom_units(),
    annotations = dplyr::mutate(empty_annotations(), package_id = character(), .before = 1)
  )
}
