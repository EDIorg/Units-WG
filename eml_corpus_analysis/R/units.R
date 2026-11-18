#' Normalize an EML unit name for exact joins
#'
#' The RCode_JP reference tables key units by lowercased names, so case is
#' folded and surrounding whitespace removed; inner text is preserved.
#'
#' @param unit Character vector of EML unit names.
#' @return Lowercased, trimmed character vector.
normalize_unit_key <- function(unit) {
  stringr::str_to_lower(stringr::str_trim(unit))
}

unit_prefixes <- "(?:pico|nano|micro|milli|centi|deci|deka|hecto|kilo|mega|giga|tera)?"
unit_bases <- paste0(
  "(?:gram|meter|liter|mole|second|minute|hour|day|week|month|year|hectare|acre|",
  "number|count|individual|cell|joule|watt|becquerel|tonne|ton|ounce|pound|foot|",
  "inch|mile|cord|calorie|pascal|bar|siemens|volt|kelvin|celsius|part|decay)"
)

#' Canonical match key for EML unit names
#'
#' Groups spelling variants (plurals, "squareMeter" vs. "meterSquared",
#' "litre" vs. "liter") and treats denominators as an unordered set, so
#' `micromolePerMilligramPerHour` and `micromolePerHourPerMilligram` match.
#' The key supports grouping and bridging only; raw names are always kept.
#'
#' @param unit Character vector of EML unit names.
#' @return Character vector of keys of the form `numerator/denominator.denominator`.
unit_match_key <- function(unit) {
  key <- normalize_unit_key(unit) |>
    stringr::str_remove_all("[\\s_-]+") |>
    stringr::str_replace_all(c("litre" = "liter", "metre" = "meter", "feet" = "foot")) |>
    stringr::str_replace_all(
      paste0("(", unit_prefixes, unit_bases, ")s(?=$|per|squared|cubed|[^a-z])"), "\\1"
    ) |>
    stringr::str_replace_all(paste0("square(", unit_prefixes, "meter)"), "\\1squared") |>
    stringr::str_replace_all(paste0("cubic(", unit_prefixes, "(?:meter|foot))"), "\\1cubed") |>
    stringr::str_replace("^(?:inverse|reciprocal)", "per")
  purrr::map_chr(key, \(k) {
    if (is.na(k)) {
      return(NA_character_)
    }
    parts <- stringr::str_split_1(k, paste0("per(?=", unit_prefixes, unit_bases, ")"))
    compose_match_key(parts[1], parts[-1])
  })
}

#' Canonical match key for QUDT unit qnames
#'
#' Translates qname tokens (e.g. `MicroMOL-PER-HR-MilliGM`) into the same key
#' space as [unit_match_key()], so EML names can be bridged to QUDT units and
#' to candidate qnames from earlier runs.
#'
#' @param qname Character vector of QUDT unit local names.
#' @return Character vector of match keys.
qname_match_key <- function(qname) {
  bases <- c(
    GM = "gram", M = "meter", M2 = "metersquared", M3 = "metercubed", L = "liter",
    MOL = "mole", MOLE = "mole", NUM = "number", SEC = "second", MIN = "minute",
    HR = "hour", DAY = "day", WK = "week", MO = "month", YR = "year", HA = "hectare",
    AC = "acre", J = "joule", W = "watt", BQ = "becquerel", PPM = "partpermillion",
    PPB = "partperbillion", TON_US = "ton", TONNE = "tonne", OZ = "ounce", LB = "pound",
    FT = "foot", FT3 = "footcubed", IN = "inch", MI_US = "mile", CORD = "cord",
    PA = "pascal", BAR = "bar", S = "siemens", V = "volt", K = "kelvin", DEG_C = "celsius",
    PERCENT = "percent", UNITLESS = "dimensionless"
  )
  prefix_pattern <- "^(Pico|Nano|Micro|Milli|Centi|Deci|Deka|Hecto|Kilo|Mega|Giga|Tera)"
  translate <- function(token) {
    prefix <- stringr::str_extract(token, prefix_pattern) |>
      dplyr::coalesce("") |>
      stringr::str_to_lower()
    base <- stringr::str_remove(token, prefix_pattern)
    base <- dplyr::coalesce(unname(bases[base]), stringr::str_to_lower(base))
    paste0(prefix, base)
  }
  purrr::map_chr(qname, \(q) {
    if (is.na(q)) {
      return(NA_character_)
    }
    tokens <- stringr::str_split_1(q, "-")
    per <- match("PER", tokens)
    numerator <- if (is.na(per)) tokens else utils::head(tokens, per - 1)
    denominator <- if (is.na(per)) {
      character()
    } else {
      tokens[seq(per + 1, length.out = length(tokens) - per)]
    }
    compose_match_key(
      paste(purrr::map_chr(numerator, translate), collapse = ""),
      purrr::map_chr(denominator, translate)
    )
  })
}

compose_match_key <- function(numerator, denominators) {
  denominators <- sort(denominators[nzchar(denominators)])
  if (length(denominators) == 0) {
    return(numerator)
  }
  paste0(numerator, "/", paste(denominators, collapse = "."))
}

#' Read the RCode_JP unit mapping as a reference input
#'
#' @param path Path to `allUnitsWithOrWithoutQUDTunits.csv`.
#' @return Tibble keyed by `unit_key` with the mapped QUDT URI and the
#'   RCode_JP pseudo-qname.
read_jp_mapping <- function(path) {
  readr::read_csv(path, show_col_types = FALSE, col_types = readr::cols(.default = "c")) |>
    dplyr::transmute(
      unit_key = normalize_unit_key(unit),
      jp_qudt_uri = dplyr::na_if(qudtUri, ""),
      jp_qname = dplyr::na_if(qudtUnit, "")
    ) |>
    dplyr::distinct(unit_key, .keep_all = TRUE)
}

#' Run a SPARQL SELECT query against QUDT and return a tibble
#'
#' @param query SPARQL query text.
#' @param endpoint SPARQL endpoint URL.
#' @return Tibble with all columns as character.
qudt_sparql <- function(query, endpoint) {
  httr2::request(endpoint) |>
    httr2::req_body_form(query = query) |>
    httr2::req_headers(Accept = "text/csv") |>
    httr2::req_timeout(300) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    I() |>
    readr::read_csv(show_col_types = FALSE, col_types = readr::cols(.default = "c"))
}

#' Fetch QUDT units with quantity kinds and dimension vectors
#'
#' @param endpoint QUDT SPARQL endpoint URL.
#' @return Tibble with one row per unit, QK relation, and quantity kind, plus
#'   `retrieved`. `qk_relation` distinguishes `unitForQuantityKind`,
#'   `categorizedByQuantityKind`, and the legacy `hasQuantityKind`.
fetch_qudt_units <- function(endpoint) {
  query <- "
    PREFIX qudt: <http://qudt.org/schema/qudt/>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    SELECT ?unit ?label ?relation ?qk ?dv ?multiplier WHERE {
      ?unit a qudt:Unit .
      OPTIONAL { ?unit rdfs:label ?label FILTER(lang(?label) = 'en' || lang(?label) = '') }
      OPTIONAL {
        VALUES ?relation {
          qudt:unitForQuantityKind qudt:categorizedByQuantityKind qudt:hasQuantityKind
        }
        ?unit ?relation ?qk
      }
      OPTIONAL { ?unit qudt:hasDimensionVector ?dv }
      OPTIONAL { ?unit qudt:conversionMultiplier ?multiplier }
    }"
  qudt_sparql(query, endpoint) |>
    dplyr::distinct(unit, relation, qk, .keep_all = TRUE) |>
    dplyr::transmute(
      unit_uri = unit,
      qudt_unit = local_name(unit),
      unit_label = label,
      qk_relation = local_name(relation),
      qk = local_name(qk),
      dimension_vector = local_name(dv),
      multiplier = multiplier,
      retrieved = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
    )
}

#' Fetch QUDT quantity kinds with dimension vectors and parents
#'
#' @param endpoint QUDT SPARQL endpoint URL.
#' @return Tibble with one row per quantity kind and parent relation.
fetch_qudt_quantity_kinds <- function(endpoint) {
  query <- "
    PREFIX qudt: <http://qudt.org/schema/qudt/>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    SELECT ?qk ?label ?dv ?specialization ?broader WHERE {
      ?qk a qudt:QuantityKind .
      OPTIONAL { ?qk rdfs:label ?label FILTER(lang(?label) = 'en' || lang(?label) = '') }
      OPTIONAL { ?qk qudt:hasDimensionVector ?dv }
      OPTIONAL { ?qk qudt:specializationOf ?specialization }
      OPTIONAL { ?qk skos:broader ?broader }
    }"
  qudt_sparql(query, endpoint) |>
    dplyr::transmute(
      qk = local_name(qk),
      qk_label = label,
      dimension_vector = local_name(dv),
      specialization_of = local_name(specialization),
      broader = local_name(broader)
    ) |>
    dplyr::distinct()
}

local_name <- function(uri) {
  stringr::str_remove(uri, "^.*[/#]")
}

#' Build a verified bridge from EML unit names to QUDT units
#'
#' The RCode_JP mapping is used first, but only when its QUDT URI still
#' exists in the current QUDT vocabulary. Otherwise, an EML name is bridged
#' when its [unit_match_key()] equals exactly one QUDT unit's
#' [qname_match_key()].
#'
#' @param unit_names Character vector of distinct EML unit names.
#' @param jp_mapping Output of [read_jp_mapping()].
#' @param qudt_units Output of [fetch_qudt_units()].
#' @return Tibble keyed by `unit_key` with `qudt_unit`, `mapping_source`,
#'   `dimension_vector`, `match_key`, and `jp_qname`.
build_unit_map <- function(unit_names, jp_mapping, qudt_units) {
  qudt_distinct <- dplyr::distinct(qudt_units, unit_uri, qudt_unit, dimension_vector)
  qudt_keys <- qudt_distinct |>
    dplyr::mutate(match_key = qname_match_key(qudt_unit)) |>
    dplyr::filter(dplyr::n() == 1, .by = match_key) |>
    dplyr::select(match_key, key_qudt_unit = qudt_unit)

  tibble::tibble(unit_key = unique(normalize_unit_key(unit_names))) |>
    dplyr::filter(!is.na(unit_key)) |>
    dplyr::mutate(match_key = unit_match_key(unit_key)) |>
    dplyr::left_join(jp_mapping, by = "unit_key") |>
    dplyr::mutate(jp_verified = jp_qudt_uri %in% qudt_distinct$unit_uri) |>
    dplyr::left_join(qudt_keys, by = "match_key") |>
    dplyr::mutate(
      qudt_unit = dplyr::case_when(
        jp_verified ~ local_name(jp_qudt_uri),
        .default = key_qudt_unit
      ),
      mapping_source = dplyr::case_when(
        jp_verified ~ "rcode_jp_verified",
        !is.na(key_qudt_unit) ~ "match_key",
        !is.na(jp_qudt_uri) ~ "rcode_jp_not_in_qudt",
        .default = "unmapped"
      )
    ) |>
    dplyr::left_join(
      dplyr::distinct(qudt_distinct, qudt_unit, .keep_all = TRUE) |>
        dplyr::select(qudt_unit, dimension_vector),
      by = "qudt_unit"
    ) |>
    dplyr::select(unit_key, match_key, qudt_unit, mapping_source, dimension_vector, jp_qname)
}

#' Attach unit keys and QUDT mappings to attributes
#'
#' @param attributes Attribute table from the parser.
#' @param unit_map Output of [build_unit_map()].
#' @return Attributes with `unit`, `unit_kind`, `unit_key`, `match_key`,
#'   `qudt_unit`, `mapping_source`, `dimension_vector`, `jp_qname`,
#'   `unit_group` (QUDT unit when mapped, otherwise the match key), and
#'   `scope`.
annotate_units <- function(attributes, unit_map) {
  attributes |>
    dplyr::mutate(
      scope = package_scope(package_id),
      unit = dplyr::coalesce(standard_unit, custom_unit),
      unit_kind = dplyr::case_when(
        !is.na(standard_unit) ~ "standard",
        !is.na(custom_unit) ~ "custom",
        .default = "none"
      ),
      unit_key = normalize_unit_key(unit)
    ) |>
    dplyr::left_join(unit_map, by = "unit_key") |>
    dplyr::mutate(unit_group = dplyr::coalesce(qudt_unit, match_key))
}
