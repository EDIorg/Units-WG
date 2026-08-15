#!/usr/bin/env bash

set -euo pipefail

if [[ $# -lt 1 ]]; then
  printf 'Usage: %s <ttl-file> [more ttl files...]\n' "$0" >&2
  exit 2
fi

for command in rapper rg; do
  if ! command -v "$command" >/dev/null 2>&1; then
    printf 'ERROR: required command not found: %s\n' "$command" >&2
    exit 2
  fi
done

errors=0
warnings=0
temp_dir=$(mktemp -d)
trap 'rm -rf "$temp_dir"' EXIT
combined_triples="${temp_dir}/combined.nt"
: >"$combined_triples"

rdf_type='http://www.w3.org/1999/02/22-rdf-syntax-ns#type'
rdfs_label='http://www.w3.org/2000/01/rdf-schema#label'
rdfs_is_defined_by='http://www.w3.org/2000/01/rdf-schema#isDefinedBy'
dcterms_description='http://purl.org/dc/terms/description'
qudt_ns='http://qudt.org/schema/qudt/'
xsd_ns='http://www.w3.org/2001/XMLSchema#'

report_error() {
  printf 'ERROR: %s\n' "$*" >&2
  errors=$((errors + 1))
}

report_warning() {
  printf 'WARN: %s\n' "$*" >&2
  warnings=$((warnings + 1))
}

predicate_count() {
  local triples=$1
  local subject=$2
  local predicate=$3
  rg -F -c "${subject} <${predicate}>" "$triples" 2>/dev/null || true
}

require_predicate() {
  local triples=$1
  local subject=$2
  local predicate=$3
  local label=$4
  if [[ $(predicate_count "$triples" "$subject" "$predicate") -eq 0 ]]; then
    report_error "${subject} missing ${label}"
  fi
}

require_object() {
  local triples=$1
  local subject=$2
  local predicate=$3
  local object=$4
  local label=$5
  if ! rg -F "${subject} <${predicate}> <${object}> ." "$triples" >/dev/null; then
    report_error "${subject} ${label} must be <${object}>"
  fi
}

warn_language_tagged_literal() {
  local triples=$1
  local subject=$2
  local predicate=$3
  local label=$4
  local triple
  triple=$(rg -F "${subject} <${predicate}>" "$triples" || true)
  if [[ "$triple" == *'"@'* ]]; then
    report_warning "${subject} ${label} has a language tag; confirm this is intentional and consistent with current QUDT guidance"
  fi
}

warn_untagged_literal() {
  local triples=$1
  local subject=$2
  local predicate=$3
  local label=$4
  local triple
  triple=$(rg -F "${subject} <${predicate}>" "$triples" || true)
  if [[ "$triple" != *'"@'* ]]; then
    report_warning "${subject} ${label} has no language tag; confirm this is intentional and consistent with current QUDT guidance"
  fi
}

check_decimal() {
  local triples=$1
  local subject=$2
  local predicate=$3
  local datatype=$4
  local label=$5
  local triple
  triple=$(rg -F "${subject} <${predicate}>" "$triples" || true)
  if [[ -n "$triple" && "$triple" != *"^^<${xsd_ns}${datatype}>"* ]]; then
    report_error "${subject} ${label} must be xsd:${datatype}"
  fi
}

for file in "$@"; do
  if [[ ! -f "$file" ]]; then
    report_error "file not found: ${file}"
    continue
  fi

  printf 'Checking %s\n' "$file"
  triples="${temp_dir}/$(printf '%s' "$file" | sha256sum | cut -d' ' -f1).nt"
  if ! rapper -q -i turtle -o ntriples "$file" >"$triples"; then
    report_error "Turtle parse failed: ${file}"
    continue
  fi
  cat "$triples" >>"$combined_triples"

  if rg -n 'quantitykind:(?:Unknown|unknown)|qkdv:(?:Unknown|unknown)|TODO_VERIFY|TO DO|\bTODO\b' "$file"; then
    report_error "unresolved placeholder found in ${file}"
  fi

  mapfile -t units < <(
    awk -v type="<${rdf_type}>" -v class="<${qudt_ns}Unit>" \
      '$2 == type && $3 == class { print $1 }' "$triples" | sort -u
  )
  for subject in "${units[@]}"; do
    require_predicate "$triples" "$subject" "$dcterms_description" dcterms:description
    require_predicate "$triples" "$subject" "${qudt_ns}conversionMultiplier" qudt:conversionMultiplier
    require_predicate "$triples" "$subject" "${qudt_ns}conversionMultiplierSN" qudt:conversionMultiplierSN
    require_predicate "$triples" "$subject" "${qudt_ns}hasDimensionVector" qudt:hasDimensionVector
    require_predicate "$triples" "$subject" "${qudt_ns}hasQuantityKind" qudt:hasQuantityKind
    require_predicate "$triples" "$subject" "$rdfs_is_defined_by" rdfs:isDefinedBy
    require_predicate "$triples" "$subject" "$rdfs_label" rdfs:label
    require_object "$triples" "$subject" "$rdfs_is_defined_by" \
      'http://qudt.org/$$QUDT_VERSION$$/vocab/unit' rdfs:isDefinedBy
    warn_language_tagged_literal "$triples" "$subject" "$dcterms_description" \
      dcterms:description
    warn_untagged_literal "$triples" "$subject" "$rdfs_label" rdfs:label

    if [[ $(predicate_count "$triples" "$subject" "${qudt_ns}hasDimensionVector") -gt 1 ]]; then
      report_error "${subject} has more than one DimensionVector"
    fi
    check_decimal "$triples" "$subject" "${qudt_ns}conversionMultiplier" decimal qudt:conversionMultiplier
    check_decimal "$triples" "$subject" "${qudt_ns}conversionMultiplierSN" double qudt:conversionMultiplierSN
    check_decimal "$triples" "$subject" "${qudt_ns}conversionOffset" decimal qudt:conversionOffset

    if [[ $(predicate_count "$triples" "$subject" "${qudt_ns}plainTextDescription") -eq 0 ]]; then
      report_warning "${subject} has no qudt:plainTextDescription"
    else
      warn_language_tagged_literal "$triples" "$subject" "${qudt_ns}plainTextDescription" \
        qudt:plainTextDescription
    fi
    if [[ $(predicate_count "$triples" "$subject" "${qudt_ns}symbol") -eq 0 ]]; then
      report_warning "${subject} has no qudt:symbol"
    fi
  done

  mapfile -t quantity_kinds < <(
    awk -v type="<${rdf_type}>" -v class="<${qudt_ns}QuantityKind>" \
      '$2 == type && $3 == class { print $1 }' "$triples" | sort -u
  )
  for subject in "${quantity_kinds[@]}"; do
    require_predicate "$triples" "$subject" "${qudt_ns}hasDimensionVector" qudt:hasDimensionVector
    require_predicate "$triples" "$subject" "${qudt_ns}plainTextDescription" qudt:plainTextDescription
    require_predicate "$triples" "$subject" "$rdfs_is_defined_by" rdfs:isDefinedBy
    require_predicate "$triples" "$subject" "$rdfs_label" rdfs:label
    require_object "$triples" "$subject" "$rdfs_is_defined_by" \
      'http://qudt.org/$$QUDT_VERSION$$/vocab/quantitykind' rdfs:isDefinedBy
    warn_language_tagged_literal "$triples" "$subject" "${qudt_ns}plainTextDescription" \
      qudt:plainTextDescription
  done

  mapfile -t dimension_vectors < <(
    awk -v type="<${rdf_type}>" -v class="<${qudt_ns}QuantityKindDimensionVector>" \
      '$2 == type && $3 == class { print $1 }' "$triples" | sort -u
  )
  for subject in "${dimension_vectors[@]}"; do
    require_predicate "$triples" "$subject" "$rdfs_is_defined_by" rdfs:isDefinedBy
    require_predicate "$triples" "$subject" "$rdfs_label" rdfs:label
    require_object "$triples" "$subject" "$rdfs_is_defined_by" \
      'http://qudt.org/$$QUDT_VERSION$$/vocab/dimensionvector' rdfs:isDefinedBy
  done
done

while read -r unit_subject quantity_kind; do
  [[ -n "$unit_subject" && -n "$quantity_kind" ]] || continue
  if ! rg -F "${quantity_kind} <${rdf_type}> <${qudt_ns}QuantityKind>" \
    "$combined_triples" >/dev/null; then
    continue
  fi

  unit_vector=$(
    awk -v subject="$unit_subject" -v predicate="<${qudt_ns}hasDimensionVector>" \
      '$1 == subject && $2 == predicate { print $3; exit }' "$combined_triples"
  )
  quantity_kind_vector=$(
    awk -v subject="$quantity_kind" -v predicate="<${qudt_ns}hasDimensionVector>" \
      '$1 == subject && $2 == predicate { print $3; exit }' "$combined_triples"
  )
  if [[ -n "$unit_vector" && -n "$quantity_kind_vector" && "$unit_vector" != "$quantity_kind_vector" ]]; then
    report_error "${unit_subject} vector ${unit_vector} conflicts with ${quantity_kind} vector ${quantity_kind_vector}"
  fi
done < <(
  awk -v type="<${rdf_type}>" -v unit_class="<${qudt_ns}Unit>" \
    -v qk_predicate="<${qudt_ns}hasQuantityKind>" '
      $2 == type && $3 == unit_class { units[$1] = 1 }
      $2 == qk_predicate { assignments[$1] = assignments[$1] " " $3 }
      END {
        for (unit in units) {
          count = split(assignments[unit], qks, " ")
          for (item = 1; item <= count; item++) {
            if (qks[item] != "") print unit, qks[item]
          }
        }
      }
    ' "$combined_triples"
)

if [[ $errors -gt 0 ]]; then
  printf 'Preflight failed with %d error(s) and %d warning(s).\n' "$errors" "$warnings" >&2
  exit 1
fi

printf 'Preflight passed with %d warning(s).\n' "$warnings"
