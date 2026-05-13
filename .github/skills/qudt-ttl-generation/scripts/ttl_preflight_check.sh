#!/usr/bin/env bash
set -euo pipefail

if [[ "$#" -lt 1 ]]; then
  echo "Usage: $0 <ttl-file> [more ttl files...]"
  exit 2
fi

errors=0

for file in "$@"; do
  if [[ ! -f "$file" ]]; then
    echo "ERROR: file not found: $file"
    errors=$((errors + 1))
    continue
  fi

  echo "Checking $file"

  if rg -n "quantitykind:(unknown|Unknown)" "$file" >/dev/null; then
    echo "ERROR: placeholder quantity kind found"
    rg -n "quantitykind:(unknown|Unknown)" "$file" || true
    errors=$((errors + 1))
  fi

  if rg -n "TO DO|to do" "$file" >/dev/null; then
    echo "ERROR: unresolved TODO markers found"
    rg -n "TO DO|to do" "$file" || true
    errors=$((errors + 1))
  fi

  if rg -n "qkdv:[A-Za-z0-9-]*D1" "$file" | rg -v "A0E0L0I0M0H0T0D1" >/dev/null; then
    echo "ERROR: D1 used on non-dimensionless vector"
    rg -n "qkdv:[A-Za-z0-9-]*D1" "$file" | rg -v "A0E0L0I0M0H0T0D1" || true
    errors=$((errors + 1))
  fi

  awk '
    function count(hay, needle,   c, pos, n) {
      c = 0
      n = length(needle)
      pos = index(hay, needle)
      while (pos > 0) {
        c++
        hay = substr(hay, pos + n)
        pos = index(hay, needle)
      }
      return c
    }
    function flush_block(   hk_count, hdv_count) {
      if (!in_unit) {
        return
      }
      if (index(block, "qudt:hasDimensionVector") == 0) {
        printf("ERROR: %s missing qudt:hasDimensionVector in %s\n", file, unit_id)
        has_error = 1
      }
      hk_count = count(block, "qudt:hasQuantityKind")
      if (hk_count == 0) {
        printf("ERROR: %s missing qudt:hasQuantityKind in %s\n", file, unit_id)
        has_error = 1
      } else if (hk_count > 1) {
        printf("WARN:  %s has %d qudt:hasQuantityKind values in %s\n", file, hk_count, unit_id)
      }
      if (index(block, "qudt:conversionMultiplier") == 0) {
        printf("ERROR: %s missing qudt:conversionMultiplier in %s\n", file, unit_id)
        has_error = 1
      }
      if (index(block, "qudt:plainTextDescription") == 0) {
        printf("ERROR: %s missing qudt:plainTextDescription in %s\n", file, unit_id)
        has_error = 1
      }
      if (index(block, "rdfs:isDefinedBy") == 0) {
        printf("ERROR: %s missing rdfs:isDefinedBy in %s\n", file, unit_id)
        has_error = 1
      }
      in_unit = 0
      unit_id = ""
      block = ""
    }
    BEGIN {
      in_unit = 0
      has_error = 0
      unit_id = ""
      block = ""
    }
    /^[[:space:]]*unit:[A-Za-z0-9._-]+[[:space:]]*$/ {
      flush_block()
      in_unit = 1
      unit_id = $1
      block = $0 "\n"
      next
    }
    {
      if (in_unit) {
        block = block $0 "\n"
      }
    }
    /^[[:space:]]*\.[[:space:]]*$/ {
      flush_block()
    }
    END {
      flush_block()
      if (has_error) {
        exit 1
      }
    }
  ' file="$file" "$file" || errors=$((errors + 1))

  mapfile -t versions < <(rg -o "http://qudt.org/[0-9]+\.[0-9]+(\.[0-9]+)?/vocab/unit" "$file" | sort -u || true)
  if [[ "${#versions[@]}" -gt 1 ]]; then
    echo "WARN: mixed QUDT unit vocabulary versions in $file"
    printf '  %s\n' "${versions[@]}"
  fi
done

if [[ "$errors" -gt 0 ]]; then
  echo "Preflight failed with $errors error group(s)."
  exit 1
fi

echo "Preflight passed."
