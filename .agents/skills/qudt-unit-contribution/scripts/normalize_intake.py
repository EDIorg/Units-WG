#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# ///

from __future__ import annotations

import argparse
import csv
from collections.abc import Iterable
from pathlib import Path

INPUT_FIELDS = (
    "unit_expression",
    "measurement_context",
    "qualifier_definition",
    "conversion_definition",
    "conversion_source",
    "source_variable",
    "dataset_id",
    "variable_name",
    "variable_definition",
    "proposed_qname",
    "proposed_quantity_kind",
    "preferred_label",
    "preferred_symbol",
    "known_conversion",
    "reference_url",
    "notes",
)
OUTPUT_FIELDS = (
    ("record_id",)
    + INPUT_FIELDS
    + (
        "source_line",
        "intake_status",
        "intake_issues",
    )
)


def _clean(value: object) -> str:
    return "" if value is None else str(value).strip()


def _read_delimited(path: Path) -> list[tuple[int, dict[str, str]]]:
    delimiter = "\t" if path.suffix.lower() == ".tsv" else ","
    with path.open(encoding="utf-8-sig", newline="") as handle:
        reader = csv.DictReader(handle, delimiter=delimiter)
        if not reader.fieldnames:
            raise ValueError(f"{path}: missing header row")
        headers = tuple(_clean(field) for field in reader.fieldnames)
        if "unit_expression" not in headers:
            raise ValueError(f"{path}: header must include unit_expression")

        records: list[tuple[int, dict[str, str]]] = []
        for row in reader:
            normalized = {_clean(key): _clean(value) for key, value in row.items()}
            records.append((reader.line_num, normalized))
        return records


def _read_legacy(path: Path) -> list[tuple[int, dict[str, str]]]:
    records: list[tuple[int, dict[str, str]]] = []
    with path.open(encoding="utf-8-sig") as handle:
        for line_number, line in enumerate(handle, start=1):
            stripped = line.strip()
            if not stripped or stripped.startswith("#"):
                continue
            for expression in stripped.split(","):
                if expression.strip():
                    records.append(
                        (line_number, {"unit_expression": expression.strip()})
                    )
    return records


def normalize_records(
    records: Iterable[tuple[int, dict[str, str]]],
) -> list[dict[str, str]]:
    normalized_records: list[dict[str, str]] = []

    for index, (source_line, source) in enumerate(records, start=1):
        record = {field: _clean(source.get(field)) for field in INPUT_FIELDS}
        issues: list[str] = []

        record["record_id"] = f"unit-{index:04d}"

        if not record["unit_expression"]:
            issues.append("unit_expression missing")

        record["source_line"] = str(source_line)
        record["intake_status"] = (
            "needs-clarification" if issues else "ready-for-research"
        )
        record["intake_issues"] = "; ".join(issues)
        normalized_records.append(record)

    return normalized_records


def normalize_file(path: Path) -> list[dict[str, str]]:
    if path.suffix.lower() in {".csv", ".tsv"}:
        records = _read_delimited(path)
    else:
        records = _read_legacy(path)
    return normalize_records(records)


def write_records(path: Path, records: list[dict[str, str]]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=OUTPUT_FIELDS)
        writer.writeheader()
        writer.writerows(records)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Normalize QUDT candidate CSV, TSV, or legacy text input."
    )
    parser.add_argument("input", type=Path, help="Candidate input file")
    parser.add_argument("--output", required=True, type=Path, help="Normalized CSV")
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    records = normalize_file(args.input)
    write_records(args.output, records)
    ready = sum(row["intake_status"] == "ready-for-research" for row in records)
    needs_clarification = len(records) - ready
    print(
        f"Normalized {len(records)} candidate(s): "
        f"{ready} ready, {needs_clarification} need clarification."
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
