from __future__ import annotations

import importlib.util
import tempfile
import unittest
from pathlib import Path

SCRIPT = Path(__file__).parents[1] / "scripts" / "normalize_intake.py"
SPEC = importlib.util.spec_from_file_location("normalize_intake", SCRIPT)
if SPEC is None or SPEC.loader is None:
    raise RuntimeError(f"Unable to load {SCRIPT}")
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)


class NormalizeIntakeTest(unittest.TestCase):
    def test_structured_row_is_ready(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            path = Path(temp_dir) / "units.csv"
            path.write_text(
                "unit_expression,measurement_context\n"
                'GM-PER-HA-DAY,"Daily biomass production per hectare"\n',
                encoding="utf-8",
            )

            rows = MODULE.normalize_file(path)

        self.assertEqual(rows[0]["record_id"], "unit-0001")
        self.assertEqual(rows[0]["intake_status"], "ready-for-research")
        self.assertEqual(rows[0]["intake_issues"], "")

    def test_expression_only_legacy_rows_are_ready_for_research(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            path = Path(temp_dir) / "units.txt"
            path.write_text("GM-PER-HA-DAY,\nNUM-PER-M2,\n", encoding="utf-8")

            rows = MODULE.normalize_file(path)

        self.assertEqual([row["record_id"] for row in rows], ["unit-0001", "unit-0002"])
        self.assertTrue(
            all(row["intake_status"] == "ready-for-research" for row in rows)
        )
        self.assertTrue(all(row["intake_issues"] == "" for row in rows))

    def test_missing_unit_expression_needs_clarification(self) -> None:
        rows = MODULE.normalize_records([(2, {})])

        self.assertEqual(rows[0]["record_id"], "unit-0001")
        self.assertEqual(rows[0]["intake_status"], "needs-clarification")
        self.assertEqual(rows[0]["intake_issues"], "unit_expression missing")


if __name__ == "__main__":
    unittest.main()
