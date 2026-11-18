# EML corpus analysis: units and ecological context

This package analyzes Ecological Metadata Language (EML) records from the
[Environmental Data Initiative (EDI)](https://portal.edirepository.org)
repository. It links units of measure to the ecological concepts they
measure. The evidence supports a strategy for QUDT QuantityKinds (QKs) with
ecological context and rules for attaching them to new and existing QUDT
units.

The pipeline:

1. Parses every EML file (versions 2.0–2.2) into tidy tables.
2. Maps EML units to current QUDT units.
3. Summarizes units, concepts, polysemy, annotations, and evidence for
   earlier MOB unit candidates.
4. Clusters attribute definitions. LLM labeling of clusters is optional.
5. Applies draft QK admission and attachment criteria.
6. Renders an interactive Quarto website for team review.

This is analysis, not a QUDT submission. It does not change the
`qudt-unit-contribution` skill or generate QUDT Turtle.

## Setup

Requirements:

- R 4.3 or later and [Quarto](https://quarto.org).
- The R packages in `DESCRIPTION` (`Imports` and `Suggests`). Install them
  with `pak::local_install_deps(dependencies = TRUE)`, or use
  `install.packages()`. `renv` is not used yet; each run records package
  versions in `data/derived/run_manifest.json`.
- A local copy of the EDI EML corpus (one XML file per package). Corpus
  contents are never committed.
- Network access to the QUDT SPARQL endpoint (`config.yml`) on the first run.

Set the corpus location with an environment variable:

```sh
export EML_CORPUS_ROOT=~/Desktop/EMLCorpus20261112
```

## Run

From this directory:

```sh
# Quick check on a seeded 500-file sample
R_CONFIG_ACTIVE=sample Rscript -e 'targets::tar_make()'

# Full corpus (about 4 minutes with 8 workers)
Rscript -e 'targets::tar_make()'
```

`targets` reruns only steps whose code, configuration, or inputs changed.
Use `targets::tar_visnetwork()` to see the pipeline graph. Sample and full
runs share one `_targets/` store, so a profile switch rebuilds most targets.

## Verify

```sh
Rscript -e 'pkgload::load_all(".", quiet = TRUE); testthat::test_dir("tests/testthat")'
Rscript -e 'print(targets::tar_outdated())'   # character(0) when up to date
Rscript -e 'lintr::lint_dir("R")'
```

Then open `report/_site/index.html`.

## Outputs

- `data/derived/`: small CSV tables (committed) and `run_manifest.json`.
  See [docs/data-dictionary.md](docs/data-dictionary.md).
- `report/_site/`: the rendered website (not committed).
- `data/work/attributes.parquet` and `_targets/`: large intermediates (not
  committed).

## Common tasks

- **Refresh QUDT.** QUDT queries run once and are then cached. To fetch
  current QUDT data:
  `Rscript -e 'targets::tar_invalidate(tidyselect::starts_with("qudt_")); targets::tar_make()'`.
- **Use a newer corpus snapshot.** Point `EML_CORPUS_ROOT` at the new
  folder, update `corpus$snapshot` in `config.yml`, and run `tar_make()`.
- **Edit concept seeds.** Change `data/reference/concept_seeds.csv` (regular
  expressions are case-insensitive) and rerun. Only dependent targets
  rebuild.
- **Change thresholds.** Edit `qk_criteria` in `config.yml` and record the
  change in [docs/decision-log.md](docs/decision-log.md).
- **Record audits.** Copy reviewed rows from `data/derived/audit_sample.csv`
  into `data/reference/audit_reviewed.csv` and rerun.
- **Edit the QK shortlist.** Change `data/reference/qk_shortlist.csv`
  (separate concepts with `;`; `qudt_qk` must be a current QUDT QK name) and
  rerun. The Findings page and `data/derived/qk_shortlist.csv` update.
- **Enable LLM labeling (opt-in).** Read
  [docs/llm-protocol.md](docs/llm-protocol.md) first. Set `llm$enabled: true`,
  export the provider key (for example `OPENAI_API_KEY`), and rerun. This
  sends cluster exemplars to a third-party API and costs money.

## Documentation

- [docs/findings.md](docs/findings.md): summary of results and recommended
  QuantityKinds (also the report's Findings page).
- [docs/methods.md](docs/methods.md): how each step works.
- [docs/data-dictionary.md](docs/data-dictionary.md): derived tables and
  columns.
- [docs/llm-protocol.md](docs/llm-protocol.md): LLM labeling and human audit.
- [docs/decision-log.md](docs/decision-log.md): thresholds, modeling
  recommendation, and criteria.
