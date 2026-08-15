---
description: "R coding standards: tidyverse style, suggested renv reproducibility, testthat, and roxygen documentation. Applies when authoring or editing R/Rmd/Quarto files."
applyTo: "**/*.{R,r,Rmd,rmd,qmd}"
---
# R Standards

- Follow the tidyverse style guide; format with `styler`, lint with `lintr`.
- Always namespace non-base functions (`pkg::fun()`); omit `base::` unless a
  disambiguation is necessary.
- Treat `renv` as suggested for dependency reproducibility; when used, record
  every new package in the lockfile.
- Prefer functions over copy-paste; document exported functions with `roxygen2`.
- Prefer `purrr` iteration (`map*`, `walk*`, `reduce`) over `for` loops unless
  a loop is clearly more readable or required for performance.
- Write tests with `testthat`; set a seed before any stochastic step.
- Never hardcode absolute paths or secrets; use `here::here()` and environment
  variables / `keyring`.
- For data work, keep raw data immutable; write derived outputs to a separate
  directory and make the transformation script re-runnable from scratch.
