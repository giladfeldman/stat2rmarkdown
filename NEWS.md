# stat2rmarkdown 0.1.1

* `generate_session_footer()` default `repo_url` now points at the public
  `giladfeldman/stat2rmarkdown` repository instead of the private app repo,
  which readers of a generated report could not open.
* `render_jmv_tables()` skips jmv tables marked `visible = FALSE`.
* Full README, CITATION.cff and man pages.

# stat2rmarkdown 0.1.0

* Initial release of shared core utilities.
* Extracted common functions from stata2rmarkdown and spss2rmarkdown:
  - `%||%` null-coalescing operator
  - `quote_var()` for safe variable quoting
  - `create_output_dir()` for timestamped output directories
  - `validate_vars()` for variable existence checks
  - `safe_as_numeric()` for safe type conversion
  - `log_message()` for logging with timestamps
* Added R Markdown skeleton generators:
  - `generate_yaml_header()` for YAML front matter
  - `generate_setup_chunk()` for setup code chunks
  - `generate_session_footer()` for session info sections
* Added HTML rendering helpers: `generate_style_block()`, `render_jmv_tables()` and
  `render_model_table()`.
* Documentation: README rewritten to cover every exported function, with a runnable
  Quickstart, limitations and citation; added `CITATION.cff`, `CONTRIBUTING.md` and help
  pages for every export. `tools/check_docs_coverage.R` (pinned by
  `tests/testthat/test-docs-coverage.R`) fails when an export is missing from the README.
  `DESCRIPTION` `URL`/`BugReports` now point at this repository.
