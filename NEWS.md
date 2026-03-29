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
