# utils.R
# Shared utility functions for statistical software to R Markdown converters

#' Null-coalescing operator
#'
#' @title Null-coalescing operator
#' @description Returns the left-hand side if it is not `NULL`, otherwise
#'   returns the right-hand side. This is a convenience operator used throughout
#'   the converter packages for default value handling.
#'
#' @param x Value to check.
#' @param y Default value to return if `x` is `NULL`.
#' @return `x` if not `NULL`, otherwise `y`.
#' @name null-coalesce
#' @export
#' @examples
#' # Returns the non-NULL value
#' "hello" %||% "default"
#'
#' # Returns the default when NULL
#' NULL %||% "default"
`%||%` <- function(x, y) if (!is.null(x)) x else y


#' Quote variable names for safe use in generated R code
#'
#' @title Quote variable names
#' @description Wraps variable names in backticks if they contain special
#'   characters (anything other than letters, digits, underscores, or dots)
#'   or if they start with a digit. Variable names that are already safe R
#'   identifiers are returned unchanged.
#'
#' @param var Character string. A single variable name.
#' @return Character string. The variable name, backtick-quoted if necessary.
#' @export
#' @examples
#' quote_var("simple_name")
#' quote_var("has space")
#' quote_var("2starts_with_digit")
quote_var <- function(var) {
  stopifnot(is.character(var), length(var) == 1L)
  if (grepl("[^a-zA-Z0-9_.]", var) || grepl("^[0-9]", var)) {
    paste0("`", var, "`")
  } else {
    var
  }
}


#' Create a timestamped output directory
#'
#' @title Create output directory
#' @description Creates a subdirectory under `output_root` with a timestamped
#'   name of the form `YYYY-MM-DD-HH-MM-SS-base_name`. Special characters in
#'   `base_name` are replaced with underscores. The directory is created
#'   recursively if needed.
#'
#' @param base_name Character string. Base name for the directory (e.g., the
#'   analysis or file name).
#' @param output_root Character string. Parent directory under which the
#'   timestamped directory is created. Defaults to `"output"`.
#' @return Character string. The path to the newly created directory.
#' @export
#' @examples
#' \dontrun{
#' dir <- create_output_dir("my_analysis")
#' dir
#' # e.g. "output/2026-03-28-14-30-00-my_analysis"
#' }
create_output_dir <- function(base_name, output_root = "output") {
  stopifnot(is.character(base_name), length(base_name) == 1L)
  timestamp <- format(Sys.time(), "%Y-%m-%d-%H-%M-%S")
  dir_name <- paste0(timestamp, "-", gsub("[^a-zA-Z0-9_-]", "_", base_name))
  dir_path <- file.path(output_root, dir_name)
  dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  dir_path
}


#' Validate that variables exist in a data frame
#'
#' @title Validate variable names
#' @description Checks that all specified variable names exist as columns in
#'   the supplied data frame. The comparison is case-insensitive to accommodate
#'   different conventions across statistical packages (e.g., Stata is
#'   case-preserving but case-insensitive, SPSS uppercases names).
#'
#' @param data A data frame.
#' @param vars Character vector of required variable names.
#' @param case_sensitive Logical. Whether to perform case-sensitive matching.
#'   Defaults to `FALSE` for cross-package compatibility.
#' @return `TRUE` invisibly if all variables are found.
#' @export
#' @examples
#' df <- data.frame(Age = 1:3, Score = 4:6)
#' validate_vars(df, c("age", "score"))
#' validate_vars(df, c("Age"), case_sensitive = TRUE)
validate_vars <- function(data, vars, case_sensitive = FALSE) {
  stopifnot(is.data.frame(data), is.character(vars))
  if (case_sensitive) {
    missing_vars <- vars[!vars %in% names(data)]
  } else {
    missing_vars <- vars[!tolower(vars) %in% tolower(names(data))]
  }
  if (length(missing_vars) > 0L) {
    stop(
      "Variables not found in data: ",
      paste(missing_vars, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}


#' Safely convert a vector to numeric
#'
#' @title Safe numeric conversion
#' @description Converts factors, character vectors, and haven-labelled vectors
#'   to numeric. For factors, converts through `as.character()` first to
#'   preserve level labels. For character vectors, uses `suppressWarnings()` to
#'   handle non-numeric strings gracefully (they become `NA`). Vectors that are
#'   already numeric are returned as-is.
#'
#' @param x A vector (factor, character, haven-labelled, or numeric).
#' @return A numeric vector.
#' @export
#' @examples
#' safe_as_numeric(factor(c("1", "2", "3")))
#' safe_as_numeric(c("1.5", "2.5", "not_a_number"))
#' safe_as_numeric(42)
safe_as_numeric <- function(x) {
  if (inherits(x, "haven_labelled")) {
    x <- as.numeric(x)
  } else if (is.factor(x)) {
    x <- as.numeric(as.character(x))
  } else if (is.character(x)) {
    x <- suppressWarnings(as.numeric(x))
  }
  x
}


#' Log a message with a timestamp
#'
#' @title Log message
#' @description Prints a formatted log message to the console with a timestamp
#'   and severity level. Useful for tracking progress during long-running
#'   conversion operations.
#'
#' @param msg Character string. The message to log.
#' @param level Character string. The log level, one of `"INFO"`, `"WARN"`,
#'   or `"ERROR"`. Defaults to `"INFO"`.
#' @return `NULL` invisibly. Called for its side effect of printing to the
#'   console.
#' @export
#' @examples
#' log_message("Processing file analysis.do")
#' log_message("Missing variable 'age'", level = "WARN")
#' log_message("Parse failed", level = "ERROR")
log_message <- function(msg, level = "INFO") {
  level <- match.arg(level, choices = c("INFO", "WARN", "ERROR"))
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(sprintf("[%s] %s: %s\n", timestamp, level, msg))
  invisible(NULL)
}
