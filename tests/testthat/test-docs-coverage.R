# Two-sided pin for the documentation-drift gate in tools/check_docs_coverage.R.
# It needs the SOURCE tree (README.md, NEWS.md, tools/), which an installed copy
# under R CMD check does not have -- there it skips and says so. Run it with
# testthat::test_local() or devtools::test() from the package root.

docs_gate_root <- function() {
  root <- normalizePath(testthat::test_path("..", ".."), mustWork = FALSE)
  gate <- file.path(root, "tools", "check_docs_coverage.R")
  if (!file.exists(gate) || !file.exists(file.path(root, "README.md"))) return(NULL)
  root
}

docs_gate_env <- function(root) {
  env <- new.env()
  sys.source(file.path(root, "tools", "check_docs_coverage.R"), envir = env)
  env
}

docs_gate_copy <- function(root) {
  tmp <- tempfile("docs-gate-")
  dir.create(tmp)
  for (f in c("NAMESPACE", "README.md", "DESCRIPTION", "NEWS.md", "CITATION.cff")) {
    if (file.exists(file.path(root, f))) file.copy(file.path(root, f), tmp)
  }
  file.copy(file.path(root, "R"), tmp, recursive = TRUE)
  if (dir.exists(file.path(root, "docs"))) file.copy(file.path(root, "docs"), tmp, recursive = TRUE)
  tmp
}

test_that("docs gate PASSES on the real package: every export, env var and option is documented", {
  root <- docs_gate_root()
  skip_if(is.null(root), "source tree not available (installed copy under R CMD check)")
  gate <- docs_gate_env(root)
  surface <- gate$docs_gate_surface(root)
  # A green result from an empty surface would be a false green.
  expect_gt(length(surface$export), 0)
  expect_identical(gate$docs_gate_undocumented(root), character(0))
  expect_identical(gate$docs_gate_versions(root), character(0))
  expect_null(gate$docs_gate_quickstart(root)$problem)
})

test_that("docs gate FAILS on a planted undocumented export, env var and option", {
  root <- docs_gate_root()
  skip_if(is.null(root), "source tree not available (installed copy under R CMD check)")
  gate <- docs_gate_env(root)
  tmp <- docs_gate_copy(root)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  cat("export(zz_planted_undocumented)\n", file = file.path(tmp, "NAMESPACE"), append = TRUE)
  writeLines(c("zz_planted_undocumented <- function() {",
               "  Sys.getenv(\"ZZ_PLANTED_ENV\")",
               "  getOption(\"zz.planted.option\")",
               "}"),
             file.path(tmp, "R", "zz_planted.R"))
  miss <- gate$docs_gate_undocumented(tmp)
  expect_true("export: zz_planted_undocumented" %in% miss)
  expect_true("env_var: ZZ_PLANTED_ENV" %in% miss)
  expect_true("option: zz.planted.option" %in% miss)
  # ...and nothing ELSE: the planted items are the only drift.
  expect_length(miss, 3L)
})

test_that("docs gate FAILS when a package-specific surface item is removed from the docs", {
  root <- docs_gate_root()
  skip_if(is.null(root), "source tree not available (installed copy under R CMD check)")
  extra <- file.path(root, "tools", "docs_surface_extra.R")
  skip_if_not(file.exists(extra), "this package has no tools/docs_surface_extra.R")
  gate <- docs_gate_env(root)
  tmp <- docs_gate_copy(root)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  dir.create(file.path(tmp, "tools"))
  file.copy(extra, file.path(tmp, "tools"))
  surface <- gate$docs_gate_surface(tmp)
  base_kinds <- c("export", "env_var", "option")
  kind <- setdiff(names(surface), base_kinds)[1]
  expect_false(is.na(kind))
  tok <- surface[[kind]][1]
  esc <- gsub("([][{}()|^$.*+?\\\\-])", "\\\\\\1", tok, perl = TRUE)
  for (f in gate$docs_gate_doc_files(tmp)) {  # README.md and every docs/*.md
    txt <- readLines(f, warn = FALSE, encoding = "UTF-8")
    txt <- gsub(paste0("(?<![A-Za-z0-9_.])", esc, "(?![A-Za-z0-9_.])"), "zzremoved", txt, perl = TRUE)
    writeLines(txt, f, useBytes = TRUE)
  }
  expect_true(paste0(kind, ": ", tok) %in% gate$docs_gate_undocumented(tmp))
})

test_that("docs gate FAILS when a version disagrees with DESCRIPTION", {
  root <- docs_gate_root()
  skip_if(is.null(root), "source tree not available (installed copy under R CMD check)")
  gate <- docs_gate_env(root)
  tmp <- docs_gate_copy(root)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  cff <- readLines(file.path(tmp, "CITATION.cff"))
  cff <- sub("^version:.*$", "version: \"0.0.0-planted\"", cff)
  writeLines(cff, file.path(tmp, "CITATION.cff"))
  expect_match(gate$docs_gate_versions(tmp), "CITATION.cff says 0.0.0-planted", all = FALSE)
})
