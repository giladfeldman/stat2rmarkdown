#!/usr/bin/env Rscript
# Documentation-drift gate: the public surface in the CODE must appear in the DOCS.
#
#   Rscript tools/check_docs_coverage.R            # surface + versions + run the quickstart
#   Rscript tools/check_docs_coverage.R --no-run   # skip executing the quickstart
#
# Run from the package root (or pass the root as the last argument).
# Exit 0 = every public name is documented, versions agree and the quickstart runs;
# exit 1 = drift, with every missing item listed.
#
# The surface is DERIVED from the package source, never from a hand-kept list:
#   * every export() in NAMESPACE (exportPattern() is refused: it cannot be enumerated),
#   * every environment variable read with Sys.getenv("NAME") in R/,
#   * every option read with getOption("name") in R/.
# "Documented" means the token appears inside an inline code span or a fenced code
# block of README.md or docs/*.md. A prose mention does not count.
#
# Also checked: DESCRIPTION Version == newest NEWS.md heading == CITATION.cff version.
#
# Pinned two-sided by tests/testthat/test-docs-coverage.R (a planted undocumented
# export must FAIL; the real package must PASS). The gate proves PRESENCE, not
# truth: re-read the README prose for stale claims when the code changes.

docs_gate_surface <- function(root = ".") {
  ns_file <- file.path(root, "NAMESPACE")
  if (!file.exists(ns_file)) stop("no NAMESPACE under ", root, call. = FALSE)
  ns <- readLines(ns_file, warn = FALSE)
  if (any(grepl("^\\s*exportPattern\\(", ns))) {
    stop("NAMESPACE uses exportPattern(); the public surface cannot be enumerated",
         call. = FALSE)
  }
  exp_lines <- grep("^\\s*export\\(", ns, value = TRUE)
  exports <- unlist(lapply(exp_lines, function(l) {
    inner <- sub("^\\s*export\\((.*)\\)\\s*$", "\\1", l)
    parts <- trimws(strsplit(inner, ",", fixed = TRUE)[[1]])
    gsub("^[\"'`]|[\"'`]$", "", parts)
  }))
  src <- unlist(lapply(
    list.files(file.path(root, "R"), pattern = "\\.[Rr]$", full.names = TRUE),
    readLines, warn = FALSE
  ))
  src <- sub("#.*$", "", src)  # a commented-out read is not a read
  grab <- function(pattern) {
    m <- regmatches(src, gregexpr(pattern, src, perl = TRUE))
    vals <- unlist(lapply(m, function(x) sub(pattern, "\\1", x, perl = TRUE)))
    sort(unique(vals))
  }
  out <- list(
    export  = sort(unique(exports)),
    env_var = grab("Sys\\.getenv\\(\\s*[\"']([A-Z][A-Z0-9_]+)[\"']"),
    option  = grab("getOption\\(\\s*[\"']([A-Za-z][A-Za-z0-9_.]+)[\"']")
  )
  # Package-specific surface (e.g. the supported-command table a converter dispatches on),
  # also DERIVED from R/ by tools/docs_surface_extra.R when that file exists.
  extra <- file.path(root, "tools", "docs_surface_extra.R")
  if (file.exists(extra)) {
    e <- new.env()
    sys.source(extra, envir = e)
    more <- e$docs_surface_extra(root)
    for (k in names(more)) {
      if (!length(more[[k]])) stop("docs_surface_extra() derived an EMPTY '", k, "' surface", call. = FALSE)
      out[[k]] <- sort(unique(c(out[[k]], more[[k]])))
    }
  }
  out
}

docs_gate_doc_files <- function(root = ".") {
  files <- file.path(root, "README.md")
  docs_dir <- file.path(root, "docs")
  if (dir.exists(docs_dir)) {
    files <- c(files, list.files(docs_dir, pattern = "\\.md$", full.names = TRUE,
                                 recursive = TRUE))
  }
  files[file.exists(files)]
}

docs_gate_code_text <- function(root = ".") {
  files <- docs_gate_doc_files(root)
  if (!length(files)) stop("no README.md under ", root, call. = FALSE)
  chunks <- character(0)
  for (f in files) {
    txt <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    # Line-anchored, so a ``` inside a string in the code (e.g. "```{r}") is not a fence.
    fence_re <- "(?ms)^[ \t]*```[^\n]*\n.*?^[ \t]*```[ \t]*$"
    fences <- regmatches(txt, gregexpr(fence_re, txt, perl = TRUE))[[1]]
    chunks <- c(chunks, fences)
    rest <- gsub(fence_re, "", txt, perl = TRUE)
    inline <- regmatches(rest, gregexpr("`[^`\n]+`", rest, perl = TRUE))[[1]]
    chunks <- c(chunks, inline)
  }
  # Markdown tables escape `|` as `\|` inside code spans (e.g. `%\|\|%`).
  gsub("\\|", "|", paste(chunks, collapse = "\n"), fixed = TRUE)
}

docs_gate_is_documented <- function(token, code) {
  # Whole-token match: `IF` must not count as documented because `IFMISS` is. Every regex
  # metacharacter in the token (operators such as %||%, names such as T-TEST) is escaped.
  esc <- gsub("([][{}()|^$.*+?\\\\-])", "\\\\\\1", token, perl = TRUE)
  grepl(paste0("(?<![A-Za-z0-9_.])", esc, "(?![A-Za-z0-9_.])"), code, perl = TRUE)
}

docs_gate_undocumented <- function(root = ".", surface = docs_gate_surface(root),
                                   code = docs_gate_code_text(root)) {
  out <- character(0)
  for (kind in names(surface)) {
    for (tok in surface[[kind]]) {
      if (!docs_gate_is_documented(tok, code)) out <- c(out, paste0(kind, ": ", tok))
    }
  }
  sort(out)
}

docs_gate_versions <- function(root = ".") {
  problems <- character(0)
  desc <- read.dcf(file.path(root, "DESCRIPTION"), fields = c("Package", "Version"))
  pkg <- unname(desc[1, "Package"])
  ver <- unname(desc[1, "Version"])
  news <- file.path(root, "NEWS.md")
  if (!file.exists(news)) {
    problems <- c(problems, "versions: no NEWS.md")
  } else {
    heads <- grep("^#\\s", readLines(news, warn = FALSE), value = TRUE)
    top <- if (length(heads)) sub(paste0("^#\\s+", pkg, "\\s+"), "", heads[1]) else ""
    top <- sub("\\s.*$", "", top)
    if (!identical(top, ver)) {
      problems <- c(problems, sprintf("versions: newest NEWS.md heading is '%s', DESCRIPTION says %s",
                                      heads[1], ver))
    }
  }
  cff <- file.path(root, "CITATION.cff")
  if (!file.exists(cff)) {
    problems <- c(problems, "versions: no CITATION.cff")
  } else {
    line <- grep("^version:", readLines(cff, warn = FALSE), value = TRUE)
    cv <- gsub("^version:\\s*[\"']?|[\"']?\\s*$", "", line[1])
    if (!identical(cv, ver)) {
      problems <- c(problems, sprintf("versions: CITATION.cff says %s, DESCRIPTION says %s", cv, ver))
    }
  }
  problems
}

docs_gate_quickstart <- function(root = ".") {
  readme <- readLines(file.path(root, "README.md"), warn = FALSE, encoding = "UTF-8")
  h <- grep("^## Quickstart", readme)
  if (!length(h)) return(list(code = NULL, problem = "quickstart: README has no '## Quickstart' heading"))
  after <- readme[(h[1] + 1):length(readme)]
  open <- grep("^```\\{?r", after)
  nxt <- grep("^## ", after)
  if (!length(open) || (length(nxt) && open[1] > nxt[1])) {
    return(list(code = NULL, problem = "quickstart: no ```r block under '## Quickstart'"))
  }
  close <- grep("^```\\s*$", after)
  close <- close[close > open[1]][1]
  list(code = after[(open[1] + 1):(close - 1)], problem = NULL)
}

docs_gate_run_quickstart <- function(root = ".") {
  qs <- docs_gate_quickstart(root)
  if (!is.null(qs$problem)) return(qs$problem)
  code <- qs$code[!grepl("install\\.packages|install_github|remotes::", qs$code)]
  work <- tempfile("quickstart-")
  dir.create(work)
  script <- file.path(work, "quickstart.R")
  writeLines(code, script)
  rscript <- file.path(R.home("bin"), "Rscript")
  old <- setwd(work)
  on.exit(setwd(old), add = TRUE)
  out <- suppressWarnings(system2(rscript, c("--vanilla", shQuote(script)),
                                  stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status")
  if (!is.null(status) && status != 0) {
    return(c(sprintf("quickstart: exited %s (it runs against the INSTALLED package)", status),
             paste0("  | ", utils::tail(out, 15))))
  }
  character(0)
}

docs_gate_main <- function(args = commandArgs(trailingOnly = TRUE)) {
  no_run <- "--no-run" %in% args
  pos <- args[!startsWith(args, "--")]
  root <- if (length(pos)) pos[length(pos)] else "."
  problems <- c(docs_gate_undocumented(root), docs_gate_versions(root))
  if (!no_run) problems <- c(problems, docs_gate_run_quickstart(root))
  s <- docs_gate_surface(root)
  if (length(problems)) {
    cat("DOCS DRIFT:", length(problems), "problem(s)\n")
    cat(paste0("  ", problems), sep = "\n")
    quit(status = 1)
  }
  counts <- paste(vapply(names(s), function(k) sprintf("%d %s", length(s[[k]]), k), ""),
                  collapse = ", ")
  cat(sprintf("DOCS OK: %s documented; versions agree%s\n", counts,
              if (no_run) "; quickstart NOT run (--no-run)" else "; quickstart ran"))
  invisible(TRUE)
}

if (sys.nframe() == 0L) docs_gate_main()
