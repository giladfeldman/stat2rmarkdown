# Tests for rmd_skeleton.R

# --- generate_yaml_header ---
test_that("generate_yaml_header produces valid YAML block", {
  result <- generate_yaml_header("My Analysis")
  expect_match(result, "^---")
  expect_match(result, "---\\s*$")
  expect_match(result, 'title: "My Analysis"')
  expect_match(result, "theme: lumen")
  expect_match(result, "toc: true")
})

test_that("generate_yaml_header uses custom parameters", {
  result <- generate_yaml_header(
    title = "Custom Title",
    author = "Custom Author",
    theme = "cosmo"
  )
  expect_match(result, 'title: "Custom Title"')
  expect_match(result, 'author: "Custom Author"')
  expect_match(result, "theme: cosmo")
})

test_that("generate_yaml_header includes default date expression", {
  result <- generate_yaml_header("Test")
  expect_match(result, "Sys\\.Date\\(\\)")
})

test_that("generate_yaml_header errors on non-character title", {
  expect_error(generate_yaml_header(123))
})

# --- generate_setup_chunk ---
test_that("generate_setup_chunk produces a valid code chunk", {
  result <- generate_setup_chunk()
  expect_match(result, "^```\\{r setup, include=FALSE\\}")
  expect_match(result, "```\\s*$")
  expect_match(result, "knitr::opts_chunk\\$set")
})

test_that("generate_setup_chunk includes default packages", {
  result <- generate_setup_chunk()
  expect_match(result, "library\\(haven\\)")
  expect_match(result, "library\\(dplyr\\)")
  expect_match(result, "library\\(tidyr\\)")
  expect_match(result, "library\\(ggplot2\\)")
  expect_match(result, "library\\(stringr\\)")
})

test_that("generate_setup_chunk uses custom packages", {
  result <- generate_setup_chunk(packages = c("haven", "fixest"))
  expect_match(result, "library\\(haven\\)")
  expect_match(result, "library\\(fixest\\)")
  expect_no_match(result, "library\\(dplyr\\)")
})

test_that("generate_setup_chunk respects custom options", {
  result <- generate_setup_chunk(options = list(echo = FALSE, fig.width = 8))
  expect_match(result, "echo = FALSE")
  expect_match(result, "fig\\.width = 8")
})

test_that("generate_setup_chunk appends extra code", {
  result <- generate_setup_chunk(
    packages = c("fixest"),
    extra_code = "setFixest_notes(FALSE)"
  )
  expect_match(result, "setFixest_notes\\(FALSE\\)")
})

test_that("generate_setup_chunk omits extra section when NULL", {
  result <- generate_setup_chunk(extra_code = NULL)
  # Should end cleanly without extra blank lines from extra_code
  expect_match(result, "library\\(stringr\\)\n```\n$")
})

# --- generate_session_footer ---
test_that("generate_session_footer produces session info section", {
  result <- generate_session_footer()
  expect_match(result, "# Session Info")
  expect_match(result, "sessionInfo\\(\\)")
  expect_match(result, "stat2rmarkdown")
})

test_that("generate_session_footer uses custom tool name and URL", {
  result <- generate_session_footer(
    tool_name = "stata2rmarkdown",
    repo_url = "https://github.com/giladfeldman/STATA2Rmarkdown"
  )
  expect_match(result, "stata2rmarkdown")
  expect_match(result, "STATA2Rmarkdown")
  expect_no_match(result, "stat2rmarkdown")
})

test_that("generate_session_footer contains markdown formatting", {
  result <- generate_session_footer()
  expect_match(result, "---")
  expect_match(result, "```\\{r session-info\\}")
  expect_match(result, "```")
})
