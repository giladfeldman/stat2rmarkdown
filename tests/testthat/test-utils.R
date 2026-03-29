# Tests for utils.R

# --- %||% ---
test_that("%||% returns x when not NULL", {
  expect_equal("hello" %||% "default", "hello")
  expect_equal(0 %||% 1, 0)
  expect_equal(FALSE %||% TRUE, FALSE)
  expect_equal(NA %||% "default", NA)
})

test_that("%||% returns y when x is NULL", {
  expect_equal(NULL %||% "default", "default")
  expect_equal(NULL %||% 42, 42)
  expect_equal(NULL %||% NULL, NULL)
})

# --- quote_var ---
test_that("quote_var returns simple names unchanged", {
  expect_equal(quote_var("age"), "age")
  expect_equal(quote_var("my_var"), "my_var")
  expect_equal(quote_var("x1"), "x1")
  expect_equal(quote_var("var.name"), "var.name")
})

test_that("quote_var backtick-quotes names with special characters", {
  expect_equal(quote_var("has space"), "`has space`")
  expect_equal(quote_var("x+y"), "`x+y`")
  expect_equal(quote_var("rate%"), "`rate%`")
})

test_that("quote_var backtick-quotes names starting with digits", {
  expect_equal(quote_var("2nd_var"), "`2nd_var`")
  expect_equal(quote_var("123"), "`123`")
})

test_that("quote_var errors on non-character or multi-element input", {
  expect_error(quote_var(123))
  expect_error(quote_var(c("a", "b")))
})

# --- create_output_dir ---
test_that("create_output_dir creates a directory", {
  tmp <- tempdir()
  root <- file.path(tmp, "test_output_dir")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  result <- create_output_dir("my_test", output_root = root)
  expect_true(dir.exists(result))
  expect_true(grepl("my_test$", result))
  expect_true(grepl("^\\d{4}-\\d{2}-\\d{2}", basename(result)))
})

test_that("create_output_dir sanitizes special characters", {
  tmp <- tempdir()
  root <- file.path(tmp, "test_output_dir2")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  result <- create_output_dir("my analysis (v2)", output_root = root)
  expect_true(dir.exists(result))
  expect_false(grepl("[() ]", basename(result)))
})

# --- validate_vars ---
test_that("validate_vars succeeds when all vars exist (case-insensitive)", {
  df <- data.frame(Age = 1:3, Score = 4:6, Name = letters[1:3])
  expect_true(validate_vars(df, c("age", "SCORE")))
  expect_true(validate_vars(df, c("Age", "Score"), case_sensitive = TRUE))
})

test_that("validate_vars errors on missing vars", {
  df <- data.frame(Age = 1:3, Score = 4:6)
  expect_error(validate_vars(df, c("age", "missing_var")), "missing_var")
})

test_that("validate_vars case-sensitive mode works", {
  df <- data.frame(Age = 1:3)
  expect_error(validate_vars(df, "age", case_sensitive = TRUE), "age")
  expect_true(validate_vars(df, "Age", case_sensitive = TRUE))
})

# --- safe_as_numeric ---
test_that("safe_as_numeric converts factors correctly", {
  x <- factor(c("1", "2", "3"))
  expect_equal(safe_as_numeric(x), c(1, 2, 3))
})

test_that("safe_as_numeric converts character vectors", {
  expect_equal(safe_as_numeric(c("1.5", "2.5")), c(1.5, 2.5))
})

test_that("safe_as_numeric returns NA for non-numeric characters", {
  result <- safe_as_numeric(c("1", "abc", "3"))
  expect_equal(result, c(1, NA, 3))
})

test_that("safe_as_numeric passes numeric through unchanged", {
  expect_equal(safe_as_numeric(c(1, 2, 3)), c(1, 2, 3))
})

# --- log_message ---
test_that("log_message prints formatted output", {
  out <- capture.output(log_message("test message"))
  expect_match(out, "\\[\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}\\] INFO: test message")
})

test_that("log_message respects level argument", {
  out <- capture.output(log_message("warning!", level = "WARN"))
  expect_match(out, "WARN: warning!")

  out <- capture.output(log_message("error!", level = "ERROR"))
  expect_match(out, "ERROR: error!")
})

test_that("log_message rejects invalid levels", {
  expect_error(log_message("test", level = "DEBUG"))
})

test_that("log_message returns NULL invisibly", {
  result <- invisible(capture.output(ret <- log_message("test")))
  expect_null(ret)
})
