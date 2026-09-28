# Contributing to stat2rmarkdown

Thank you for helping. Bug reports, fixes and documentation improvements are all welcome.

## Reporting a problem

Open an issue at <https://github.com/giladfeldman/stat2rmarkdown/issues> with:

- what you ran (a minimal R snippet),
- what you expected and what happened instead (the full error message, if any),
- the output of `sessionInfo()`.

## Making a change

1. Fork the repository and create a branch.
2. Keep functions documented with roxygen2 comments and regenerate the help pages with
   `roxygen2::roxygenise()`.
3. Add or update a test under `tests/testthat/` for any behaviour you change.
4. Run the tests from the package root: `testthat::test_local()`.
5. **Update `README.md`** for any new or changed exported function, option or environment
   variable, and add a `NEWS.md` entry. `Rscript tools/check_docs_coverage.R` fails when an
   export is missing from the README, and the test suite runs the same check.
6. Open a pull request describing the change and how you tested it.

## Scope

This package holds helpers shared by the converter packages. Format-specific parsing belongs
in the converter for that format (`spss2rmarkdown`, `jamovi2rmarkdown`, `jasp2rmarkdown`,
`stata2rmarkdown`).
