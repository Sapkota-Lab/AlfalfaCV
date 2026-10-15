library(testthat)

# Run from project root: Rscript -e "testthat::test_dir('tests/testthat')"
# or: Rscript tests/testthat.R

root <- if (basename(getwd()) == "tests") {
  normalizePath("..")
} else {
  normalizePath(".")
}

testthat::test_dir(
  file.path(root, "tests", "testthat"),
  reporter = "summary"
)
