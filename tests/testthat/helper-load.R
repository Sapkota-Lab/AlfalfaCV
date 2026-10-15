# Shared setup for Phase 1A unit tests.

project_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
source(file.path(project_root, "R", "image_io.R"), local = TRUE)

fixture_dir <- file.path(project_root, "tests", "fixtures")
synthetic_png <- file.path(fixture_dir, "PI_100001.png")
