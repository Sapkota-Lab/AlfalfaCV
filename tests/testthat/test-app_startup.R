test_that("required application files exist", {
  expect_true(file.exists(file.path(project_root, "app.R")))
  expect_true(file.exists(file.path(project_root, "R", "image_io.R")))
  expect_true(file.exists(file.path(project_root, "README.md")))
})

test_that("required packages are installed", {
  required <- c("shiny", "bslib", "testthat")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  expect_equal(missing, character(0), info = paste("Missing:", paste(missing, collapse = ", ")))
})

test_that("Shiny application object can be created", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(project_root)

  app <- shiny::shinyAppFile(file.path(project_root, "app.R"))
  expect_s3_class(app, "shiny.appobj")
  expect_true(is.function(app$httpHandler) || !is.null(app$ui))
})
