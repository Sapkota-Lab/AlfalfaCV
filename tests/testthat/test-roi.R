test_that("default ROI initializes from centralized normalized bounds", {
  roi <- initialize_default_roi(1000, 2000)
  expect_equal(roi$x_min, as.integer(floor(0.58 * 1000)))
  expect_equal(roi$x_max, as.integer(ceiling(0.65 * 1000)))
  expect_equal(roi$y_min, as.integer(floor(0.07 * 2000)))
  expect_equal(roi$y_max, as.integer(ceiling(0.88 * 2000)))
  expect_true(validate_roi(roi, 1000, 2000)$ok)
})

test_that("normalized and pixel ROI conversions round-trip within one pixel", {
  roi <- initialize_default_roi(10757, 9900)
  norm <- roi_to_normalized(roi, 10757, 9900)
  back <- normalized_to_roi(norm, 10757, 9900)
  expect_lte(abs(back$x_min - roi$x_min), 1L)
  expect_lte(abs(back$x_max - roi$x_max), 1L)
  expect_lte(abs(back$y_min - roi$y_min), 1L)
  expect_lte(abs(back$y_max - roi$y_max), 1L)
})

test_that("ROI clamping keeps bounds inside large image dimensions", {
  raw <- make_roi(-10, 20000, -5, 15000)
  roi <- clamp_roi(raw, 10757, 9900)
  expect_equal(roi$x_min, 0L)
  expect_equal(roi$y_min, 0L)
  expect_equal(roi$x_max, 10757L)
  expect_equal(roi$y_max, 9900L)
  expect_true(validate_roi(roi, 10757, 9900)$ok)
})

test_that("empty and out-of-bounds ROIs fail validation", {
  expect_false(validate_roi(make_roi(10, 10, 0, 20), 100, 100)$ok)
  expect_false(validate_roi(make_roi(0, 50, 0, 200), 100, 100)$ok)
  expect_false(validate_roi(NULL, 100, 100)$ok)
})

test_that("reset uses the same default initializer", {
  a <- initialize_default_roi(800, 600)
  b <- initialize_default_roi(800, 600)
  expect_identical(a, b)
})

test_that("brush mapping scales preview coordinates to original image", {
  brush <- list(xmin = 10, xmax = 110, ymin = 20, ymax = 220)
  roi <- brush_to_roi(brush, preview_width = 200, preview_height = 400, image_width = 1000, image_height = 2000)
  expect_equal(roi$x_min, 50L)
  expect_equal(roi$x_max, 550L)
  expect_equal(roi$y_min, 100L)
  expect_equal(roi$y_max, 1100L)
  expect_true(validate_roi(roi, 1000, 2000)$ok)
})

test_that("preview mapping is consistent with brush mapping scale", {
  roi <- make_roi(100, 300, 50, 250)
  preview_roi <- roi_to_preview(roi, 200, 400, 1000, 2000)
  expect_equal(preview_roi$x_min, 20L)
  expect_equal(preview_roi$x_max, 60L)
  expect_equal(preview_roi$y_min, 10L)
  expect_equal(preview_roi$y_max, 50L)
})

test_that("half-open ROI maps to inclusive 1-based array indices without off-by-one", {
  roi <- make_roi(0, 10, 0, 5)
  idx <- roi_to_array_indices(roi)
  expect_equal(idx$col_min, 1L)
  expect_equal(idx$col_max, 10L)
  expect_equal(idx$row_min, 1L)
  expect_equal(idx$row_max, 5L)
  # Width/height of inclusive slice equals half-open span.
  expect_equal(idx$col_max - idx$col_min + 1L, roi_width(roi))
  expect_equal(idx$row_max - idx$row_min + 1L, roi_height(roi))
})
