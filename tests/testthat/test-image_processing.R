test_that("full preview is downscaled and preserves aspect ratio", {
  skip_if_not_installed("magick")
  skip_if_not(file.exists(synthetic_png), "Synthetic fixture missing")

  img <- load_image_magick(synthetic_png)
  dims <- magick_dimensions(img)
  preview <- create_full_preview(img, max_width = 200L)

  expect_lte(preview$width, 200L)
  expect_equal(preview$original_width, dims$width)
  expect_equal(preview$original_height, dims$height)
  expect_lt(abs((preview$width / preview$height) - (dims$width / dims$height)), 0.02)
})

test_that("ROI crop geometry matches half-open bounds", {
  skip_if_not_installed("magick")
  skip_if_not(file.exists(synthetic_png), "Synthetic fixture missing")

  img <- load_image_magick(synthetic_png)
  dims <- magick_dimensions(img)
  roi <- make_roi(100L, 300L, 50L, 200L)
  crop <- crop_roi_magick(img, roi)
  cdims <- magick_dimensions(crop)

  expect_equal(cdims$width, roi_width(roi))
  expect_equal(cdims$height, roi_height(roi))
  expect_true(validate_roi(roi, dims$width, dims$height)$ok)
})

test_that("invalid ROI crop fails clearly", {
  skip_if_not_installed("magick")
  skip_if_not(file.exists(synthetic_png), "Synthetic fixture missing")

  img <- load_image_magick(synthetic_png)
  expect_error(crop_roi_magick(img, make_roi(10, 10, 0, 20)))
})

test_that("session image directories can be created and cleared", {
  path <- create_session_image_dir()
  expect_true(dir.exists(path))
  clear_session_image_dir(path)
  expect_false(dir.exists(path))
})
