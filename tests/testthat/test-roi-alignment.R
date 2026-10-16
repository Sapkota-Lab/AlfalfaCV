test_that("ROI crop dimensions and origin match half-open bounds exactly", {
  skip_if_not_installed("magick")
  skip_if_not_installed("png")
  path <- ensure_orientation_fixture()

  img <- load_image_magick(path)
  dims <- magick_dimensions(img)
  expect_equal(dims$width, 200L)
  expect_equal(dims$height, 100L)

  roi <- make_roi(10L, 60L, 20L, 70L)
  crop <- crop_roi_magick(img, roi)
  cdims <- magick_dimensions(crop)
  expect_equal(cdims$width, 50L)
  expect_equal(cdims$height, 50L)

  # Known pixel: original (15,25) is dark background near UL red.
  # Crop-local (5,5) corresponds to original (15,25).
  full <- magick_to_rgb_array(img)
  crgb <- magick_to_rgb_array(crop)
  expect_equal(crgb[5, 5, ], full[25, 15, ], tolerance = 1e-6)

  # Red square occupies original [1,20]x[1,20]; crop starts at x=10,y=20 so
  # only the bottom strip of red remains in the crop at rows 1 and cols 1:10.
  expect_true(is_reddish(patch_mean(full, 1:20, 1:20)))
})

test_that("ROI overlay preview mapping matches crop origin on non-square images", {
  roi <- make_roi(100L, 300L, 50L, 250L)
  preview <- roi_to_preview(roi, preview_width = 200, preview_height = 400, image_width = 1000, image_height = 2000)
  expect_equal(preview$x_min, 20L)
  expect_equal(preview$x_max, 60L)
  expect_equal(preview$y_min, 10L)
  expect_equal(preview$y_max, 50L)

  # Inverse via brush using the same preview coords recovers the ROI.
  brush <- list(
    xmin = preview$x_min, xmax = preview$x_max,
    ymin = preview$y_min, ymax = preview$y_max
  )
  back <- brush_to_roi(brush, 200, 400, 1000, 2000)
  expect_equal(back$x_min, roi$x_min)
  expect_equal(back$x_max, roi$x_max)
  expect_equal(back$y_min, roi$y_min)
  expect_equal(back$y_max, roi$y_max)
})

test_that("ROI near image boundaries clamps without changing valid interior edges", {
  roi <- make_roi(0L, 50L, 0L, 40L)
  expect_true(validate_roi(roi, 200, 100)$ok)
  expect_equal(clamp_roi(roi, 200, 100), roi)

  oversized <- make_roi(-5L, 250L, -3L, 150L)
  clamped <- clamp_roi(oversized, 200, 100)
  expect_equal(clamped$x_min, 0L)
  expect_equal(clamped$x_max, 200L)
  expect_equal(clamped$y_min, 0L)
  expect_equal(clamped$y_max, 100L)
})

test_that("repeated ROI adjustments do not accumulate drift", {
  roi <- make_roi(40L, 120L, 10L, 80L)
  for (i in 1:5) {
    pr <- roi_to_preview(roi, 100, 50, 200, 100)
    brush <- list(xmin = pr$x_min, xmax = pr$x_max, ymin = pr$y_min, ymax = pr$y_max)
    roi <- brush_to_roi(brush, 100, 50, 200, 100)
  }
  expect_equal(roi$x_min, 40L)
  expect_equal(roi$x_max, 120L)
  expect_equal(roi$y_min, 10L)
  expect_equal(roi$y_max, 80L)
})

test_that("large-image ROI conversion stays within integer bounds", {
  roi <- initialize_default_roi(10757, 9900)
  expect_true(validate_roi(roi, 10757, 9900)$ok)
  expect_gte(roi$x_min, 0L)
  expect_lte(roi$x_max, 10757L)
  expect_gte(roi$y_min, 0L)
  expect_lte(roi$y_max, 9900L)
  expect_gt(roi_width(roi), 0L)
  expect_gt(roi_height(roi), 0L)
})
