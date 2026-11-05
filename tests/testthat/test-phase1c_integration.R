# Calibration confirmation and measurement unit behavior.

test_that("invalid calibration cannot become confirmed", {
  bad <- empty_calibration(method = "automatic")
  conf <- confirm_calibration(bad)
  expect_false(conf$confirmed)
  expect_true(is.na(conf$pixels_per_mm))
})

test_that("measurements stay pixel-only until calibration confirmed", {
  m <- matrix(FALSE, 30, 30)
  m[5:14, 5:14] <- TRUE
  det <- list(id = 1L, mask = m, bbox_roi = list(x_min = 0L, x_max = 30L, y_min = 0L, y_max = 30L))
  uncal <- measure_detections(list(det), pixels_per_mm = NA_real_)
  expect_false(uncal$calibrated[[1]])
  expect_true(is.na(uncal$area_mm2[[1]]))
  expect_equal(uncal$area_px[[1]], 100)

  cal <- measure_detections(list(det), pixels_per_mm = 10)
  expect_true(cal$calibrated[[1]])
  expect_equal(cal$area_mm2[[1]], 1)
})

test_that("no detections yields empty measurement table", {
  df <- measure_detections(list(), pixels_per_mm = 10)
  expect_equal(nrow(df), 0L)
  disp <- measurements_display_table(df, calibrated = TRUE)
  expect_true("Message" %in% names(disp))
})

test_that("multiple detections produce one row each", {
  dets <- lapply(1:3, function(i) {
    m <- matrix(FALSE, 25, 25)
    m[5:(5 + i), 5:(5 + i)] <- TRUE
    list(
      id = as.integer(i),
      mask = m,
      bbox_roi = list(x_min = 0L, x_max = 25L, y_min = 0L, y_max = 25L)
    )
  })
  df <- measure_detections(dets, pixels_per_mm = 5)
  expect_equal(nrow(df), 3L)
  expect_equal(df$id, 1:3)
})

test_that("replacing calibration does not alter pixel area", {
  m <- matrix(FALSE, 20, 20)
  m[2:11, 2:11] <- TRUE
  det <- list(id = 1L, mask = m, bbox_roi = list(x_min = 0L, x_max = 20L, y_min = 0L, y_max = 20L))
  a <- measure_detections(list(det), pixels_per_mm = NA_real_)
  b <- measure_detections(list(det), pixels_per_mm = 8)
  expect_equal(a$area_px, b$area_px)
  expect_equal(a$aspect_ratio, b$aspect_ratio)
  expect_true(is.na(a$area_mm2))
  expect_true(is.finite(b$area_mm2))
})

test_that("seed ROI fallback is broader than the old exclusive 0.65 strip", {
  expect_gt(DEFAULT_SEED_ROI_NORMALIZED$right, 0.65)
  expect_lte(DEFAULT_SEED_ROI_NORMALIZED$right, 0.80)
})

test_that("default ruler ROI is independent of seed ROI", {
  expect_equal(DEFAULT_RULER_ROI_NORMALIZED$top, 0.88)
  expect_equal(DEFAULT_RULER_ROI_NORMALIZED$bottom, 1.00)
  expect_true(DEFAULT_RULER_ROI_NORMALIZED$top >= DEFAULT_SEED_ROI_NORMALIZED$bottom ||
                DEFAULT_SEED_ROI_NORMALIZED$bottom < 1)
})
