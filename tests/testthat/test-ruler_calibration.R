test_that("empty_calibration has UNCALIBRATED status", {
  cal <- empty_calibration()
  expect_equal(cal$status, CALIBRATION_STATUS_UNCALIBRATED)
  expect_false(cal$confirmed)
  expect_true(is.na(cal$pixels_per_mm))
})

test_that("confirm_calibration promotes a proposed horizontal scale", {
  cal <- propose_horizontal_10mm_calibration(0, 4, 200, 4, image_width = 400, image_height = 40)
  expect_equal(cal$status, CALIBRATION_STATUS_PROPOSED)
  expect_equal(cal$pixels_per_mm, 20)
  conf <- confirm_calibration(cal)
  expect_equal(conf$status, CALIBRATION_STATUS_CONFIRMED)
  expect_true(conf$confirmed)
  expect_equal(conf$pixels_per_mm, 20)
  expect_equal(conf$method, "manual")
})

test_that("confirm_calibration rejects an invalid scale", {
  conf <- confirm_calibration(empty_calibration())
  expect_false(conf$confirmed)
  expect_equal(conf$status, CALIBRATION_STATUS_UNCALIBRATED)
})

test_that("preview_point_to_original maps preview pixels into the ruler ROI", {
  roi <- make_roi(100, 500, 800, 900)
  pt <- preview_point_to_original(50, 25, preview_width = 200, preview_height = 50, ruler_roi = roi)
  expect_equal(pt$x, 200)
  expect_equal(pt$y, 850)
})
