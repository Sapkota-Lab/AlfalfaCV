# Phase 1C — morphological measurement engine tests.

test_that("empty mask returns invalid geometry with NA metrics", {
  g <- measure_mask_geometry(matrix(FALSE, 5, 5))
  expect_false(g$valid)
  expect_true(is.na(g$area_px))
  d <- measure_shape_descriptors(g)
  expect_true(is.na(d$circularity))
})

test_that("single-pixel mask is handled safely", {
  m <- matrix(FALSE, 5, 5)
  m[3, 3] <- TRUE
  g <- measure_mask_geometry(m)
  expect_true(g$valid)
  expect_equal(g$area_px, 1)
  expect_equal(g$bbox_width_px, 1)
  expect_equal(g$bbox_height_px, 1)
  expect_equal(g$convex_hull_area_px, 1)
  expect_true(is.na(g$perimeter_px) || is.finite(g$perimeter_px))
  expect_true(is.na(g$major_axis_px))
  d <- measure_shape_descriptors(g)
  expect_true(is.na(d$aspect_ratio))
})

test_that("square mask has expected pixel area, bbox, solidity, extent", {
  m <- matrix(FALSE, 40, 40)
  m[10:29, 10:29] <- TRUE
  g <- measure_mask_geometry(m)
  expect_equal(g$area_px, 400)
  expect_equal(g$bbox_width_px, 20)
  expect_equal(g$bbox_height_px, 20)
  expect_equal(g$centroid_x, 19)
  expect_equal(g$centroid_y, 19)
  expect_equal(g$convex_hull_area_px, 400)
  d <- measure_shape_descriptors(g)
  expect_equal(d$solidity, 1)
  expect_equal(d$extent, 1)
  expect_lt(abs(d$aspect_ratio - 1), 0.05)
  expect_lt(d$eccentricity, 0.1)
  expect_true(d$circularity > 0.7 && d$circularity < 1)
})

test_that("rectangle reports aspect and extent correctly", {
  m <- matrix(FALSE, 50, 50)
  m[10:19, 5:34] <- TRUE # 10 x 30
  g <- measure_mask_geometry(m)
  expect_equal(g$area_px, 300)
  expect_equal(g$bbox_width_px, 30)
  expect_equal(g$bbox_height_px, 10)
  d <- measure_shape_descriptors(g)
  expect_equal(d$extent, 1)
  expect_gt(d$aspect_ratio, 2.5)
  expect_lt(d$aspect_ratio, 3.5)
})

test_that("circle-like mask has near-unit aspect and high circularity", {
  m <- make_ellipse_mask(61, 61, cx = 31, cy = 31, rx = 15, ry = 15)
  g <- measure_mask_geometry(m)
  expect_gt(g$area_px, 600)
  expect_lt(g$area_px, 800)
  d <- measure_shape_descriptors(g)
  expect_lt(abs(d$aspect_ratio - 1), 0.08)
  expect_gt(d$circularity, 0.85)
  expect_lt(d$eccentricity, 0.2)
  expect_gt(d$solidity, 0.93)
})

test_that("ellipse mask has elevated aspect and eccentricity", {
  m <- make_ellipse_mask(80, 80, cx = 40, cy = 40, rx = 25, ry = 10)
  g <- measure_mask_geometry(m)
  d <- measure_shape_descriptors(g)
  expect_gt(d$aspect_ratio, 2)
  expect_gt(d$eccentricity, 0.7)
  expect_lt(d$extent, 1)
})

test_that("concave mask has solidity < 1", {
  m <- make_concave_c_mask()
  g <- measure_mask_geometry(m)
  d <- measure_shape_descriptors(g)
  expect_true(g$valid)
  expect_lt(d$solidity, 1)
  expect_gt(d$solidity, 0)
})

test_that("line-like degenerate mask avoids impossible ratios", {
  m <- matrix(FALSE, 20, 20)
  m[10, 3:18] <- TRUE
  g <- measure_mask_geometry(m)
  expect_true(g$valid)
  expect_equal(g$area_px, 16)
  d <- measure_shape_descriptors(g)
  # minor may be NA or tiny; aspect/eccentricity must not be Inf
  expect_true(is.na(d$aspect_ratio) || is.finite(d$aspect_ratio))
  expect_true(is.na(d$eccentricity) || (is.finite(d$eccentricity) && d$eccentricity <= 1))
})

test_that("physical conversion uses length/s and area/s^2", {
  m <- matrix(FALSE, 30, 30)
  m[5:14, 5:14] <- TRUE # 100 px
  g <- measure_mask_geometry(m)
  phys <- apply_calibration_to_geometry(g, pixels_per_mm = 10)
  expect_true(phys$calibrated)
  expect_equal(phys$area_mm2, 1)
  expect_equal(phys$bbox_width_mm, 1)
  expect_equal(phys$bbox_height_mm, 1)
  expect_true(is.finite(phys$equivalent_diameter_mm))
})

test_that("missing calibration leaves physical metrics NA", {
  m <- matrix(FALSE, 20, 20)
  m[5:10, 5:10] <- TRUE
  g <- measure_mask_geometry(m)
  phys <- apply_calibration_to_geometry(g, pixels_per_mm = NA_real_)
  expect_false(phys$calibrated)
  expect_true(is.na(phys$area_mm2))
  expect_true(is.na(phys$perimeter_mm))
})

test_that("dimensionless descriptors unchanged by calibration", {
  m <- make_ellipse_mask(50, 50, 25, 25, 12, 8)
  g <- measure_mask_geometry(m)
  d1 <- measure_shape_descriptors(g)
  # Descriptors do not take calibration; physical layer is separate.
  expect_true(is.finite(d1$aspect_ratio) || is.na(d1$aspect_ratio))
  phys <- apply_calibration_to_geometry(g, 5)
  expect_null(phys$aspect_ratio)
})

test_that("measure_detections builds a table with ids", {
  m1 <- matrix(FALSE, 20, 20)
  m1[5:10, 5:10] <- TRUE
  m2 <- matrix(FALSE, 20, 20)
  m2[8:15, 8:15] <- TRUE
  dets <- list(
    list(id = 1L, mask = m1, bbox_roi = list(x_min = 0L, x_max = 20L, y_min = 0L, y_max = 20L)),
    list(id = 2L, mask = m2, bbox_roi = list(x_min = 0L, x_max = 20L, y_min = 0L, y_max = 20L))
  )
  df <- measure_detections(dets, pixels_per_mm = 10)
  expect_equal(nrow(df), 2L)
  expect_equal(df$id, c(1L, 2L))
  expect_true(all(df$calibrated))
  expect_true(all(is.finite(df$area_mm2)))

  disp <- measurements_display_table(df, calibrated = TRUE)
  expect_true("Area (mm²)" %in% names(disp))
  expect_false(any(is.na(disp[["Area (mm²)"]])))

  disp_uncal <- measurements_display_table(df, calibrated = FALSE)
  expect_true(all(is.na(disp_uncal[["Area (mm²)"]])))
})

test_that("equivalent diameter matches definition", {
  m <- matrix(FALSE, 40, 40)
  m[10:29, 10:29] <- TRUE
  g <- measure_mask_geometry(m)
  expect_equal(g$equivalent_diameter_px, sqrt(4 * 400 / pi))
})
