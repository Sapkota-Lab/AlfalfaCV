test_that("uniform dark background yields no detections", {
  rgb <- array(0.01, dim = c(80, 80, 3))
  result <- detect_seeds_in_rgb(rgb, params = make_detection_params(min_area = 10L, sensitivity = 0.2))
  expect_equal(result$status, "no_detections")
  expect_equal(result$count, 0L)
})

test_that("bright multi-color seeds on dark background are detected", {
  syn <- make_synthetic_seed_rgb()
  result <- detect_seeds_in_rgb(
    syn$rgb,
    params = make_detection_params(sensitivity = 0.2, min_area = 20L, max_area_frac = 0.25, min_contrast = 0.05)
  )
  expect_equal(result$status, "ok")
  expect_equal(result$count, length(syn$truth))
})

test_that("low-contrast foreground can be rejected by contrast filter", {
  rgb <- array(0.02, dim = c(100, 100, 3))
  # Weak bump barely above background
  rgb[40:55, 40:55, ] <- 0.06
  result <- detect_seeds_in_rgb(
    rgb,
    params = make_detection_params(sensitivity = 0.05, min_area = 10L, min_contrast = 0.08)
  )
  expect_equal(result$count, 0L)
})

test_that("differently colored foreground objects are retained", {
  syn <- make_synthetic_seed_rgb(
    seeds = list(
      list(x = 40, y = 40, rx = 10, ry = 8, color = c(0.9, 0.2, 0.2)),
      list(x = 140, y = 50, rx = 10, ry = 8, color = c(0.2, 0.3, 0.9)),
      list(x = 60, y = 180, rx = 11, ry = 8, color = c(0.2, 0.85, 0.3))
    )
  )
  result <- detect_seeds_in_rgb(
    syn$rgb,
    params = make_detection_params(sensitivity = 0.15, min_area = 20L, max_area_frac = 0.3, min_contrast = 0.05)
  )
  expect_equal(result$count, 3L)
})

test_that("small isolated noise components are rejected by min area", {
  rgb <- array(0.02, dim = c(100, 100, 3))
  rgb[10, 10, ] <- 1
  rgb[12, 14, ] <- 1
  result <- detect_seeds_in_rgb(
    rgb,
    params = make_detection_params(sensitivity = 0.5, min_area = 15L)
  )
  expect_equal(result$count, 0L)
})

test_that("exact minimum-area inclusion and exclusion behave as documented", {
  rgb <- array(0.02, dim = c(120, 120, 3))
  # Build a solid 10x10 = 100 px block
  rgb[30:39, 40:49, 1] <- 0.9
  rgb[30:39, 40:49, 2] <- 0.7
  rgb[30:39, 40:49, 3] <- 0.2

  keep <- detect_seeds_in_rgb(
    rgb,
    params = make_detection_params(sensitivity = 0.2, min_area = 100L, max_area_frac = 0.5, min_contrast = 0.05)
  )
  expect_equal(keep$count, 1L)

  drop <- detect_seeds_in_rgb(
    rgb,
    params = make_detection_params(sensitivity = 0.2, min_area = 101L, max_area_frac = 0.5, min_contrast = 0.05)
  )
  expect_equal(drop$count, 0L)
})

test_that("bounding boxes enclose the corresponding foreground object", {
  syn <- make_synthetic_seed_rgb(
    width = 120L,
    height = 120L,
    seeds = list(list(x = 60, y = 60, rx = 12, ry = 9, color = c(0.85, 0.7, 0.2)))
  )
  result <- detect_seeds_in_rgb(
    syn$rgb,
    params = make_detection_params(sensitivity = 0.2, min_area = 20L, max_area_frac = 0.5, min_contrast = 0.05)
  )
  expect_equal(result$count, 1L)
  d <- result$detections[[1]]
  # Seed center must lie inside half-open bbox.
  expect_gte(60, d$bbox_roi$x_min)
  expect_lt(60, d$bbox_roi$x_max)
  expect_gte(60, d$bbox_roi$y_min)
  expect_lt(60, d$bbox_roi$y_max)
  expect_true(is.matrix(d$mask))
  expect_true(any(d$mask))
})

test_that("segmentation mask aligns with seed pixels at known location", {
  rgb <- array(0.02, dim = c(80, 80, 3))
  rgb[20:35, 25:40, 1] <- 0.9
  rgb[20:35, 25:40, 2] <- 0.75
  rgb[20:35, 25:40, 3] <- 0.2
  seg <- segment_foreground(rgb, sensitivity = 0.2)
  expect_true(seg$mask[28, 30])
  expect_false(seg$mask[5, 5])
})

test_that("absolute coordinates include ROI offset", {
  syn <- make_synthetic_seed_rgb(
    width = 100L,
    height = 100L,
    seeds = list(list(x = 40, y = 40, rx = 10, ry = 8, color = c(0.9, 0.8, 0.1)))
  )
  roi <- make_roi(100, 200, 50, 150)
  result <- detect_seeds_in_rgb(
    syn$rgb,
    params = make_detection_params(sensitivity = 0.2, min_area = 20L, max_area_frac = 0.5, min_contrast = 0.05),
    roi = roi
  )
  expect_equal(result$count, 1L)
  d <- result$detections[[1]]
  expect_equal(d$bbox$x_min, roi$x_min + d$bbox_roi$x_min)
  expect_equal(d$bbox$y_min, roi$y_min + d$bbox_roi$y_min)
})

test_that("deterministic candidate ordering is top-to-bottom then left-to-right", {
  syn <- make_synthetic_seed_rgb(
    seeds = list(
      list(x = 40, y = 40, rx = 8, ry = 6, color = c(0.9, 0.8, 0.2)),
      list(x = 140, y = 45, rx = 8, ry = 6, color = c(0.2, 0.8, 0.9)),
      list(x = 50, y = 200, rx = 9, ry = 7, color = c(0.8, 0.3, 0.3))
    )
  )
  result <- detect_seeds_in_rgb(
    syn$rgb,
    params = make_detection_params(sensitivity = 0.2, min_area = 15L, max_area_frac = 0.25, min_contrast = 0.05)
  )
  expect_equal(result$count, 3L)
  ys <- vapply(result$detections, function(d) d$centroid_roi$y, numeric(1))
  expect_true(all(diff(ys) >= -1e-6))
  expect_lt(result$detections[[1]]$centroid_roi$x, result$detections[[2]]$centroid_roi$x)
  expect_equal(vapply(result$detections, function(d) d$id, integer(1)), 1:3)
})

test_that("suggest_min_area scales with ROI size near USDA reference", {
  # ~2250 x 8000 ≈ 18e6 → around 720
  expect_gt(suggest_min_area(2250, 8000), 500L)
  expect_lt(suggest_min_area(2250, 8000), 1000L)
  # Tiny ROI floors at 50
  expect_equal(suggest_min_area(50, 50), 50L)
})

test_that("components near ROI edges can still be detected", {
  rgb <- array(0.02, dim = c(100, 100, 3))
  rgb[1:12, 1:14, 1] <- 0.9
  rgb[1:12, 1:14, 2] <- 0.75
  rgb[1:12, 1:14, 3] <- 0.2
  result <- detect_seeds_in_rgb(
    rgb,
    params = make_detection_params(sensitivity = 0.25, min_area = 20L, max_area_frac = 0.5, min_contrast = 0.05)
  )
  expect_gte(result$count, 1L)
})

test_that("failed / empty detection clears to a usable empty result", {
  empty <- empty_detection_result(status = "error", message = "boom")
  expect_equal(empty$count, 0L)
  expect_equal(length(empty$detections), 0L)
  expect_null(empty$binary_mask)
})

test_that("fixture detection with conservative defaults finds eight seeds", {
  skip_if_not_installed("magick")
  skip_if_not(file.exists(synthetic_png), "Synthetic fixture missing")

  img <- load_image_magick(synthetic_png)
  dims <- magick_dimensions(img)
  roi <- initialize_default_roi(dims$width, dims$height)
  crop <- crop_roi_magick(img, roi)
  params <- make_detection_params(
    sensitivity = 0.05,
    min_area = suggest_min_area(roi_width(roi), roi_height(roi)),
    max_area_frac = 0.24,
    roi_w = roi_width(roi),
    roi_h = roi_height(roi)
  )
  # Synthetic seeds are small; use a fixture-appropriate min_area floor.
  params$min_area <- 50L
  res <- detect_seeds_in_magick(crop, params = params, roi = roi)
  expect_equal(res$status, "ok")
  expect_equal(res$count, 8L)
})
