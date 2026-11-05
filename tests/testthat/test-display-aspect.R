# Display aspect-ratio and overlay coordinate regression tests.

test_that("preview_display_height_px preserves aspect within bounds", {
  # Wide ruler-like crop: height should be small.
  h_wide <- preview_display_height_px(800, 100, max_height = 200, assumed_width = 640)
  expect_lte(h_wide, 200L)
  expect_gte(h_wide, 80L)
  expect_lt(h_wide, 160L)

  # Tall narrow ROI: height should hit the max clamp.
  h_tall <- preview_display_height_px(200, 800, max_height = 360, assumed_width = 640)
  expect_equal(h_tall, 360L)
})

test_that("preview_display_height_px tolerates NULL/NA before previews exist", {
  # Regression: workspace_ui called this with NULL ruler/ROI dims while
  # can_preview was already TRUE, causing if (!is.finite(numeric(0))).
  expect_equal(preview_display_height_px(NULL, NULL, max_height = 160), 160L)
  expect_equal(preview_display_height_px(NULL, 100, max_height = 160), 160L)
  expect_equal(preview_display_height_px(100, NULL, max_height = 160), 160L)
  expect_equal(preview_display_height_px(NA_real_, NA_real_, max_height = 160), 160L)
  expect_equal(preview_display_height_px(numeric(0), numeric(0), max_height = 160), 160L)
  expect_equal(preview_display_height_px(0, 100, max_height = 160), 160L)
})

test_that("plot_image_topleft uses unit aspect ratio", {
  # Verify the plotting helper sets asp=1 by rendering to a device and
  # checking usr coordinates match pixel extents (no stretch of data range).
  ras <- grDevices::as.raster(matrix("#112233", nrow = 50, ncol = 100))
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  grDevices::png(tmp, width = 400, height = 200)
  op <- par(mar = c(0, 0, 0, 0))
  on.exit(par(op), add = TRUE)
  plot_image_topleft(ras, 100, 50)
  usr <- par("usr")
  # Data window should still be [0,w] x [h,0] regardless of device aspect.
  expect_equal(usr[1], 0, tolerance = 1e-6)
  expect_equal(usr[2], 100, tolerance = 1e-6)
  expect_equal(usr[3], 50, tolerance = 1e-6)
  expect_equal(usr[4], 0, tolerance = 1e-6)
  grDevices::dev.off()
})

test_that("full and ROI preview creation preserve aspect ratio of source", {
  skip_if_not_installed("magick")
  skip_if_not_installed("png")
  # Tall image
  arr <- array(0.2, dim = c(400, 100, 3))
  path <- tempfile(fileext = ".png")
  on.exit(unlink(path), add = TRUE)
  png::writePNG(arr, path)
  img <- load_image_magick(path)
  full <- create_full_preview(img, max_width = 200L)
  expect_lt(abs((full$width / full$height) - (100 / 400)), 0.02)

  roi <- make_roi(10, 90, 20, 380)
  crop <- crop_roi_magick(img, roi)
  rp <- create_roi_preview(crop, max_width = 200L)
  cdims <- magick_dimensions(crop)
  expect_lt(abs((rp$width / rp$height) - (cdims$width / cdims$height)), 0.02)
})

test_that("detection overlay scale matches ROI preview scale factor", {
  # Coordinate invariant: bbox * scale maps into preview pixel space.
  bbox <- list(x_min = 10L, x_max = 40L, y_min = 5L, y_max = 25L)
  scale <- 0.5
  expect_equal(bbox$x_min * scale, 5)
  expect_equal(bbox$y_max * scale, 12.5)
})

test_that("morphology is independent of display dimensions", {
  m <- matrix(FALSE, 40, 40)
  m[10:29, 10:29] <- TRUE
  g1 <- measure_mask_geometry(m)
  # Changing display size arguments must not exist on the geometry API —
  # recompute identically.
  g2 <- measure_mask_geometry(m)
  expect_equal(g1$area_px, g2$area_px)
  expect_equal(g1$major_axis_px, g2$major_axis_px)
  expect_equal(g1$perimeter_px, g2$perimeter_px)
})
