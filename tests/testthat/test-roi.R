test_that("conservative fallback ROI is wider than the old 0.65 strip", {
  expect_equal(DEFAULT_SEED_ROI_NORMALIZED$left, 0.58)
  expect_equal(DEFAULT_SEED_ROI_NORMALIZED$right, 0.72)
  expect_gt(DEFAULT_SEED_ROI_NORMALIZED$right, 0.65)
  expect_lt(DEFAULT_SEED_ROI_NORMALIZED$right, SEED_SEARCH_ROI_NORMALIZED$right)
  # Fallback starts right of the search-band edge where the palette can reach.
  expect_gt(DEFAULT_SEED_ROI_NORMALIZED$left, SEED_SEARCH_ROI_NORMALIZED$left)
})

test_that("default ROI initializes from centralized normalized bounds without preview", {
  roi <- initialize_default_roi(1000, 2000)
  expect_equal(roi$x_min, as.integer(floor(0.58 * 1000)))
  expect_equal(roi$x_max, as.integer(ceiling(0.72 * 1000)))
  expect_equal(roi$y_min, as.integer(floor(0.06 * 2000)))
  expect_equal(roi$y_max, as.integer(ceiling(0.85 * 2000)))
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

test_that("reset uses the same default initializer without preview", {
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

test_that("content-aware ROI includes both seed columns and excludes white circles", {
  skip_if_not_installed("mmand")
  layout <- make_two_column_seed_layout_rgb()
  roi <- initialize_default_roi(
    layout$width, layout$height,
    preview_rgb = layout$rgb,
    preview_width = layout$width,
    preview_height = layout$height
  )
  expect_true(validate_roi(roi, layout$width, layout$height)$ok)
  # Both seed columns should fall inside the ROI.
  expect_lt(roi$x_min, layout$seed_x_range[[1]] - 2)
  expect_gt(roi$x_max, layout$seed_x_range[[2]] + 2)
  # White circles at ~0.88 width must stay outside.
  circle_x <- as.integer(round(0.88 * layout$width))
  expect_lt(roi$x_max, circle_x - 5L)
})

test_that("content-aware ROI handles shifted columns and missing seeds", {
  skip_if_not_installed("mmand")
  layout <- make_two_column_seed_layout_rgb(
    shift_x = 0.03,
    missing_seeds = c(2L, 7L),
    seed_rx = 7,
    seed_ry = 5
  )
  roi <- initialize_default_roi(
    layout$width, layout$height,
    preview_rgb = layout$rgb,
    preview_width = layout$width,
    preview_height = layout$height
  )
  expect_true(validate_roi(roi, layout$width, layout$height)$ok)
  expect_lt(roi$x_min, min(layout$seed_x_range))
  expect_gt(roi$x_max, max(layout$seed_x_range))
  expect_lt(roi$x_max, as.integer(0.84 * layout$width))
})

test_that("content-aware ROI falls back when no seed-like components exist", {
  rgb <- array(0.02, dim = c(200, 300, 3))
  roi <- initialize_default_roi(
    300, 200,
    preview_rgb = rgb,
    preview_width = 300,
    preview_height = 200
  )
  fallback <- normalized_to_roi(DEFAULT_SEED_ROI_NORMALIZED, 300, 200)
  expect_identical(roi, fallback)
})

palette_cases <- list(
  inside_band = c(0.40, 0.54),
  crossing_band_edge = c(0.30, 0.50),
  near_seeds = c(0.44, 0.545)
)
for (nm in names(palette_cases)) {
  local({
    pal <- palette_cases[[nm]]
    test_that(sprintf("palette swatches are excluded from the seed ROI (%s)", nm), {
      skip_if_not_installed("mmand")
      layout <- make_two_column_seed_layout_rgb(palette_x = pal)
      roi <- initialize_default_roi(
        layout$width, layout$height,
        preview_rgb = layout$rgb,
        preview_width = layout$width,
        preview_height = layout$height
      )
      expect_true(validate_roi(roi, layout$width, layout$height)$ok)
      expect_true(roi$x_min %% 1 == 0 && roi$x_max %% 1 == 0)
      expect_lt(roi$x_max, layout$width)
      # Left edge at or right of the palette's right edge.
      expect_gte(roi$x_min, as.integer(round(pal[[2]] * layout$width)))
      # Left column of seeds (cx - rx) still inside; right column too.
      expect_lt(roi$x_min, layout$seed_x_range[[1]] - 9L)
      expect_gt(roi$x_max, layout$seed_x_range[[2]] + 9L)
      # Right boundary behavior unchanged: circles excluded.
      expect_lt(roi$x_max, as.integer(round(0.88 * layout$width)) - 28L)
    })
  })
}

test_that("palette components are reported as references, not seeds", {
  skip_if_not_installed("mmand")
  layout <- make_two_column_seed_layout_rgb(palette_x = c(0.40, 0.54))
  sp <- normalized_to_roi(SEED_SEARCH_ROI_NORMALIZED, layout$width, layout$height)
  idx <- roi_to_array_indices(sp)
  cl <- classify_seed_search_components(
    layout$rgb[idx$row_min:idx$row_max, idx$col_min:idx$col_max, , drop = FALSE]
  )
  expect_equal(nrow(cl$seed_components), 8L)
  expect_gt(nrow(cl$reference_components), 0L)
  pal_right_crop <- round(0.54 * layout$width) - sp$x_min
  expect_true(all(cl$seed_components$x_min > pal_right_crop))
})

test_that("palette exclusion does not assume eight seeds", {
  skip_if_not_installed("mmand")
  layout <- make_two_column_seed_layout_rgb(
    palette_x = c(0.40, 0.54), missing_seeds = c(1L, 3L, 6L)
  )
  roi <- initialize_default_roi(
    layout$width, layout$height,
    preview_rgb = layout$rgb, preview_width = layout$width, preview_height = layout$height
  )
  expect_gte(roi$x_min, as.integer(round(0.54 * layout$width)))
  xs <- vapply(layout$truth_seeds, function(s) s$cx, numeric(1))
  expect_true(all(xs > roi$x_min & xs < roi$x_max))
})

test_that("palette exclusion holds when ROI is refined from a downscaled preview", {
  skip_if_not_installed("mmand")
  layout <- make_two_column_seed_layout_rgb(width = 800L, height = 1000L,
    seed_rx = 16, seed_ry = 12, palette_x = c(0.40, 0.54), palette_swatch = 44L)
  # Preview at half resolution; ROI must come back in original coordinates.
  prev <- layout$rgb[seq(1, 1000, by = 2), seq(1, 800, by = 2), , drop = FALSE]
  roi <- initialize_default_roi(800L, 1000L, preview_rgb = prev, preview_width = 400L, preview_height = 500L)
  expect_true(validate_roi(roi, 800L, 1000L)$ok)
  expect_gte(roi$x_min, as.integer(round(0.54 * 800)))
  expect_lt(roi$x_min, layout$seed_x_range[[1]] - 16L)
  expect_gt(roi$x_max, layout$seed_x_range[[2]] + 16L)
})

test_that("unequal seed sizes and mixed colors still refine ROI", {
  skip_if_not_installed("mmand")
  layout <- make_two_column_seed_layout_rgb(seed_rx = 11, seed_ry = 5, n_circles = 2L)
  roi <- refine_seed_roi_from_preview(
    layout$rgb, layout$width, layout$height, layout$width, layout$height
  )
  expect_false(is.null(roi))
  expect_lt(roi$x_max / layout$width, 0.84)
  expect_gt(roi_width(roi), 20L)
})
