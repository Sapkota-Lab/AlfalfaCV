test_that("magick RGB conversion preserves asymmetric corner colors", {
  skip_if_not_installed("magick")
  skip_if_not_installed("png")
  path <- ensure_orientation_fixture()

  img <- load_image_magick(path)
  rgb <- magick_to_rgb_array(img)

  expect_true(is_reddish(patch_mean(rgb, 1:20, 1:20)))
  expect_true(is_blueish(patch_mean(rgb, 1:20, 181:200)))
  expect_true(is_greenish(patch_mean(rgb, 81:100, 1:20)))
  expect_true(is_yellowish(patch_mean(rgb, 81:100, 181:200)))
})

test_that("plot raster from magick preserves corner orientation", {
  skip_if_not_installed("magick")
  skip_if_not_installed("png")
  path <- ensure_orientation_fixture()

  img <- load_image_magick(path)
  ras <- magick_to_plot_raster(img)
  expect_equal(dim(ras), c(100L, 200L))
  expect_true(grepl("^#ff0000", ras[1, 1], ignore.case = TRUE))
  expect_true(grepl("^#0000ff", ras[1, 200], ignore.case = TRUE))
  expect_true(grepl("^#00ff00", ras[100, 1], ignore.case = TRUE))
  expect_true(grepl("^#ffff00", ras[100, 200], ignore.case = TRUE))
})

test_that("ROI crop preserves independently known corner pixels", {
  skip_if_not_installed("magick")
  skip_if_not_installed("png")
  path <- ensure_orientation_fixture()

  img <- load_image_magick(path)
  # Crop upper-left 40x30 including red square only.
  roi <- make_roi(0L, 40L, 0L, 30L)
  crop <- crop_roi_magick(img, roi)
  crgb <- magick_to_rgb_array(crop)
  expect_equal(dim(crgb)[1], 30L)
  expect_equal(dim(crgb)[2], 40L)
  expect_true(is_reddish(patch_mean(crgb, 1:20, 1:20)))
  # Lower-right of this crop should remain dark background, not yellow.
  expect_lt(mean(crgb[25:30, 35:40, ]), 0.2)
})

test_that("ROI preview inherits crop orientation", {
  skip_if_not_installed("magick")
  skip_if_not_installed("png")
  path <- ensure_orientation_fixture()

  img <- load_image_magick(path)
  roi <- make_roi(160L, 200L, 70L, 100L) # lower-right yellow region
  crop <- crop_roi_magick(img, roi)
  preview <- create_roi_preview(crop, max_width = 200L)
  prgb <- magick_to_rgb_array(preview$image)
  # Yellow square maps into the lower-right of this crop.
  nr <- dim(prgb)[1]
  nc <- dim(prgb)[2]
  expect_true(is_yellowish(patch_mean(
    prgb,
    max(1L, nr - 15L):nr,
    max(1L, nc - 15L):nc
  )))
  expect_lt(mean(prgb[, , 3]), 0.25)
})

test_that("plot_image_topleft does not flip or mirror the bitmap", {
  skip_if_not_installed("png")

  w <- 40L
  h <- 20L
  arr <- array(0, dim = c(h, w, 3))
  arr[1:4, 1:4, 1] <- 1
  arr[1:4, (w - 3):w, 3] <- 1
  arr[(h - 3):h, 1:4, 2] <- 1
  arr[(h - 3):h, (w - 3):w, 1:2] <- 1
  ras <- rgb_array_to_raster(arr)

  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp, width = 400, height = 200)
  par(mar = c(0, 0, 0, 0))
  plot_image_topleft(ras, w, h)
  grDevices::dev.off()

  rendered <- png::readPNG(tmp)
  nr <- dim(rendered)[1]
  nc <- dim(rendered)[2]
  ul <- rendered[5, 5, 1:3]
  ur <- rendered[5, nc - 5, 1:3]
  ll <- rendered[nr - 5, 5, 1:3]
  lr <- rendered[nr - 5, nc - 5, 1:3]

  expect_true(ul[1] > 0.8 && ul[2] < 0.2 && ul[3] < 0.2)
  expect_true(ur[3] > 0.8 && ur[1] < 0.2 && ur[2] < 0.2)
  expect_true(ll[2] > 0.8 && ll[1] < 0.2 && ll[3] < 0.2)
  expect_true(lr[1] > 0.8 && lr[2] > 0.8 && lr[3] < 0.2)
})
