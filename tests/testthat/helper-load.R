# Shared setup for AlfalfaCV automated tests.

project_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))

source(file.path(project_root, "R", "image_io.R"), local = TRUE)
source(file.path(project_root, "R", "roi.R"), local = TRUE)
source(file.path(project_root, "R", "image_processing.R"), local = TRUE)
source(file.path(project_root, "R", "seed_detection.R"), local = TRUE)

fixture_dir <- file.path(project_root, "tests", "fixtures")
synthetic_png <- file.path(fixture_dir, "PI_100001.png")
orientation_png <- file.path(fixture_dir, "orientation_corners.png")

#' Build a synthetic RGB array with a dark background and oval seeds.
make_synthetic_seed_rgb <- function(
    width = 200L,
    height = 300L,
    seeds = list(
      list(x = 50, y = 60, rx = 10, ry = 7, color = c(0.85, 0.70, 0.20)),
      list(x = 140, y = 70, rx = 9, ry = 6, color = c(0.20, 0.75, 0.35)),
      list(x = 55, y = 160, rx = 11, ry = 8, color = c(0.75, 0.35, 0.55)),
      list(x = 145, y = 170, rx = 10, ry = 7, color = c(0.90, 0.90, 0.25))
    ),
    noise_n = 0L
) {
  rgb <- array(0.02, dim = c(height, width, 3))
  truth <- list()

  for (i in seq_along(seeds)) {
    s <- seeds[[i]]
    yy <- max(1L, as.integer(s$y - s$ry)):min(height, as.integer(s$y + s$ry))
    xx <- max(1L, as.integer(s$x - s$rx)):min(width, as.integer(s$x + s$rx))
    area <- 0L
    for (y in yy) {
      for (x in xx) {
        if (((x - s$x) / s$rx)^2 + ((y - s$y) / s$ry)^2 <= 1) {
          rgb[y, x, 1] <- s$color[[1]]
          rgb[y, x, 2] <- s$color[[2]]
          rgb[y, x, 3] <- s$color[[3]]
          area <- area + 1L
        }
      }
    }
    truth[[i]] <- list(
      centroid_x = s$x,
      centroid_y = s$y,
      area = area
    )
  }

  if (noise_n > 0L) {
    set.seed(1)
    for (k in seq_len(noise_n)) {
      x <- sample.int(width, 1)
      y <- sample.int(height, 1)
      rgb[y, x, ] <- runif(3, 0.4, 1)
    }
  }

  list(rgb = rgb, truth = truth)
}

#' Write / refresh the asymmetric orientation fixture.
ensure_orientation_fixture <- function() {
  if (file.exists(orientation_png)) {
    return(orientation_png)
  }
  if (!requireNamespace("png", quietly = TRUE)) {
    skip("png package required for orientation fixture")
  }
  w <- 200L
  h <- 100L
  arr <- array(0.05, dim = c(h, w, 3))
  # UL red, UR blue, LL green, LR yellow
  arr[1:20, 1:20, 1] <- 1
  arr[1:20, 1:20, 2:3] <- 0
  arr[1:20, 181:200, 3] <- 1
  arr[1:20, 181:200, 1:2] <- 0
  arr[81:100, 1:20, 2] <- 1
  arr[81:100, 1:20, c(1, 3)] <- 0
  arr[81:100, 181:200, 1:2] <- 1
  arr[81:100, 181:200, 3] <- 0
  dir.create(dirname(orientation_png), recursive = TRUE, showWarnings = FALSE)
  png::writePNG(arr, orientation_png)
  orientation_png
}

patch_mean <- function(rgb, rows, cols) {
  c(
    mean(rgb[rows, cols, 1]),
    mean(rgb[rows, cols, 2]),
    mean(rgb[rows, cols, 3])
  )
}

is_reddish <- function(v) v[1] > 0.8 && v[2] < 0.2 && v[3] < 0.2
is_blueish <- function(v) v[3] > 0.8 && v[1] < 0.2 && v[2] < 0.2
is_greenish <- function(v) v[2] > 0.8 && v[1] < 0.2 && v[3] < 0.2
is_yellowish <- function(v) v[1] > 0.8 && v[2] > 0.8 && v[3] < 0.2
