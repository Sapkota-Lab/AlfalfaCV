# Shared setup for AlfalfaCV automated tests.

project_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))

source(file.path(project_root, "R", "image_io.R"), local = TRUE)
source(file.path(project_root, "R", "roi.R"), local = TRUE)
source(file.path(project_root, "R", "image_processing.R"), local = TRUE)
source(file.path(project_root, "R", "seed_detection.R"), local = TRUE)
source(file.path(project_root, "R", "ruler_calibration.R"), local = TRUE)
source(file.path(project_root, "R", "morphology.R"), local = TRUE)

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

#' Filled ellipse logical mask for morphology tests.
make_ellipse_mask <- function(nr, nc, cx, cy, rx, ry) {
  m <- matrix(FALSE, nr, nc)
  for (r in seq_len(nr)) {
    for (c in seq_len(nc)) {
      if (((c - cx) / rx)^2 + ((r - cy) / ry)^2 <= 1) {
        m[r, c] <- TRUE
      }
    }
  }
  m
}

#' Simple C-shaped concave mask.
make_concave_c_mask <- function(nr = 40L, nc = 40L) {
  m <- matrix(FALSE, nr, nc)
  m[8:32, 8:32] <- TRUE
  m[12:28, 12:32] <- FALSE
  m
}

#' Synthetic USDA-like layout for ROI refinement tests (downscaled RGB).
#'
#' Dark background, two columns of colored seeds, large white reference
#' circles on the right, optional color-chart noise on the left.
make_two_column_seed_layout_rgb <- function(
    width = 400L,
    height = 500L,
    seed_cols = c(0.58, 0.68),
    seed_rows = c(0.18, 0.35, 0.52, 0.69),
    seed_rx = 8,
    seed_ry = 6,
    circle_x = 0.88,
    circle_r = 28,
    n_circles = 3L,
    missing_seeds = integer(),
    shift_x = 0,
    palette_x = NULL,
    palette_swatch = 22L
) {
  width <- as.integer(width)
  height <- as.integer(height)
  rgb <- array(0.03, dim = c(height, width, 3))
  # Left color-chart strip (distractor).
  rgb[, 1:as.integer(0.12 * width), 1] <- 0.7
  rgb[, 1:as.integer(0.12 * width), 2] <- 0.2
  rgb[, 1:as.integer(0.12 * width), 3] <- 0.2

  # Optional swatch palette spanning normalized x range palette_x = c(left, right)
  # (can cross into the seed search band, like real USDA layouts).
  if (!is.null(palette_x)) {
    sw <- as.integer(palette_swatch)
    gapp <- 4L
    sw_cols <- list(
      c(0.80, 0.15, 0.15), c(0.15, 0.70, 0.20), c(0.20, 0.25, 0.80),
      c(0.90, 0.85, 0.20), c(0.55, 0.55, 0.55), c(0.80, 0.45, 0.10),
      c(0.60, 0.30, 0.70), c(0.35, 0.65, 0.65)
    )
    x_lo <- as.integer(round(palette_x[[1]] * width)) + 1L
    x_hi <- as.integer(round(palette_x[[2]] * width))
    k <- 0L
    x0 <- x_hi - sw + 1L
    while (x0 >= 1L) {
      xs <- max(x_lo, x0):(x0 + sw - 1L)
      y0 <- as.integer(0.08 * height)
      while (y0 + sw - 1L <= as.integer(0.80 * height)) {
        k <- k + 1L
        col <- sw_cols[[((k - 1L) %% length(sw_cols)) + 1L]]
        for (ch in 1:3) rgb[y0:(y0 + sw - 1L), xs, ch] <- col[[ch]]
        y0 <- y0 + sw + gapp
      }
      if (x0 <= x_lo) break
      x0 <- x0 - sw - gapp
    }
  }

  colors <- list(
    c(0.85, 0.70, 0.20),
    c(0.25, 0.75, 0.35),
    c(0.75, 0.35, 0.55),
    c(0.90, 0.88, 0.25)
  )
  seed_id <- 0L
  truth_seeds <- list()
  for (ci in seq_along(seed_cols)) {
    for (ri in seq_along(seed_rows)) {
      seed_id <- seed_id + 1L
      if (seed_id %in% missing_seeds) next
      cx <- as.integer(round((seed_cols[[ci]] + shift_x) * width))
      cy <- as.integer(round(seed_rows[[ri]] * height))
      col <- colors[[((seed_id - 1L) %% length(colors)) + 1L]]
      rx <- seed_rx + ((seed_id %% 3L) - 1L)
      ry <- seed_ry + ((seed_id %% 2L))
      for (y in max(1L, cy - ry):min(height, cy + ry)) {
        for (x in max(1L, cx - rx):min(width, cx + rx)) {
          if (((x - cx) / rx)^2 + ((y - cy) / ry)^2 <= 1) {
            rgb[y, x, ] <- col
          }
        }
      }
      truth_seeds[[length(truth_seeds) + 1L]] <- list(cx = cx, cy = cy, id = seed_id)
    }
  }

  for (k in seq_len(n_circles)) {
    cx <- as.integer(round(circle_x * width))
    cy <- as.integer(round((0.20 + (k - 1) * 0.22) * height))
    for (y in max(1L, cy - circle_r):min(height, cy + circle_r)) {
      for (x in max(1L, cx - circle_r):min(width, cx + circle_r)) {
        if ((x - cx)^2 + (y - cy)^2 <= circle_r^2) {
          rgb[y, x, ] <- 0.95
        }
      }
    }
  }

  list(
    rgb = rgb,
    width = width,
    height = height,
    truth_seeds = truth_seeds,
    seed_x_range = range(vapply(truth_seeds, function(s) s$cx, numeric(1)))
  )
}
