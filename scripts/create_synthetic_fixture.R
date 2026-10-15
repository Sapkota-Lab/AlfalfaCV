#!/usr/bin/env Rscript
# Create a synthetic seed-layout fixture for UI testing only.
# Not a scientific reference image. The Shiny app does not depend on this file.

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg))
} else {
  normalizePath("scripts/create_synthetic_fixture.R")
}
project_root <- normalizePath(file.path(dirname(script_path), ".."))

out_dir <- file.path(project_root, "tests", "fixtures")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_path <- file.path(out_dir, "PI_100001.png")

if (!requireNamespace("png", quietly = TRUE)) {
  stop("Package 'png' is required to write the synthetic fixture.")
}

width <- 800L
height <- 500L

# RGB array, values in [0, 1]. Start with black background.
img <- array(0, dim = c(height, width, 3))

# Left color calibration palette (vertical swatches).
palette_colors <- matrix(
  c(
    1.00, 0.00, 0.00,
    0.00, 1.00, 0.00,
    0.00, 0.00, 1.00,
    1.00, 1.00, 0.00,
    1.00, 1.00, 1.00,
    0.50, 0.50, 0.50,
    0.20, 0.60, 0.20,
    0.80, 0.40, 0.10
  ),
  ncol = 3,
  byrow = TRUE
)

swatch_h <- 40L
swatch_w <- 50L
palette_x0 <- 20L
palette_y0 <- 40L
for (i in seq_len(nrow(palette_colors))) {
  y1 <- palette_y0 + (i - 1L) * (swatch_h + 8L)
  y2 <- y1 + swatch_h - 1L
  x1 <- palette_x0
  x2 <- palette_x0 + swatch_w - 1L
  img[y1:y2, x1:x2, 1] <- palette_colors[i, 1]
  img[y1:y2, x1:x2, 2] <- palette_colors[i, 2]
  img[y1:y2, x1:x2, 3] <- palette_colors[i, 3]
}

# Three rows of seed-like ellipses in the center.
set.seed(100001)
seed_color <- c(0.72, 0.58, 0.28)
row_ys <- c(120L, 220L, 320L)
for (ry in row_ys) {
  xs <- as.integer(seq(160, 620, length.out = 10))
  for (sx in xs) {
    rx <- sample(6:9, 1)
    ry_r <- sample(4:7, 1)
    yy <- max(1L, ry - ry_r):min(height, ry + ry_r)
    xx <- max(1L, sx - rx):min(width, sx + rx)
    for (y in yy) {
      for (x in xx) {
        if (((x - sx) / rx)^2 + ((y - ry) / ry_r)^2 <= 1) {
          jitter <- runif(1, 0.85, 1.05)
          img[y, x, 1] <- min(1, seed_color[1] * jitter)
          img[y, x, 2] <- min(1, seed_color[2] * jitter)
          img[y, x, 3] <- min(1, seed_color[3] * jitter)
        }
      }
    }
  }
}

# White circular row markers on the right.
marker_x <- 740L
for (ry in row_ys) {
  rr <- 10L
  yy <- (ry - rr):(ry + rr)
  xx <- (marker_x - rr):(marker_x + rr)
  for (y in yy) {
    for (x in xx) {
      if ((x - marker_x)^2 + (y - ry)^2 <= rr^2) {
        img[y, x, ] <- 1
      }
    }
  }
}

# Bottom ruler with millimeter tick marks.
ruler_y1 <- 450L
ruler_y2 <- 480L
img[ruler_y1:ruler_y2, 100:700, ] <- 0.92
for (tick in seq(100L, 700L, by = 10L)) {
  tick_h <- if ((tick - 100L) %% 50L == 0L) 28L else 14L
  y_top <- ruler_y2 - tick_h
  img[y_top:ruler_y2, tick, ] <- 0.05
}

png::writePNG(img, out_path)
message("Wrote synthetic fixture: ", out_path)
message("Dimensions: ", width, " x ", height)
