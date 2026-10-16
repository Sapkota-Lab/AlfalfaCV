#!/usr/bin/env Rscript
# Synthetic seed-layout fixture for UI / detection smoke testing.
# Not a scientific reference. The Shiny app does not depend on this file.

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
img <- array(0, dim = c(height, width, 3))

# Left color calibration palette
palette_colors <- matrix(
  c(
    1, 0, 0,
    0, 1, 0,
    0, 0, 1,
    1, 1, 0,
    1, 1, 1,
    0.5, 0.5, 0.5,
    0.2, 0.6, 0.2,
    0.8, 0.4, 0.1
  ),
  ncol = 3, byrow = TRUE
)
for (i in seq_len(nrow(palette_colors))) {
  y1 <- 40L + (i - 1L) * 48L
  y2 <- y1 + 39L
  img[y1:y2, 20:69, 1] <- palette_colors[i, 1]
  img[y1:y2, 20:69, 2] <- palette_colors[i, 2]
  img[y1:y2, 20:69, 3] <- palette_colors[i, 3]
}

# Seeds placed inside the default Phase 1B ROI band (~0.58–0.65 width).
set.seed(100001)
seed_xs <- as.integer(seq(480, 510, length.out = 2))
seed_ys <- c(80L, 160L, 240L, 320L)
seed_colors <- list(
  c(0.85, 0.70, 0.25),
  c(0.25, 0.75, 0.40),
  c(0.80, 0.35, 0.55),
  c(0.90, 0.88, 0.20)
)
for (ri in seq_along(seed_ys)) {
  for (sx in seed_xs) {
    col <- seed_colors[[((ri + sx) %% length(seed_colors)) + 1L]]
    rx <- 9L
    ry <- 6L
    ry0 <- seed_ys[[ri]]
    for (y in (ry0 - ry):(ry0 + ry)) {
      for (x in (sx - rx):(sx + rx)) {
        if (x < 1 || x > width || y < 1 || y > height) next
        if (((x - sx) / rx)^2 + ((y - ry0) / ry)^2 <= 1) {
          img[y, x, 1] <- col[[1]]
          img[y, x, 2] <- col[[2]]
          img[y, x, 3] <- col[[3]]
        }
      }
    }
  }
}

# White row markers on the right (outside default ROI)
for (ry in seed_ys) {
  for (y in (ry - 10):(ry + 10)) {
    for (x in (740 - 10):(740 + 10)) {
      if ((x - 740)^2 + (y - ry)^2 <= 100) img[y, x, ] <- 1
    }
  }
}

# Bottom ruler
img[450:480, 100:700, ] <- 0.92
for (tick in seq(100L, 700L, by = 10L)) {
  tick_h <- if ((tick - 100L) %% 50L == 0L) 28L else 14L
  img[(480L - tick_h):480L, tick, ] <- 0.05
}

png::writePNG(img, out_path)
message("Wrote synthetic fixture: ", out_path)
message("Dimensions: ", width, " x ", height)
