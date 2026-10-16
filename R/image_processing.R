# Image preparation helpers for AlfalfaCV Phase 1B.
#
# Uses magick for session-scoped load / downscale / crop.
#
# Coordinate / orientation convention:
# - Top-left origin; X right; Y down.
# - magick images and RGB arrays use row 1 = image top, col 1 = image left.
# - Previews are plotted with rasterImage() into ylim=c(height, 0) so plot
#   coordinates match image coordinates (y increases downward).
# - Plot axes use xaxs="i", yaxs="i" (no 4% padding) so overlays align.
#
# Memory behavior (magick / ImageMagick):
# - image_read() creates an ImageMagick object; pixels may be disk- or
#   memory-backed depending on ImageMagick policy.
# - Previews are downscaled; segmentation uses only the ROI crop.
# - Full-resolution R arrays are created only for the ROI during detection.
# - EXIF orientation is applied once on load via image_orient().

PREVIEW_MAX_WIDTH <- 1200L
ROI_PREVIEW_MAX_WIDTH <- 900L

#' Create a session working directory for temporary image artifacts.
create_session_image_dir <- function(root = tempdir()) {
  path <- file.path(root, paste0("alfalfacv_", as.integer(Sys.time()), "_", sample.int(1e6, 1)))
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  normalizePath(path, mustWork = TRUE)
}

#' Delete a session image directory and its contents.
clear_session_image_dir <- function(path) {
  if (!is.null(path) && length(path) == 1L && !is.na(path) && nzchar(path) && dir.exists(path)) {
    unlink(path, recursive = TRUE, force = TRUE)
  }
  invisible()
}

#' Load an image with magick, applying EXIF orientation.
#'
#' @param path Filesystem path.
#' @return magick-image object in display-correct orientation.
load_image_magick <- function(path) {
  if (is.null(path) || !file.exists(path)) {
    stop("Image path is missing or does not exist.")
  }
  img <- magick::image_read(path)
  # Apply embedded orientation and reset the tag so pixels match display.
  magick::image_orient(img)
}

#' Return width/height of a magick image as integers.
magick_dimensions <- function(img) {
  info <- magick::image_info(img)
  list(width = as.integer(info$width[[1]]), height = as.integer(info$height[[1]]))
}

#' Build a downscaled full-image preview (for ROI selection UI).
#'
#' @param img magick-image
#' @param max_width Maximum preview width in pixels.
#' @return List with image (magick), width, height, scale factors.
create_full_preview <- function(img, max_width = PREVIEW_MAX_WIDTH) {
  dims <- magick_dimensions(img)
  target <- paste0(as.integer(max_width), "x")
  preview <- magick::image_scale(img, target)
  pdims <- magick_dimensions(preview)
  list(
    image = preview,
    width = pdims$width,
    height = pdims$height,
    scale_x = pdims$width / dims$width,
    scale_y = pdims$height / dims$height,
    original_width = dims$width,
    original_height = dims$height
  )
}

#' Crop the original-resolution ROI (half-open bounds).
#'
#' @param img magick-image original
#' @param roi Half-open ROI in original coordinates
#' @return magick-image crop
crop_roi_magick <- function(img, roi) {
  dims <- magick_dimensions(img)
  check <- validate_roi(roi, dims$width, dims$height)
  if (!isTRUE(check$ok)) {
    stop(check$message)
  }
  geometry <- magick::geometry_area(
    width = roi_width(roi),
    height = roi_height(roi),
    x_off = roi$x_min,
    y_off = roi$y_min
  )
  magick::image_crop(img, geometry, repage = TRUE)
}

#' Create an enlarged ROI preview for researcher inspection.
create_roi_preview <- function(roi_img, max_width = ROI_PREVIEW_MAX_WIDTH) {
  dims <- magick_dimensions(roi_img)
  if (dims$width <= as.integer(max_width)) {
    return(list(image = roi_img, width = dims$width, height = dims$height, scale = 1))
  }
  preview <- magick::image_scale(roi_img, paste0(as.integer(max_width), "x"))
  pdims <- magick_dimensions(preview)
  list(
    image = preview,
    width = pdims$width,
    height = pdims$height,
    scale = pdims$width / dims$width
  )
}

#' Write a magick image to a PNG path for Shiny serving / plotting.
write_preview_png <- function(img, path) {
  magick::image_write(img, path = path, format = "png")
  normalizePath(path, mustWork = TRUE)
}

#' Convert a magick image to a native raster for plotting (orientation-preserving).
magick_to_plot_raster <- function(img) {
  grDevices::as.raster(img)
}

#' Convert a magick image to a numeric RGB array [0,1] with dims [rows, cols, 3].
#'
#' Prefer calling this only on ROI crops, not full research images.
#'
#' `magick::image_data()` returns raw bytes as [channel, width, height].
#' Each channel slice is reshaped to a width×height matrix and transposed
#' into R's row×column image layout (row 1 = top). Use `as.integer()` for raw.
magick_to_rgb_array <- function(img) {
  img <- magick::image_convert(img, colorspace = "sRGB", depth = 8)
  data <- magick::image_data(img, channels = "rgb")
  width <- dim(data)[[2]]
  height <- dim(data)[[3]]
  nchan <- min(3L, dim(data)[[1]])
  rgb <- array(0, dim = c(height, width, 3))
  for (c in seq_len(nchan)) {
    slice <- matrix(as.integer(data[c, , ]), nrow = width, ncol = height)
    rgb[, , c] <- t(slice) / 255
  }
  rgb
}

#' Convert RGB array [0,1] to a raster suitable for plot helpers.
rgb_array_to_raster <- function(rgb) {
  grDevices::as.raster(rgb)
}

#' Plot an image raster in top-left origin, y-down coordinates.
#'
#' Uses rasterImage() instead of plot.raster() + inverted ylim, because
#' plot(raster, ylim=c(h,0)) vertically flips the bitmap relative to
#' the coordinate system used for overlays and brushes.
#'
#' Axes use xaxs/yaxs = "i" so data coordinates map exactly onto the
#' drawn image (no default 4% padding drift).
#'
#' @param ras raster object (row 1 = top)
#' @param width,height Pixel dimensions matching the raster
plot_image_topleft <- function(ras, width, height) {
  w <- as.numeric(width)
  h <- as.numeric(height)
  plot(
    NA,
    xlim = c(0, w),
    ylim = c(h, 0),
    xaxs = "i",
    yaxs = "i",
    asp = NA,
    axes = FALSE,
    xlab = "",
    ylab = "",
    type = "n"
  )
  # Place row 1 at y=0 (top) and last row at y=h (bottom).
  graphics::rasterImage(ras, 0, h, w, 0, interpolate = FALSE)
}
