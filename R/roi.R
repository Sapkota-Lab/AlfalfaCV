# ROI helpers for AlfalfaCV Phase 1B.
#
# Coordinate convention (documented once, used everywhere):
# - Origin at the top-left of the original image.
# - X increases to the right; Y increases downward.
# - Stored ROI uses integer pixel boundaries on a half-open interval:
#     [x_min, x_max) × [y_min, y_max)
# - Mapping to R's 1-based raster indexing for inclusive crop slices:
#     columns = (x_min + 1L) : x_max
#     rows    = (y_min + 1L) : y_max
# - Normalized coordinates are fractions of image width/height in [0, 1],
#   using the same top-left origin (top < bottom, left < right).

# Provisional USDA seed-layout defaults (not scientifically validated).
DEFAULT_SEED_ROI_NORMALIZED <- list(
  left = 0.58,
  right = 0.65,
  top = 0.07,
  bottom = 0.88
)

#' Create an ROI list with half-open integer bounds.
#'
#' @param x_min,x_max,y_min,y_max Integer-like boundaries.
#' @return Named list ROI.
make_roi <- function(x_min, x_max, y_min, y_max) {
  list(
    x_min = as.integer(x_min),
    x_max = as.integer(x_max),
    y_min = as.integer(y_min),
    y_max = as.integer(y_max)
  )
}

#' ROI width in pixels (half-open).
roi_width <- function(roi) {
  as.integer(roi$x_max - roi$x_min)
}

#' ROI height in pixels (half-open).
roi_height <- function(roi) {
  as.integer(roi$y_max - roi$y_min)
}

#' Convert normalized ROI fractions to original-image half-open pixel ROI.
#'
#' @param norm List with left, right, top, bottom in [0, 1].
#' @param image_width,image_height Original image dimensions in pixels.
#' @return Clamped integer ROI.
normalized_to_roi <- function(norm, image_width, image_height) {
  w <- as.integer(image_width)
  h <- as.integer(image_height)
  stopifnot(w > 0L, h > 0L)

  x_min <- as.integer(floor(norm$left * w))
  x_max <- as.integer(ceiling(norm$right * w))
  y_min <- as.integer(floor(norm$top * h))
  y_max <- as.integer(ceiling(norm$bottom * h))

  clamp_roi(make_roi(x_min, x_max, y_min, y_max), w, h)
}

#' Convert original-image ROI to normalized fractions.
roi_to_normalized <- function(roi, image_width, image_height) {
  w <- as.numeric(image_width)
  h <- as.numeric(image_height)
  list(
    left = roi$x_min / w,
    right = roi$x_max / w,
    top = roi$y_min / h,
    bottom = roi$y_max / h
  )
}

#' Initialize the default seed ROI for an image size.
initialize_default_roi <- function(
    image_width,
    image_height,
    norm = DEFAULT_SEED_ROI_NORMALIZED
) {
  normalized_to_roi(norm, image_width, image_height)
}

#' Clamp ROI to image bounds and ensure non-empty half-open interval when possible.
clamp_roi <- function(roi, image_width, image_height) {
  w <- as.integer(image_width)
  h <- as.integer(image_height)

  x_min <- max(0L, min(as.integer(roi$x_min), w))
  x_max <- max(0L, min(as.integer(roi$x_max), w))
  y_min <- max(0L, min(as.integer(roi$y_min), h))
  y_max <- max(0L, min(as.integer(roi$y_max), h))

  if (x_max < x_min) {
    tmp <- x_min
    x_min <- x_max
    x_max <- tmp
  }
  if (y_max < y_min) {
    tmp <- y_min
    y_min <- y_max
    y_max <- tmp
  }

  # Ensure at least a 1-pixel span when the image is non-empty.
  if (w > 0L && x_min == x_max) {
    if (x_max < w) {
      x_max <- x_min + 1L
    } else if (x_min > 0L) {
      x_min <- x_max - 1L
    }
  }
  if (h > 0L && y_min == y_max) {
    if (y_max < h) {
      y_max <- y_min + 1L
    } else if (y_min > 0L) {
      y_min <- y_max - 1L
    }
  }

  make_roi(x_min, x_max, y_min, y_max)
}

#' Validate an ROI against image dimensions.
#'
#' @return List with ok (logical) and message.
validate_roi <- function(roi, image_width, image_height) {
  if (is.null(roi) || !all(c("x_min", "x_max", "y_min", "y_max") %in% names(roi))) {
    return(list(ok = FALSE, message = "ROI is missing required boundaries."))
  }
  w <- as.integer(image_width)
  h <- as.integer(image_height)
  if (is.na(w) || is.na(h) || w <= 0L || h <= 0L) {
    return(list(ok = FALSE, message = "Image dimensions are invalid."))
  }

  vals <- unlist(roi[c("x_min", "x_max", "y_min", "y_max")], use.names = FALSE)
  if (any(is.na(vals))) {
    return(list(ok = FALSE, message = "ROI contains missing values."))
  }
  if (roi$x_min < 0L || roi$y_min < 0L || roi$x_max > w || roi$y_max > h) {
    return(list(ok = FALSE, message = "ROI extends outside the image bounds."))
  }
  if (roi$x_max <= roi$x_min || roi$y_max <= roi$y_min) {
    return(list(ok = FALSE, message = "ROI is empty."))
  }

  list(ok = TRUE, message = "ROI is valid.")
}

#' Map a brush selection from preview coordinates to original-image ROI.
#'
#' Preview and original share the same top-left origin and aspect ratio.
#' Brush coordinates are interpreted in preview pixel space with the same
#' half-open convention after integerization.
#'
#' @param brush Shiny brush list with xmin, xmax, ymin, ymax.
#' @param preview_width,preview_height Preview raster size.
#' @param image_width,image_height Original image size.
#' @return Clamped original-image ROI.
brush_to_roi <- function(
    brush,
    preview_width,
    preview_height,
    image_width,
    image_height
) {
  if (is.null(brush)) {
    stop("Brush selection is missing.")
  }

  pw <- as.numeric(preview_width)
  ph <- as.numeric(preview_height)
  stopifnot(pw > 0, ph > 0)

  # Shiny brush ymin/ymax follow the plot's y orientation. Callers must
  # render previews with ylim = c(preview_height, 0) so brush y matches
  # top-left origin (y down).
  x_min_p <- min(brush$xmin, brush$xmax)
  x_max_p <- max(brush$xmin, brush$xmax)
  y_min_p <- min(brush$ymin, brush$ymax)
  y_max_p <- max(brush$ymin, brush$ymax)

  scale_x <- as.numeric(image_width) / pw
  scale_y <- as.numeric(image_height) / ph

  roi <- make_roi(
    floor(x_min_p * scale_x),
    ceiling(x_max_p * scale_x),
    floor(y_min_p * scale_y),
    ceiling(y_max_p * scale_y)
  )
  clamp_roi(roi, image_width, image_height)
}

#' Map original-image ROI into preview pixel coordinates (half-open).
roi_to_preview <- function(roi, preview_width, preview_height, image_width, image_height) {
  scale_x <- as.numeric(preview_width) / as.numeric(image_width)
  scale_y <- as.numeric(preview_height) / as.numeric(image_height)
  make_roi(
    floor(roi$x_min * scale_x),
    ceiling(roi$x_max * scale_x),
    floor(roi$y_min * scale_y),
    ceiling(roi$y_max * scale_y)
  )
}

#' Convert half-open ROI to inclusive 1-based row/column indices for R arrays.
#'
#' @return List with row_min, row_max, col_min, col_max (inclusive) or NULL if empty.
roi_to_array_indices <- function(roi) {
  if (roi$x_max <= roi$x_min || roi$y_max <= roi$y_min) {
    return(NULL)
  }
  list(
    col_min = roi$x_min + 1L,
    col_max = roi$x_max,
    row_min = roi$y_min + 1L,
    row_max = roi$y_max
  )
}
