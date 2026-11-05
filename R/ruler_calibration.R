# Ruler calibration for AlfalfaCV.
#
# The active workflow is a confirmed horizontal 10 mm reference on the ruler
# preview: pixels_per_mm = (x2 - x1) / 10 in original-image pixels.
# Physical measurements read only that confirmed scale.
#
# Coordinate convention: top-left origin, x right, y down. Preview clicks are
# converted with preview_point_to_original(); they are not CSS distances.

DEFAULT_RULER_ROI_NORMALIZED <- list(
  left = 0.05,
  right = 0.95,
  top = 0.88,
  bottom = 1.00
)

CALIBRATION_STATUS_UNCALIBRATED <- "UNCALIBRATED"
CALIBRATION_STATUS_PROPOSED <- "PROPOSED"
CALIBRATION_STATUS_CONFIRMED <- "CONFIRMED"

#' Empty / failed calibration object.
empty_calibration <- function(
    status = CALIBRATION_STATUS_UNCALIBRATED,
    message = "No calibration available.",
    warnings = character(),
    method = NA_character_
) {
  list(
    status = status,
    method = method,
    pixels_per_mm = NA_real_,
    tick_spacing_px = NA_real_,
    assumed_interval_mm = NA_real_,
    ruler_roi = NULL,
    tick_positions_x = numeric(),
    major_positions_x = numeric(),
    observed_positions_x = numeric(),
    predicted_positions_x = numeric(),
    tick_lengths = numeric(),
    n_ticks = 0L,
    spacing_cv = NA_real_,
    graduation_band = NULL,
    period_px = NA_real_,
    quality = list(
      n_ticks = 0L,
      period_px = NA_real_,
      period_strength = NA_real_,
      n_supporting_intervals = 0L,
      spacing_consistency = NA_real_,
      major_interval_consistency = NA_real_,
      hierarchy_ratio = NA_real_,
      confidence = "none",
      ambiguity = TRUE
    ),
    warnings = warnings,
    message = message,
    confirmed = FALSE
  )
}

#' Initialize default ruler ROI for an image size.
initialize_default_ruler_roi <- function(
    image_width,
    image_height,
    norm = DEFAULT_RULER_ROI_NORMALIZED
) {
  normalized_to_roi(norm, image_width, image_height)
}

#' Confirm a proposed (or manual) calibration.
confirm_calibration <- function(calibration) {
  if (is.null(calibration) || !is.list(calibration)) {
    return(empty_calibration(message = "Nothing to confirm."))
  }
  if (!is.finite(calibration$pixels_per_mm) || calibration$pixels_per_mm <= 0) {
    calibration$status <- CALIBRATION_STATUS_UNCALIBRATED
    calibration$confirmed <- FALSE
    calibration$message <- "Cannot confirm: invalid scale."
    return(calibration)
  }
  calibration$status <- CALIBRATION_STATUS_CONFIRMED
  calibration$confirmed <- TRUE
  calibration$message <- sprintf(
    "Confirmed scale: %.3f px/mm (%s).",
    calibration$pixels_per_mm,
    calibration$method
  )
  calibration
}

#' Map click/brush coordinates from a ruler preview into original-image space.
#'
#' `x` and `y` are preview pixels (the plot data coordinates), not CSS pixels.
preview_point_to_original <- function(x, y, preview_width, preview_height, ruler_roi) {
  scale_x <- roi_width(ruler_roi) / as.numeric(preview_width)
  scale_y <- roi_height(ruler_roi) / as.numeric(preview_height)
  list(
    x = ruler_roi$x_min + x * scale_x,
    y = ruler_roi$y_min + y * scale_y
  )
}

#' Map an original-image point into ruler-preview pixels.
orig_to_ruler_preview <- function(x, y, preview_width, preview_height, ruler_roi) {
  list(
    x = (as.numeric(x) - ruler_roi$x_min) * as.numeric(preview_width) / roi_width(ruler_roi),
    y = (as.numeric(y) - ruler_roi$y_min) * as.numeric(preview_height) / roi_height(ruler_roi)
  )
}

# Fixed physical length of the active manual reference. Not a user entry.
MANUAL_REFERENCE_MM <- 10

#' Horizontal two-point scale for a fixed 10 mm reference.
#'
#' pixels_per_mm = (x2 - x1) / 10, with x in original-image pixels.
#' The segment must already be horizontal. A different y is rejected:
#' the horizontal projection of a tilted ruler is not the mark spacing.
propose_horizontal_10mm_calibration <- function(
    x1, y1, x2, y2,
    image_width = NULL,
    image_height = NULL,
    ruler_roi = NULL,
    y_tolerance = 0.51
) {
  tilt_warning <- paste(
    "Scale uses the horizontal pixel distance only.",
    "A ruler that is not horizontal in the image is not corrected."
  )
  fail <- function(message, extra = character()) {
    empty_calibration(
      message = message,
      method = "manual",
      warnings = c(tilt_warning, extra)
    )
  }

  vals <- c(x1, y1, x2, y2)
  if (any(!is.finite(vals))) {
    return(fail("Manual calibration failed: nonfinite coordinates."))
  }
  if (is.finite(y_tolerance) && abs(as.numeric(y2) - as.numeric(y1)) > y_tolerance) {
    return(fail(
      "The reference must be horizontal. A tilted ruler is not corrected; align the ruler in the image before calibrating.",
      "Rejected a non-horizontal selection instead of using its horizontal projection."
    ))
  }
  y2 <- y1

  if (!is.null(image_width) && !is.null(image_height)) {
    w <- as.numeric(image_width)
    h <- as.numeric(image_height)
    outside <- !is.finite(w) || !is.finite(h) ||
      x1 < -1e-6 || x2 < -1e-6 || y1 < -1e-6 || y1 > h + 1e-6 ||
      x1 > w + 1e-6 || x2 > w + 1e-6
    if (isTRUE(outside)) {
      return(fail("Manual calibration failed: a point lies outside the image."))
    }
    snap <- function(v, maxv) min(max(0, v), maxv)
    x1 <- snap(x1, w)
    x2 <- snap(x2, w)
    y1 <- snap(y1, h)
    y2 <- y1
  }

  dx <- as.numeric(x2) - as.numeric(x1)
  if (!is.finite(dx) || dx == 0) {
    return(fail("Manual calibration failed: the two marks are the same point."))
  }
  if (dx < 0) {
    return(fail("Manual calibration failed: the second mark must be to the right of the first."))
  }

  ppm <- dx / MANUAL_REFERENCE_MM
  list(
    status = CALIBRATION_STATUS_PROPOSED,
    method = "manual",
    pixels_per_mm = as.numeric(ppm),
    tick_spacing_px = NA_real_,
    period_px = NA_real_,
    assumed_interval_mm = MANUAL_REFERENCE_MM,
    reference_mm = MANUAL_REFERENCE_MM,
    ruler_roi = ruler_roi,
    tick_positions_x = numeric(),
    major_positions_x = numeric(),
    observed_positions_x = numeric(),
    predicted_positions_x = numeric(),
    tick_lengths = numeric(),
    n_ticks = 0L,
    spacing_cv = NA_real_,
    graduation_band = NULL,
    quality = list(
      n_ticks = 0L,
      period_px = NA_real_,
      period_strength = NA_real_,
      n_supporting_intervals = 0L,
      spacing_consistency = NA_real_,
      major_interval_consistency = NA_real_,
      hierarchy_ratio = NA_real_,
      confidence = "manual",
      ambiguity = FALSE
    ),
    warnings = tilt_warning,
    message = sprintf(
      "Manual scale ready to confirm: %.3f px/mm (%.2f px over %g mm, horizontal).",
      ppm, dx, MANUAL_REFERENCE_MM
    ),
    confirmed = FALSE,
    manual_points = list(
      x1 = as.numeric(x1), y1 = as.numeric(y1),
      x2 = as.numeric(x2), y2 = as.numeric(y2),
      distance_mm = MANUAL_REFERENCE_MM,
      distance_px = as.numeric(dx)
    )
  )
}

#' Pixels per millimetre from the confirmed horizontal 10 mm calibration.
#'
#' Automatic and unconfirmed scales are ignored. This is the only scale
#' the measurement UI should pass into physical-unit conversion.
active_pixels_per_mm <- function(calibration, filename = NULL) {
  if (is.null(calibration) || !is.list(calibration)) {
    return(NA_real_)
  }
  if (!isTRUE(calibration$confirmed) || !identical(calibration$method, "manual")) {
    return(NA_real_)
  }
  ref <- calibration$reference_mm
  if (length(ref) != 1L || !isTRUE(is.finite(ref)) ||
        abs(as.numeric(ref) - MANUAL_REFERENCE_MM) > 1e-8) {
    return(NA_real_)
  }
  if (!is.null(filename) && !is.null(calibration$source_filename) &&
        !identical(as.character(calibration$source_filename), as.character(filename))) {
    return(NA_real_)
  }
  ppm <- calibration$pixels_per_mm
  pts <- calibration$manual_points
  if (length(ppm) != 1L || !isTRUE(is.finite(ppm)) || ppm <= 0 ||
        !is.list(pts) || length(pts$x1) != 1L || length(pts$x2) != 1L ||
        !isTRUE(is.finite(pts$x1)) || !isTRUE(is.finite(pts$x2))) {
    return(NA_real_)
  }
  if (isTRUE(is.finite(pts$y1)) && isTRUE(is.finite(pts$y2)) &&
        abs(as.numeric(pts$y2) - as.numeric(pts$y1)) > 0.51) {
    return(NA_real_)
  }
  expected <- (as.numeric(pts$x2) - as.numeric(pts$x1)) / MANUAL_REFERENCE_MM
  if (!isTRUE(is.finite(expected)) || expected <= 0 || abs(expected - as.numeric(ppm)) > 1e-6) {
    return(NA_real_)
  }
  as.numeric(ppm)
}

#' Debug rows for the active manual calibration. Omits automatic-only fields.
manual_calibration_debug_rows <- function(calibration) {
  if (is.null(calibration)) {
    return(data.frame(label = character(), value = character(), stringsAsFactors = FALSE))
  }
  pts <- calibration$manual_points
  pt_txt <- "—"
  dist_txt <- "—"
  if (is.list(pts) && length(pts$x1) == 1L && isTRUE(is.finite(pts$x1)) &&
        length(pts$x2) == 1L && isTRUE(is.finite(pts$x2))) {
    pt_txt <- sprintf("(%.1f, %.1f) → (%.1f, %.1f)", pts$x1, pts$y1, pts$x2, pts$y2)
    if (isTRUE(is.finite(pts$distance_px))) {
      dist_txt <- sprintf("%.2f px", pts$distance_px)
    }
  }
  ref <- calibration$reference_mm
  method <- calibration$method
  if (is.null(method) || length(method) != 1L || is.na(method)) {
    method <- "—"
  }
  image <- calibration$source_filename
  if (is.null(image) || length(image) != 1L || is.na(image)) {
    image <- "—"
  }
  data.frame(
    label = c("Image", "Method", "Status", "Confirmed", "Pixels per mm", "Reference", "Points", "Horizontal distance"),
    value = c(
      as.character(image),
      as.character(method),
      as.character(calibration$status),
      if (isTRUE(calibration$confirmed)) "yes" else "no",
      if (length(calibration$pixels_per_mm) == 1L && isTRUE(is.finite(calibration$pixels_per_mm))) {
        sprintf("%.3f px/mm", calibration$pixels_per_mm)
      } else {
        "—"
      },
      if (length(ref) == 1L && isTRUE(is.finite(ref))) sprintf("%g mm", ref) else "—",
      pt_txt,
      dist_txt
    ),
    stringsAsFactors = FALSE
  )
}

RULER_PREVIEW_MAX_WIDTH <- 1000L

#' Build a downscaled ruler-region preview from an original-resolution image.
create_ruler_preview <- function(img, ruler_roi, max_width = RULER_PREVIEW_MAX_WIDTH) {
  crop <- crop_roi_magick(img, ruler_roi)
  create_roi_preview(crop, max_width = max_width)
}

