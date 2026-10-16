# Seed segmentation and detection for AlfalfaCV Phase 1B.
#
# Algorithm (conventional CV, no ML, no fixed seed hue):
# 1. Estimate a dark background color from the darkest ROI border pixels.
# 2. Compute per-pixel luminance and Euclidean RGB distance from that background.
# 3. Threshold with a sensitivity-controlled cutoff (background-aware, not yellow-only).
# 4. Light morphological opening to remove speckles without heavy boundary erosion.
# 5. Connected-component labeling (mmand).
# 6. Filter by min/max area and minimum mean contrast vs background.
# 7. Assign deterministic IDs: top-to-bottom, then left-to-right by centroid.
#
# Default parameters were tuned toward conservative detection on dark-background
# USDA-like imagery (manual reference: sensitivity≈0.05, min_area≈705 on a
# large ROI, max_area_frac≈0.24). min_area is ROI-aware rather than a fixed 705.
#
# Limitations:
# - Assumes a relatively dark, uniform background inside the ROI.
# - Bright non-seed objects inside the ROI may still become candidates.
# - Soft seed edges and touching seeds can merge or split components.
# - Not a scientifically validated seed detector.

# Conservative defaults (researcher-validated direction; ROI-aware min area).
DEFAULT_DETECTION_PARAMS <- list(
  sensitivity = 0.05,
  min_area = NA_integer_,       # resolved via suggest_min_area() when NA
  max_area_frac = 0.24,
  max_candidates = 200L,
  min_contrast = 0.08           # mean luminance lift vs estimated background
)

#' Suggest a minimum component area from ROI dimensions.
#'
#' Scales roughly with ROI area so small fixtures and large USDA images
#' share similar relative filtering. Anchored near ~705 px for an ~18 MP ROI.
#'
#' @param roi_w,roi_h ROI width/height in pixels
#' @return Integer minimum area (inclusive)
suggest_min_area <- function(roi_w, roi_h) {
  area <- as.numeric(roi_w) * as.numeric(roi_h)
  if (!is.finite(area) || area <= 0) {
    return(50L)
  }
  # 705 / 18e6 ≈ 3.9e-5; use 4e-5 with bounds for tiny/huge ROIs.
  as.integer(max(50L, min(5000L, round(4e-5 * area))))
}

#' Resolve detection parameters, filling adaptive defaults.
#'
#' @param sensitivity,min_area,max_area_frac,max_candidates,min_contrast Params
#' @param roi_w,roi_h Optional ROI size for adaptive min_area
make_detection_params <- function(
    sensitivity = DEFAULT_DETECTION_PARAMS$sensitivity,
    min_area = DEFAULT_DETECTION_PARAMS$min_area,
    max_area_frac = DEFAULT_DETECTION_PARAMS$max_area_frac,
    max_candidates = DEFAULT_DETECTION_PARAMS$max_candidates,
    min_contrast = DEFAULT_DETECTION_PARAMS$min_contrast,
    roi_w = NULL,
    roi_h = NULL
) {
  resolved_min <- if (is.null(min_area) || length(min_area) != 1L || is.na(min_area)) {
    if (!is.null(roi_w) && !is.null(roi_h)) {
      suggest_min_area(roi_w, roi_h)
    } else {
      200L
    }
  } else {
    as.integer(min_area)
  }

  list(
    sensitivity = as.numeric(sensitivity),
    min_area = resolved_min,
    max_area_frac = as.numeric(max_area_frac),
    max_candidates = as.integer(max_candidates),
    min_contrast = as.numeric(min_contrast)
  )
}

#' Empty detection result structure.
empty_detection_result <- function(
    status = "empty",
    message = "No detection has been run.",
    warnings = character()
) {
  list(
    status = status,
    message = message,
    warnings = warnings,
    count = 0L,
    detections = list(),
    binary_mask = NULL,
    roi = NULL,
    params = NULL
  )
}

#' Estimate dark background RGB from the darkest border pixels of an ROI array.
estimate_dark_background <- function(rgb) {
  nr <- dim(rgb)[1]
  nc <- dim(rgb)[2]
  border <- max(1L, as.integer(round(0.02 * min(nr, nc))))

  top <- rgb[seq_len(min(border, nr)), , , drop = FALSE]
  bottom <- rgb[seq.int(max(1L, nr - border + 1L), nr), , , drop = FALSE]
  left <- rgb[, seq_len(min(border, nc)), , drop = FALSE]
  right <- rgb[, seq.int(max(1L, nc - border + 1L), nc), , drop = FALSE]

  samples <- rbind(
    cbind(as.numeric(top[, , 1]), as.numeric(top[, , 2]), as.numeric(top[, , 3])),
    cbind(as.numeric(bottom[, , 1]), as.numeric(bottom[, , 2]), as.numeric(bottom[, , 3])),
    cbind(as.numeric(left[, , 1]), as.numeric(left[, , 2]), as.numeric(left[, , 3])),
    cbind(as.numeric(right[, , 1]), as.numeric(right[, , 2]), as.numeric(right[, , 3]))
  )

  lum <- 0.299 * samples[, 1] + 0.587 * samples[, 2] + 0.114 * samples[, 3]
  # Use a tighter dark reference (lowest 15%) to avoid bright border contamination.
  cutoff <- stats::quantile(lum, probs = 0.15, names = FALSE, type = 7)
  keep <- lum <= cutoff
  if (!any(keep)) {
    keep <- rep(TRUE, length(lum))
  }
  bg <- colMeans(samples[keep, , drop = FALSE])
  list(
    r = bg[[1]],
    g = bg[[2]],
    b = bg[[3]],
    luminance = 0.299 * bg[[1]] + 0.587 * bg[[2]] + 0.114 * bg[[3]]
  )
}

#' Map sensitivity in [0, 1] to a distance threshold.
#'
#' Higher sensitivity => lower threshold => more foreground pixels.
#' At sensitivity≈0.05 the threshold is ~0.21 (conservative USDA-like setting).
sensitivity_to_threshold <- function(sensitivity) {
  s <- min(1, max(0, as.numeric(sensitivity)))
  # Conservative high end raised slightly so low sensitivity rejects weak noise.
  max_t <- 0.24
  min_t <- 0.05
  max_t - s * (max_t - min_t)
}

#' Segment foreground against an estimated dark background.
#'
#' @param rgb ROI RGB array [rows, cols, 3] in [0, 1]
#' @param sensitivity Detection sensitivity in [0, 1]
#' @return List with mask (logical) and background estimate
segment_foreground <- function(rgb, sensitivity = DEFAULT_DETECTION_PARAMS$sensitivity) {
  bg <- estimate_dark_background(rgb)
  thr <- sensitivity_to_threshold(sensitivity)

  r <- rgb[, , 1]
  g <- rgb[, , 2]
  b <- rgb[, , 3]
  dist <- sqrt((r - bg$r)^2 + (g - bg$g)^2 + (b - bg$b)^2)
  lum <- 0.299 * r + 0.587 * g + 0.114 * b

  # Require BOTH color distance and luminance lift. The previous OR rule was
  # overly permissive on textured dark backgrounds (weak noise passed either gate).
  mask <- (dist >= thr) & (lum >= (bg$luminance + thr * 0.75))

  mask_int <- matrix(as.integer(mask), nrow = nrow(mask), ncol = ncol(mask))
  opened <- tryCatch(
    {
      kern <- mmand::shapeKernel(c(3, 3), type = "box")
      out <- mmand::opening(mask_int, kern)
      out_l <- matrix(as.numeric(out) > 0, nrow = nrow(mask), ncol = ncol(mask))
      if (!any(out_l) && any(mask)) {
        mask
      } else {
        out_l
      }
    },
    error = function(e) {
      mask
    }
  )

  list(mask = opened, background = bg, threshold = thr)
}

#' Label connected components on a logical mask.
label_components <- function(mask) {
  mask_int <- matrix(as.integer(mask), nrow = nrow(mask), ncol = ncol(mask))
  kern <- mmand::shapeKernel(c(3, 3), type = "box")
  labeled <- mmand::components(mask_int, kern)
  labeled[is.na(labeled)] <- 0
  storage.mode(labeled) <- "integer"
  labeled
}

#' Build detection records from a labeled component image.
components_to_detections <- function(
    labeled,
    rgb,
    background,
    params = make_detection_params(),
    roi = NULL
) {
  labels <- sort(unique(as.integer(labeled)))
  labels <- labels[labels > 0L]
  warnings <- character()

  if (length(labels) == 0L) {
    return(empty_detection_result(
      status = "no_detections",
      message = "No seed candidates detected.",
      warnings = warnings
    ))
  }

  roi_area <- length(labeled)
  max_area <- as.integer(max(
    params$min_area,
    floor(params$max_area_frac * roi_area)
  ))

  lum <- 0.299 * rgb[, , 1] + 0.587 * rgb[, , 2] + 0.114 * rgb[, , 3]
  bg_lum <- background$luminance

  records <- list()
  for (lab in labels) {
    idx <- which(labeled == lab, arr.ind = TRUE)
    area <- nrow(idx)
    # Inclusive minimum: area >= min_area. A component of exactly min_area is kept;
    # area == min_area - 1 is rejected (explains 704 vs 705 cliff behavior).
    if (area < params$min_area || area > max_area) {
      next
    }

    rows <- idx[, 1]
    cols <- idx[, 2]
    mean_lum <- mean(lum[idx])
    contrast <- mean_lum - bg_lum
    if (contrast < params$min_contrast) {
      next
    }

    x_min <- as.integer(min(cols) - 1L)
    x_max <- as.integer(max(cols))
    y_min <- as.integer(min(rows) - 1L)
    y_max <- as.integer(max(rows))
    cx <- mean(cols - 0.5)
    cy <- mean(rows - 0.5)

    mask_crop <- labeled[(y_min + 1L):y_max, (x_min + 1L):x_max, drop = FALSE] == lab

    abs_bbox <- list(x_min = x_min, x_max = x_max, y_min = y_min, y_max = y_max)
    abs_centroid <- list(x = cx, y = cy)
    if (!is.null(roi)) {
      abs_bbox <- list(
        x_min = roi$x_min + x_min,
        x_max = roi$x_min + x_max,
        y_min = roi$y_min + y_min,
        y_max = roi$y_min + y_max
      )
      abs_centroid <- list(x = roi$x_min + cx, y = roi$y_min + cy)
    }

    records[[length(records) + 1L]] <- list(
      id = NA_integer_,
      bbox_roi = list(x_min = x_min, x_max = x_max, y_min = y_min, y_max = y_max),
      bbox = abs_bbox,
      centroid_roi = list(x = cx, y = cy),
      centroid = abs_centroid,
      area_px = as.integer(area),
      contrast = as.numeric(contrast),
      mask = mask_crop
    )
  }

  if (length(records) == 0L) {
    return(empty_detection_result(
      status = "no_detections",
      message = "No seed candidates detected after filtering.",
      warnings = c(warnings, "All components were removed by area/contrast filters.")
    ))
  }

  order_idx <- order(
    vapply(records, function(r) r$centroid_roi$y, numeric(1)),
    vapply(records, function(r) r$centroid_roi$x, numeric(1))
  )
  records <- records[order_idx]
  for (i in seq_along(records)) {
    records[[i]]$id <- as.integer(i)
  }

  if (length(records) > params$max_candidates) {
    warnings <- c(
      warnings,
      paste0(
        "Detected ", length(records),
        " candidates; truncating to ", params$max_candidates, "."
      )
    )
    records <- records[seq_len(params$max_candidates)]
    for (i in seq_along(records)) {
      records[[i]]$id <- as.integer(i)
    }
  }

  count <- length(records)
  list(
    status = "ok",
    message = paste0(count, " seed candidate", if (count == 1L) "" else "s", " detected"),
    warnings = warnings,
    count = as.integer(count),
    detections = records,
    binary_mask = labeled > 0L,
    roi = roi,
    params = params
  )
}

#' Run seed detection on an ROI RGB array.
detect_seeds_in_rgb <- function(
    rgb,
    params = make_detection_params(),
    roi = NULL,
    progress = NULL
) {
  report <- function(msg, value) {
    if (is.function(progress)) {
      progress(msg, value)
    }
  }

  if (is.null(rgb) || length(dim(rgb)) != 3L || dim(rgb)[3] < 3L) {
    return(empty_detection_result(
      status = "error",
      message = "Segmentation failed: ROI image data is invalid."
    ))
  }
  if (dim(rgb)[1] < 2L || dim(rgb)[2] < 2L) {
    return(empty_detection_result(
      status = "error",
      message = "Segmentation failed: ROI is too small."
    ))
  }

  # Fill adaptive min_area from RGB dims when still unresolved.
  if (is.null(params$min_area) || is.na(params$min_area)) {
    params$min_area <- suggest_min_area(dim(rgb)[2], dim(rgb)[1])
  }

  report("Segmenting foreground...", 0.35)
  seg <- tryCatch(
    segment_foreground(rgb, sensitivity = params$sensitivity),
    error = function(e) NULL
  )
  if (is.null(seg)) {
    return(empty_detection_result(
      status = "error",
      message = "Segmentation failed while estimating foreground."
    ))
  }

  if (!any(seg$mask)) {
    return(empty_detection_result(
      status = "no_detections",
      message = "No seed candidates detected.",
      warnings = "Foreground mask was empty."
    ))
  }

  report("Identifying candidates...", 0.6)
  labeled <- tryCatch(
    label_components(seg$mask),
    error = function(e) NULL
  )
  if (is.null(labeled)) {
    return(empty_detection_result(
      status = "error",
      message = "Segmentation failed while labeling components."
    ))
  }

  report("Filtering detections...", 0.8)
  result <- components_to_detections(
    labeled,
    rgb = rgb,
    background = seg$background,
    params = params,
    roi = roi
  )
  result$binary_mask <- seg$mask
  result
}

#' Run seed detection on a magick ROI crop.
detect_seeds_in_magick <- function(
    roi_img,
    params = make_detection_params(),
    roi = NULL,
    progress = NULL
) {
  report <- function(msg, value) {
    if (is.function(progress)) {
      progress(msg, value)
    }
  }

  report("Preparing ROI pixels...", 0.2)
  rgb <- tryCatch(
    magick_to_rgb_array(roi_img),
    error = function(e) NULL
  )
  if (is.null(rgb)) {
    return(empty_detection_result(
      status = "error",
      message = "Failed to prepare ROI pixels for segmentation."
    ))
  }

  dims <- magick_dimensions(roi_img)
  params <- make_detection_params(
    sensitivity = params$sensitivity,
    min_area = params$min_area,
    max_area_frac = params$max_area_frac,
    max_candidates = params$max_candidates,
    min_contrast = params$min_contrast,
    roi_w = dims$width,
    roi_h = dims$height
  )

  detect_seeds_in_rgb(rgb, params = params, roi = roi, progress = progress)
}
