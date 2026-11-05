# ROI helpers for AlfalfaCV.
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

# Broader seed *search* band used for content-aware refinement.
# Intentionally wider than the previous fixed right=0.65 so both seed
# columns can be found; white reference circles typically lie farther right.
SEED_SEARCH_ROI_NORMALIZED <- list(
  left = 0.48,
  right = 0.80,
  top = 0.05,
  bottom = 0.86
)

# Conservative normalized fallback when content-aware refinement fails.
# Wider than the old right=0.65 strip so both seed columns are usually kept;
# still left of the typical white reference-circle column. The colour palette
# on USDA layouts can reach past x = 0.52, so the fallback starts at 0.58.
DEFAULT_SEED_ROI_NORMALIZED <- list(
  left = 0.58,
  right = 0.72,
  top = 0.06,
  bottom = 0.85
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
#' Preview and original share the same top-left origin. Brush coordinates are
#' interpreted in preview pixel space. When the preview is displayed with
#' asp=1, brush data coordinates still match the preview pixel frame.
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

#' Map a display fraction of the preview image into preview pixels.
#'
#' Fractions are relative to the preview raster, not to CSS pixels.
display_fraction_to_preview <- function(nx, ny, preview_width, preview_height) {
  list(
    x = as.numeric(nx) * as.numeric(preview_width),
    y = as.numeric(ny) * as.numeric(preview_height)
  )
}

#' Map a normalized preview rectangle to an original-image ROI.
#'
#' CSS pixel size is not an input. Equal fractions on two preview sizes of
#' the same image produce the same original-image ROI.
display_fraction_rect_to_roi <- function(
    nx_min, ny_min, nx_max, ny_max,
    preview_width, preview_height,
    image_width, image_height
) {
  p1 <- display_fraction_to_preview(nx_min, ny_min, preview_width, preview_height)
  p2 <- display_fraction_to_preview(nx_max, ny_max, preview_width, preview_height)
  brush_to_roi(
    list(xmin = p1$x, xmax = p2$x, ymin = p1$y, ymax = p2$y),
    preview_width, preview_height, image_width, image_height
  )
}

#' Whether a preview-pixel point lies in the ROI (half-open).
preview_point_inside_roi <- function(
    x, y, roi, preview_width, preview_height, image_width, image_height
) {
  if (is.null(roi) || length(x) != 1L || length(y) != 1L ||
        !isTRUE(is.finite(x)) || !isTRUE(is.finite(y))) {
    return(FALSE)
  }
  pr <- roi_to_preview(roi, preview_width, preview_height, image_width, image_height)
  x >= pr$x_min && x < pr$x_max && y >= pr$y_min && y < pr$y_max
}

#' CSS percentage box for an ROI drawn over a preview that fills its frame.
roi_box_style <- function(roi, preview_width, preview_height, image_width, image_height) {
  pr <- roi_to_preview(roi, preview_width, preview_height, image_width, image_height)
  pw <- as.numeric(preview_width)
  ph <- as.numeric(preview_height)
  sprintf(
    "left:%.4f%%;top:%.4f%%;width:%.4f%%;height:%.4f%%;",
    100 * pr$x_min / pw,
    100 * pr$y_min / ph,
    100 * (pr$x_max - pr$x_min) / pw,
    100 * (pr$y_max - pr$y_min) / ph
  )
}

#' Stable tag so measurement tables can detect a replaced ROI.
roi_state_tag <- function(roi) {
  if (is.null(roi)) {
    return("")
  }
  paste(roi$x_min, roi$x_max, roi$y_min, roi$y_max, sep = ",")
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

#' Expand an ROI by fractional margins of its own size, clamped to the image.
expand_roi_margin <- function(roi, image_width, image_height, margin_frac = 0.08) {
  mw <- max(2L, as.integer(round(roi_width(roi) * margin_frac)))
  mh <- max(2L, as.integer(round(roi_height(roi) * margin_frac)))
  clamp_roi(
    make_roi(
      roi$x_min - mw,
      roi$x_max + mw,
      roi$y_min - mh,
      roi$y_max + mh
    ),
    image_width,
    image_height
  )
}

#' Classify connected components in a search-band RGB crop as seed-like or not.
#'
#' Uses spatial/geometric cues only (no hue assumption):
#' - Reject large, bright, near-circular blobs (white reference circles).
#' - Keep compact mid-sized non-background blobs as seed candidates.
#'
#' @param rgb Search-band RGB array [rows, cols, 3] in [0, 1].
#' @return List with seed_components (data.frame) and method notes.
classify_seed_search_components <- function(rgb) {
  if (is.null(rgb) || length(dim(rgb)) != 3L) {
    return(list(seed_components = data.frame(), n_total = 0L, n_rejected_circles = 0L))
  }
  nr <- dim(rgb)[[1]]
  nc <- dim(rgb)[[2]]
  search_area <- as.numeric(nr) * as.numeric(nc)
  if (search_area < 100) {
    return(list(seed_components = data.frame(), n_total = 0L, n_rejected_circles = 0L))
  }

  lum <- 0.299 * rgb[, , 1] + 0.587 * rgb[, , 2] + 0.114 * rgb[, , 3]
  # Dark-background USDA layout: anything clearly above the dark floor.
  dark_floor <- stats::quantile(lum, probs = 0.20, names = FALSE, type = 7)
  thr <- dark_floor + max(0.08, 0.25 * (1 - dark_floor))
  fg <- lum >= thr

  if (!any(fg)) {
    return(list(seed_components = data.frame(), n_total = 0L, n_rejected_circles = 0L))
  }

  labeled <- tryCatch(
    {
      mask_int <- matrix(as.integer(fg), nrow = nr, ncol = nc)
      kern <- mmand::shapeKernel(c(3, 3), type = "box")
      mmand::components(mask_int, kern)
    },
    error = function(e) NULL
  )
  if (is.null(labeled) || !any(labeled > 0, na.rm = TRUE)) {
    return(list(seed_components = data.frame(), n_total = 0L, n_rejected_circles = 0L))
  }

  labs <- sort(unique(as.integer(labeled[labeled > 0])))
  min_area <- max(20L, as.integer(round(1.5e-4 * search_area)))
  max_seed_area <- as.integer(round(0.035 * search_area))
  circle_area_min <- as.integer(round(0.012 * search_area))

  rows_out <- list()
  refs_out <- list()
  n_circles <- 0L
  for (lab in labs) {
    idx <- which(labeled == lab, arr.ind = TRUE)
    area <- nrow(idx)
    if (area < min_area) next

    ys <- idx[, 1]
    xs <- idx[, 2]
    x_min <- min(xs) - 1L
    x_max <- max(xs)
    y_min <- min(ys) - 1L
    y_max <- max(ys)
    bw <- x_max - x_min
    bh <- y_max - y_min
    if (bw < 1L || bh < 1L) next

    aspect <- max(bw, bh) / max(1, min(bw, bh))
    extent <- area / as.numeric(bw * bh)
    # Index by arr.ind — avoid logical NA from labeled==lab on NA background.
    mean_lum <- mean(lum[cbind(ys, xs)])
    cx <- mean(xs) - 0.5
    cy <- mean(ys) - 0.5
    # Equivalent-circle fill for near-white round markers.
    eq_r <- sqrt(area / pi)
    circle_fill <- area / (pi * max(bw, bh)^2 / 4)

    is_ref_circle <- isTRUE(
      area >= circle_area_min &&
        is.finite(mean_lum) && mean_lum >= 0.72 &&
        is.finite(aspect) && aspect <= 1.45 &&
        is.finite(extent) && extent >= 0.55 &&
        is.finite(circle_fill) && circle_fill >= 0.45
    )
    if (is_ref_circle) {
      n_circles <- n_circles + 1L
      next
    }

    # Colour-palette swatches: either truncated by the band's left edge (the
    # palette continues outside the search band) or uniform axis-aligned
    # rectangles. Seeds are elliptical (extent ~0.79) and lie inside the band.
    lum_sd <- stats::sd(lum[cbind(ys, xs)])
    touches_left <- x_min <= 0L
    is_rect_patch <- isTRUE(
      is.finite(extent) && extent >= 0.90 &&
        is.finite(aspect) && aspect <= 2.5 &&
        (!is.finite(lum_sd) || lum_sd <= 0.06)
    )
    if (touches_left || is_rect_patch) {
      refs_out[[length(refs_out) + 1L]] <- data.frame(
        x_min = x_min, x_max = x_max, y_min = y_min, y_max = y_max,
        reason = if (touches_left) "truncated at search-band left edge" else "uniform rectangular patch",
        stringsAsFactors = FALSE
      )
      next
    }

    # Seed-like: mid area, not huge, not extremely elongated, not pure white sheet.
    is_seed <- isTRUE(
      area <= max_seed_area &&
        is.finite(aspect) && aspect <= 3.5 &&
        is.finite(mean_lum) && mean_lum < 0.92 &&
        is.finite(extent) && extent >= 0.25
    )
    if (!is_seed) next

    rows_out[[length(rows_out) + 1L]] <- data.frame(
      area = area,
      x_min = x_min, x_max = x_max,
      y_min = y_min, y_max = y_max,
      cx = cx, cy = cy,
      mean_lum = mean_lum,
      aspect = aspect,
      stringsAsFactors = FALSE
    )
  }

  seeds <- if (length(rows_out) == 0L) {
    data.frame()
  } else {
    do.call(rbind, rows_out)
  }

  list(
    seed_components = seeds,
    reference_components = if (length(refs_out) == 0L) data.frame() else do.call(rbind, refs_out),
    n_total = length(labs),
    n_rejected_circles = n_circles
  )
}

#' Refine seed ROI from a downscaled full-image RGB preview.
#'
#' Operates only on the search band of the preview (no full-resolution decode).
#' Returns NULL when refinement is unreliable (caller should use fallback).
#'
#' @param preview_rgb Downscaled full-image RGB [rows, cols, 3].
#' @param preview_width,preview_height Preview dimensions.
#' @param image_width,image_height Original dimensions.
#' @param search_norm Normalized search band.
#' @param margin_frac Margin around the union of seed-like components.
#' @return Clamped original-image ROI or NULL.
refine_seed_roi_from_preview <- function(
    preview_rgb,
    preview_width,
    preview_height,
    image_width,
    image_height,
    search_norm = SEED_SEARCH_ROI_NORMALIZED,
    margin_frac = 0.10
) {
  pw <- as.integer(preview_width)
  ph <- as.integer(preview_height)
  search_prev <- normalized_to_roi(search_norm, pw, ph)
  idx <- roi_to_array_indices(search_prev)
  if (is.null(idx)) {
    return(NULL)
  }

  crop <- preview_rgb[idx$row_min:idx$row_max, idx$col_min:idx$col_max, , drop = FALSE]
  classified <- classify_seed_search_components(crop)
  seeds <- classified$seed_components
  if (is.null(seeds) || nrow(seeds) < 1L) {
    return(NULL)
  }

  # Require some spatial spread so a single spurious blob does not drive the ROI.
  if (nrow(seeds) == 1L) {
    # Single detection: only accept if it sits in the expected mid band.
    if (seeds$cx[[1]] < 0.15 * ncol(crop) || seeds$cx[[1]] > 0.90 * ncol(crop)) {
      return(NULL)
    }
  }

  # Drop extreme right-side outliers that may be residual circle fragments.
  if (nrow(seeds) >= 3L) {
    x_med <- stats::median(seeds$cx)
    x_mad <- stats::mad(seeds$cx, constant = 1)
    if (is.finite(x_mad) && x_mad > 1) {
      keep <- seeds$cx <= (x_med + 4 * max(x_mad, 1))
      # Prefer keeping left/center mass; also drop if far right of 85% of search.
      keep <- keep & (seeds$cx <= 0.92 * ncol(crop))
      if (any(keep)) {
        seeds <- seeds[keep, , drop = FALSE]
      }
    }
  }

  if (nrow(seeds) < 1L) {
    return(NULL)
  }

  # Union bbox in search-crop coordinates → preview → original.
  union_prev <- make_roi(
    search_prev$x_min + min(seeds$x_min),
    search_prev$x_min + max(seeds$x_max),
    search_prev$y_min + min(seeds$y_min),
    search_prev$y_min + max(seeds$y_max)
  )

  scale_x <- as.numeric(image_width) / as.numeric(pw)
  scale_y <- as.numeric(image_height) / as.numeric(ph)
  roi_orig <- make_roi(
    floor(union_prev$x_min * scale_x),
    ceiling(union_prev$x_max * scale_x),
    floor(union_prev$y_min * scale_y),
    ceiling(union_prev$y_max * scale_y)
  )
  roi_orig <- expand_roi_margin(roi_orig, image_width, image_height, margin_frac = margin_frac)

  # Never expand past the search band right edge (protects against circles).
  search_orig <- normalized_to_roi(search_norm, image_width, image_height)
  roi_orig$x_max <- min(roi_orig$x_max, search_orig$x_max)
  roi_orig$x_min <- max(roi_orig$x_min, search_orig$x_min)

  # Margin must not re-enter a rejected reference object (palette) on the left.
  refs <- classified$reference_components
  if (!is.null(refs) && nrow(refs) > 0L) {
    left_refs <- refs[refs$x_max <= min(seeds$x_min), , drop = FALSE]
    if (nrow(left_refs) > 0L) {
      ref_edge <- ceiling((search_prev$x_min + max(left_refs$x_max)) * scale_x)
      seed_edge <- floor((search_prev$x_min + min(seeds$x_min)) * scale_x)
      # Leave a small gap only when there is room between palette and seeds.
      gap <- max(0L, as.integer(floor((seed_edge - ref_edge) / 2)))
      roi_orig$x_min <- max(roi_orig$x_min, min(ref_edge + gap, seed_edge))
    }
  }
  roi_orig$y_min <- max(roi_orig$y_min, search_orig$y_min)
  roi_orig$y_max <- min(roi_orig$y_max, search_orig$y_max)
  roi_orig <- clamp_roi(roi_orig, image_width, image_height)

  check <- validate_roi(roi_orig, image_width, image_height)
  if (!isTRUE(check$ok)) {
    return(NULL)
  }

  # Sanity: refined ROI should cover a meaningful fraction of the search width
  # when multiple seeds exist (both columns).
  if (nrow(seeds) >= 2L) {
    span_frac <- roi_width(roi_orig) / max(1, roi_width(search_orig))
    if (span_frac < 0.12) {
      return(NULL)
    }
  }

  roi_orig
}

#' Initialize the default seed ROI for an image.
#'
#' When a downscaled preview RGB is supplied, attempts lightweight content-aware
#' refinement inside SEED_SEARCH_ROI_NORMALIZED. Falls back to
#' DEFAULT_SEED_ROI_NORMALIZED when refinement fails.
#'
#' @param image_width,image_height Original dimensions.
#' @param preview_rgb Optional downscaled full-image RGB for refinement.
#' @param preview_width,preview_height Preview dimensions matching preview_rgb.
#' @param norm Fallback normalized ROI (default DEFAULT_SEED_ROI_NORMALIZED).
initialize_default_roi <- function(
    image_width,
    image_height,
    preview_rgb = NULL,
    preview_width = NULL,
    preview_height = NULL,
    norm = DEFAULT_SEED_ROI_NORMALIZED
) {
  fallback <- normalized_to_roi(norm, image_width, image_height)
  if (is.null(preview_rgb) || is.null(preview_width) || is.null(preview_height)) {
    return(fallback)
  }

  refined <- tryCatch(
    refine_seed_roi_from_preview(
      preview_rgb = preview_rgb,
      preview_width = preview_width,
      preview_height = preview_height,
      image_width = image_width,
      image_height = image_height
    ),
    error = function(e) NULL
  )
  if (is.null(refined)) {
    return(fallback)
  }
  refined
}
