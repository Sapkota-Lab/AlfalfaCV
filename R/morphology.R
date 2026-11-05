# Quantitative seed morphology for AlfalfaCV Phase 1C.
#
# Measurement layers:
# A. Pixel-based geometry (mask-authoritative)
# B. Calibration-dependent physical units
# C. Dimensionless shape descriptors
#
# Definitions (documented):
# - area_px: count of foreground pixels in the seed mask.
# - perimeter_px: Euclidean length of the ordered outer contour (8-connected
#   boundary follow). Degenerate/single-pixel masks return NA.
# - bbox width/height: half-open axis-aligned bbox spans in pixels.
# - centroid: mean of foreground pixel centers (x = col-0.5, y = row-0.5)
#   in the coordinate frame of the supplied mask (ROI-local unless noted).
# - convex_hull_area_px: area of the polygon from grDevices::chull on
#   foreground pixel corners in half-open grid coords (shoelace), so the
#   hull contains the digital object and solidity stays <= 1.
# - major/minor_axis_px: ImageJ-style second-moment ellipse axes:
#     major = 4 * sqrt(lambda_max), minor = 4 * sqrt(lambda_min)
#   where lambda are eigenvalues of the covariance of pixel center coords.
# - equivalent_diameter_px = sqrt(4 * area_px / pi)
# - circularity = 4 * pi * area / perimeter^2
# - solidity = area / convex_hull_area
# - extent = area / (bbox_width * bbox_height)
# - aspect_ratio = major / minor
# - eccentricity = sqrt(1 - (minor/major)^2)
# - convexity = convex_hull_perimeter / seed_perimeter
#
# Physical conversion (confirmed calibration s = px/mm):
#   length_mm = length_px / s
#   area_mm2  = area_px / (s * s)
# Dimensionless descriptors are unchanged by calibration.

#' Shoelace polygon area for an ordered ring of points (x, y).
polygon_area <- function(x, y) {
  n <- length(x)
  if (n < 3L) {
    return(NA_real_)
  }
  x2 <- c(x, x[[1]])
  y2 <- c(y, y[[1]])
  abs(sum(x2[1:n] * y2[2:(n + 1)] - x2[2:(n + 1)] * y2[1:n]) / 2)
}

#' Shoelace polygon perimeter for an ordered ring.
polygon_perimeter <- function(x, y) {
  n <- length(x)
  if (n < 2L) {
    return(NA_real_)
  }
  x2 <- c(x, x[[1]])
  y2 <- c(y, y[[1]])
  sum(sqrt((x2[2:(n + 1)] - x2[1:n])^2 + (y2[2:(n + 1)] - y2[1:n])^2))
}

#' Extract foreground pixel centers (x right, y down) from a logical mask.
mask_pixel_centers <- function(mask) {
  idx <- which(mask, arr.ind = TRUE)
  if (nrow(idx) == 0L) {
    return(data.frame(x = numeric(), y = numeric()))
  }
  # arr.ind: [,1]=row (y), [,2]=col (x); centers at -0.5
  data.frame(
    x = idx[, 2] - 0.5,
    y = idx[, 1] - 0.5
  )
}

#' Identify boundary foreground pixels (FG with >=1 BG 4-neighbor).
mask_boundary_centers <- function(mask) {
  if (!any(mask)) {
    return(data.frame(x = numeric(), y = numeric()))
  }
  nr <- nrow(mask)
  nc <- ncol(mask)
  pad <- matrix(FALSE, nr + 2L, nc + 2L)
  pad[2:(nr + 1), 2:(nc + 1)] <- mask
  boundary <- matrix(FALSE, nr, nc)
  for (r in seq_len(nr)) {
    for (c in seq_len(nc)) {
      if (!mask[r, c]) next
      pr <- r + 1L
      pc <- c + 1L
      if (!pad[pr - 1L, pc] || !pad[pr + 1L, pc] ||
            !pad[pr, pc - 1L] || !pad[pr, pc + 1L]) {
        boundary[r, c] <- TRUE
      }
    }
  }
  mask_pixel_centers(boundary)
}

#' Approximate outer perimeter from radially sorted boundary pixel centers.
#'
#' Documented estimator: collect 4-connected boundary pixels, sort by angle
#' about the centroid, and sum Euclidean edge lengths of the closed polygon.
#' Suitable for compact seed-like blobs; highly concave shapes are approximate.
mask_contour_points <- function(mask) {
  pts <- mask_boundary_centers(mask)
  if (nrow(pts) < 2L) {
    return(pts)
  }
  cx <- mean(pts$x)
  cy <- mean(pts$y)
  ang <- atan2(pts$y - cy, pts$x - cx)
  pts[order(ang), , drop = FALSE]
}


#' Compute pixel-based geometry for one logical mask.
measure_mask_geometry <- function(mask) {
  na_geom <- list(
    area_px = NA_real_,
    perimeter_px = NA_real_,
    bbox_width_px = NA_real_,
    bbox_height_px = NA_real_,
    centroid_x = NA_real_,
    centroid_y = NA_real_,
    convex_hull_area_px = NA_real_,
    convex_hull_perimeter_px = NA_real_,
    major_axis_px = NA_real_,
    minor_axis_px = NA_real_,
    equivalent_diameter_px = NA_real_,
    valid = FALSE
  )

  if (is.null(mask) || !is.logical(mask) || length(dim(mask)) != 2L || !any(mask)) {
    return(na_geom)
  }

  pts <- mask_pixel_centers(mask)
  n <- nrow(pts)
  area_px <- as.numeric(n)

  rows <- which(rowSums(mask) > 0)
  cols <- which(colSums(mask) > 0)
  # Half-open bbox in mask coordinates.
  y_min <- min(rows) - 1L
  y_max <- max(rows)
  x_min <- min(cols) - 1L
  x_max <- max(cols)
  bbox_w <- as.numeric(x_max - x_min)
  bbox_h <- as.numeric(y_max - y_min)

  cx <- mean(pts$x)
  cy <- mean(pts$y)

  # Contour perimeter.
  contour <- tryCatch(mask_contour_points(mask), error = function(e) data.frame(x = numeric(), y = numeric()))
  perimeter_px <- if (nrow(contour) >= 2L) {
    polygon_perimeter(contour$x, contour$y)
  } else if (n == 1L) {
    NA_real_ # single pixel: Euclidean perimeter undefined for our contour method
  } else {
    NA_real_
  }

  # Convex hull of foreground pixel corners (half-open grid), so hull
  # contains the digital object and solidity remains <= 1.
  ch_area <- NA_real_
  ch_perim <- NA_real_
  idx <- which(mask, arr.ind = TRUE)
  if (n >= 1L) {
    corners_x <- c(idx[, 2] - 1, idx[, 2], idx[, 2] - 1, idx[, 2])
    corners_y <- c(idx[, 1] - 1, idx[, 1] - 1, idx[, 1], idx[, 1])
    if (n == 1L) {
      ch_area <- 1
      ch_perim <- 4
    } else {
      hull_i <- grDevices::chull(corners_x, corners_y)
      hx <- corners_x[hull_i]
      hy <- corners_y[hull_i]
      ch_area <- polygon_area(hx, hy)
      ch_perim <- polygon_perimeter(hx, hy)
    }
  }

  # Second-moment axes.
  major <- NA_real_
  minor <- NA_real_
  if (n >= 2L) {
    covm <- stats::cov(cbind(pts$x, pts$y))
    if (all(is.finite(covm))) {
      ev <- eigen(covm, symmetric = TRUE, only.values = TRUE)$values
      ev <- pmax(ev, 0)
      ev <- sort(ev, decreasing = TRUE)
      major <- 4 * sqrt(ev[[1]])
      minor <- 4 * sqrt(ev[[2]])
      if (minor < 1e-12) {
        minor <- NA_real_
      }
    }
  }

  eq_d <- sqrt(4 * area_px / pi)

  list(
    area_px = area_px,
    perimeter_px = as.numeric(perimeter_px),
    bbox_width_px = bbox_w,
    bbox_height_px = bbox_h,
    centroid_x = as.numeric(cx),
    centroid_y = as.numeric(cy),
    convex_hull_area_px = as.numeric(ch_area),
    convex_hull_perimeter_px = as.numeric(ch_perim),
    major_axis_px = as.numeric(major),
    minor_axis_px = as.numeric(minor),
    equivalent_diameter_px = as.numeric(eq_d),
    valid = TRUE
  )
}

#' Dimensionless descriptors from pixel geometry.
measure_shape_descriptors <- function(geom) {
  area <- geom$area_px
  perim <- geom$perimeter_px
  ch_area <- geom$convex_hull_area_px
  ch_perim <- geom$convex_hull_perimeter_px
  major <- geom$major_axis_px
  minor <- geom$minor_axis_px
  bw <- geom$bbox_width_px
  bh <- geom$bbox_height_px

  circularity <- if (is.finite(area) && is.finite(perim) && perim > 0) {
    4 * pi * area / (perim * perim)
  } else {
    NA_real_
  }

  solidity <- if (is.finite(area) && is.finite(ch_area) && ch_area > 0) {
    area / ch_area
  } else {
    NA_real_
  }

  extent <- if (is.finite(area) && is.finite(bw) && is.finite(bh) && (bw * bh) > 0) {
    area / (bw * bh)
  } else {
    NA_real_
  }

  aspect_ratio <- if (is.finite(major) && is.finite(minor) && minor > 0) {
    major / minor
  } else {
    NA_real_
  }

  eccentricity <- if (is.finite(major) && is.finite(minor) && major > 0 && minor >= 0) {
    ratio <- minor / major
    ratio <- min(1, max(0, ratio))
    sqrt(1 - ratio * ratio)
  } else {
    NA_real_
  }

  convexity <- if (is.finite(ch_perim) && is.finite(perim) && perim > 0) {
    ch_perim / perim
  } else {
    NA_real_
  }

  list(
    aspect_ratio = as.numeric(aspect_ratio),
    circularity = as.numeric(circularity),
    solidity = as.numeric(solidity),
    extent = as.numeric(extent),
    eccentricity = as.numeric(eccentricity),
    convexity = as.numeric(convexity)
  )
}

#' Convert pixel lengths/areas using confirmed px/mm scale.
#'
#' length_mm = px / s; area_mm2 = px / (s^2)
apply_calibration_to_geometry <- function(geom, pixels_per_mm) {
  s <- as.numeric(pixels_per_mm)
  calibrated <- is.finite(s) && s > 0
  convert_len <- function(v) {
    if (!calibrated || !is.finite(v)) NA_real_ else v / s
  }
  convert_area <- function(v) {
    if (!calibrated || !is.finite(v)) NA_real_ else v / (s * s)
  }

  list(
    calibrated = calibrated,
    area_mm2 = convert_area(geom$area_px),
    perimeter_mm = convert_len(geom$perimeter_px),
    bbox_width_mm = convert_len(geom$bbox_width_px),
    bbox_height_mm = convert_len(geom$bbox_height_px),
    major_axis_mm = convert_len(geom$major_axis_px),
    minor_axis_mm = convert_len(geom$minor_axis_px),
    equivalent_diameter_mm = convert_len(geom$equivalent_diameter_px),
    convex_hull_area_mm2 = convert_area(geom$convex_hull_area_px)
  )
}

#' Full measurement record for one detection.
measure_detection <- function(detection, pixels_per_mm = NA_real_) {
  mask <- detection$mask
  # Rebuild full ROI-local mask from bbox + compact mask when needed.
  if (!is.null(mask) && is.logical(mask) && length(dim(mask)) == 2L) {
    geom <- measure_mask_geometry(mask)
    # Centroids in compact-mask space → shift to ROI-local using bbox_roi.
    if (isTRUE(geom$valid) && !is.null(detection$bbox_roi)) {
      geom$centroid_x <- geom$centroid_x + detection$bbox_roi$x_min
      geom$centroid_y <- geom$centroid_y + detection$bbox_roi$y_min
      geom$bbox_width_px <- as.numeric(detection$bbox_roi$x_max - detection$bbox_roi$x_min)
      geom$bbox_height_px <- as.numeric(detection$bbox_roi$y_max - detection$bbox_roi$y_min)
    }
  } else {
    geom <- measure_mask_geometry(NULL)
  }

  desc <- measure_shape_descriptors(geom)
  phys <- apply_calibration_to_geometry(geom, pixels_per_mm)

  c(
    list(
      id = detection$id,
      area_px = geom$area_px,
      perimeter_px = geom$perimeter_px,
      bbox_width_px = geom$bbox_width_px,
      bbox_height_px = geom$bbox_height_px,
      centroid_x = geom$centroid_x,
      centroid_y = geom$centroid_y,
      convex_hull_area_px = geom$convex_hull_area_px,
      major_axis_px = geom$major_axis_px,
      minor_axis_px = geom$minor_axis_px,
      equivalent_diameter_px = geom$equivalent_diameter_px,
      valid_geometry = geom$valid
    ),
    desc,
    phys
  )
}

#' Measure all detections; returns a data.frame for UI tables.
measure_detections <- function(detections, pixels_per_mm = NA_real_) {
  if (is.null(detections) || length(detections) == 0L) {
    return(data.frame())
  }
  rows <- lapply(detections, function(d) measure_detection(d, pixels_per_mm))
  # Flatten list-of-lists to data.frame
  keys <- unique(unlist(lapply(rows, names)))
  out <- lapply(keys, function(k) {
    vapply(rows, function(r) {
      val <- r[[k]]
      if (is.null(val) || length(val) != 1L) NA else val
    }, numeric(1))
  })
  names(out) <- keys
  # Keep id as integer when possible
  df <- as.data.frame(out, stringsAsFactors = FALSE)
  if ("id" %in% names(df)) {
    df$id <- as.integer(df$id)
  }
  if ("valid_geometry" %in% names(df)) {
    df$valid_geometry <- as.logical(df$valid_geometry)
  }
  if ("calibrated" %in% names(df)) {
    df$calibrated <- as.logical(df$calibrated)
  }
  df
}

#' Compact table for the default Individual Measurements UI.
measurements_display_table <- function(df, calibrated = FALSE) {
  if (is.null(df) || nrow(df) == 0L) {
    return(data.frame(Message = "No measurements available."))
  }

  round_na <- function(x, digits) {
    ifelse(is.finite(x), round(x, digits), NA_real_)
  }

  out <- data.frame(
    ID = df$id,
    `Area (px)` = round_na(df$area_px, 0),
    `Area (mm²)` = if (calibrated) round_na(df$area_mm2, 3) else NA_real_,
    `Perimeter (mm)` = if (calibrated) round_na(df$perimeter_mm, 3) else NA_real_,
    `Major (mm)` = if (calibrated) round_na(df$major_axis_mm, 3) else NA_real_,
    `Minor (mm)` = if (calibrated) round_na(df$minor_axis_mm, 3) else NA_real_,
    `Aspect ratio` = round_na(df$aspect_ratio, 3),
    Circularity = round_na(df$circularity, 3),
    Solidity = round_na(df$solidity, 3),
    Eccentricity = round_na(df$eccentricity, 3),
    check.names = FALSE
  )
  out
}
