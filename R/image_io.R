# Image input helpers for AlfalfaCV Phase 1A (USDA Display Draft).
# Filename validation and lightweight metadata extraction (header-only).
# No detection, segmentation, or morphological analysis.

SUPPORTED_IMAGE_EXTENSIONS <- c("jpg", "jpeg", "png")
SUPPORTED_ACCESSION_PREFIXES <- c("PI", "W6")

#' Return the lowercase file extension without a leading dot.
#'
#' @param filename Character filename or path.
#' @return Lowercase extension string, or NA_character_ if none.
get_file_extension <- function(filename) {
  if (is.null(filename) || length(filename) != 1L || is.na(filename) || !nzchar(filename)) {
    return(NA_character_)
  }
  parts <- strsplit(basename(as.character(filename)), "\\.", fixed = FALSE)[[1]]
  if (length(parts) < 2L) {
    return(NA_character_)
  }
  tolower(parts[[length(parts)]])
}

#' Check whether a filename has a supported image extension.
#'
#' @param filename Character filename or path.
#' @return Logical.
is_supported_image_extension <- function(filename) {
  ext <- get_file_extension(filename)
  !is.na(ext) && ext %in% SUPPORTED_IMAGE_EXTENSIONS
}

#' Parse an accession identifier from an image filename.
#'
#' Accepted forms (case-insensitive prefix/extension; prefix normalized to upper):
#'   PI_<digits>[optional _alphanumeric components].(jpg|jpeg|png)
#'   W6_<digits>[optional _alphanumeric components].(jpg|jpeg|png)
#'
#' Examples:
#'   PI_123456.jpg              -> PI_123456
#'   PI_227393_85i_120.jpg      -> PI_227393
#'   W6_123456_85i_120.png      -> W6_123456
#'
#' Does not invent an accession identifier when parsing fails.
#' Additional underscore-separated components are optional suffixes and are
#' not part of the accession identifier. Empty suffix components are rejected.
#'
#' @param filename Character filename or path.
#' @return Named list with ok, accession_id, message.
parse_accession_filename <- function(filename) {
  empty <- list(
    ok = FALSE,
    accession_id = NA_character_,
    message = "No filename provided."
  )

  if (is.null(filename) || length(filename) != 1L || is.na(filename) || !nzchar(filename)) {
    return(empty)
  }

  name <- basename(as.character(filename))

  if (!is_supported_image_extension(name)) {
    return(list(
      ok = FALSE,
      accession_id = NA_character_,
      message = paste0(
        "Unsupported file extension. Expected .jpg, .jpeg, or .png ",
        "(received: ", name, ")."
      )
    ))
  }

  stem <- sub("\\.[^.]+$", "", name)

  # Reject empty components from leading/trailing/consecutive underscores.
  # (strsplit drops a trailing empty field, so check the stem explicitly.)
  if (grepl("^_|_$|__", stem)) {
    return(list(
      ok = FALSE,
      accession_id = NA_character_,
      message = paste0(
        "Invalid filename. Empty underscore-separated components are not allowed. ",
        "Received: ", name, "."
      )
    ))
  }

  parts <- strsplit(stem, "_", fixed = TRUE)[[1]]

  # Require PREFIX + accession digits at minimum.
  if (length(parts) < 2L || any(!nzchar(parts))) {
    return(list(
      ok = FALSE,
      accession_id = NA_character_,
      message = paste0(
        "Invalid filename. Expected PI_<number> or W6_<number> ",
        "with optional underscore-separated suffix components. Received: ",
        name, "."
      )
    ))
  }

  prefix <- toupper(parts[[1]])
  if (!prefix %in% SUPPORTED_ACCESSION_PREFIXES) {
    return(list(
      ok = FALSE,
      accession_id = NA_character_,
      message = paste0(
        "Invalid accession prefix. Expected PI or W6. Received: ", name, "."
      )
    ))
  }

  accession_number <- parts[[2]]
  if (!grepl("^[0-9]+$", accession_number)) {
    return(list(
      ok = FALSE,
      accession_id = NA_character_,
      message = paste0(
        "Invalid accession number. Expected digits after the prefix. ",
        "Received: ", name, "."
      )
    ))
  }

  if (length(parts) > 2L) {
    suffix_parts <- parts[seq(3L, length(parts))]
    if (!all(grepl("^[A-Za-z0-9]+$", suffix_parts))) {
      return(list(
        ok = FALSE,
        accession_id = NA_character_,
        message = paste0(
          "Invalid filename suffix. Optional components must be alphanumeric. ",
          "Received: ", name, "."
        )
      ))
    }
  }

  accession_id <- paste0(prefix, "_", accession_number)
  list(
    ok = TRUE,
    accession_id = accession_id,
    message = paste0("Accession identifier extracted: ", accession_id, ".")
  )
}

# Backward-compatible alias used by earlier Phase 1A code/tests.
parse_pi_filename <- function(filename) {
  result <- parse_accession_filename(filename)
  list(
    ok = result$ok,
    pi_id = result$accession_id,
    accession_id = result$accession_id,
    message = result$message
  )
}

#' Read a big-endian unsigned 32-bit integer from raw bytes.
#' @noRd
.read_uint32_be <- function(bytes) {
  sum(as.numeric(bytes) * c(16777216, 65536, 256, 1))
}

#' Read a big-endian unsigned 16-bit integer from raw bytes.
#' @noRd
.read_uint16_be <- function(bytes) {
  as.integer(bytes[[1]]) * 256L + as.integer(bytes[[2]])
}

#' Extract PNG width/height from the IHDR chunk without decoding pixels.
#' @noRd
.read_png_dimensions <- function(con) {
  signature <- readBin(con, what = "raw", n = 8L)
  expected <- as.raw(c(137, 80, 78, 71, 13, 10, 26, 10))
  if (length(signature) < 8L || !identical(signature, expected)) {
    stop("Not a PNG file.")
  }

  # First chunk must be IHDR: length(4) + type(4) + data(13) ...
  length_bytes <- readBin(con, what = "raw", n = 4L)
  type_bytes <- readBin(con, what = "raw", n = 4L)
  if (length(length_bytes) < 4L || length(type_bytes) < 4L) {
    stop("PNG header truncated.")
  }
  if (!identical(type_bytes, charToRaw("IHDR"))) {
    stop("PNG IHDR chunk missing.")
  }
  chunk_len <- .read_uint32_be(length_bytes)
  if (chunk_len < 8) {
    stop("PNG IHDR chunk too short.")
  }
  ihdr <- readBin(con, what = "raw", n = as.integer(chunk_len))
  if (length(ihdr) < 8L) {
    stop("PNG IHDR data truncated.")
  }

  width <- as.integer(.read_uint32_be(ihdr[1:4]))
  height <- as.integer(.read_uint32_be(ihdr[5:8]))
  c(width, height)
}

#' Extract JPEG width/height from SOF markers without decoding pixels.
#' @noRd
.read_jpeg_dimensions <- function(con) {
  soi <- readBin(con, what = "raw", n = 2L)
  if (length(soi) < 2L || !identical(soi, as.raw(c(0xFF, 0xD8)))) {
    stop("Not a JPEG file.")
  }

  # SOF0 / SOF1 / SOF2 contain frame dimensions.
  sof_markers <- as.raw(c(0xC0, 0xC1, 0xC2))

  repeat {
    # Seek next 0xFF marker prefix.
    b <- readBin(con, what = "raw", n = 1L)
    if (length(b) < 1L) {
      stop("JPEG marker scan ended unexpectedly.")
    }
    if (b != as.raw(0xFF)) {
      next
    }

    # Skip fill bytes (repeated 0xFF).
    marker <- readBin(con, what = "raw", n = 1L)
    while (length(marker) == 1L && marker == as.raw(0xFF)) {
      marker <- readBin(con, what = "raw", n = 1L)
    }
    if (length(marker) < 1L) {
      stop("JPEG marker truncated.")
    }

    # Standalone markers without a length field.
    if (marker == as.raw(0xD9) || (marker >= as.raw(0xD0) && marker <= as.raw(0xD7))) {
      next
    }
    # SOS means image data follows; dimensions should already have been found.
    if (marker == as.raw(0xDA)) {
      stop("JPEG SOF dimensions not found before scan data.")
    }

    len_bytes <- readBin(con, what = "raw", n = 2L)
    if (length(len_bytes) < 2L) {
      stop("JPEG segment length truncated.")
    }
    seg_len <- .read_uint16_be(len_bytes)
    if (is.na(seg_len) || seg_len < 2L) {
      stop("Invalid JPEG segment length.")
    }
    payload_len <- seg_len - 2L

    if (marker %in% sof_markers) {
      # SOF payload: precision(1), height(2), width(2), ...
      if (payload_len < 5L) {
        stop("JPEG SOF segment too short.")
      }
      sof <- readBin(con, what = "raw", n = payload_len)
      if (length(sof) < 5L) {
        stop("JPEG SOF data truncated.")
      }
      height <- .read_uint16_be(sof[2:3])
      width <- .read_uint16_be(sof[4:5])
      return(c(as.integer(width), as.integer(height)))
    }

    # Skip other segments.
    if (payload_len > 0L) {
      skipped <- readBin(con, what = "raw", n = payload_len)
      if (length(skipped) < payload_len) {
        stop("JPEG segment data truncated.")
      }
    }
  }
}

#' Read width and height from JPEG/PNG headers without decoding the raster.
#'
#' This intentionally avoids jpeg::readJPEG / png::readPNG full-image loads,
#' which allocate large in-memory arrays for research photographs.
#'
#' @param path Path to an image file on disk.
#' @return Named list with ok, width, height, message.
read_image_metadata <- function(path) {
  fail <- function(msg) {
    list(ok = FALSE, width = NA_integer_, height = NA_integer_, message = msg)
  }

  if (is.null(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    return(fail("No image path provided."))
  }
  if (!file.exists(path)) {
    return(fail("Image file was not found."))
  }

  ext <- get_file_extension(path)
  if (is.na(ext) || !ext %in% SUPPORTED_IMAGE_EXTENSIONS) {
    return(fail("Unsupported image format for metadata extraction."))
  }

  dims <- tryCatch(
    {
      con <- file(path, "rb")
      on.exit(close(con), add = TRUE)
      if (ext == "png") {
        .read_png_dimensions(con)
      } else {
        .read_jpeg_dimensions(con)
      }
    },
    error = function(e) {
      NULL
    }
  )

  if (is.null(dims) || length(dims) != 2L || any(is.na(dims)) || any(dims <= 0)) {
    return(fail(
      "Unable to read image. The file may be corrupt or unreadable."
    ))
  }

  list(
    ok = TRUE,
    width = as.integer(dims[[1]]),
    height = as.integer(dims[[2]]),
    message = "Image metadata extracted successfully."
  )
}

#' Empty session image record used for resets and failures.
#' @noRd
.empty_image_record <- function(
    filename = NA_character_,
    status = "error",
    message = "Upload failed.",
    can_preview = FALSE
) {
  list(
    filepath = NULL,
    filename = filename,
    accession_id = NA_character_,
    width = NA_integer_,
    height = NA_integer_,
    status = status,
    message = message,
    can_preview = can_preview
  )
}

#' Build a session-scoped image record from an uploaded file.
#'
#' Filename parsing and image readability are evaluated independently.
#' A readable image may be previewed even when the accession filename is invalid.
#' Does not persist files.
#'
#' @param datapath Temporary path provided by shiny::fileInput.
#' @param filename Original client filename.
#' @return Named list describing upload/validation state.
build_image_record <- function(datapath, filename) {
  original_name <- if (is.null(filename) || length(filename) != 1L || is.na(filename)) {
    NA_character_
  } else {
    as.character(filename)
  }

  base <- .empty_image_record(
    filename = original_name,
    status = "error",
    message = "Upload failed.",
    can_preview = FALSE
  )

  if (is.null(datapath) || length(datapath) != 1L || is.na(datapath) ||
        !nzchar(datapath) || !file.exists(datapath)) {
    base$message <- "Upload failed: temporary image file was not available."
    return(base)
  }

  # Keep parsing independent of image decoding.
  parsed <- tryCatch(
    parse_accession_filename(original_name),
    error = function(e) {
      list(
        ok = FALSE,
        accession_id = NA_character_,
        message = "Filename validation failed unexpectedly."
      )
    }
  )

  meta <- tryCatch(
    read_image_metadata(datapath),
    error = function(e) {
      list(
        ok = FALSE,
        width = NA_integer_,
        height = NA_integer_,
        message = "Unable to read image. The file may be corrupt or unreadable."
      )
    }
  )

  if (!isTRUE(meta$ok)) {
    base$status <- "unreadable"
    base$can_preview <- FALSE
    base$accession_id <- if (isTRUE(parsed$ok)) parsed$accession_id else NA_character_
    base$message <- if (!isTRUE(parsed$ok)) {
      paste(meta$message, parsed$message)
    } else {
      meta$message
    }
    return(base)
  }

  base$filepath <- datapath
  base$width <- meta$width
  base$height <- meta$height
  base$can_preview <- TRUE

  if (!isTRUE(parsed$ok)) {
    base$accession_id <- NA_character_
    base$status <- "invalid_filename"
    base$message <- parsed$message
    return(base)
  }

  base$accession_id <- parsed$accession_id
  base$status <- "ready"
  base$message <- paste0(
    "Upload successful. ",
    parsed$message,
    " Dimensions: ", meta$width, " x ", meta$height, " px."
  )
  base
}
