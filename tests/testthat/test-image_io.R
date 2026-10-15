test_that("simple PI filenames parse correctly", {
  expect_true(parse_accession_filename("PI_123456.jpg")$ok)
  expect_equal(parse_accession_filename("PI_123456.jpg")$accession_id, "PI_123456")

  expect_true(parse_accession_filename("PI_1.jpeg")$ok)
  expect_equal(parse_accession_filename("PI_1.jpeg")$accession_id, "PI_1")

  expect_true(parse_accession_filename("PI_654321.png")$ok)
  expect_equal(parse_accession_filename("PI_654321.png")$accession_id, "PI_654321")
})

test_that("PI filenames with suffix components parse accession only", {
  result <- parse_accession_filename("PI_227393_85i_120.jpg")
  expect_true(result$ok)
  expect_equal(result$accession_id, "PI_227393")

  result2 <- parse_accession_filename("PI_495179_87i_094.jpeg")
  expect_true(result2$ok)
  expect_equal(result2$accession_id, "PI_495179")
})

test_that("W6 filenames are supported", {
  expect_equal(parse_accession_filename("W6_123456.jpg")$accession_id, "W6_123456")
  expect_true(parse_accession_filename("W6_123456.jpg")$ok)

  result <- parse_accession_filename("W6_123456_85i_120.png")
  expect_true(result$ok)
  expect_equal(result$accession_id, "W6_123456")
})

test_that("accession parsing is case-insensitive and normalizes prefix", {
  expect_equal(parse_accession_filename("pi_123456.JPG")$accession_id, "PI_123456")
  expect_equal(parse_accession_filename("Pi_99.PnG")$accession_id, "PI_99")
  expect_equal(parse_accession_filename("w6_33670.JPEG")$accession_id, "W6_33670")
  expect_equal(parse_accession_filename("W6_42_abC_9.png")$accession_id, "W6_42")
})

test_that("invalid accession filenames do not invent identifiers", {
  bad_names <- c(
    "seed.jpg",
    "PI123456.jpg",
    "PI_.jpg",
    "PI_abc.jpg",
    "PI_123456_.jpg",
    "PI_123456__x.jpg",
    "PI_123456_85i_.jpg",
    "PI_123456_85-i_120.jpg",
    "XX_123456.jpg",
    "PI_123456.gif",
    ""
  )

  for (name in bad_names) {
    result <- parse_accession_filename(name)
    expect_false(result$ok, info = name)
    expect_true(is.na(result$accession_id), info = name)
    expect_true(nzchar(result$message), info = name)
  }
})

test_that("supported extensions are accepted and invalid ones rejected", {
  expect_true(is_supported_image_extension("PI_1.jpg"))
  expect_true(is_supported_image_extension("PI_1.jpeg"))
  expect_true(is_supported_image_extension("PI_1.png"))
  expect_true(is_supported_image_extension("photo.JPG"))

  expect_false(is_supported_image_extension("photo.gif"))
  expect_false(is_supported_image_extension("photo.tif"))
  expect_false(is_supported_image_extension("photo.bmp"))
  expect_false(is_supported_image_extension("photo"))
  expect_false(is_supported_image_extension(""))
})

test_that("paths use basename for accession parsing", {
  result <- parse_accession_filename("/tmp/uploads/PI_777_85i_120.png")
  expect_true(result$ok)
  expect_equal(result$accession_id, "PI_777")
})

test_that("parse_pi_filename alias still returns accession fields", {
  result <- parse_pi_filename("PI_227393_85i_120.jpg")
  expect_true(result$ok)
  expect_equal(result$pi_id, "PI_227393")
  expect_equal(result$accession_id, "PI_227393")
})

test_that("image metadata extraction works for synthetic PNG fixture", {
  skip_if_not(file.exists(synthetic_png), "Synthetic fixture PNG not found")

  meta <- read_image_metadata(synthetic_png)
  expect_true(meta$ok)
  expect_equal(meta$width, 800L)
  expect_equal(meta$height, 500L)
})

test_that("header-only metadata reads declared large PNG dimensions", {
  # Minimal PNG with IHDR only — enough for dimension parsing, not full decode.
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  width <- 10757L
  height <- 9900L
  signature <- as.raw(c(137, 80, 78, 71, 13, 10, 26, 10))
  ihdr_type <- charToRaw("IHDR")
  ihdr_data <- c(
    writeBin(width, raw(), size = 4L, endian = "big"),
    writeBin(height, raw(), size = 4L, endian = "big"),
    as.raw(c(8, 2, 0, 0, 0))
  )
  len <- writeBin(13L, raw(), size = 4L, endian = "big")
  crc <- writeBin(0L, raw(), size = 4L, endian = "big")
  writeBin(c(signature, len, ihdr_type, ihdr_data, crc), tmp)

  meta <- read_image_metadata(tmp)
  expect_true(meta$ok)
  expect_equal(meta$width, width)
  expect_equal(meta$height, height)
})

test_that("header-only metadata reads declared large JPEG dimensions", {
  tmp <- tempfile(fileext = ".jpg")
  on.exit(unlink(tmp), add = TRUE)

  width <- 10757L
  height <- 9900L
  # SOI + SOF0 (baseline) + EOI
  sof0 <- c(
    as.raw(c(0xFF, 0xC0)),
    writeBin(11L, raw(), size = 2L, endian = "big"), # Lf
    as.raw(8L), # precision
    writeBin(height, raw(), size = 2L, endian = "big"),
    writeBin(width, raw(), size = 2L, endian = "big"),
    as.raw(c(1L, 1L, 0x11, 0L)) # Nf + one component
  )
  bytes <- c(as.raw(c(0xFF, 0xD8)), sof0, as.raw(c(0xFF, 0xD9)))
  writeBin(bytes, tmp)

  meta <- read_image_metadata(tmp)
  expect_true(meta$ok)
  expect_equal(meta$width, width)
  expect_equal(meta$height, height)
})

test_that("build_image_record handles valid synthetic upload", {
  skip_if_not(file.exists(synthetic_png), "Synthetic fixture PNG not found")

  record <- build_image_record(synthetic_png, "PI_100001.png")
  expect_equal(record$status, "ready")
  expect_equal(record$accession_id, "PI_100001")
  expect_equal(record$filename, "PI_100001.png")
  expect_true(record$can_preview)
  expect_equal(record$width, 800L)
  expect_equal(record$height, 500L)
})

test_that("build_image_record handles suffixed USDA-style names", {
  skip_if_not(file.exists(synthetic_png), "Synthetic fixture PNG not found")

  record <- build_image_record(synthetic_png, "PI_227393_85i_120.png")
  expect_equal(record$status, "ready")
  expect_equal(record$accession_id, "PI_227393")
  expect_equal(record$filename, "PI_227393_85i_120.png")
  expect_true(record$can_preview)
})

test_that("preview does not depend on accession validity", {
  skip_if_not(file.exists(synthetic_png), "Synthetic fixture PNG not found")

  record <- build_image_record(synthetic_png, "not_a_pi.png")
  expect_equal(record$status, "invalid_filename")
  expect_true(is.na(record$accession_id))
  expect_true(record$can_preview)
  expect_equal(record$filename, "not_a_pi.png")
  expect_false(is.na(record$width))
})

test_that("unreadable files produce a clear failure state", {
  bogus <- tempfile(fileext = ".png")
  writeBin(as.raw(c(0x00, 0x01, 0x02, 0x03)), bogus)
  on.exit(unlink(bogus), add = TRUE)

  record <- build_image_record(bogus, "PI_999.png")
  expect_equal(record$status, "unreadable")
  expect_false(record$can_preview)
  expect_match(record$message, "corrupt|unreadable|Unable to read", ignore.case = TRUE)
})

test_that("image replacement clears prior accession metadata", {
  skip_if_not(file.exists(synthetic_png), "Synthetic fixture PNG not found")

  first <- build_image_record(synthetic_png, "PI_227393_85i_120.png")
  expect_equal(first$accession_id, "PI_227393")

  second <- build_image_record(synthetic_png, "W6_33670_2010i_043.png")
  expect_equal(second$status, "ready")
  expect_equal(second$accession_id, "W6_33670")
  expect_equal(second$filename, "W6_33670_2010i_043.png")
  expect_true(second$can_preview)

  # Failed replacement must not retain previous accession/dimensions/path.
  bogus <- tempfile(fileext = ".jpg")
  writeBin(as.raw(c(0xFF, 0xD8, 0x00)), bogus)
  on.exit(unlink(bogus), add = TRUE)

  third <- build_image_record(bogus, "PI_111_bad.jpg")
  expect_equal(third$status, "unreadable")
  expect_null(third$filepath)
  expect_false(third$can_preview)
  expect_true(is.na(third$width))
  expect_true(is.na(third$height))
  # Accession may still parse from filename even when image is unreadable.
  expect_equal(third$accession_id, "PI_111")
  expect_equal(third$filename, "PI_111_bad.jpg")
})

test_that("missing temporary file recovers to a failed record", {
  record <- build_image_record(tempfile(fileext = ".png"), "PI_123.png")
  expect_equal(record$status, "error")
  expect_false(record$can_preview)
  expect_null(record$filepath)
  expect_true(is.na(record$accession_id))
})
