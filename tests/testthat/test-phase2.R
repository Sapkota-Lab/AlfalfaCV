# Phase 2 — workspace interaction, direct ROI editing, manual 10 mm calibration.

ui_output_text <- function(value) {
  if (is.list(value) && !is.null(value$html)) {
    return(paste(as.character(value$html), collapse = ""))
  }
  paste(as.character(value), collapse = "")
}

upload_frame <- function(name, path) {
  data.frame(
    name = name,
    size = file.size(path),
    type = "image/png",
    datapath = path,
    stringsAsFactors = FALSE
  )
}

write_seed_png <- function(width = 640L, height = 400L) {
  syn <- make_synthetic_seed_rgb(width = width, height = height)
  path <- tempfile(fileext = ".png")
  png::writePNG(syn$rgb, path)
  list(path = path, rgb = syn$rgb, truth = syn$truth)
}

test_that("horizontal 10 mm scale is (x2 - x1) / 10 in original pixels", {
  cal <- propose_horizontal_10mm_calibration(
    100, 40, 1010, 40,
    image_width = 9427, image_height = 7000
  )
  expect_equal(cal$status, CALIBRATION_STATUS_PROPOSED)
  expect_equal(cal$method, "manual")
  expect_equal(cal$reference_mm, 10)
  expect_equal(cal$pixels_per_mm, 91)
  expect_equal(cal$manual_points$distance_px, 910)
  expect_false(cal$confirmed)
  conf <- confirm_calibration(cal)
  expect_equal(active_pixels_per_mm(conf), 91)
  expect_true(is.na(active_pixels_per_mm(cal)))
})

test_that("horizontal calibration rejects zero, leftward, tilted, and out-of-bounds points", {
  expect_equal(
    propose_horizontal_10mm_calibration(10, 5, 10, 5, image_width = 100, image_height = 50)$status,
    CALIBRATION_STATUS_UNCALIBRATED
  )
  left <- propose_horizontal_10mm_calibration(80, 5, 20, 5, image_width = 200, image_height = 40)
  expect_equal(left$status, CALIBRATION_STATUS_UNCALIBRATED)
  expect_match(left$message, "right")
  tilted <- propose_horizontal_10mm_calibration(0, 10, 100, 25, image_width = 500, image_height = 80)
  expect_equal(tilted$status, CALIBRATION_STATUS_UNCALIBRATED)
  expect_match(tilted$message, "horizontal")
  expect_true(is.na(tilted$pixels_per_mm))
  outside <- propose_horizontal_10mm_calibration(0, 5, 50, 5, image_width = 40, image_height = 20)
  expect_equal(outside$status, CALIBRATION_STATUS_UNCALIBRATED)
  expect_match(outside$message, "outside")
})

test_that("preview resizing does not change a 91 px/mm horizontal scale", {
  roi <- make_roi(523L, 9950L, 8657L, 9838L)
  image_width <- 11000
  image_height <- 11000
  x1 <- roi$x_min + 100
  x2 <- x1 + 910
  y <- roi$y_min + 40
  ppms <- vapply(c(400, 1000, 1600), function(pw) {
    ph <- round(pw * roi_height(roi) / roi_width(roi))
    p1 <- orig_to_ruler_preview(x1, y, pw, ph, roi)
    p2 <- orig_to_ruler_preview(x2, y, pw, ph, roi)
    back1 <- preview_point_to_original(p1$x, p1$y, pw, ph, roi)
    back2 <- preview_point_to_original(p2$x, p2$y, pw, ph, roi)
    expect_equal(back2$x - back1$x, 910, tolerance = 1e-6)
    expect_equal(back1$y, back2$y)
    propose_horizontal_10mm_calibration(
      back1$x, back1$y, back2$x, back2$y,
      image_width = image_width, image_height = image_height, ruler_roi = roi
    )$pixels_per_mm
  }, numeric(1))
  expect_equal(ppms, c(91, 91, 91))
})

test_that("display fractions map to original pixels without a CSS size", {
  wide <- display_fraction_rect_to_roi(0.2, 0.1, 0.5, 0.4, 1200, 900, 9427, 7070)
  narrow <- display_fraction_rect_to_roi(0.2, 0.1, 0.5, 0.4, 600, 450, 9427, 7070)
  expect_equal(wide, narrow)
  expect_equal(wide$x_min, as.integer(floor(0.2 * 9427)))

  roi <- display_fraction_rect_to_roi(0.1, 0.1, 0.4, 0.4, 800, 400, 8000, 4000)
  expect_equal(roi_width(roi) / roi_height(roi), 2, tolerance = 0.05)
})

test_that("right-click hit testing is half-open and ignores points outside the ROI", {
  roi <- make_roi(1000L, 4000L, 500L, 3000L)
  expect_true(preview_point_inside_roi(100, 50, roi, 1000, 500, 10000, 5000))
  pr <- roi_to_preview(roi, 1000, 500, 10000, 5000)
  expect_true(preview_point_inside_roi(pr$x_min, pr$y_min, roi, 1000, 500, 10000, 5000))
  expect_false(preview_point_inside_roi(pr$x_max, pr$y_min + 1, roi, 1000, 500, 10000, 5000))
  expect_false(preview_point_inside_roi(1, 1, roi, 1000, 500, 10000, 5000))
})

test_that("active scale ignores automatic and unconfirmed calibrations", {
  auto <- empty_calibration(method = "automatic", status = CALIBRATION_STATUS_CONFIRMED)
  auto$pixels_per_mm <- 91
  auto$confirmed <- TRUE
  auto$reference_mm <- 10
  expect_true(is.na(active_pixels_per_mm(auto)))

  rows <- manual_calibration_debug_rows(propose_horizontal_10mm_calibration(0, 1, 910, 1, image_width = 2000, image_height = 100))
  expect_false(any(rows$label %in% c("1 mm period", "Major marks", "Spacing")))
  expect_true(any(rows$label == "Pixels per mm"))
})

test_that("phase 2 interface source keeps workflows and hides legacy controls", {
  app_src <- paste(readLines(file.path(project_root, "app.R")), collapse = "\n")
  css <- paste(readLines(file.path(project_root, "www", "app.css")), collapse = "\n")
  js <- paste(readLines(file.path(project_root, "www", "app.js")), collapse = "\n")

  expect_true(grepl("Seeds", app_src))
  expect_true(grepl("Pods", app_src))
  expect_true(grepl("Leaves", app_src))
  expect_true(grepl("Whole Plants", app_src))
  expect_true(grepl('checkboxInput\\("debug_mode"', app_src))
  expect_true(grepl("sidebar_collapse", app_src))
  expect_true(grepl("sidebar_expand", app_src))
  expect_true(grepl("value = 200", app_src))
  expect_false(grepl("adjust_roi", app_src))
  expect_false(grepl("Auto-Calibrate", app_src))
  expect_false(grepl("input\\$auto_calibrate", app_src))
  expect_equal(DEFAULT_DETECTION_PARAMS$sensitivity, 0.05)
  expect_equal(DEFAULT_DETECTION_PARAMS$max_area_frac, 0.24)
  expect_true(is.na(DEFAULT_DETECTION_PARAMS$min_area))

  expect_true(grepl("sidebar-collapsed", css))
  expect_true(grepl("44px", css))
  expect_true(grepl("140ms", css))
  expect_true(grepl("justify-content: center", css))
  expect_true(grepl("align-items: center", css))
  expect_true(grepl("overflow: auto", css))
  workspace_css <- regmatches(css, regexpr("\\.pane-workspace \\{[\\s\\S]*?\\}", css, perl = TRUE))
  frame_css <- regmatches(css, regexpr("\\.plot-frame \\{[\\s\\S]*?\\}", css, perl = TRUE))
  expect_true(grepl("overflow-y: auto", workspace_css))
  expect_true(grepl("overflow-x: hidden", workspace_css))
  expect_true(grepl("overflow: visible", frame_css))
  expect_false(grepl("overflow: auto", frame_css))
  expect_true(grepl("max-height: none", frame_css))
  expect_true(grepl("max-height: none", css))
  roi_css <- regmatches(css, regexpr("\\.plot-stage\\.plot-stage-roi\\.is-tall \\{[\\s\\S]*?\\}", css, perl = TRUE))
  expect_true(grepl("420px", roi_css))
  expect_true(grepl("overflow: hidden", roi_css))
  expect_false(grepl("overflow: auto", roi_css))
  expect_true(grepl("plot-stage-roi", app_src))
  expect_true(grepl("ROI_PREVIEW_MAX_HEIGHT = 420", js))
  expect_false(grepl("roiFocusFraction", js))
  expect_false(grepl("object-fit:\\s*fill", js))
  roi_img_css <- regmatches(
    css,
    regexpr("\\.plot-stage\\.plot-stage-roi\\.is-tall \\.shiny-plot-output img[\\s\\S]*?\\}", css, perl = TRUE)
  )
  expect_true(grepl("object-fit: contain", roi_img_css))
  expect_true(grepl("object-position: center center", roi_img_css))
  expect_false(grepl("position: absolute", roi_img_css))
  expect_true(grepl("minmax\\(0, 1fr\\)", css))
  expect_false(grepl("object-fit:\\s*fill", css))
  expect_true(grepl("object-fit: contain", css))
  expect_true(grepl("alfalfacv_upload_progress", app_src))
  expect_true(grepl("alfalfacv_upload_progress", js))
  expect_true(grepl("hideUploadProgress", js))
  expect_true(grepl("scrollJustHappened", js))
  expect_true(grepl("touch-action: pan-y", css))

  expect_true(grepl("sessionStorage", js))
  expect_true(grepl("preventDefault", js))
  expect_false(grepl('setInputValue\\("workflow"', js))
  menu_fn <- regmatches(js, regexpr("function onContextMenu[\\s\\S]*?function drawGuide", js, perl = TRUE))
  expect_length(menu_fn, 1L)
  expect_false(grepl("reset_roi", menu_fn))
  expect_true(grepl("cal-guide-line", js))
  expect_true(grepl("y1", js))
})

test_that("empty workspace is generic and upload status stays concise", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("magick")
  skip_if_not_installed("png")
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(project_root)
  img <- write_seed_png()
  on.exit(unlink(img$path), add = TRUE)
  upload <- file.path(tempdir(), "PI_424242.png")
  file.copy(img$path, upload, overwrite = TRUE)
  on.exit(unlink(upload), add = TRUE)

  app <- shiny::shinyAppFile(file.path(project_root, "app.R"))
  shiny::testServer(app, {
    empty_html <- ui_output_text(output$workspace_ui)
    expect_match(empty_html, "No image loaded")
    expect_match(empty_html, "Upload a JPEG or PNG image to begin.")
    expect_false(grepl("seed image", empty_html, ignore.case = TRUE))

    session$setInputs(workflow = "leaves")
    session$setInputs(debug_mode = TRUE)
    expect_equal(input$workflow, "leaves")
    expect_null(app_state$roi)

    session$setInputs(seed_image = upload_frame("PI_424242.png", upload))
    expect_equal(app_state$accession_id, "PI_424242")
    expect_equal(app_state$status, "ready")
    status_html <- ui_output_text(output$upload_status_ui)
    expect_match(status_html, "Ready — Image uploaded")
    expect_false(grepl("Upload successful", status_html))
    expect_false(grepl("Original filename", status_html))
    expect_equal(input$workflow, "leaves")

    roi_before <- app_state$roi
    cal_before <- app_state$calibration$status
    session$setInputs(debug_mode = FALSE)
    expect_equal(app_state$roi, roi_before)
    expect_equal(app_state$calibration$status, cal_before)
    session$setInputs(auto_calibrate = 1)
    expect_equal(app_state$calibration$status, CALIBRATION_STATUS_UNCALIBRATED)
  })
})

test_that("ROI gestures, reset, and a new image stay in original pixels", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("magick")
  skip_if_not_installed("png")
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(project_root)
  first <- write_seed_png(640L, 400L)
  second <- write_seed_png(500L, 320L)
  on.exit(unlink(c(first$path, second$path)), add = TRUE)
  path_a <- file.path(tempdir(), "PI_424242.png")
  path_b <- file.path(tempdir(), "W6_424242.png")
  file.copy(first$path, path_a, overwrite = TRUE)
  file.copy(second$path, path_b, overwrite = TRUE)
  on.exit(unlink(c(path_a, path_b)), add = TRUE)

  app <- shiny::shinyAppFile(file.path(project_root, "app.R"))
  shiny::testServer(app, {
    session$setInputs(seed_image = upload_frame("PI_424242.png", path_a))
    original <- app_state$roi
    session$setInputs(full_preview_brush = list(
      xmin = 30, xmax = 480, ymin = 20, ymax = 300, nonce = 1
    ))
    expect_false(identical(app_state$roi, original))
    expect_equal(app_state$detection$status, "stale")
    expect_false(app_state$measurements_current)
    expect_true(app_state$roi$x_min >= 0)
    expect_true(app_state$roi$x_max <= app_state$width)
    expect_true(app_state$roi$y_max <= app_state$height)

    session$setInputs(reset_roi = 1)
    expected <- initialize_default_roi(
      app_state$width, app_state$height,
      preview_rgb = app_state$preview_rgb,
      preview_width = app_state$preview_width,
      preview_height = app_state$preview_height
    )
    expect_equal(app_state$roi, expected)

    session$setInputs(seed_image = upload_frame("W6_424242.png", path_b))
    expect_equal(app_state$accession_id, "W6_424242")
    expect_false(identical(app_state$roi, original))
    expect_equal(app_state$calibration$status, CALIBRATION_STATUS_UNCALIBRATED)
    expect_equal(app_state$cal_mode, "idle")
    expect_length(app_state$manual_points, 0L)
  })
})

test_that("manual ruler calibration confirms, cancels, retries, and invalidates millimetres", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("magick")
  skip_if_not_installed("png")
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(project_root)
  img <- write_seed_png(640L, 400L)
  on.exit(unlink(img$path), add = TRUE)
  upload <- file.path(tempdir(), "PI_515151.png")
  file.copy(img$path, upload, overwrite = TRUE)
  on.exit(unlink(upload), add = TRUE)

  app <- shiny::shinyAppFile(file.path(project_root, "app.R"))
  shiny::testServer(app, {
    session$setInputs(seed_image = upload_frame("PI_515151.png", upload))
    session$setInputs(
      sensitivity = DEFAULT_DETECTION_PARAMS$sensitivity,
      min_area = 20,
      max_area_frac = 0.5
    )
    app_state$roi <- make_roi(0L, app_state$width, 0L, app_state$height)
    session$setInputs(run_detection = 1)
    expect_equal(app_state$detection$status, "ok")
    expect_equal(app_state$detection$params$sensitivity, DEFAULT_DETECTION_PARAMS$sensitivity)
    expect_equal(app_state$detection$params$min_area, 20L)
    expect_equal(app_state$detection$params$max_area_frac, 0.5)

    mask <- matrix(FALSE, 10L, 10L)
    mask[, ] <- TRUE
    app_state$detection <- list(
      status = "ok",
      message = "ok",
      warnings = character(),
      count = 1L,
      detections = list(list(
        id = 1L,
        mask = mask,
        bbox_roi = list(x_min = 0L, x_max = 10L, y_min = 0L, y_max = 10L)
      )),
      binary_mask = NULL,
      roi = app_state$roi,
      params = app_state$detection$params
    )
    app_state$detection_current <- TRUE

    pw <- app_state$ruler_preview_width
    ph <- app_state$ruler_preview_height
    ruler <- app_state$ruler_roi
    y <- ruler$y_min + roi_height(ruler) / 2
    click_at <- function(x_orig, nonce) {
      pt <- orig_to_ruler_preview(x_orig, y, pw, ph, ruler)
      session$setInputs(ruler_cal_click = list(x = pt$x, y = pt$y, nonce = nonce))
    }

    session$setInputs(start_calibration = 1)
    expect_equal(app_state$cal_mode, "first")
    click_at(ruler$x_min + 30, 1)
    expect_equal(app_state$cal_mode, "second")
    click_at(ruler$x_min + 20, 2)
    expect_equal(app_state$cal_mode, "second")
    expect_null(app_state$cal_candidate)
    expect_false(isTRUE(app_state$calibration$confirmed))

    click_at(ruler$x_min + 130, 3)
    expect_equal(app_state$cal_mode, "confirm")
    expect_equal(app_state$cal_candidate$pixels_per_mm, 10, tolerance = 1e-6)
    expect_false(isTRUE(app_state$calibration$confirmed))
    expect_true(is.na(active_pixels_per_mm(app_state$calibration, app_state$filename)))

    session$setInputs(cancel_calibration = 1)
    expect_equal(app_state$cal_mode, "idle")
    expect_null(app_state$cal_candidate)
    expect_equal(app_state$calibration$status, CALIBRATION_STATUS_UNCALIBRATED)

    session$setInputs(start_calibration = 2)
    click_at(ruler$x_min + 30, 4)
    click_at(ruler$x_min + 130, 5)
    session$setInputs(confirm_manual_calibration = 1)
    expect_true(isTRUE(app_state$calibration$confirmed))
    expect_equal(app_state$calibration$method, "manual")
    expect_equal(app_state$calibration$reference_mm, 10)
    expect_equal(app_state$calibration$pixels_per_mm, 10, tolerance = 1e-6)
    expect_equal(app_state$calibration$source_filename, "PI_515151.png")
    expect_equal(app_state$cal_mode, "idle")
    expect_equal(app_state$measurements$area_px[[1]], 100)
    expect_equal(app_state$measurements$area_mm2[[1]], 1, tolerance = 1e-4)
    expect_equal(app_state$measurements_ppm, 10, tolerance = 1e-6)

    session$setInputs(recalibrate = 1)
    expect_equal(app_state$cal_mode, "first")
    expect_equal(app_state$measurements$area_mm2[[1]], 1)
    click_at(ruler$x_min + 40, 6)
    session$setInputs(retry_calibration = 1)
    expect_equal(app_state$cal_mode, "first")
    expect_length(app_state$manual_points, 0L)
    expect_equal(app_state$calibration$pixels_per_mm, 10)
    expect_true(isTRUE(app_state$calibration$confirmed))

    click_at(ruler$x_min + 40, 7)
    click_at(ruler$x_min + 240, 8)
    expect_equal(app_state$cal_mode, "confirm")
    expect_equal(app_state$measurements$area_mm2[[1]], 1)
    session$setInputs(confirm_manual_calibration = 2)
    expect_equal(app_state$calibration$pixels_per_mm, 20, tolerance = 1e-6)
    expect_equal(app_state$measurements$area_mm2[[1]], 0.25, tolerance = 1e-4)
    expect_equal(app_state$measurements$area_px[[1]], 100)

    session$setInputs(full_preview_brush = list(
      xmin = 40, xmax = 500, ymin = 30, ymax = 280, nonce = 9
    ))
    expect_equal(app_state$detection$status, "stale")
    expect_false(app_state$measurements_current)
    expect_equal(nrow(app_state$measurements), 0L)
  })
})
