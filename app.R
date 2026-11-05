# AlfalfaCV — Phase 2: image interaction and manual 10 mm calibration.
# Upload, direct ROI editing, seed detection, ruler preview calibration, morphology.

library(shiny)
library(bslib)
library(magick)
library(mmand)

source(file.path("R", "image_io.R"), local = TRUE)
source(file.path("R", "roi.R"), local = TRUE)
source(file.path("R", "image_processing.R"), local = TRUE)
source(file.path("R", "seed_detection.R"), local = TRUE)
source(file.path("R", "ruler_calibration.R"), local = TRUE)
source(file.path("R", "morphology.R"), local = TRUE)

options(shiny.maxRequestSize = 50 * 1024^2)

app_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#2c5f2d",
  secondary = "#97a97c",
  success = "#3a7d44",
  info = "#4a6fa5",
  warning = "#b08900",
  danger = "#8b3a3a",
  "font-size-base" = "0.95rem"
)

workflow_pending <- function(title) {
  div(
    class = "workflow-pending",
    h2(title),
    p("This workflow is not available yet.")
  )
}

sidebar_icon <- function() {
  HTML(
    '<svg class="sidebar-icon" width="16" height="16" viewBox="0 0 16 16" aria-hidden="true" focusable="false"><rect x="1.4" y="2" width="13.2" height="12" rx="1.4" fill="none" stroke="currentColor" stroke-width="1.4"/><path d="M6 2.2v11.6" stroke="currentColor" stroke-width="1.4"/></svg>'
  )
}

action_button <- function(input_id, label, class) {
  tags$button(
    type = "button",
    class = class,
    `data-input-action` = input_id,
    label
  )
}

ui <- page_fillable(
  padding = 0,
  gap = 0,
  fillable_mobile = TRUE,
  theme = app_theme,
  title = "AlfalfaCV",
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "app.css?v=4"),
    tags$script(src = "app.js?v=4")
  ),
  as_fill_carrier(
    div(
      class = "app-shell",
      tags$nav(
        class = "app-nav",
        `aria-label` = "Primary",
        div(
          class = "sidebar-rail",
          tags$button(
            type = "button",
            id = "sidebar_expand",
            class = "sidebar-toggle",
            title = "Expand sidebar",
            `aria-label` = "Expand sidebar",
            sidebar_icon()
          )
        ),
        div(
          class = "app-nav-expanded",
          div(
            class = "app-nav-head",
            tags$h1("AlfalfaCV", class = "app-brand"),
            tags$button(
              type = "button",
              id = "sidebar_collapse",
              class = "sidebar-toggle",
              title = "Collapse sidebar",
              `aria-label` = "Collapse sidebar",
              `aria-expanded` = "true",
              sidebar_icon()
            )
          ),
          div(
            class = "workflow-nav",
            radioButtons(
              "workflow",
              "Workflows",
              choices = c(
                "Seeds" = "seeds",
                "Pods" = "pods",
                "Leaves" = "leaves",
                "Whole Plants" = "plants"
              ),
              selected = "seeds"
            )
          ),
          div(
            class = "nav-footer",
            checkboxInput("debug_mode", "Debug Mode", value = FALSE)
          )
        )
      ),
      tags$div(
        id = "roi_menu",
        class = "roi-menu",
        role = "menu",
        tags$button(
          type = "button",
          id = "roi_reset_action",
          class = "roi-menu-item",
          role = "menuitem",
          `data-input-action` = "reset_roi",
          "Reset ROI"
        )
      ),
      div(
        class = "app-main",
        conditionalPanel(
          condition = "!input.workflow || input.workflow === 'seeds'",
          div(
            class = "app-stage",
            div(
              class = "pane pane-input",
              div(
                class = "pane-body",
                fileInput(
                  "seed_image",
                  "Upload seed image",
                  multiple = FALSE,
                  accept = c(".jpg", ".jpeg", ".png", "image/jpeg", "image/png"),
                  width = "100%"
                ),
                uiOutput("upload_status_ui"),
                actionButton(
                  "run_detection",
                  "Run Seed Detection",
                  class = "btn-primary",
                  width = "100%"
                ),
                conditionalPanel(
                  condition = "input.debug_mode == true",
                  div(
                    class = "debug-panel",
                    div(class = "section-label", "Image metadata"),
                    tableOutput("image_meta_table"),
                    div(class = "section-label", "Region of interest"),
                    uiOutput("roi_status_ui"),
                    div(class = "section-label", "Detection parameters"),
                    sliderInput(
                      "sensitivity",
                      "Detection sensitivity",
                      min = 0, max = 1, value = DEFAULT_DETECTION_PARAMS$sensitivity, step = 0.05
                    ),
                    # Placeholder before an ROI exists. DEFAULT_DETECTION_PARAMS$min_area
                    # is NA; after upload, suggest_min_area() replaces this value.
                    # Do not treat 200 as a new scientific default.
                    numericInput(
                      "min_area",
                      "Minimum component area (px)",
                      value = 200,
                      min = 1, step = 1
                    ),
                    numericInput(
                      "max_area_frac",
                      "Maximum component area (fraction of ROI)",
                      value = DEFAULT_DETECTION_PARAMS$max_area_frac,
                      min = 0.01, max = 0.5, step = 0.01
                    ),
                    div(class = "section-label", "Calibration details"),
                    uiOutput("calibration_debug_ui")
                  )
                )
              )
            ),
            div(
              class = "pane pane-workspace",
              div(
                class = "pane-body",
                uiOutput("workspace_ui")
              )
            ),
            div(
              class = "pane pane-results",
              div(
                class = "pane-body",
                div(
                  class = "result-section",
                  h5("1. Seed Detection"),
                  uiOutput("detection_results_ui")
                ),
                div(
                  class = "result-section",
                  h5("2. Individual Measurements"),
                  uiOutput("measurements_status_ui"),
                  tableOutput("measurements_table")
                ),
                div(
                  class = "result-section",
                  h5("3. Statistical Summary"),
                  p(class = "mb-0 text-muted", "No results available.")
                )
              )
            )
          )
        ),
        conditionalPanel(
          condition = "input.workflow === 'pods'",
          workflow_pending("Pods")
        ),
        conditionalPanel(
          condition = "input.workflow === 'leaves'",
          workflow_pending("Leaves")
        ),
        conditionalPanel(
          condition = "input.workflow === 'plants'",
          workflow_pending("Whole Plants")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  app_state <- reactiveValues(
    filepath = NULL,
    filename = NULL,
    accession_id = NULL,
    width = NULL,
    height = NULL,
    status = "empty",
    message = "No image uploaded.",
    can_preview = FALSE,
    session_dir = NULL,
    magick_img = NULL,
    preview_raster = NULL,
    preview_rgb = NULL,
    preview_width = NULL,
    preview_height = NULL,
    roi = NULL,
    roi_raster = NULL,
    roi_preview_width = NULL,
    roi_preview_height = NULL,
    roi_scale = 1,
    detection = empty_detection_result(),
    detection_current = FALSE,
    calibration = empty_calibration(),
    ruler_roi = NULL,
    ruler_raster = NULL,
    ruler_preview_width = NULL,
    ruler_preview_height = NULL,
    ruler_preview_scale = 1,
    manual_points = list(),
    manual_reference = NULL,
    cal_mode = "idle",
    cal_candidate = NULL,
    cal_message = NULL,
    measurements = data.frame(),
    measurements_current = FALSE,
    measurements_ppm = NA_real_,
    measurements_file = NULL,
    measurements_roi_tag = ""
  )

  clear_detection <- function(status = "empty", message = "No detection has been run.") {
    app_state$detection <- empty_detection_result(status = status, message = message)
    app_state$detection_current <- FALSE
    clear_measurements()
  }

  clear_measurements <- function() {
    app_state$measurements <- data.frame()
    app_state$measurements_current <- FALSE
    app_state$measurements_ppm <- NA_real_
    app_state$measurements_file <- NULL
    app_state$measurements_roi_tag <- ""
  }

  clear_calibration <- function(message = "No calibration available.") {
    app_state$calibration <- empty_calibration(message = message)
    app_state$manual_points <- list()
    app_state$manual_reference <- NULL
    app_state$cal_candidate <- NULL
    app_state$cal_mode <- "idle"
    app_state$cal_message <- NULL
    # The ruler preview is part of the image, not the scale. Leave it in
    # place so a reset calibration can be redrawn on the same preview.
    refresh_measurements()
  }

  clear_image_artifacts <- function() {
    app_state$magick_img <- NULL
    app_state$preview_raster <- NULL
    app_state$preview_rgb <- NULL
    app_state$preview_width <- NULL
    app_state$preview_height <- NULL
    app_state$roi <- NULL
    app_state$roi_raster <- NULL
    app_state$roi_preview_width <- NULL
    app_state$roi_preview_height <- NULL
    app_state$roi_scale <- 1
    app_state$ruler_roi <- NULL
    app_state$ruler_raster <- NULL
    app_state$ruler_preview_width <- NULL
    app_state$ruler_preview_height <- NULL
    app_state$ruler_preview_scale <- 1
    clear_session_image_dir(app_state$session_dir)
    app_state$session_dir <- NULL
    clear_detection()
    clear_calibration()
    gc(verbose = FALSE)
  }

  sync_min_area_default <- function() {
    if (is.null(app_state$roi)) {
      return(invisible())
    }
    suggested <- suggest_min_area(roi_width(app_state$roi), roi_height(app_state$roi))
    updateNumericInput(session, "min_area", value = suggested)
  }

  refresh_roi_preview <- function() {
    if (is.null(app_state$magick_img) || is.null(app_state$roi)) {
      app_state$roi_raster <- NULL
      return(invisible())
    }
    check <- validate_roi(app_state$roi, app_state$width, app_state$height)
    if (!isTRUE(check$ok)) {
      app_state$roi_raster <- NULL
      return(invisible())
    }
    crop <- tryCatch(
      crop_roi_magick(app_state$magick_img, app_state$roi),
      error = function(e) NULL
    )
    if (is.null(crop)) {
      app_state$roi_raster <- NULL
      return(invisible())
    }
    preview <- create_roi_preview(crop)
    app_state$roi_raster <- magick_to_plot_raster(preview$image)
    app_state$roi_preview_width <- preview$width
    app_state$roi_preview_height <- preview$height
    app_state$roi_scale <- preview$scale
  }

  refresh_ruler_preview <- function() {
    clear_ruler_preview_dims <- function() {
      app_state$ruler_raster <- NULL
      app_state$ruler_preview_width <- NULL
      app_state$ruler_preview_height <- NULL
      app_state$ruler_preview_scale <- 1
    }
    if (is.null(app_state$magick_img) || is.null(app_state$ruler_roi)) {
      clear_ruler_preview_dims()
      return(invisible())
    }
    check <- validate_roi(app_state$ruler_roi, app_state$width, app_state$height)
    if (!isTRUE(check$ok)) {
      clear_ruler_preview_dims()
      return(invisible())
    }
    preview <- tryCatch(
      create_ruler_preview(app_state$magick_img, app_state$ruler_roi),
      error = function(e) NULL
    )
    if (is.null(preview)) {
      clear_ruler_preview_dims()
      return(invisible())
    }
    app_state$ruler_raster <- magick_to_plot_raster(preview$image)
    app_state$ruler_preview_width <- preview$width
    app_state$ruler_preview_height <- preview$height
    app_state$ruler_preview_scale <- preview$scale
  }

  #' Recompute measurements from current detections + confirmed calibration only.
  #' Does not re-run segmentation.
  refresh_measurements <- function() {
    det <- app_state$detection
    if (!isTRUE(app_state$detection_current) ||
          is.null(det) ||
          length(det$detections) == 0L ||
          !det$status %in% c("ok", "no_detections")) {
      app_state$measurements <- data.frame()
      app_state$measurements_current <- FALSE
      return(invisible())
    }
    if (identical(det$status, "no_detections")) {
      app_state$measurements <- data.frame()
      app_state$measurements_current <- TRUE
      return(invisible())
    }

    ppm <- active_pixels_per_mm(app_state$calibration, app_state$filename)
    app_state$measurements <- tryCatch(
      measure_detections(det$detections, pixels_per_mm = ppm),
      error = function(e) data.frame()
    )
    app_state$measurements_current <- TRUE
    app_state$measurements_ppm <- ppm
    app_state$measurements_file <- app_state$filename
    app_state$measurements_roi_tag <- roi_state_tag(app_state$roi)
  }

  preview_device_size <- function(kind) {
    dims <- switch(
      kind,
      full = c(app_state$preview_width, app_state$preview_height),
      roi = c(app_state$roi_preview_width, app_state$roi_preview_height),
      ruler = c(app_state$ruler_preview_width, app_state$ruler_preview_height),
      c(NA_real_, NA_real_)
    )
    w <- suppressWarnings(as.numeric(dims[[1]]))
    h <- suppressWarnings(as.numeric(dims[[2]]))
    if (length(w) != 1L || length(h) != 1L || !is.finite(w) || !is.finite(h) || w < 2 || h < 2) {
      return(list(width = 300L, height = 200L))
    }
    list(width = as.integer(round(w)), height = as.integer(round(h)))
  }

  finite_input <- function(value, fallback) {
    if (is.null(value) || length(value) != 1L || !is.finite(suppressWarnings(as.numeric(value)))) {
      return(fallback)
    }
    as.numeric(value)
  }

  begin_calibration <- function() {
    if (!isTRUE(app_state$can_preview) || is.null(app_state$ruler_roi)) {
      app_state$cal_message <- "Upload an image before calibrating."
      return(invisible())
    }
    # Leave a previously confirmed scale in place until a new one is confirmed.
    app_state$manual_points <- list()
    app_state$cal_candidate <- NULL
    app_state$cal_message <- NULL
    app_state$cal_mode <- "first"
  }

  draw_reference_segment <- function(x1, y1, x2, y2, preview_w, preview_h, ruler_roi, col, lwd = 2) {
    if (is.null(ruler_roi) || !all(is.finite(c(x1, y1, x2, y2, preview_w, preview_h)))) {
      return(invisible())
    }
    a <- orig_to_ruler_preview(x1, y1, preview_w, preview_h, ruler_roi)
    b <- orig_to_ruler_preview(x2, y2, preview_w, preview_h, ruler_roi)
    segments(a$x, a$y, b$x, b$y, col = col, lwd = lwd)
    points(c(a$x, b$x), c(a$y, b$y), pch = 3, col = col, lwd = 2, cex = 1.2)
  }

  apply_image_record <- function(record) {
    app_state$filepath <- record$filepath
    app_state$filename <- record$filename
    app_state$accession_id <- record$accession_id
    app_state$width <- record$width
    app_state$height <- record$height
    app_state$status <- record$status
    app_state$message <- record$message
    app_state$can_preview <- isTRUE(record$can_preview)
  }

  observeEvent(input$seed_image, {
    upload <- input$seed_image
    if (is.null(upload)) {
      return()
    }

    clear_image_artifacts()
    apply_image_record(.empty_image_record(
      filename = upload$name,
      status = "error",
      message = "Processing upload...",
      can_preview = FALSE
    ))

    record <- tryCatch(
      build_image_record(upload$datapath, upload$name),
      error = function(e) {
        .empty_image_record(
          filename = upload$name,
          status = "error",
          message = "Upload failed unexpectedly. Please try another image.",
          can_preview = FALSE
        )
      }
    )
    apply_image_record(record)

    if (!isTRUE(record$can_preview)) {
      return()
    }

    prepared <- tryCatch(
      {
        session_dir <- create_session_image_dir()
        local_path <- file.path(session_dir, basename(upload$name))
        file.copy(upload$datapath, local_path, overwrite = TRUE)
        img <- load_image_magick(local_path)
        dims <- magick_dimensions(img)
        preview <- create_full_preview(img)
        preview_rgb <- magick_to_rgb_array(preview$image)
        list(
          session_dir = session_dir,
          local_path = local_path,
          img = img,
          dims = dims,
          preview = preview,
          preview_rgb = preview_rgb,
          raster = magick_to_plot_raster(preview$image)
        )
      },
      error = function(e) NULL
    )

    if (is.null(prepared)) {
      app_state$status <- "error"
      app_state$message <- "Upload succeeded, but image preparation failed."
      app_state$can_preview <- FALSE
      return()
    }

    app_state$session_dir <- prepared$session_dir
    app_state$filepath <- prepared$local_path
    app_state$magick_img <- prepared$img
    app_state$width <- prepared$dims$width
    app_state$height <- prepared$dims$height
    app_state$preview_raster <- prepared$raster
    app_state$preview_rgb <- prepared$preview_rgb
    app_state$preview_width <- prepared$preview$width
    app_state$preview_height <- prepared$preview$height
    app_state$roi <- initialize_default_roi(
      prepared$dims$width,
      prepared$dims$height,
      preview_rgb = prepared$preview_rgb,
      preview_width = prepared$preview$width,
      preview_height = prepared$preview$height
    )
    app_state$ruler_roi <- initialize_default_ruler_roi(
      prepared$dims$width, prepared$dims$height
    )
    refresh_roi_preview()
    refresh_ruler_preview()
    sync_min_area_default()
    clear_detection(status = "empty", message = "No detection has been run.")
    clear_calibration(message = "Upload a new image — calibration reset.")
  }, ignoreNULL = TRUE)

  observeEvent(input$reset_roi, {
    req(isTRUE(app_state$can_preview), !is.null(app_state$width))
    app_state$roi <- initialize_default_roi(
      app_state$width,
      app_state$height,
      preview_rgb = app_state$preview_rgb,
      preview_width = app_state$preview_width,
      preview_height = app_state$preview_height
    )
    refresh_roi_preview()
    sync_min_area_default()
    clear_detection(status = "stale", message = "ROI reset. Run seed detection again.")
  })

  observeEvent(input$full_preview_brush, {
    brush <- input$full_preview_brush
    if (is.null(brush) || is.null(app_state$preview_width) || is.null(app_state$roi)) {
      return()
    }
    if (!all(vapply(c("xmin", "xmax", "ymin", "ymax"), function(nm) {
      is.finite(suppressWarnings(as.numeric(brush[[nm]])))
    }, logical(1)))) {
      return()
    }
    if (abs(brush$xmax - brush$xmin) < 2 || abs(brush$ymax - brush$ymin) < 2) {
      return()
    }
    new_roi <- tryCatch(
      brush_to_roi(
        brush,
        app_state$preview_width,
        app_state$preview_height,
        app_state$width,
        app_state$height
      ),
      error = function(e) NULL
    )
    if (is.null(new_roi)) {
      return()
    }
    check <- validate_roi(new_roi, app_state$width, app_state$height)
    if (!isTRUE(check$ok) || identical(new_roi, app_state$roi)) {
      return()
    }
    app_state$roi <- new_roi
    refresh_roi_preview()
    sync_min_area_default()
    clear_detection(status = "stale", message = "ROI updated. Run seed detection again.")
  }, ignoreNULL = TRUE)

  observeEvent(list(input$sensitivity, input$min_area, input$max_area_frac), {
    if (isTRUE(app_state$detection_current) || identical(app_state$detection$status, "ok")) {
      app_state$detection_current <- FALSE
      app_state$detection$status <- "stale"
      app_state$detection$message <- "Parameters changed. Run seed detection again."
      clear_measurements()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$run_detection, {
    if (!isTRUE(app_state$can_preview) || is.null(app_state$magick_img) || is.null(app_state$roi)) {
      clear_detection(status = "error", message = "Upload an image before running detection.")
      return()
    }
    check <- validate_roi(app_state$roi, app_state$width, app_state$height)
    if (!isTRUE(check$ok)) {
      clear_detection(status = "error", message = check$message)
      return()
    }

    params <- make_detection_params(
      sensitivity = finite_input(input$sensitivity, DEFAULT_DETECTION_PARAMS$sensitivity),
      min_area = as.integer(finite_input(input$min_area, 200)),
      max_area_frac = finite_input(input$max_area_frac, DEFAULT_DETECTION_PARAMS$max_area_frac),
      roi_w = roi_width(app_state$roi),
      roi_h = roi_height(app_state$roi)
    )

    result <- tryCatch(
      {
        withProgress(message = "Running seed detection", value = 0, {
          setProgress(value = 0.05, detail = "Preparing ROI...")
          crop <- crop_roi_magick(app_state$magick_img, app_state$roi)
          setProgress(value = 0.15, detail = "Extracting ROI...")

          progress_cb <- function(msg, value) {
            setProgress(value = value, detail = msg)
          }

          det <- detect_seeds_in_magick(
            crop,
            params = params,
            roi = app_state$roi,
            progress = progress_cb
          )
          setProgress(value = 0.95, detail = "Preparing visualization...")
          det
        })
      },
      error = function(e) {
        empty_detection_result(
          status = "error",
          message = "Seed detection failed. Check the ROI and try again."
        )
      }
    )

    app_state$detection <- result
    app_state$detection_current <- identical(result$status, "ok") ||
      identical(result$status, "no_detections")
    refresh_measurements()
  })

  observeEvent(input$start_calibration, begin_calibration(), ignoreNULL = TRUE)
  observeEvent(input$recalibrate, begin_calibration(), ignoreNULL = TRUE)

  observeEvent(input$cancel_calibration, {
    app_state$manual_points <- list()
    app_state$cal_candidate <- NULL
    app_state$cal_message <- NULL
    app_state$cal_mode <- "idle"
  }, ignoreNULL = TRUE)

  observeEvent(input$retry_calibration, {
    app_state$manual_points <- list()
    app_state$cal_candidate <- NULL
    app_state$cal_message <- NULL
    if (isTRUE(app_state$can_preview)) {
      app_state$cal_mode <- "first"
    }
  }, ignoreNULL = TRUE)

  observeEvent(input$confirm_manual_calibration, {
    cand <- app_state$cal_candidate
    if (is.null(cand) || !identical(app_state$cal_mode, "confirm")) {
      return()
    }
    cand$source_filename <- app_state$filename
    cand$image_width <- app_state$width
    cand$image_height <- app_state$height
    cand$confirmed_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
    confirmed <- confirm_calibration(cand)
    if (!is.finite(active_pixels_per_mm(confirmed, app_state$filename))) {
      app_state$cal_message <- "Could not confirm that reference."
      return()
    }
    app_state$calibration <- confirmed
    app_state$cal_candidate <- NULL
    app_state$manual_points <- list()
    app_state$cal_mode <- "idle"
    app_state$cal_message <- NULL
    refresh_measurements()
  }, ignoreNULL = TRUE)

  observeEvent(input$ruler_cal_click, {
    click <- input$ruler_cal_click
    mode <- app_state$cal_mode
    if (!mode %in% c("first", "second")) {
      return()
    }
    if (is.null(click) || is.null(app_state$ruler_roi) ||
          is.null(app_state$ruler_preview_width) || is.null(app_state$ruler_preview_height)) {
      return()
    }
    x <- suppressWarnings(as.numeric(click$x))
    y <- suppressWarnings(as.numeric(click$y))
    pw <- app_state$ruler_preview_width
    ph <- app_state$ruler_preview_height
    if (!is.finite(x) || !is.finite(y) || x < 0 || y < 0 || x > pw || y > ph) {
      app_state$cal_message <- "That point is outside the ruler preview."
      return()
    }
    if (identical(mode, "first")) {
      orig <- preview_point_to_original(x, y, pw, ph, app_state$ruler_roi)
      if (orig$x < -1e-6 || orig$y < -1e-6 ||
            orig$x > app_state$width + 1e-6 || orig$y > app_state$height + 1e-6) {
        app_state$cal_message <- "That point is outside the image."
        return()
      }
      app_state$manual_points <- list(list(
        preview_x = x, preview_y = y, x = orig$x, y = orig$y
      ))
      app_state$cal_candidate <- NULL
      app_state$cal_message <- NULL
      app_state$cal_mode <- "second"
      return()
    }

    p1 <- app_state$manual_points[[1]]
    orig2 <- preview_point_to_original(x, p1$preview_y, pw, ph, app_state$ruler_roi)
    cand <- propose_horizontal_10mm_calibration(
      p1$x, p1$y, orig2$x, p1$y,
      image_width = app_state$width,
      image_height = app_state$height,
      ruler_roi = app_state$ruler_roi
    )
    if (!identical(cand$status, CALIBRATION_STATUS_PROPOSED) ||
          !is.finite(cand$pixels_per_mm) || cand$pixels_per_mm <= 0) {
      app_state$cal_message <- cand$message
      app_state$cal_candidate <- NULL
      return()
    }
    app_state$manual_points <- list(
      p1,
      list(preview_x = x, preview_y = p1$preview_y, x = orig2$x, y = p1$y)
    )
    app_state$cal_candidate <- cand
    app_state$cal_message <- NULL
    app_state$cal_mode <- "confirm"
  }, ignoreNULL = TRUE)

  session$onSessionEnded(function() {
    isolate({
      clear_image_artifacts()
    })
  })

  status_class <- reactive({
    switch(
      app_state$status,
      ready = "status-ready",
      invalid_filename = "status-warn",
      unreadable = "status-error",
      error = "status-error",
      "status-empty"
    )
  })

  status_label <- reactive({
    switch(
      app_state$status,
      empty = "Waiting for upload",
      ready = "Ready",
      invalid_filename = "Accession identifier unavailable",
      unreadable = "Unreadable image",
      error = "Upload error",
      app_state$status
    )
  })

  output$upload_status_ui <- renderUI({
    if (identical(app_state$status, "empty")) {
      return(NULL)
    }
    if (identical(app_state$status, "ready")) {
      return(p(class = "status-ready mb-2", "Ready — Image uploaded"))
    }
    tagList(
      p(class = status_class(), status_label()),
      p(class = "small mb-2", app_state$message)
    )
  })

  output$image_meta_table <- renderTable({
    accession_display <- if (is.null(app_state$accession_id) || is.na(app_state$accession_id)) {
      "Unavailable"
    } else {
      app_state$accession_id
    }
    filename_display <- if (is.null(app_state$filename) || is.na(app_state$filename)) {
      "—"
    } else {
      app_state$filename
    }
    dims_display <- if (
      is.null(app_state$width) || is.na(app_state$width) ||
        is.null(app_state$height) || is.na(app_state$height)
    ) {
      "—"
    } else {
      paste0(app_state$width, " × ", app_state$height, " px")
    }
    data.frame(
      Field = c("Original filename", "Accession identifier", "Image dimensions", "Upload status"),
      Value = c(filename_display, accession_display, dims_display, status_label()),
      stringsAsFactors = FALSE
    )
  }, colnames = FALSE, width = "100%", class = "meta-table table-sm")

  output$roi_status_ui <- renderUI({
    if (is.null(app_state$roi)) {
      return(p(class = "text-muted small mb-0", "ROI: not initialized"))
    }
    roi <- app_state$roi
    p(
      class = "text-muted small mb-0",
      sprintf(
        "ROI [x=%d,%d) × [y=%d,%d)  (%d × %d px)",
        roi$x_min, roi$x_max, roi$y_min, roi$y_max,
        roi_width(roi), roi_height(roi)
      )
    )
  })

  output$calibration_debug_ui <- renderUI({
    if (!isTRUE(input$debug_mode)) {
      return(NULL)
    }
    cal <- if (identical(app_state$cal_mode, "confirm") && !is.null(app_state$cal_candidate)) {
      app_state$cal_candidate
    } else {
      app_state$calibration
    }
    rows <- manual_calibration_debug_rows(cal)
    warn_ui <- NULL
    if (!is.null(cal) && length(cal$warnings) > 0L) {
      warn_ui <- tags$ul(
        class = "small mb-1",
        lapply(cal$warnings, function(w) tags$li(w))
      )
    }
    tagList(
      p(class = "small text-muted mb-1", "Automatic calibration is not used."),
      if (!is.null(cal) && !is.null(cal$message) && nzchar(cal$message)) {
        p(class = "small mb-1", cal$message)
      },
      tags$table(
        class = "table table-sm cal-table mb-1",
        lapply(seq_len(nrow(rows)), function(i) {
          tags$tr(tags$td(rows$label[[i]]), tags$td(rows$value[[i]]))
        })
      ),
      warn_ui
    )
  })

  output$calibration_compact_ui <- renderUI({
    mode <- app_state$cal_mode
    if (!is.null(app_state$cal_message) && nzchar(app_state$cal_message) &&
          mode %in% c("first", "second")) {
      message_ui <- p(class = "status-error mb-1", app_state$cal_message)
    } else {
      message_ui <- NULL
    }
    if (identical(mode, "first")) {
      return(tagList(
        message_ui,
        div(
          class = "cal-compact",
          p(class = "status-warn", "Click the first ruler mark."),
          action_button("cancel_calibration", "Cancel", "btn btn-link btn-sm")
        )
      ))
    }
    if (identical(mode, "second")) {
      return(tagList(
        message_ui,
        div(
          class = "cal-compact",
          p(class = "status-warn", "Click a second mark to the right."),
          action_button("cancel_calibration", "Cancel", "btn btn-link btn-sm")
        )
      ))
    }
    if (identical(mode, "confirm")) {
      return(div(
        class = "cal-compact",
        p(class = "status-warn", "Confirm the 10 mm reference on the preview.")
      ))
    }
    ppm <- active_pixels_per_mm(app_state$calibration, app_state$filename)
    if (is.finite(ppm)) {
      return(div(
        class = "cal-compact",
        p(class = "status-ready", "Reference confirmed"),
        action_button("recalibrate", "Recalibrate", "btn btn-outline-secondary btn-sm")
      ))
    }
    div(
      class = "cal-compact",
      p(class = "status-empty", "Not calibrated"),
      action_button("start_calibration", "Calibrate", "btn btn-outline-primary btn-sm")
    )
  })

  output$workspace_ui <- renderUI({
    if (!isTRUE(app_state$can_preview)) {
      return(div(
        class = "empty-state",
        h5("No image loaded"),
        p(class = "mb-0", "Upload a JPEG or PNG image to begin.")
      ))
    }
    pw <- app_state$preview_width
    ph <- app_state$preview_height
    rw <- app_state$ruler_preview_width
    rh <- app_state$ruler_preview_height
    tagList(
      p(
        class = "workspace-hint",
        "Drag on the image to set the region of interest. Right-click the region to reset it."
      ),
      div(class = "section-label", "Full image"),
      div(
        id = "full_plot_stage",
        class = "plot-frame plot-stage",
        `data-pw` = if (is.null(pw)) "" else pw,
        `data-ph` = if (is.null(ph)) "" else ph,
        plotOutput("full_preview_plot", height = "auto", width = "100%"),
        uiOutput("roi_overlay_ui")
      ),
      div(class = "section-label", "ROI preview + detections"),
      div(
        id = "roi_plot_stage",
        class = "plot-frame plot-stage plot-stage-roi",
        plotOutput("roi_preview_plot", height = "auto", width = "100%")
      ),
      div(class = "section-label", "Ruler calibration"),
      uiOutput("calibration_compact_ui"),
      div(
        id = "ruler_plot_stage",
        class = "plot-frame plot-stage",
        `data-pw` = if (is.null(rw)) "" else rw,
        `data-ph` = if (is.null(rh)) "" else rh,
        plotOutput("ruler_preview_plot", height = "auto", width = "100%"),
        uiOutput("ruler_overlay_ui")
      ),
      p(
        class = "cal-note",
        "The reference distance is exactly 10 mm and must be a horizontal line. ",
        "If the ruler is tilted in the image, AlfalfaCV does not correct it — ",
        "align the ruler horizontally before calibrating."
      )
    )
  })

  output$roi_overlay_ui <- renderUI({
    if (!isTRUE(app_state$can_preview) || is.null(app_state$roi) ||
          is.null(app_state$preview_width) || is.null(app_state$preview_height)) {
      return(NULL)
    }
    pr <- roi_to_preview(
      app_state$roi,
      app_state$preview_width,
      app_state$preview_height,
      app_state$width,
      app_state$height
    )
    handles <- lapply(c("nw", "n", "ne", "e", "se", "s", "sw", "w"), function(handle) {
      tags$span(class = paste("roi-handle", handle), `data-handle` = handle)
    })
    div(
      class = "interaction-layer",
      div(
        class = "roi-box",
        style = roi_box_style(
          app_state$roi,
          app_state$preview_width,
          app_state$preview_height,
          app_state$width,
          app_state$height
        ),
        `data-xmin` = pr$x_min,
        `data-ymin` = pr$y_min,
        `data-xmax` = pr$x_max,
        `data-ymax` = pr$y_max,
        handles
      )
    )
  })

  output$ruler_overlay_ui <- renderUI({
    if (!isTRUE(app_state$can_preview)) {
      return(NULL)
    }
    mode <- if (is.null(app_state$cal_mode)) "idle" else app_state$cal_mode
    pts <- app_state$manual_points
    x1 <- if (length(pts) >= 1L) pts[[1]]$preview_x else ""
    y1 <- if (length(pts) >= 1L) pts[[1]]$preview_y else ""
    pop <- NULL
    if (identical(mode, "confirm") && !is.null(app_state$cal_candidate) && length(pts) >= 2L) {
      dist <- app_state$cal_candidate$manual_points$distance_px
      pw <- as.numeric(app_state$ruler_preview_width)
      ph <- as.numeric(app_state$ruler_preview_height)
      left <- 100 * ((pts[[1]]$preview_x + pts[[2]]$preview_x) / 2) / pw
      top <- 100 * pts[[1]]$preview_y / ph
      pop <- div(
        class = "cal-popover",
        style = sprintf("left:%.3f%%;top:%.3f%%;", left, top),
        p(tags$strong(sprintf("%.1f px apart", dist))),
        p("Confirm these marks are exactly 10 mm apart?"),
        div(
          class = "d-flex flex-wrap gap-1",
          action_button("confirm_manual_calibration", "Confirm", "btn btn-success btn-sm"),
          action_button("retry_calibration", "Retry", "btn btn-outline-secondary btn-sm"),
          action_button("cancel_calibration", "Cancel", "btn btn-link btn-sm")
        )
      )
    }
    div(
      class = "interaction-layer",
      `data-cal-mode` = mode,
      `data-x1` = x1,
      `data-y1` = y1,
      pop
    )
  })

  output$full_preview_plot <- renderPlot({
    req(app_state$preview_raster, app_state$preview_width, app_state$preview_height)
    w <- app_state$preview_width
    h <- app_state$preview_height
    op <- par(mar = c(0, 0, 0, 0))
    on.exit(par(op), add = TRUE)
    plot_image_topleft(app_state$preview_raster, w, h)
    if (!is.null(app_state$roi)) {
      pr <- roi_to_preview(
        app_state$roi, w, h, app_state$width, app_state$height
      )
      rect(
        pr$x_min, pr$y_min, pr$x_max, pr$y_max,
        border = "#00E5FF", lwd = 2
      )
    }
    if (!is.null(app_state$ruler_roi)) {
      rr <- roi_to_preview(
        app_state$ruler_roi, w, h, app_state$width, app_state$height
      )
      rect(
        rr$x_min, rr$y_min, rr$x_max, rr$y_max,
        border = "#FFD60A", lwd = 1.5, lty = 2
      )
    }
  }, width = function() preview_device_size("full")$width,
     height = function() preview_device_size("full")$height,
     execOnResize = FALSE, res = 72)

  output$roi_preview_plot <- renderPlot({
    req(app_state$roi_raster, app_state$roi_preview_width, app_state$roi_preview_height)
    w <- app_state$roi_preview_width
    h <- app_state$roi_preview_height
    scale <- app_state$roi_scale
    op <- par(mar = c(0, 0, 0, 0))
    on.exit(par(op), add = TRUE)
    plot_image_topleft(app_state$roi_raster, w, h)

    det <- app_state$detection
    if (!is.null(det) && length(det$detections) > 0L &&
          det$status %in% c("ok", "stale")) {
      for (d in det$detections) {
        x0 <- d$bbox_roi$x_min * scale
        x1 <- d$bbox_roi$x_max * scale
        y0 <- d$bbox_roi$y_min * scale
        y1 <- d$bbox_roi$y_max * scale
        col <- if (identical(det$status, "stale")) "#FFB000" else "#FF3B30"
        rect(x0, y0, x1, y1, border = col, lwd = 2)
        text(
          x0 + 4, y0 + 4,
          labels = d$id,
          col = col,
          cex = 1.1,
          font = 2,
          adj = c(0, 1)
        )
      }
    }
  }, width = function() preview_device_size("roi")$width,
     height = function() preview_device_size("roi")$height,
     execOnResize = FALSE, res = 72)

  output$ruler_preview_plot <- renderPlot({
    req(app_state$ruler_raster, app_state$ruler_preview_width, app_state$ruler_preview_height)
    w <- app_state$ruler_preview_width
    h <- app_state$ruler_preview_height
    op <- par(mar = c(0, 0, 0, 0))
    on.exit(par(op), add = TRUE)
    plot_image_topleft(app_state$ruler_raster, w, h)

    cal <- app_state$calibration
    if (isTRUE(cal$confirmed) && identical(cal$method, "manual") && is.list(cal$manual_points)) {
      mp <- cal$manual_points
      draw_reference_segment(mp$x1, mp$y1, mp$x2, mp$y2, w, h, app_state$ruler_roi, "#3a7d44", 2)
    }
    if (identical(app_state$cal_mode, "confirm") && !is.null(app_state$cal_candidate)) {
      mp <- app_state$cal_candidate$manual_points
      draw_reference_segment(mp$x1, mp$y1, mp$x2, mp$y2, w, h, app_state$ruler_roi, "#00E5FF", 2)
    } else if (identical(app_state$cal_mode, "second") && length(app_state$manual_points) >= 1L) {
      p1 <- app_state$manual_points[[1]]
      marker <- orig_to_ruler_preview(p1$x, p1$y, w, h, app_state$ruler_roi)
      points(marker$x, marker$y, pch = 3, col = "#00E5FF", lwd = 2, cex = 1.3)
    }
  }, width = function() preview_device_size("ruler")$width,
     height = function() preview_device_size("ruler")$height,
     execOnResize = FALSE, res = 72)

  output$detection_results_ui <- renderUI({
    det <- app_state$detection
    if (is.null(det) || identical(det$status, "empty")) {
      return(p(class = "mb-0 text-muted", "No analysis available."))
    }

    status_txt <- switch(
      det$status,
      ok = "Detection complete",
      stale = "Results outdated",
      no_detections = "No candidates",
      error = "Detection error",
      det$status
    )
    status_cls <- switch(
      det$status,
      ok = "status-ready",
      stale = "status-warn",
      no_detections = "status-warn",
      error = "status-error",
      "status-empty"
    )

    warn_ui <- NULL
    if (length(det$warnings) > 0L) {
      warn_ui <- tags$ul(lapply(det$warnings, function(w) tags$li(w)))
    }
    param_ui <- NULL
    if (isTRUE(input$debug_mode) && is.list(det$params)) {
      param_ui <- p(
        class = "small text-muted mb-1",
        sprintf(
          "Parameters used: sensitivity %.2f, min area %s px, max area fraction %.2f.",
          det$params$sensitivity, det$params$min_area, det$params$max_area_frac
        )
      )
    }

    tagList(
      p(class = status_cls, status_txt),
      p(class = "mb-1", det$message),
      param_ui,
      if (det$count > 0L) {
        p(
          class = "mb-1",
          sprintf(
            "%d seed candidate%s detected",
            det$count,
            if (det$count == 1L) "" else "s"
          )
        )
      },
      warn_ui
    )
  })

  output$measurements_status_ui <- renderUI({
    cal <- app_state$calibration
    det <- app_state$detection
    if (!isTRUE(app_state$detection_current) || is.null(det) ||
          identical(det$status, "empty") || identical(det$status, "stale")) {
      return(p(class = "mb-1 text-muted", "Run seed detection to compute measurements."))
    }
    if (identical(det$status, "no_detections") || length(det$detections) == 0L) {
      return(p(class = "mb-1 text-muted", "No detections to measure."))
    }
    ppm <- active_pixels_per_mm(cal, app_state$filename)
    if (is.finite(ppm) && ppm > 0) {
      p(class = "mb-1 status-ready", "Physical units use the confirmed 10 mm reference.")
    } else {
      p(
        class = "mb-1 status-warn",
        "Pixel and shape metrics are shown. Millimetre units need a confirmed 10 mm calibration."
      )
    }
  })

  measurements_are_current <- function() {
    ppm_now <- active_pixels_per_mm(app_state$calibration, app_state$filename)
    ppm_used <- app_state$measurements_ppm
    same_ppm <- (is.na(ppm_now) && is.na(ppm_used)) ||
      (is.finite(ppm_now) && is.finite(ppm_used) && abs(ppm_now - ppm_used) < 1e-8)
    same_ppm &&
      identical(app_state$measurements_file, app_state$filename) &&
      identical(app_state$measurements_roi_tag, roi_state_tag(app_state$roi))
  }

  output$measurements_table <- renderTable({
    det <- app_state$detection
    if (!isTRUE(app_state$measurements_current) ||
          is.null(det) ||
          !det$status %in% c("ok", "no_detections") ||
          !isTRUE(app_state$detection_current) ||
          !isTRUE(measurements_are_current())) {
      return(data.frame(Message = "No measurements available."))
    }
    df <- app_state$measurements
    ppm <- active_pixels_per_mm(app_state$calibration, app_state$filename)
    measurements_display_table(df, calibrated = is.finite(ppm) && ppm > 0)
  }, striped = TRUE, spacing = "s", width = "100%", digits = 3)
}

shinyApp(ui = ui, server = server)
