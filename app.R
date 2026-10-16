# AlfalfaCV — Phase 1B: USDA Display Draft (Seed Detection Foundation)
# Upload, accession parsing, ROI selection, seed segmentation, detection overlays.

library(shiny)
library(bslib)
library(magick)
library(mmand)

source(file.path("R", "image_io.R"), local = TRUE)
source(file.path("R", "roi.R"), local = TRUE)
source(file.path("R", "image_processing.R"), local = TRUE)
source(file.path("R", "seed_detection.R"), local = TRUE)

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

ui <- page_fluid(
  theme = app_theme,
  tags$head(
    tags$style(HTML("
      .meta-table td:first-child { font-weight: 600; width: 42%; color: #334; }
      .workspace-panel { min-height: 30rem; }
      .plot-frame {
        background: #111; border: 1px solid #d0d7de; border-radius: 0.35rem;
        padding: 0.4rem; margin-bottom: 0.75rem;
      }
      .empty-state { color: #6c757d; text-align: center; padding: 2rem 1rem; }
      .placeholder-card {
        background: #f8f9fa; border: 1px dashed #c5ccd3; border-radius: 0.35rem;
        padding: 0.85rem; margin-bottom: 0.75rem;
      }
      .placeholder-card h5 { margin-bottom: 0.35rem; font-size: 1rem; }
      .status-ready { color: #2c5f2d; font-weight: 600; }
      .status-warn { color: #9a6700; font-weight: 600; }
      .status-error { color: #8b3a3a; font-weight: 600; }
      .status-empty { color: #6c757d; font-weight: 600; }
      .section-label { font-size: 0.85rem; font-weight: 600; margin-top: 0.5rem; }
    "))
  ),
  title = "AlfalfaCV",

  layout_columns(
    col_widths = 12,
    card(
      card_body(
        class = "p-3",
        h2("AlfalfaCV", class = "mb-1"),
        p(class = "text-muted mb-0", "USDA Display Draft — Seed Morphology Analysis")
      )
    )
  ),

  layout_columns(
    col_widths = c(3, 6, 3),

    card(
      card_header("Image Input"),
      card_body(
        fileInput(
          "seed_image",
          "Upload seed image",
          multiple = FALSE,
          accept = c(".jpg", ".jpeg", ".png", "image/jpeg", "image/png"),
          width = "100%"
        ),
        uiOutput("upload_status_ui"),
        tableOutput("image_meta_table"),
        hr(),
        div(class = "section-label", "Region of interest"),
        checkboxInput("adjust_roi", "Adjust ROI (brush on full preview)", value = FALSE),
        div(
          class = "d-flex gap-2",
          actionButton("reset_roi", "Reset ROI", class = "btn-outline-secondary btn-sm")
        ),
        uiOutput("roi_status_ui"),
        hr(),
        div(class = "section-label", "Detection parameters"),
        sliderInput(
          "sensitivity",
          "Detection sensitivity",
          min = 0, max = 1, value = DEFAULT_DETECTION_PARAMS$sensitivity, step = 0.05
        ),
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
        actionButton(
          "run_detection",
          "Run Seed Detection",
          class = "btn-primary",
          width = "100%"
        )
      )
    ),

    card(
      class = "workspace-panel",
      card_header("Image Workspace"),
      card_body(
        uiOutput("workspace_ui")
      )
    ),

    card(
      card_header("Results Workspace"),
      card_body(
        div(
          class = "placeholder-card",
          h5("1. Seed Detection"),
          uiOutput("detection_results_ui")
        ),
        div(
          class = "placeholder-card",
          h5("2. Individual Measurements"),
          p(class = "mb-0 text-muted", "No measurements available.")
        ),
        div(
          class = "placeholder-card",
          h5("3. Statistical Summary"),
          p(class = "mb-0 text-muted", "No results available.")
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
    preview_width = NULL,
    preview_height = NULL,
    roi = NULL,
    roi_raster = NULL,
    roi_preview_width = NULL,
    roi_preview_height = NULL,
    roi_scale = 1,
    detection = empty_detection_result(),
    detection_current = FALSE
  )

  clear_detection <- function(status = "empty", message = "No detection has been run.") {
    app_state$detection <- empty_detection_result(status = status, message = message)
    app_state$detection_current <- FALSE
  }

  clear_image_artifacts <- function() {
    app_state$magick_img <- NULL
    app_state$preview_raster <- NULL
    app_state$preview_width <- NULL
    app_state$preview_height <- NULL
    app_state$roi <- NULL
    app_state$roi_raster <- NULL
    app_state$roi_preview_width <- NULL
    app_state$roi_preview_height <- NULL
    app_state$roi_scale <- 1
    clear_session_image_dir(app_state$session_dir)
    app_state$session_dir <- NULL
    clear_detection()
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
        list(
          session_dir = session_dir,
          local_path = local_path,
          img = img,
          dims = dims,
          preview = preview,
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
    app_state$preview_width <- prepared$preview$width
    app_state$preview_height <- prepared$preview$height
    app_state$roi <- initialize_default_roi(prepared$dims$width, prepared$dims$height)
    refresh_roi_preview()
    sync_min_area_default()
    clear_detection(status = "empty", message = "No detection has been run.")
  }, ignoreNULL = TRUE)

  observeEvent(input$reset_roi, {
    req(isTRUE(app_state$can_preview), !is.null(app_state$width))
    app_state$roi <- initialize_default_roi(app_state$width, app_state$height)
    refresh_roi_preview()
    sync_min_area_default()
    clear_detection(status = "stale", message = "ROI reset. Run seed detection again.")
  })

  observeEvent(input$full_preview_brush, {
    if (!isTRUE(input$adjust_roi)) {
      return()
    }
    brush <- input$full_preview_brush
    if (is.null(brush) || is.null(app_state$preview_width)) {
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
    if (!isTRUE(check$ok)) {
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
      sensitivity = input$sensitivity,
      min_area = as.integer(input$min_area),
      max_area_frac = as.numeric(input$max_area_frac),
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
  })

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
    tagList(
      p(class = status_class(), status_label()),
      p(app_state$message)
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

  output$workspace_ui <- renderUI({
    if (!isTRUE(app_state$can_preview)) {
      return(div(
        class = "empty-state",
        h5("No image loaded"),
        p(class = "mb-0", "Upload a JPEG or PNG seed image to begin.")
      ))
    }
    tagList(
      div(class = "section-label", "Full image + ROI"),
      div(
        class = "plot-frame",
        plotOutput(
          "full_preview_plot",
          height = "360px",
          brush = brushOpts(
            id = "full_preview_brush",
            resetOnNew = TRUE,
            clip = TRUE
          )
        )
      ),
      div(class = "section-label", "ROI preview + detections"),
      div(
        class = "plot-frame",
        plotOutput("roi_preview_plot", height = "420px")
      )
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
  })

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
  })

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

    tagList(
      p(class = status_cls, status_txt),
      p(class = "mb-1", det$message),
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
}

shinyApp(ui = ui, server = server)
