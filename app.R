# AlfalfaCV — Phase 1A: USDA Display Draft (Seed Morphology Analysis)
# Local Shiny foundation: upload, accession parsing, image display, placeholders.
# Seed detection and morphological analysis are intentionally not implemented.

library(shiny)
library(bslib)

source(file.path("R", "image_io.R"), local = TRUE)

# Reasonable upload limit for large research photographs (MB).
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
      .meta-table td:first-child {
        font-weight: 600;
        width: 40%;
        color: #334;
      }
      .workspace-panel {
        min-height: 28rem;
      }
      .image-frame {
        background: #111;
        border: 1px solid #d0d7de;
        border-radius: 0.35rem;
        padding: 0.75rem;
        display: flex;
        align-items: center;
        justify-content: center;
        min-height: 24rem;
        max-height: 70vh;
        overflow: auto;
      }
      .image-frame img {
        max-width: 100%;
        max-height: 65vh;
        height: auto;
        width: auto;
        object-fit: contain;
      }
      .empty-state {
        color: #6c757d;
        text-align: center;
        padding: 2rem 1rem;
      }
      .placeholder-card {
        background: #f8f9fa;
        border: 1px dashed #c5ccd3;
        border-radius: 0.35rem;
        padding: 1rem;
        margin-bottom: 0.85rem;
      }
      .placeholder-card h5 {
        margin-bottom: 0.35rem;
        font-size: 1rem;
      }
      .status-ready { color: #2c5f2d; font-weight: 600; }
      .status-warn { color: #9a6700; font-weight: 600; }
      .status-error { color: #8b3a3a; font-weight: 600; }
      .status-empty { color: #6c757d; font-weight: 600; }
    "))
  ),
  title = "AlfalfaCV",

  layout_columns(
    col_widths = 12,
    card(
      card_body(
        class = "p-3",
        h2("AlfalfaCV", class = "mb-1"),
        p(
          class = "text-muted mb-0",
          "USDA Display Draft — Seed Morphology Analysis"
        )
      )
    )
  ),

  layout_columns(
    col_widths = c(3, 6, 3),

    # A. Image Input -----------------------------------------------------------
    card(
      card_header("Image Input"),
      card_body(
        fileInput(
          inputId = "seed_image",
          label = "Upload seed image",
          multiple = FALSE,
          accept = c(".jpg", ".jpeg", ".png", "image/jpeg", "image/png"),
          width = "100%"
        ),
        hr(),
        h5("Upload details"),
        uiOutput("upload_status_ui"),
        tableOutput("image_meta_table")
      )
    ),

    # B. Image Workspace -------------------------------------------------------
    card(
      class = "workspace-panel",
      card_header("Image Workspace"),
      card_body(
        div(
          class = "image-frame",
          conditionalPanel(
            condition = "!output.show_preview",
            uiOutput("image_empty_state")
          ),
          conditionalPanel(
            condition = "output.show_preview",
            imageOutput("seed_preview", width = "100%", height = "auto")
          )
        )
      )
    ),

    # C. Results Workspace -----------------------------------------------------
    card(
      card_header("Results Workspace"),
      card_body(
        div(
          class = "placeholder-card",
          h5("1. Seed Detection"),
          p(class = "mb-0 text-muted", "No analysis available.")
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
        ),
        hr(),
        tags$button(
          id = "run_analysis",
          class = "btn btn-secondary action-button disabled",
          disabled = NA,
          style = "width: 100%;",
          "Run analysis (unavailable)"
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Session-scoped state. Future phases can extend this list without
  # rewriting the upload/display flow (e.g., detections, calibration).
  app_state <- reactiveValues(
    filepath = NULL,
    filename = NULL,
    accession_id = NULL,
    width = NULL,
    height = NULL,
    status = "empty",
    message = "No image uploaded.",
    can_preview = FALSE
  )

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

    # Clear previous upload state immediately so a failed/replaced upload
    # cannot leave stale metadata from the prior image.
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
  }, ignoreNULL = TRUE)

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

  output$show_preview <- reactive({
    isTRUE(app_state$can_preview)
  })
  outputOptions(output, "show_preview", suspendWhenHidden = FALSE)

  output$upload_status_ui <- renderUI({
    tagList(
      p(
        class = status_class(),
        status_label()
      ),
      p(app_state$message)
    )
  })

  output$image_meta_table <- renderTable({
    accession_display <- if (
      is.null(app_state$accession_id) || is.na(app_state$accession_id)
    ) {
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
      Field = c(
        "Original filename",
        "Accession identifier",
        "Image dimensions",
        "Upload status"
      ),
      Value = c(filename_display, accession_display, dims_display, status_label()),
      stringsAsFactors = FALSE
    )
  }, colnames = FALSE, width = "100%", class = "meta-table table-sm")

  output$image_empty_state <- renderUI({
    if (identical(app_state$status, "empty")) {
      div(
        class = "empty-state",
        h5("No image loaded"),
        p(class = "mb-0", "Upload a JPEG or PNG seed image to preview it here.")
      )
    } else {
      div(
        class = "empty-state",
        h5("Preview unavailable"),
        p(class = "mb-0", app_state$message)
      )
    }
  })

  output$seed_preview <- renderImage({
    req(isTRUE(app_state$can_preview))
    req(!is.null(app_state$filepath), file.exists(app_state$filepath))

    list(
      src = app_state$filepath,
      contentType = if (identical(get_file_extension(app_state$filename), "png")) {
        "image/png"
      } else {
        "image/jpeg"
      },
      alt = paste("Uploaded seed image:", app_state$filename),
      width = "100%",
      height = "auto"
    )
  }, deleteFile = FALSE)
}

shinyApp(ui = ui, server = server)
