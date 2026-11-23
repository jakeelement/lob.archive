

lobster_archive <- function() {

  # ============================================================
  # 1. PACKAGES AND ORACLE CONNECTION
  # ============================================================

  required_packages <- c(
    "shiny", "shinyjs",
    "sortable", "DBI", "ROracle", "shinyWidgets"
  )

  missing_packages <- required_packages[
    !vapply(required_packages, requireNamespace,
            logical(1), quietly = TRUE)
  ]

  if (length(missing_packages)) {
    stop(
      "Please install the following packages: ",
      paste(missing_packages, collapse = ", ")
    )
  }

  # Retrieve credentials from R objects, or environment variables
  get_credential <- function(name) {
    value <- get0(name, envir = .GlobalEnv, inherits = FALSE)

    if (is.null(value) || !nzchar(as.character(value))) {
      value <- Sys.getenv(name, unset = "")
    }

    if (!nzchar(value)) {
      stop("Missing Oracle credential: ", name)
    }

    value
  }

  con <- DBI::dbConnect(
    ROracle::Oracle(),
    username = get_credential("oracle.username"),
    password = get_credential("oracle.password"),
    dbname   = get_credential("oracle.server")
  )

  # ============================================================
  # 2. DATABASE HELPERS
  # ============================================================

  arch_table <- "LOBARCHIVE"
  species_table <- "LOBARCHIVE_SPECIES"

  get_archive <- function(table_name = NULL) {
    DBI::dbGetQuery(
      con,
      paste0("SELECT * FROM ", table_name)
    )
  }

  get_columns <- function(table_name = NULL) {
    names(DBI::dbGetQuery(
      con,
      paste0("SELECT * FROM ", table_name, " WHERE 1=0")
    ))
  }

  get_tags <- function() {
    setdiff(
      get_columns(arch_table),
      c("URI", "DOCUMENT_NAME", "PAGES")
    )
  }

  get_existing_files <- function() {
    data <- get_archive(arch_table)

    # Strip the L:/ prefix and return just the filenames
    basename(gsub("\\\\", "/", data$URI))
  }

  # Safely quote SQL character values
  sql_string <- function(x) {
    if (is.na(x)) {
      return("NULL")
    }

    as.character(DBI::dbQuoteString(con, as.character(x)))
  }

  # Insert one new archive record
  insert_arch_record <- function(uri, document_name, pages, tags) {

    columns <- get_columns(arch_table)
    tag_columns <- get_tags()

    values <- list()

    for (column in columns) {

      if (column == "URI") {
        values[[column]] <- sql_string(uri)

      } else if (column == "DOCUMENT_NAME") {
        values[[column]] <- sql_string(document_name)

      } else if (column == "PAGES") {
        values[[column]] <- as.character(as.integer(pages))

      } else if (column %in% tag_columns) {

        values[[column]] <- sql_string(
          if (column %in% tags) "Y" else "N"
        )
      }
    }

    sql <- paste0(
      "INSERT INTO ", arch_table,
      " (", paste(
        DBI::dbQuoteIdentifier(con, columns),
        collapse = ", "
      ), ") VALUES (",
      paste(unlist(values), collapse = ", "),
      ")"
    )

    DBI::dbExecute(con, sql)
  }


  insert_species_record <- function(uri, species_name, species_code) {

    columns <- get_columns(species_table)

    values <- list()

    for (column in columns) {

      if (column == "URI") {
        values[[column]] <- sql_string(uri)

      } else if (column == "SPECIES_NAME") {
        values[[column]] <- sql_string(species_name)

      } else if (column == "SPECIES_CODE") {
        values[[column]] <- as.character(as.integer(species_code))

      }
    }
    sql <- paste0(
      "INSERT INTO ", species_table,
      " (", paste(
        DBI::dbQuoteIdentifier(con, columns),
        collapse = ", "
      ), ") VALUES (",
      paste(unlist(values), collapse = ", "),
      ")"
    )

    DBI::dbExecute(con, sql)
    }

  # Create a new tag column
  create_tag <- function(tag_name) {

    tag_name <- toupper(gsub(
      "[^A-Za-z0-9_]", "_", trimws(tag_name)
    ))

    if (!grepl("^[A-Z][A-Z0-9_]*$", tag_name)) {
      stop("Tag names must begin with a letter.")
    }

    if (nchar(tag_name, type = "bytes") > 30) {
      stop("Oracle tag names must be 30 bytes or fewer.")
    }

    if (tag_name %in% get_columns()) {
      stop("This tag already exists.")
    }

    DBI::dbExecute(
      con,
      paste0(
        "ALTER TABLE ", arch_table,
        " ADD (",
        as.character(DBI::dbQuoteIdentifier(con, tag_name)),
        " VARCHAR2(1) DEFAULT 'N' NOT NULL)"
      )
    )

    tag_name
  }


  # ============================================================
  # 3. SHINY UI
  # ============================================================

  ui <- shiny::fluidPage(

    shinyjs::useShinyjs(),

    shiny::tags$head(

      shiny::tags$style(shiny::HTML("

    /* Fixed dimensions for drag-and-drop windows */

    .bucket-list-container {
      height: 400px !important;
      max-height: 400px !important;
      overflow: hidden !important;
    }

    /* Individual bucket windows */

    .rank-list-container {
      height: 400px !important;
      max-height: 400px !important;
      overflow-y: auto !important;
      overflow-x: hidden !important;
    }

    /* Keep individual filenames readable */

    .rank-list-item {
      overflow-wrap: anywhere;
      white-space: normal;
    }

     /* Unchecked button state */
      .btn-white {
        background-color: #ffffff;
        color: #333333;
        border-color: #cccccc;
      }
      /* Checked/active button state */
      .btn-white.active, .btn-white:active {
          background-color: #0D6EFD !important;
          color: #ffffff !important;
          border-color: #0D6EFD !important;
      }
  .selected-tags-summary {
    white-space: pre-line;
  }
  "))

    ),

    shiny::sidebarLayout(

      # --------------------------------------------------------
      # Sidebar: directory mapping
      # --------------------------------------------------------

      shiny::sidebarPanel(

        shiny::h4("Archive Directory"),

        shiny::textInput(
          "pdf_dir_path",
          "Archive Folder:",
          value = lob.archive.dir,
          placeholder = "Paste or enter the folder path..."
        ),

        shiny::actionButton(
          "browse_pdf_dir",
          "Browse...",
          icon = shiny::icon("folder-open")
        ),

        shiny::actionButton(
          "set_pdf_dir",
          "Set Directory"
        ),

        shiny::textOutput("pdf_dir_display"),

        shiny::hr(),

        shiny::h4("Export Directory"),

        shiny::textInput(
          "export_dir_path",
          "Export Folder:",
          value = "",
          placeholder = "Paste or enter the export path..."
        ),

        shiny::actionButton(
          "browse_export_dir",
          "Browse...",
          icon = shiny::icon("folder-open")
        ),

        shiny::actionButton(
          "set_export_dir",
          "Set Directory"
        ),

        shiny::textOutput("export_dir_display"),

        width = 1
      ),

      # --------------------------------------------------------
      # Main window: two views
      # --------------------------------------------------------

      shiny::mainPanel(

        shiny::tabsetPanel(

          id = "main_tabs",

          # ====================================================
          # VIEW 1: ARCHIVE RESOURCE
          # ====================================================

          shiny::tabPanel(

            "Archive Resource",

            shiny::br(),

            shiny::textOutput("unarchived_count"),

            shiny::uiOutput("file_buckets"),

            shiny::uiOutput("duplicate_warning"),

            shiny::hr(),

            shiny::h4("Document Information"),

            shiny::p(
              "The following information will be applied ",
              "to all currently selected files."
            ),

            shiny::fluidRow(

              shiny::column(
                2,

            shiny::textInput(
              "document_name",
              "Document Name:"
            )
            ),
            shiny::column(
              1,
            shiny::numericInput(
              "pages",
              "#Pages:",
              value = NA,
              min = 1,
              step = 1
            )),

            shiny::column(
              2,

              shiny::textInput(
                "species",
                "Species:"
              )
            ),

            shiny::column(
              8,
            shiny::uiOutput("archive_tag_options")
            ),



            ),

            shiny::hr(),

            shiny::h4("Create New Tag"),

            shiny::fluidRow(

              shiny::column(
                4,

                shiny::textInput(
                  "new_tag",
                  "New Tag Name:",
                  placeholder = "e.g. TEMPERATURE"
                )
              ),

              shiny::column(
                5,

                shiny::br(),

                shiny::actionButton(
                  "create_tag",
                  "Create Tag"
                )
              )
            ),

            shiny::hr(),

            shiny::actionButton(
              "enter_data",
              "Enter Data",
              class = "btn-success"
            )

          ),


          # ====================================================
          # VIEW 2: RETRIEVE RESOURCE
          # ====================================================

          shiny::tabPanel(

            "Retrieve Resource",

            shiny::br(),

            shiny::h4("Search Archive"),

            shiny::p(
              "Select one or more tags to find documents ",
              "associated with those tags."
            ),

            shiny::uiOutput("retrieve_tag_options"),

            shiny::actionButton(
              "search_archive",
              "Search Archive"
            ),

            shiny::hr(),

            shiny::h4("Matching Documents"),

            shiny::textOutput("result_count"),

            shiny::tableOutput("search_results"),

            shiny::hr(),

            shiny::h4("Export Selected Documents"),

            shiny::selectizeInput(
              "export_files",
              "Choose files to export:",
              choices = NULL,
              multiple = TRUE
            ),

            shiny::actionButton(
              "export_files_button",
              "Copy Selected Files",
              class = "btn-primary"
            ),

            shiny::textOutput("export_status")

          )
        ),

        width = 9
      )
    )
  )


  # ============================================================
  # 4. SHINY SERVER
  # ============================================================

  server <- function(input, output, session) {

    # ----------------------------------------------------------
    # Reactive storage
    # ----------------------------------------------------------

    pdf_dir <- shiny::reactiveVal(lob.archive.dir)
    export_dir <- shiny::reactiveVal(NULL)

    archive_data <- shiny::reactiveVal(get_archive(arch_table))

    search_data <- shiny::reactiveVal(NULL)


    # ----------------------------------------------------------
    # Refresh database information
    # ----------------------------------------------------------

    refresh_archive <- function() {
      archive_data(get_archive(arch_table))
    }

    tag_columns <- shiny::reactive({
      setdiff(
        names(archive_data()),
        c("URI", "DOCUMENT_NAME", "PAGES")
      )
    })

    # ----------------------------------------------------------
    # Display number of PDFs awaiting entry
    # ----------------------------------------------------------

    output$unarchived_count <- shiny::renderText({

      shiny::req(pdf_dir())

      n <- length(available_files())

      paste(
        n,
        "Files found without Oracle records."
      )

    })
    # ============================================================
    # DIRECTORY SELECTION
    # ============================================================

    # Helper to validate and normalize a directory path

    validate_directory <- function(path) {

      path <- trimws(path)

      if (!nzchar(path) || !dir.exists(path)) {
        stop("The specified directory does not exist.")
      }

      normalizePath(
        path,
        winslash = "/",
        mustWork = TRUE
      )
    }


    # ------------------------------------------------------------
    # Archive directory: manual path entry
    # ------------------------------------------------------------

    shiny::observeEvent(input$set_pdf_dir, {

      tryCatch({

        path <- validate_directory(input$pdf_dir_path)

        pdf_dir(path)

        shiny::updateTextInput(
          session,
          "pdf_dir_path",
          value = path
        )

        shiny::showNotification(
          "Archive directory successfully mapped.",
          type = "message"
        )

      }, error = function(e) {

        shiny::showNotification(
          conditionMessage(e),
          type = "error"
        )

      })

    })


    # ------------------------------------------------------------
    # Archive directory: Windows folder browser
    # ------------------------------------------------------------

    shiny::observeEvent(input$browse_pdf_dir, {

      shiny::req(.Platform$OS.type == "windows")

      default_path <- if (is.null(pdf_dir())) {
        "L:/"
      } else {
        pdf_dir()
      }

      path <- utils::choose.dir(
        default = default_path,
        caption = "Select Archive Directory"
      )

      if (!is.na(path) && nzchar(path)) {

        path <- validate_directory(path)

        pdf_dir(path)

        shiny::updateTextInput(
          session,
          "pdf_dir_path",
          value = path
        )

      }

    })


    # ------------------------------------------------------------
    # Export directory: manual path entry
    # ------------------------------------------------------------

    shiny::observeEvent(input$set_export_dir, {

      tryCatch({

        path <- validate_directory(input$export_dir_path)

        export_dir(path)

        shiny::updateTextInput(
          session,
          "export_dir_path",
          value = path
        )

        shiny::showNotification(
          "Export directory successfully mapped.",
          type = "message"
        )

      }, error = function(e) {

        shiny::showNotification(
          conditionMessage(e),
          type = "error"
        )

      })

    })


    # ------------------------------------------------------------
    # Export directory: Windows folder browser
    # ------------------------------------------------------------

    shiny::observeEvent(input$browse_export_dir, {

      shiny::req(.Platform$OS.type == "windows")

      default_path <- if (is.null(export_dir())) {
        "L:/"
      } else {
        export_dir()
      }

      path <- utils::choose.dir(
        default = default_path,
        caption = "Select Export Directory"
      )

      if (!is.na(path) && nzchar(path)) {

        path <- validate_directory(path)

        export_dir(path)

        shiny::updateTextInput(
          session,
          "export_dir_path",
          value = path
        )

      }

    })


    # ------------------------------------------------------------
    # Display selected directories
    # ------------------------------------------------------------

    output$pdf_dir_display <- shiny::renderText({

      if (is.null(pdf_dir())) {
        "No archive directory mapped."
      } else {
        paste("Mapped:", pdf_dir())
      }

    })

    output$export_dir_display <- shiny::renderText({

      if (is.null(export_dir())) {
        "No export directory mapped."
      } else {
        paste("Mapped:", export_dir())
      }

    })



    # ==========================================================
    # 5. ARCHIVE RESOURCE
    # ==========================================================

    # ----------------------------------------------------------
    # Find Files in the selected directory
    # ----------------------------------------------------------

    # ----------------------------------------------------------
    # Find Files that have NOT already been entered into Oracle
    # ----------------------------------------------------------

    available_files <- shiny::reactive({

      shiny::req(pdf_dir())

      # All PDFs in the selected directory
      all_files <- list.files(
        pdf_dir(),
        pattern = "\\.pdf$",
        full.names = FALSE,
        ignore.case = TRUE
      )

      # Existing filenames in Oracle
      existing_files <- basename(gsub(
        "\\\\", "/", archive_data()$URI
      ))

      # Compare case-insensitively
      remaining_files <- all_files[
        !tolower(all_files) %in% tolower(existing_files)
      ]

      remaining_files

    })


    # ----------------------------------------------------------
    # Drag-and-drop interface
    # ----------------------------------------------------------

    output$file_buckets <- shiny::renderUI({
      shiny::req(available_files())

      shiny::fluidRow(
        shiny::column(
          width = 10,
          sortable::bucket_list(
            header = NULL,
            group_name = "pdf_archive",
            orientation = "horizontal",
            sortable::add_rank_list(
              text = "Available Files",
              labels = available_files(),
              input_id = "available"
            ),
            sortable::add_rank_list(
              text = "Selected Files",
              labels = character(0),
              input_id = "selected"
            )
          )
        ),
        shiny::column(
          width = 1,
          shiny::uiOutput("selected_tags_summary")
        )
      )
    })

    # ----------------------------------------------------------
    # Duplicate detection
    # ----------------------------------------------------------

    duplicate_files <- shiny::reactive({

      selected <- input$selected

      if (is.null(selected) || !length(selected)) {
        return(character(0))
      }

      existing <- basename(gsub(
        "\\\\", "/", archive_data()$URI
      ))

      selected[selected %in% existing]

    })


    output$duplicate_warning <- shiny::renderUI({

      duplicates <- duplicate_files()

      if (length(duplicates)) {

        shiny::div(

          class = "alert alert-warning",

          shiny::strong("Warning: Existing files detected!"),

          shiny::p(
            "The following files already exist in ",
            "the Oracle archive:"
          ),

          shiny::tags$ul(
            lapply(duplicates, shiny::tags$li)
          ),

          shiny::p(
            "These files will not be inserted again."
          )
        )

      } else {

        shiny::div(
          class = "alert alert-success",
          "No duplicate files detected."
        )

      }
    })


    # ----------------------------------------------------------
    # Dynamic tag selection
    # ----------------------------------------------------------

    # ----------------------------------------------------------
    # Dynamic tag selection buttons
    # ----------------------------------------------------------

    output$archive_tag_options <- shiny::renderUI({

      shinyWidgets::checkboxGroupButtons(

        inputId = "archive_tags",

        label = "Data Types:",

        choices = tag_columns(),

        selected = NULL,

        justified = FALSE,

        direction = "horizontal",

        size = "xs",

        status = "white"

      )

    })

    # ----------------------------------------------------------
    # Display currently selected tags
    # ----------------------------------------------------------

    output$selected_tags_summary <- shiny::renderUI({
      selected <- input$archive_tags

      shiny::tagList(
        shiny::tags$small(
          style = "font-weight: bold;",
          "Tags:",
          class = "selected-tags-summary"
        ),
        if (is.null(selected) || length(selected) == 0) {
          shiny::tags$small(
            style = "color: #888;",
            "None selected"
          )
        } else {
          shiny::tags$small(
            style = "display: block; overflow-wrap: anywhere;",
            paste(selected, collapse = "\n")
          )
        }
      )
    })

    # ----------------------------------------------------------
    # Create a new tag
    # ----------------------------------------------------------

    shiny::observeEvent(input$create_tag, {

      shiny::req(input$new_tag)

      tryCatch({

        new_tag <- create_tag(input$new_tag)

        refresh_archive()

        shiny::updateTextInput(
          session,
          "new_tag",
          value = ""
        )

        shiny::showNotification(
          paste("Tag created:", new_tag),
          type = "message"
        )

      }, error = function(e) {

        shiny::showNotification(
          conditionMessage(e),
          type = "error"
        )

      })

    })


    # ----------------------------------------------------------
    # Enter new records into Oracle
    # ----------------------------------------------------------

    shiny::observeEvent(input$enter_data, {

      selected <- input$selected

      shiny::req(selected)
      shiny::req(pdf_dir())


      # Remove files that already exist in Oracle
      duplicates <- duplicate_files()

      new_files <- setdiff(selected, duplicates)

      if (!length(new_files)) {

        shiny::showNotification(
          "No new files to enter.",
          type = "warning"
        )

        return()
      }

      # Check that the files physically exist
      existing_paths <- file.path(
        pdf_dir(),
        new_files
      )

      valid_files <- file.exists(existing_paths)

      new_files <- new_files[valid_files]

      if (!length(new_files)) {

        shiny::showNotification(
          "No valid files were found.",
          type = "error"
        )

        return()
      }

      # Insert records
      tryCatch({

        for (file in new_files) {

          insert_arch_record(
            uri = paste0("L:/", file),
            document_name = input$document_name,
            pages = input$pages,
            tags = input$archive_tags
          )

          insert_species_record(
            uri = paste0("L:/", file),
            species_name = input$species,
            species_code = 00000
          )

        }

        # Explicitly commit Oracle changes
        DBI::dbExecute(con, "COMMIT")

        refresh_archive()

        shiny::showNotification(
          paste(
            length(new_files),
            "document(s) successfully added to Oracle."
          ),
          type = "message",
          duration = 6
        )

      }, error = function(e) {

        # Roll back uncommitted Oracle changes
        try(
          DBI::dbExecute(con, "ROLLBACK"),
          silent = TRUE
        )

        shiny::showNotification(
          paste(
            "Error entering records:",
            conditionMessage(e)
          ),
          type = "error",
          duration = NULL
        )

      })

    })


    # ==========================================================
    # 6. RETRIEVE RESOURCE
    # ==========================================================

    # ----------------------------------------------------------
    # Dynamic retrieval tags
    # ----------------------------------------------------------

    output$retrieve_tag_options <- shiny::renderUI({

      shiny::checkboxGroupInput(
        "retrieve_tags",
        "Filter by tags:",
        choices = tag_columns(),
        selected = NULL
      )

    })


    # ============================================================
    # SEARCH ORACLE ARCHIVE
    # ============================================================

    shiny::observeEvent(input$search_archive, {

      data <- archive_data()

      selected_tags <- input$retrieve_tags

      # Apply AND filtering
      if (length(selected_tags)) {

        for (tag in selected_tags) {

          data <- data[
            !is.na(data[[tag]]) &
              data[[tag]] == "Y",
            ,
            drop = FALSE
          ]

        }

      }

      # Store results, even if there are zero rows
      search_data(data)

      # Update export choices
      if (nrow(data) > 0) {

        filenames <- basename(gsub(
          "\\\\", "/", data$URI
        ))

        labels <- paste0(
          filenames,
          " - ",
          data$DOCUMENT_NAME
        )

        choices <- stats::setNames(
          data$URI,
          labels
        )

      } else {

        choices <- character(0)

      }

      shiny::updateSelectizeInput(
        session,
        "export_files",
        choices = choices,
        selected = character(0),
        server = TRUE
      )

    })


    # ============================================================
    # DISPLAY SEARCH RESULTS
    # ============================================================

    output$search_results <- shiny::renderTable({

      data <- search_data()

      shiny::validate(
        shiny::need(
          !is.null(data),
          "Run a search to display results."
        ),

        shiny::need(
          nrow(data) > 0,
          "No documents matched the selected tags."
        )
      )

      data[, c(
        "URI",
        "DOCUMENT_NAME",
        "PAGES"
      ), drop = FALSE]

    })


    # ============================================================
    # DISPLAY RESULT COUNT
    # ============================================================

    output$result_count <- shiny::renderText({

      data <- search_data()

      if (is.null(data)) {
        return("Run a search to display results.")
      }

      if (nrow(data) == 0) {
        return("No matching documents found.")
      }

      paste(
        nrow(data),
        "matching document(s) found."
      )

    })



    # ----------------------------------------------------------
    # Export selected Files
    # ----------------------------------------------------------

    shiny::observeEvent(input$export_files_button, {

      shiny::req(input$export_files)
      shiny::req(pdf_dir())
      shiny::req(export_dir())

      selected <- input$export_files

      # Convert Oracle URIs into actual file paths

      filenames <- basename(gsub(
        "\\\\", "/", selected
      ))

      source_paths <- file.path(
        pdf_dir(),
        filenames
      )

      destination_paths <- file.path(
        export_dir(),
        filenames
      )

      # Check that source files exist

      missing <- !file.exists(source_paths)

      if (any(missing)) {

        shiny::showNotification(
          paste(
            "Some files could not be found:",
            paste(filenames[missing], collapse = ", ")
          ),
          type = "error",
          duration = NULL
        )

      }

      # Copy files without overwriting existing files

      valid <- !missing

      copied <- file.copy(
        from = source_paths[valid],
        to = destination_paths[valid],
        overwrite = FALSE
      )

      output$export_status <- shiny::renderText({

        paste(
          sum(copied),
          "of",
          sum(valid),
          "files successfully copied."
        )

      })

    })


    # ----------------------------------------------------------
    # Disconnect from Oracle when the session ends
    # ----------------------------------------------------------

    session$onSessionEnded(function() {

      try(
        DBI::dbDisconnect(con),
        silent = TRUE
      )

    })

  }


  # ============================================================
  # 7. LAUNCH APPLICATION
  # ============================================================

  shiny::shinyApp(
    ui = ui,
    server = server
  )

}

