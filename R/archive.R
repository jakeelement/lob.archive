

lobster_archive <- function() {

  # ============================================================
  # 1. PACKAGES AND ORACLE CONNECTION
  # ============================================================

  required_packages <- c(
    "shiny", "shinyjs",
    "sortable", "DBI", "ROracle", "shinyWidgets", "dplyr"
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
  lfa_table <- "LOBARCHIVE_LFA"
  statdist_table <- "LOBARCHIVE_STATDIST"
  community_table <- "LOBARCHIVE_COMMUNITY"
  port_table <- "LOBARCHIVE_PORT"
  province_table <- "LOBARCHIVE_PROVINCE"
  year_table <- "LOBARCHIVE_YEAR"
  name_table <- "LOBARCHIVE_NAME"

  get_archive <- function(table_name = NULL) {
    DBI::dbGetQuery(
      con,
      paste0("SELECT * FROM ", table_name)
    )
  }


  # Lookup tables
  species_lookup <- get_archive("LOBSTER.LOBSTERARCHIVE_SPECIES_LOOKUP")
  port_lookup <- get_archive("LOBSTER.LOBSTERARCHIVE_PORT_LOOKUP")
  district_lookup <- get_archive("LOBSTER.LOBSTERARCHIVE_DISTRICT_LOOKUP")
  species_names <- c("",sort(unique(paste(species_lookup$COMMON, species_lookup$SPECIES_CODE, sep = ":"))))
  port.names <- c("",sort(unique(port_lookup$PORT_NAME)))
  port.codes <- c("",sort(unique(port_lookup$PORT_CODE)))
  lfa.codes <- c("", sort(unique(district_lookup$LFA)))
  district.codes <- c("", sort(unique(district_lookup$LOBSTER_DISTRICT)))
  community.codes <- c("",sort(unique(as.numeric(port_lookup$COMMUNITY_CODE[port_lookup$COMMUNITY_CODE != "NA"]))))
  statdist.codes <- c("",sort(unique(as.numeric(port_lookup$STAT_DIST_CODE))))
  province.codes <- c("",sort(unique(port_lookup$PROV_CODE)))


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
        values[[column]] <- sql_string(as.character(as.integer(pages)))

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


  insert_other_record <- function(uri, table_name, A,a,B=NULL,b=NULL) {

    columns <- get_columns(table_name)

    values <- list()

    a[a == ""] <- NA
    if(!is.null(b)){b[b == ""] <- NA}

    for (column in columns) {

      if (column == "URI") {
        values[[column]] <- sql_string(uri)

      } else if (column == A) {
        values[[column]] <- sql_string(a)

      } else if (column == B) {
        values[[column]] <- sql_string(b)

      }
    }
    sql <- paste0(
      "INSERT INTO ", table_name,
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


  ###search other databases for values
  search_table <- function(a.dat, a,A, file.names){
    if(!(a %in% c("",NULL,NA))){
      a.dat <- a.dat %>% filter(.data[[A]] %in% a)
      file.names <- file.names[file.names %in% unique(a.dat$URI)]
    }
    return(file.names)
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

            # shiny::p(
            #   "The following information will be applied ",
            #   "to all currently selected files."
            # ),

            shiny::fluidRow(

              shiny::column(2, shiny::textInput("document_name","Document Name:")),

              shiny::column(1, shiny::numericInput("pages","#Pages:", value = NA, min = 1, step = 1)),

              shiny::column(2, shiny::selectInput("species","Species:", choices = species_names)),

              shiny::column(1, shiny::textInput("year","Year:")),

              shiny::column(2, shiny::textInput("first_name","First Name:")),

              shiny::column(2, shiny::textInput("last_name","Last Name:"))
            ),

            shiny::hr(),
            shiny::h4("Areas"),
            shiny::fluidRow(
              shiny::column(1, shiny::selectInput("lfa","LFA:", choices = lfa.codes)),
              shiny::column(1, shiny::selectInput("district","District:", choices = district.codes)),
              shiny::column(1, shiny::selectInput("statdist","Stat District:", choices = statdist.codes)),
              shiny::column(1, shiny::selectInput("community","Community:", choices = community.codes)),
              shiny::column(1, shiny::selectInput("port_name","Port:", choices = port.names)),
              shiny::column(1, shiny::selectInput("port_code","Port Code:", choices = port.codes)),
              shiny::column(1, shiny::selectInput("province","Province:", choices = province.codes))

            ),

            shiny::hr(),
            shiny::fluidRow(
            shiny::column(
              8,
            shiny::uiOutput("archive_tag_options")
            )
            ),

            shiny::hr(),

            # shiny::h4("Create New Data Type:"),

            shiny::fluidRow(

              shiny::column(
                4,

                shiny::textInput(
                  "new_tag",
                  "Create New Data Type:",
                  placeholder = "e.g. TEMPERATURE"
                )
              ),

              shiny::column(
                5,

                shiny::br(),

                shiny::actionButton(
                  "create_tag",
                  "Create Data Type"
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
              "Enter data and/or select data types to find documents "
            ),

            shiny::hr(),
            shiny::h4("Document Information"),
            shiny::fluidRow(

              shiny::column(2, shiny::textInput("s_document_name","Document Name:")),

              shiny::column(1, shiny::numericInput("s_pages","#Pages:", value = NA, min = 1, step = 1)),

              shiny::column(2, shiny::selectInput("s_species","Species:", choices = species_names)),

              shiny::column(1, shiny::textInput("s_year","Year:")),

              shiny::column(2, shiny::textInput("s_first_name","First Name:")),

              shiny::column(2, shiny::textInput("s_last_name","Last Name:"))
            ),

            shiny::hr(),
            shiny::h4("Areas"),
            shiny::fluidRow(
              shiny::column(1, shiny::selectInput("s_lfa","LFA:", choices = lfa.codes)),
              shiny::column(1, shiny::selectInput("s_district","District:", choices = district.codes)),
              shiny::column(1, shiny::selectInput("s_statdist","Stat District:", choices = statdist.codes)),
              shiny::column(1, shiny::selectInput("s_community","Community:", choices = community.codes)),
              shiny::column(1, shiny::selectInput("s_port_name","Port:", choices = port.names)),
              shiny::column(1, shiny::selectInput("s_port_code","Port Code:", choices = port.codes)),
              shiny::column(1, shiny::selectInput("s_province","Province:", choices = province.codes))
            ),

            shiny::fluidRow(
              shiny::column(
                8,
                shiny::uiOutput("retrieve_tag_options")
              )),

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

        shiny::column(1,
          shiny::br(),
          shiny::actionButton(
            "preview_data",
            "Preview Data"
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
      selected <- c(input$document_name, input$pages, input$species, input$year, input$first_name, input$last_name,
                    input$lfa, input$district, input$statdist, input$community, input$port_name, input$port_code, input$province, selected)

      selected <- selected[!is.na(selected) & selected != ""]

      shiny::tagList(
        shiny::tags$small(
          style = "font-weight: bold;",
          "Tagged Info:",
          class = "selected-tags-summary"
        ),
        if (is.null(selected) || length(selected) == 0) {
          shiny::tags$small(
            style = "color: #888;",
            "No info added"
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
    # Preview selected files
    # ----------------------------------------------------------

    shiny::observeEvent(input$preview_data, {

      selected <- input$selected

      shiny::req(selected)
      shiny::req(pdf_dir())

      for(i in 1:length(selected)){
        check.files <- file.path(pdf_dir(), selected[i])
        browseURL(check.files)
      }

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

          insert_other_record(
            table_name = species_table,
            uri = paste0("L:/", file),
            A = "SPECIES_NAME",
            a = input$species,
            B = "SPECIES_CODE",
            b = paste(rev(strsplit(input$species, ":", fixed = TRUE)[[1]]), collapse = ":")
          )

          insert_other_record(
            table_name = year_table,
            uri = paste0("L:/", file),
            A = "YEAR",
            a = input$year
          )

          insert_other_record(
            table_name = name_table,
            uri = paste0("L:/", file),
            A = "FIRST",
            a = input$first_name,
            B = "LAST",
            b = input$last_name
          )

        ##### AREAS ################
          insert_other_record(
            table_name = lfa_table,
            uri = paste0("L:/", file),
            A = "LFA",
            a = input$lfa,
            B = "DISTRICT",
            b = input$district
          )

          insert_other_record(
            table_name = statdist_table,
            uri = paste0("L:/", file),
            A = "STATDIST",
            a = input$statdist
          )

          insert_other_record(
            table_name = community_table,
            uri = paste0("L:/", file),
            A = "COMMUNITY_CODE",
            a = input$community
          )

          insert_other_record(
            table_name = port_table,
            uri = paste0("L:/", file),
            A = "PORT_NAME",
            a = input$port_name,
            B = "PORT_CODE",
            b = input$port_code
          )

          insert_other_record(
            table_name = province_table,
            uri = paste0("L:/", file),
            A = "PROV",
            a = input$province
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

      ####refresh data fields
      shinyjs::delay(5,{
        updateTextInput(session, "document_name", value = NA)
        updateNumericInput(session, "pages", value = NA)
        updateSelectInput(session, "species", selected = "")
        updateTextInput(session, "year", value = NA)
        updateTextInput(session, "first_name", value = NA)
        updateTextInput(session, "last_name", value = NA)
        updateSelectInput(session, "lfa", selected = "")
        updateSelectInput(session, "district", selected = "")
        updateSelectInput(session, "statdist", selected = "")
        updateSelectInput(session, "community", selected = "")
        updateSelectInput(session, "port_name", selected = "")
        updateSelectInput(session, "port_code", selected = "")
        updateSelectInput(session, "province", selected = "")
      })
    })


    # ==========================================================
    # 6. RETRIEVE RESOURCE
    # ==========================================================

    # ----------------------------------------------------------
    # Dynamic retrieval tags
    # ----------------------------------------------------------

    output$retrieve_tag_options <- shiny::renderUI({

      shinyWidgets::checkboxGroupButtons(

        inputId = "search_tags",

        label = "Data Types:",

        choices = tag_columns(),

        selected = NULL,

        justified = FALSE,

        direction = "horizontal",

        size = "xs",

        status = "white"

      )

    })


    # ============================================================
    # SEARCH ORACLE ARCHIVE
    # ============================================================

    shiny::observeEvent(input$search_archive, {

      data <- archive_data()
      species.dat <- get_archive(species_table)
      year.dat <- get_archive(year_table)
      name.dat <- get_archive(name_table)
      lfa.dat <- get_archive(lfa_table)
      statdist.dat <- get_archive(statdist_table)
      community.dat <- get_archive(community_table)
      port.dat <- get_archive(port_table)
      province.dat <- get_archive(province_table)

      selected_tags <- input$search_tags

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

      if(!input$s_document_name %in% c(NA,NULL,"")){
        data <- data %>% filter(DOCUMENT_NAME %in% input$s_document_name)
      }

      if(!input$s_pages %in% c(NA,NULL,"")){
        data <- data %>% filter(PAGES %in% input$s_pages)
      }

      ## filter the the archive table by the other metdata table selections
      file.names <- data$URI

      file.names <- search_table(species.dat, input$s_species, "SPECIES_NAME", file.names)
      file.names <- search_table(year.dat, input$s_year, "YEAR", file.names)
      file.names <- search_table(name.dat, input$s_first_name, "FIRST", file.names)
      file.names <- search_table(name.dat, input$s_last_name, "LAST", file.names)
      file.names <- search_table(lfa.dat, input$s_lfa, "LFA", file.names)
      file.names <- search_table(lfa.dat, input$s_district, "DISTRICT", file.names)
      file.names <- search_table(statdist.dat, input$s_statdist, "STATDIST", file.names)
      file.names <- search_table(community.dat, input$s_community, "COMMUNITY_CODE", file.names)
      file.names <- search_table(port.dat, input$s_port_name, "PORT_NAME", file.names)
      file.names <- search_table(port.dat, input$s_port_code, "PORT_CODE", file.names)
      file.names <- search_table(province.dat, input$s_province, "PROV", file.names)

        data <- data |> filter(URI %in% file.names)


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
    # DISPLAY RESULT COUNT
    # ============================================================

    output$result_count <- shiny::renderText({

      data <- search_data()

      if (is.null(data)) {
        return("Run a search to display results.")
      } else paste(
        nrow(data),
        "matching document(s) found."
      )
    })


    # ============================================================
    # DISPLAY SEARCH RESULTS
    # ============================================================

    output$search_results <- shiny::renderTable({

      data <- search_data()

      data[, c(
        "URI",
        "DOCUMENT_NAME",
        "PAGES"
      ), drop = FALSE]

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

