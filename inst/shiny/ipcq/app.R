library(shiny)

required_columns <- TforT::ipcq_input_spec()

read_upload <- function(file_info) {
  extension <- tolower(tools::file_ext(file_info$name))
  switch(
    extension,
    csv = utils::read.csv(file_info$datapath, check.names = FALSE),
    xlsx = as.data.frame(readxl::read_excel(file_info$datapath)),
    rds = readRDS(file_info$datapath),
    stop("Supported file types are CSV, XLSX, and RDS.", call. = FALSE)
  )
}

ui <- fluidPage(
  tags$head(tags$style(HTML("\
    body { background: #f5f7fa; }\
    .navbar { margin-bottom: 24px; }\
    .well { background: white; border-radius: 8px; }\
    .btn-primary { width: 100%; font-weight: 600; }\
    .help-block { color: #52606d; }\
    .status-box { padding: 12px; margin-bottom: 14px; border-radius: 6px;\
                  background: #edf6ff; border-left: 4px solid #2474b5; }\
  "))),
  navbarPage(
    title = "TforT iPCQ calculator",
    tabPanel(
      "Calculate",
      sidebarLayout(
        sidebarPanel(
          fileInput("file", "Upload iPCQ data",
                    accept = c(".csv", ".xlsx", ".rds")),
          downloadButton("download_template", "Download input template"),
          tags$hr(),
          numericInput("input_period", "Weeks represented by the responses",
                       value = 4, min = 0.1, step = 1),
          numericInput("target_period", "Weeks for which costs are required",
                       value = 4, min = 0.1, step = 1),
          numericInput("reference_year", "Reference-price year",
                       value = 2024, min = 2000, step = 1),
          selectInput("price_mode", "Hourly unit costs",
                      choices = c("Dutch reference prices" = "dutch",
                                  "Provide my own prices" = "custom")),
          conditionalPanel(
            "input.price_mode == 'dutch'",
            selectInput("dutch_currency", "Currency",
                        choices = c("Euro (EUR)" = "EUR",
                                    "International dollar (INT$)" = "INT$"))
          ),
          conditionalPanel(
            "input.price_mode == 'custom'",
            textInput("custom_currency", "Currency label", value = "GBP"),
            numericInput("paid_price", "Paid-work cost per hour",
                         value = 20, min = 0),
            numericInput("unpaid_price", "Unpaid-work cost per hour",
                         value = 8, min = 0)
          ),
          selectInput("friction_mode", "Friction period",
                      choices = c("Dutch friction period" = "dutch",
                                  "Provide my own period" = "custom")),
          conditionalPanel(
            "input.friction_mode == 'custom'",
            numericInput("custom_friction", "Friction period (calendar days)",
                         value = 84, min = 1, step = 1)
          ),
          tags$hr(),
          actionButton("calculate", "Calculate costs", class = "btn-primary")
        ),
        mainPanel(
          uiOutput("status"),
          tabsetPanel(
            tabPanel("Results", DT::DTOutput("results")),
            tabPanel("Required columns", DT::DTOutput("specification")),
            tabPanel(
              "Calculation settings",
              verbatimTextOutput("settings")
            )
          ),
          tags$br(),
          uiOutput("download_area")
        )
      )
    ),
    tabPanel(
      "About",
      h3("What this calculator does"),
      p("The calculator converts iPCQ responses into costs of short and long",
        "absenteeism, presenteeism, and lost unpaid work."),
      p("Dutch EUR and international-dollar prices are supplied by the",
        "tatooheene package. Researchers from other countries can provide",
        "their own hourly prices and friction period."),
      h4("Important interpretation"),
      p("Reported counts must all describe the selected input period. Costs",
        "for a longer target period are projections. Long absence is valued",
        "only where the sickness/friction window overlaps the target period."),
      h4("Privacy"),
      p("Avoid uploading directly identifying information. The calculator",
        "does not need names, addresses, or contact details.")
    )
  )
)

server <- function(input, output, session) {
  upload_error <- reactiveVal(NULL)

  uploaded_data <- reactive({
    req(input$file)
    upload_error(NULL)
    tryCatch(
      {
        dat <- read_upload(input$file)
        if (!is.data.frame(dat)) stop("The uploaded object is not a data frame.")
        missing <- setdiff(required_columns$column, names(dat))
        if (length(missing)) {
          stop(sprintf("Missing columns: %s", paste(missing, collapse = ", ")))
        }
        for (column in c("date_of_this_measurement", "date_start_sickness")) {
          dat[[column]] <- as.Date(dat[[column]])
        }
        dat
      },
      error = function(e) {
        upload_error(conditionMessage(e))
        NULL
      }
    )
  })

  calculated_data <- reactiveVal(NULL)

  observeEvent(input$file, {
    calculated_data(NULL)
  }, ignoreInit = TRUE)

  observeEvent(input$calculate, {
    dat <- uploaded_data()
    validate(need(!is.null(dat), upload_error() %||% "Upload a valid data file."))

    custom_prices <- identical(input$price_mode, "custom")
    custom_friction <- identical(input$friction_mode, "custom")
    currency <- if (custom_prices) input$custom_currency else input$dutch_currency

    result <- tryCatch(
      TforT::calculate_ipcq(
        dat = dat,
        reference_year = input$reference_year,
        currency = currency,
        input_period_weeks = input$input_period,
        target_period_weeks = input$target_period,
        paid_work_hour_price = if (custom_prices) input$paid_price else NULL,
        unpaid_work_hour_price = if (custom_prices) input$unpaid_price else NULL,
        friction_period_days = if (custom_friction) input$custom_friction else NULL
      ),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = NULL)
        NULL
      }
    )
    calculated_data(result)
  }, ignoreInit = TRUE)

  output$status <- renderUI({
    if (is.null(input$file)) {
      return(div(class = "status-box", "Upload a CSV, XLSX, or RDS file to begin."))
    }
    if (!is.null(upload_error())) {
      return(div(class = "alert alert-danger", upload_error()))
    }
    if (is.null(calculated_data())) {
      return(div(class = "status-box", "File accepted. Select settings and click Calculate costs."))
    }
    div(class = "alert alert-success",
        sprintf("Calculated %s participant row(s).", nrow(calculated_data())))
  })

  output$results <- DT::renderDT({
    req(calculated_data())
    DT::datatable(calculated_data(), options = list(pageLength = 15,
                                                     scrollX = TRUE),
                  rownames = FALSE)
  })

  output$specification <- DT::renderDT({
    DT::datatable(required_columns, options = list(dom = "t", pageLength = 10),
                  rownames = FALSE)
  })

  output$settings <- renderPrint({
    req(calculated_data())
    result <- calculated_data()
    print(unique(result[c(
      "ipcq_extrapolation_factor", "paid_work_hour_price",
      "unpaid_work_hour_price", "friction_period_days", "cost_currency",
      "paid_work_price_source", "unpaid_work_price_source",
      "friction_period_source"
    )]))
  })

  output$download_area <- renderUI({
    req(calculated_data())
    tagList(downloadButton("download_csv", "Download CSV"),
            downloadButton("download_xlsx", "Download Excel"),
            downloadButton("download_rds", "Download RDS"))
  })

  output$download_template <- downloadHandler(
    filename = "ipcq_input_template.csv",
    content = function(file) {
      file.copy(system.file("extdata", "ipcq_input_template.csv",
                            package = "TforT"), file)
    }
  )
  output$download_csv <- downloadHandler(
    filename = "ipcq_results.csv",
    content = function(file) utils::write.csv(calculated_data(), file,
                                               row.names = FALSE, na = "")
  )
  output$download_xlsx <- downloadHandler(
    filename = "ipcq_results.xlsx",
    content = function(file) writexl::write_xlsx(calculated_data(), file)
  )
  output$download_rds <- downloadHandler(
    filename = "ipcq_results.rds",
    content = function(file) saveRDS(calculated_data(), file)
  )
}

`%||%` <- function(x, y) if (is.null(x) || !nzchar(x)) y else x

shinyApp(ui, server)
