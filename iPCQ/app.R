#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

setwd("~/TforT/TforT-package/R")

library(shiny)
library(tidyverse)
library(DT)
library(writexl)
library(MHQoL)
library(fmsb)
library(here)
library(shinyalert)

source("iPCQ_function.R")

# Define UI for application that draws a histogram
ui <- fluidPage(includeCSS("www/styles.css"),

                navbarPage(title = "Calculating productivity costs based on the iPCQ",


                           tabPanel(title = "About"








                                    ),

                           tabPanel(title = "Calculate the productivity costs ⚙️",

                                    sidebarPanel(
                                      div(style = "text-align: center; margin-bottom: 20px;",
                                          actionButton("Calculate_costa", "Calculate the costs", class = "btn-primary btn-lg")),

                                      fileInput("file", "Choose a file (CSV, Excel, RDS)",
                                                accept = c(".csv", ".xlsx", ".rds")),

                                      textOutput("warning_message"),

                                      h4("Example data"),
                                      p("iPCQ example dataset:", a(img(src = "images/icon-excel.png", height = 24, width = 24), href = "~/TforT/TforT-package/R/data/example_data.xlsx", target = "_blank"), style = "margin-bottom;0"),
                                      hr(),

                                      numericInput("recall_period",
                                                   label = "Recall period in weeks",
                                                   min = 0,
                                                   value = 13),

                                      numericInput("research_year",
                                                   label = "Year of research",
                                                   value = 2023,
                                                   min = 2022,
                                                   max = 2023,
                                                   step = 1),

                                      selectInput("currency_decission",
                                                  "Currency of presentation",
                                                  choices = c("INT$", "EUR"),
                                                  selected = "EUR")

                                    ),


                                    mainPanel(
                                      DTOutput("data_output"),

                                      # Download buttons
                                      uiOutput("download_buttons")
                                    )
                                    )
                           )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {

  uploaded_data <- reactive({


  req(input$file)

  file_path <- input$file$datapath

  # Read the file based on its extension
  data <- tryCatch({
    if (grepl("\\.csv$", input$file$name)) {
      read_csv(file_path)
    } else if (grepl("\\.xlsx$", input$file$name)) {
      readxl::read_excel(file_path)
    } else if (grepl("\\.rds$", input$file$name)) {
      readRDS(file_path)
    } else {
      return(NULL)
    }
  },error = function(e) return(NULL)  # Return NULL if there's an error
  )

  date_cols <- c(
    "date_of_this_measurement",
    "date_start_sickness"
  )

  data[date_cols] <- lapply(data[date_cols], as.Date)

  # Define required columns (ID, Group) and productivity cost columns dynamically
  descriptive_columns <- c("ID", "Group")
  prod_columns <- names(data)[names(data) %in% c("date_of_this_measurement",
                                                     "hours_work_week",
                                                     "days_work_week",
                                                     "days_sick",
                                                     "sick_longer_than_4_weeks",
                                                     "date_start_sickness",
                                                     "days_suffering_from_problems",
                                                     "rate_of_work",
                                                     "days_less_unpaid_work",
                                                     "average_hours_unpaid_work")]


  # Check for missing descriptive columns
  missing_columns <- setdiff(descriptive_columns, colnames(data))
  if (length(missing_columns) > 0) {
    shinyalert("Error!", paste("Missing required columns:", paste(missing_columns, collapse = ", ")), type = "Error")
    stop("🚨 Error: Missing required columns. Execution stopped.")
  }

  # Check for missing expected productivity cost dimensions
  if (length(prod_columns) < 10) {
    shinyalert("Warning!", "Some expected dimensions are missing!", type = "warning")
  }

  # Check for missing values (NAs)
  if (any(is.na(data))){
    shinyalert("Warning!", "Your dataset contains missing values (NAs).", type = "warning")
  }

  # Define the required cols
  descriptive_columns <- c("ID", "Group")

  descriptives <- data %>%
    dplyr::select(all_of(descriptive_columns))

  prod_columns <- names(data)[names(data) %in% c("date_of_this_measurement",
                                                      "hours_work_week",
                                                      "days_work_week",
                                                      "days_sick",
                                                      "sick_longer_than_4_weeks",
                                                      "date_start_sickness",
                                                      "days_suffering_from_problems",
                                                      "rate_of_work",
                                                      "days_less_unpaid_work",
                                                      "average_hours_unpaid_work")]


  # Calculate the productivity costs
  data_prod <- func_iPCQ(dat = data[, prod_columns],
                         recall_weeks = as.numeric(input$recall_period),
                         reference_year = input$research_year,
                         currency = input$currency_decission)


  data <- cbind(descriptives, data_prod)

  data <- data %>%
    mutate(across(where(is.numeric), ~ round(.x, 2)))

  return(data)

  })

  # Warning message if the file is invalid
  output$warning_message <- renderText({
    if (is.null(uploaded_data())) {
      return("⚠️ Please upload a valid dataframe (CSV, Excel, or RDS).")
    }
    return(NULL)  # No warning if file is valid
  })

  # Render the processed table
  output$data_output <- renderDT({
    req(uploaded_data())
    datatable(uploaded_data(), options = list(pageLength = 15))
  })

  # Conditionally show the download buttons when the table is rendered
  output$download_buttons <- renderUI({
    req(uploaded_data())  # Ensure data exists before showing buttons

    tagList(
      downloadButton("download_rds", "Download as RDS"),
      downloadButton("download_excel", "Download as Excel")
    )
  })

  # Download handler for RDS
  output$download_rds <- downloadHandler(
    filename = function() { "iPCQ_data.rds" },
    content = function(file) {
      saveRDS(uploaded_data(), file)
    }
  )

  # Download handler for Excel
  output$download_excel <- downloadHandler(
    filename = function() { "iPCQ_data.xlsx" },
    content = function(file) {
      writexl::write_xlsx(uploaded_data(), file)
    }
  )


# For the descriptives of iPCQ --------------------------------------------

}


# Run the application
shinyApp(ui = ui, server = server)
