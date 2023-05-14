library(shiny)
library(shinydashboard)
library(ggplot2)
library(bslib)
library(gridlayout)
library(DT)
library(summarytools)
library(kableExtra)
source("R/utils.R")
source("R/pre_process_data.R")

# parameters
model_boxes <- c("forecast_valuebox",
                       "actual_valuebox",
                       "accuracy_valuebox")

alternative_boxes <- c("manual_forecast_valuebox",
                       "manual_actual_valuebox",
                       "manual_accuracy_valuebox")

value_boxes <- map(model_boxes, create_valuebox)
aternative_value_boxes <- map(alternative_boxes, create_valuebox)




ui <- navbarPage(
  title = "Demand Forecasting App",
  selected = "Data Summary",
  collapsible = TRUE,
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "flatly",
    `enable-gradients` = TRUE,
    `enable-shadows` = TRUE,
    primary = "#25bca4",
    success = "#875AE7",
    font_scale = NULL
  ),
  tabPanel(
    title = "Data Summary",
    icon = icon("magnifying-glass-chart"),
    card(
      full_screen = TRUE,
      card_header(
        card(full_screen = TRUE, card_header("Data Upload")),
        fileInput(
          inputId = "dataUpload",
          label = "Upload CSV file",
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        )
      ),
      card_body(
        card(
          card_header(em("Data Summary")),
          card_body(
            verbatimTextOutput(outputId = "summaryTable"),
            min_height = "500px",
            fill = TRUE
          ),
          full_screen = TRUE
        )
      )
    )
  ),
  tabPanel(
    title = "Forecast",
    icon = icon("magic-wand-sparkles"),
    grid_container(
      layout = c(
        "user_input forecastArea"
      ),
      row_sizes = c(
        "1fr"
      ),
      col_sizes = c(
        "250px",
        "1fr"
      ),
      gap_size = "10px",
      grid_card(
        area = "user_input",
        card_header("Settings"),
        card_body(
          selectInput(
            inputId = "facilityInput",
            label = "Select a facility:",
            choices = NULL
          ),
          selectizeInput(
            inputId = "productInput",
            "Select a product:",
            choices = NULL
          ),
          sliderInput(
            inputId = "forecast_period_input",
            label = "Period to Forecast",
            min = 0,
            max = 36,
            value = 3,
            width = "100%"
          ),
          numericInput(
            inputId = "start_year_input",
            label = "Data's Start Year",
            value = 2020,
            min = 2020
          ),
          numericInput(
            inputId = "start_month_input",
            label = "Data's start month",
            value = 5,
            min = 1,
            max = 12
          ),
          radioButtons(
            inputId = "frequencyInput",
            label = "Report Frequency",
            choices = list("Bi Monthly" = 6, "Monthly" = 12, "Quarterly" = 4),
            width = "100%"
          ),
          actionButton(
            inputId = "forecastButton",
            icon = icon("magic-wand-sparkles"),
            label = "Forecast"
          )
        )
      ),
      grid_card(
        area = "forecastArea",
        card_body(plotOutput(outputId = "forecast_plot")),
        card_footer(
          DTOutput(outputId = "forecast_table", width = "100%")
        )
      )
    )
  ),
  tabPanel(
    title = "Accuracy",
    icon = icon("bullseye"),
    grid_container(
      layout = c(
        "Demand",
        "Demand"
      ),
      row_sizes = c(
        "165px",
        "1fr"
      ),
      col_sizes = c(
        "1fr"
      ),
      gap_size = "10px",
      grid_card(
        area = "Demand",
        full_screen = TRUE,
        card_header(
          em("Input options"),
          fluidRow(
            column(
              width = 4,
              div(
                class = "form-group",
                selectizeInput(
                  inputId = "forecast_year",
                  label = "Year to forecast",
                  selected = 2021,
                  choices = c(2021, 2022, 2023)
                ),
                numericInput(
                  inputId = "manual_forecast",
                  label = "Insert alternative forecast",
                  value = NULL,
                  min = 1,
                  max = 10^9
                ),
                tags$br()
              )
            ),
            column(
              width = 8,
              "Assumptions;",
              tags$ul(
                tags$li("for the months without reported amc, \n the following reported amc would be used as their monthly consumption"),
                tags$li("The financial year starts on July and ends on June"),
                tags$li("Outliers in the records have been replaced by median values"),
                tags$li("The formula used to check for percentage accuracy sourced from BUQ guideline workbook")
              ),
              tags$img(src = "formula.png")
            ),

          card(full_screen = FALSE)
        ),
        tags$br(),
            em(h4("Model Accuracy"),style = "color: black;"),
        card_body(
          fluidRow(
            value_boxes,
            style = "background-color: #25bca4; border-radius: 15px; padding: 20px;"
          ),
        tags$br(),
            em(h4("Alternative Method Accuracy"),style = "color: black;"),
          fluidRow(
            aternative_value_boxes,
            style = "background-color: #875AE7; border-radius: 15px; padding: 20px; margin-top: 20px;"
          )
        )
      )
    )
  )
))


# shinyApp(ui = ui, server = server)
