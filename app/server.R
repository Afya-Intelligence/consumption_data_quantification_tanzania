
server <- function(input, output, session) {
  # bs_themer()
  # for debugging
  options(shiny.debug = TRUE)
  #see if the server function is called
  print("Server function called.")

# Define a reactive object to store the selected data ---------------------


  data <- reactive({
    req(input$dataUpload) # Require that a file is selected
    print("Selected file path:")
    print(input$dataUpload$datapath)
    pre_process_data(input$dataUpload$datapath)
  })


# create input for the products -------------------------------------------

  product_choices <- reactive({
    choices <- unique(data()$product_cleaned)
    return(choices)
  })

  # Update selectizeInput choices when input$selected_values changes
  observeEvent(input$facilityInput, {
    isolate({
      updateSelectizeInput(
        session = session,
        inputId = "productInput",
        choices = product_choices(),
        server = TRUE
      )
    })

  })


# create input for the facilities -----------------------------------------

  # Create a reactive object to get the distinct variables in the facility column
  facility_choices <- reactive({
    choices <- unique(data()$facility)
    return(choices)
  })

  # Update the choices of the productInput selectInput widget
  observe({
    updateSelectInput(
      session = session,
      inputId = "facilityInput",
      choices = facility_choices()
    )
  })


# display summary of the data ---------------------------------------------


  observeEvent(input$dataUpload, {
    isolate({
      output$summaryTable <- renderPrint({
        dfSummary(data(), graph.col = FALSE)
      })
    })
  })


# create forecast plot and table ------------------------------------------


  observeEvent(input$forecastButton, {
    start_year <- input$start_year_input
    start_month <- ceiling(as.numeric(input$start_month_input))
    frequency <- ceiling(as.numeric(input$frequencyInput))
    df <- data()
    # Get the forecast plot
    #
    output_data <- forecast_facility_product(df = df,product_name = input$productInput, facility_name = input$facilityInput, reporting_frequency = frequency, start_year = start_year, start_month = start_month, forecast_period = input$forecast_period_input)

    output$forecast_plot <- renderPlot({
      output_data$plot
    })
    output$forecast_table <- renderDT({
      output_data$summary
    })

  })


# create value boxes for facility x product accuracy metrics --------------


  observeEvent(input$forecast_year, {
    forecast_year <- as.numeric(input$forecast_year)

    output$forecast_valuebox <- renderValueBox({
      valueBox(
        "Forecast Value",
        value = round(check_model_accuracy(df = data(), forecast_year = forecast_year, facility_name = input$facilityInput, product_name = input$productInput)$forecast,1),
        icon = icon("ranking-star")
      )
    })
  })
  #
  observeEvent(input$forecast_year, {
    forecast_year <- as.numeric(input$forecast_year)

    output$actual_valuebox <- renderValueBox({
      valueBox(
        "Actual Consumption",
        value = round(check_model_accuracy(df = data(), forecast_year = forecast_year, facility_name = input$facilityInput, product_name = input$productInput)$consumption,1),
        icon = icon("chart-simple")
      )
    })
  })

  observeEvent(input$forecast_year, {
    forecast_year <- as.numeric(input$forecast_year)

    output$accuracy_valuebox <- renderValueBox({
      valueBox(
        "Accuracy",
        value = round(calculate_accuracy(df = data(), forecast_year = forecast_year, facility_name = input$facilityInput, product_name = input$productInput),1),
        icon = icon("bullseye")
      )
    })
  })


  observeEvent(input$manual_forecast, {
    manual_forecast <- as.numeric(input$manual_forecast)

    output$manual_forecast_valuebox <- renderValueBox({
      valueBox(
        "Accuracy",
        value = manual_forecast,
        icon = icon("ranking-star")
      )
    })
  })

  observeEvent(input$forecast_year, {
    forecast_year <- as.numeric(input$forecast_year)

    output$manual_actual_valuebox <- renderValueBox({
      valueBox(
        "Actual Consumption",
        value = round(check_model_accuracy(df = data(), forecast_year = forecast_year, facility_name = input$facilityInput, product_name = input$productInput)$consumption),
        icon = icon("chart-simple")
      )
    })
  })

  observeEvent(input$manual_forecast, {
    forecast_year <- as.numeric(input$forecast_year)
    manual_forecast <- as.numeric(input$manual_forecast)


    output$manual_accuracy_valuebox <- renderValueBox({
      annual_consumption <- round(check_model_accuracy(df = data(), forecast_year = forecast_year, facility_name = input$facilityInput, product_name = input$productInput)$consumption)
      valueBox(
        "Alternative accuracy",
        value = round(calculate_manual_accuracy(forecast = manual_forecast, consumption = annual_consumption),1),
        icon = icon("bullseye")
      )
    })
  })



}
