# Load the Shiny package
library(shiny)

# Define the UI (User Interface)
ui <- fluidPage(
  titlePanel("Dosing calculator"),

  sidebarLayout(
    sidebarPanel(
      title= "aaa",
      numericInput("pH", "pH", value = 7, min = 1, max = 14),
      numericInput("dry", "Drymatter g/L", value = 0, min = 0, max = 100),
      numericInput("temp", "Storage temperature (Celsius)", value = 12, min = 0, max = 50),
      numericInput("urea", "Applied Urea (kg/m^3)", value = 1, min = 0, max = 100),
      numericInput("n_target", "Nitrogen target in the soil (kg/ha)", value = 60, min = 0, max = 100),
      numericInput("va_vol", "VA Volume (m3)", value = 1000, min = 0, max = 10000),
    ),

    mainPanel(
      # Display table output
      #tableOutput("resultTable")
      h4("Duration of treatment in days:"),
      div(
        textOutput("duration"),
        style = "font-size: 16px; font-weight: bold; color: #2E86C1;"
      ),
      h4("Application rate (m^3/ha):"),
      div(
        textOutput("application"),
        style = "font-size: 16px; font-weight: bold; color: #2E86C1;"
      ),
      h4("Land extension (ha):"),
      div(
        textOutput("land"),
        style = "font-size: 16px; font-weight: bold; color: #2E86C1;"
      ),


      # div(
      #  tableOutput("resultTable"),
      #style = "font-size: 16px; font-weight: bold; color: #2E86C1;"
      #),
      # Download button
      downloadButton("downloadData", "Download Table as CSV")

    )
  )
)

# Define the Server logic
server <- function(input, output) {

  # Reactive expression to calculate C and D based on inputs A and B
  result <- reactive({
    pKa<- (2729.92/(input$temp+273.15))+0.090181
    f_nh3<- 1/(10^(pKa-input$temp)+1)
    TAN <- (input$dry*0.145+input$urea*0.466)
    NH3aq <- TAN*f_nh3
    duration <- -5/(-0.8*((TAN/14.01)*1000)*0.0037*exp(0.066*input$temp))
    C_n <- input$dry *0.171 + input$urea *0.466
    application <- input$n_target/C_n
    land <- input$va_vol/application

    # Create a data frame to store the values
    data.frame(pKa = round(pKa,1),
               f_nh3 = round(f_nh3,1),
               TAN = round(TAN,1),
               NH3aq = round(NH3aq,1),
               duration = round(duration,1),
               C_n=C_n,application= round(application,1),
               land=round(land,1))
  })

  # Render the table on the UI
  output$resultTable <- renderTable({
    result()
  })

  output$duration <- renderText({
    result()$duration
  })

  output$application <- renderText({
    result()$application
  })

  output$land <- renderText({
    result()$land
  })

  # Allow the user to download the table as a CSV file
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("results", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(result(), file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
