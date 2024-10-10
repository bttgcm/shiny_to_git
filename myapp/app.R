# Load the Shiny package
library(shiny)

# Define the UI (User Interface)
ui <- fluidPage(
  titlePanel("Dosing calculator"),

  sidebarLayout(
    sidebarPanel(
      title= "aaa",
      numericInput("pH", "pH", value = 7.5, min = 1, max = 14, step=0.1),
      numericInput("dry", "Drymatter g/L", value = 0, min = 0, max = 100),
      numericInput("temp", "Storage temperature (Celsius)", value = 12, min = 0, max = 50),
      numericInput("urea", "Applied Urea (kg/m^3)", value = 1, min = 0, max = 100),
      numericInput("n_target", "Nitrogen target in the soil (kg/ha)", value = 60, min = 0, max = 100),
      numericInput("va_vol", "VA Volume (m3)", value = 1000, min = 0, max = 10000)
    ),

    mainPanel(
      # Display table output
      #tableOutput("resultTable")
      h4("Duration of treatment in days:"),
      div(
        textOutput("duration"),
        style = "font-size: 16px; font-weight: bold; color: #2E86C1;"
      ),


      p("the duration is mostly affected by the amount of urea used"),
      h4("Application rate (m^3/ha):"),
      p("amount of fertilised applied to agricultural land"),
      div(
        textOutput("application"),
        style = "font-size: 16px; font-weight: bold; color: #2E86C1;"
      ),
      h4("Land extension (ha):"),
      p("Size of land that can be fertilised with the given rate of fertilisation."),
      div(
        textOutput("land"),
        style = "font-size: 16px; font-weight: bold; color: #2E86C1;"
      ),


      # div(
      #  tableOutput("resultTable"),
      #style = "font-size: 16px; font-weight: bold; color: #2E86C1;"
      #),
      # Download button
      downloadButton("downloadData", "Download Results in PDF")

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

################################################## PDF export functionality
output$downloadData <- downloadHandler(
  filename = function() {
    paste("calculation_result_", Sys.Date(), ".pdf", sep = "")
  },
  content = function(file) {
    # Create a temporary markdown file
    tempReport <- file.path(tempdir(), "tempReport.Rmd")

    # Write the R Markdown content
    reportContent <- "

# Summary of calculation

---

## Input values

The following values were provided:

- **pH**: `r input$pH`
- **Drymatter**: `r input$dry` $g/L$
- **Temperature**: `r input$temp` C
- **Applied Urea**: `r input$urea` $kg/m^3$

---

## Output

Given the input data the following are calculated:

### Duration of treatment: `r result()$duration` days

### Application rate: `r result()$application`  $m^3/ha$

### Land extension: `r result()$land` $ha$

---

## To Do:
expand text, add logo and images. Add calculations


      "

    # Write the content to the temporary file
    writeLines(reportContent, tempReport)

    # Render the PDF
    rmarkdown::render(tempReport, output_file = file)
  }
)
}






# Run the app
shinyApp(ui = ui, server = server)
