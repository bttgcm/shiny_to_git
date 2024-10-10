# Install required libraries (if not already installed)
# install.packages("shiny")
# install.packages("rmarkdown")

library(shiny)
library(rmarkdown)

# Define UI for the app
ui <- fluidPage(
  titlePanel("A + B Calculator with PDF Export"),

  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      # Input fields for A and B
      numericInput("numA", "Enter value for A:", 0),
      numericInput("numB", "Enter value for B:", 0),

      # Button to export result to PDF
      downloadButton("exportPDF", "Export to PDF")
    ),

    # Show the result of A + B
    mainPanel(
      h3("Calculation:"),
      verbatimTextOutput("result")
    )
  )
)

# Define server logic required to calculate A + B and export to PDF
server <- function(input, output) {

  # Reactive expression to calculate the sum of A and B
  result <- reactive({
    A <- input$numA
    B <- input$numB
    C <- A + B
    return(C)
  })

  # Display the result in the UI
  output$result <- renderText({
    paste("A + B =", result())
  })

  # PDF export functionality
  output$exportPDF <- downloadHandler(
    filename = function() {
      paste("calculation_result_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      # Create a temporary markdown file
      tempReport <- file.path(tempdir(), "tempReport.Rmd")

      # Write the R Markdown content
      reportContent <- "
      ---
      title: 'Calculation Result'
      output: pdf_document
      ---

      ```{r}
      A <- input$numA
      B <- input$numB
      C <- A + B
      cat('The result of A + B is', C)
      ```

      **Additional Text:**

      This is a basic Shiny app that adds two numbers together and exports the result to a PDF file.
      "

      # Write the content to the temporary file
      writeLines(reportContent, tempReport)

      # Render the PDF
      rmarkdown::render(tempReport, output_file = file, params = list(A = input$numA, B = input$numB))
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
