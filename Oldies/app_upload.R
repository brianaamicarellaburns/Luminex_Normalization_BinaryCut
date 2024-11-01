# Load required packages
library(shiny)
library(DT)

# Function to clean and split comma-separated input values
clean_and_split <- function(input_string) {
  if (is.null(input_string) || nchar(input_string) == 0) {
    return(character(0))  # Return an empty character vector if input is empty
  } else {
    return(strsplit(gsub("\\s+", "", input_string), ",")[[1]])  # Split on commas after removing spaces
  }
}

# Define the UI
ui <- fluidPage(
  
  # Title
  titlePanel("Luminex Data Normalization and Clustering App"),
  
  # Sidebar layout
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      
      # Conditionally show the file input options or the column input options
      conditionalPanel(
        condition = "input.confirmUpload == 0",  # Show file inputs until the "All Data is Uploaded" button is pressed
        numericInput("numPlates", "How many plates were run?", value = NULL, min = 1),
        uiOutput("fileInputs"),
        actionButton("confirmUpload", "All Data is Uploaded")
      ),
      
      # Conditionally show column input fields once the "All Data is Uploaded" button is pressed
      conditionalPanel(
        condition = "input.confirmUpload > 0",  # Show column inputs after pressing the button
        
        # Text input for target bead columns
        textInput("targetBeads", "Enter the Target Bead Column Names (comma separated)", ""),
        
        # Text input for negative control bead columns
        textInput("negativeControlBeads", "Enter the Negative Control Bead Column Name(s) (comma separated)", ""),
        
        # Dropdown for selecting blank bead column
        uiOutput("blankBeadColumn"),
        
        # Dropdown for selecting sample ID column
        uiOutput("sampleIDColumn"),
        
        # Dropdown for selecting dilution column (from calibration data)
        uiOutput("calibrationColumns"),
        
        # Button to begin normalization step
        actionButton("startNormalization", "Begin Step 1 of Normalization")
      )
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      h4("Preview of Uploaded Data"),
      
      # Output: Preview uploaded sample and calibration data with highlighting
      uiOutput("previewTables")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Dynamically generate file inputs based on the number of plates
  output$fileInputs <- renderUI({
    req(input$numPlates)  # Ensure numPlates is entered before showing uploads
    numPlates <- input$numPlates
    uiList <- list()
    
    for (i in 1:numPlates) {
      uiList[[length(uiList) + 1]] <- fileInput(paste0("sampleFile", i), 
                                                paste("Upload Sample Data for Plate ", i),
                                                accept = c("text/csv", ".csv"))
      
      uiList[[length(uiList) + 1]] <- fileInput(paste0("calibrationFile", i), 
                                                paste("Upload Calibration Data for Plate ", i),
                                                accept = c("text/csv", ".csv"))
    }
    
    do.call(tagList, uiList)
  })
  
  # Reactive expression to store sample and calibration data separately
  sampleDataList <- reactive({
    req(input$numPlates)
    numPlates <- input$numPlates
    sampleList <- list()
    
    for (i in 1:numPlates) {
      sampleFile <- input[[paste0("sampleFile", i)]]
      if (!is.null(sampleFile)) {
        sampleList[[i]] <- read.csv(sampleFile$datapath)
      }
    }
    sampleList
  })
  
  calibrationDataList <- reactive({
    req(input$numPlates)
    numPlates <- input$numPlates
    calibrationList <- list()
    
    for (i in 1:numPlates) {
      calibrationFile <- input[[paste0("calibrationFile", i)]]
      if (!is.null(calibrationFile)) {
        calibrationList[[i]] <- read.csv(calibrationFile$datapath)
      }
    }
    calibrationList
  })
  
  # Combine columns from both sample and calibration data
  allColumnNames <- reactive({
    sampleData <- sampleDataList()[[1]]  # Use first sample plate data
    calibrationData <- calibrationDataList()[[1]]  # Use first calibration plate data
    req(sampleData, calibrationData)
    
    # Get unique column names across both sample and calibration data
    combinedColumns <- unique(c(colnames(sampleData), colnames(calibrationData)))
    return(combinedColumns)
  })
  
  # Generate the previews for each uploaded file
  output$previewTables <- renderUI({
    req(input$confirmUpload)  # Wait until user confirms upload
    numPlates <- input$numPlates
    sampleData <- sampleDataList()
    calibrationData <- calibrationDataList()
    
    previewList <- list()
    
    for (i in 1:numPlates) {
      
      # Sample Data preview
      if (!is.null(sampleData[[i]])) {
        totalRows <- nrow(sampleData[[i]])
        previewList[[length(previewList) + 1]] <- h4(paste("Sample Data for Plate", i))
        previewList[[length(previewList) + 1]] <- datatable(
          head(sampleData[[i]], 5), 
          options = list(
            pageLength = 5, 
            dom = 't', 
            info = TRUE, 
            lengthChange = FALSE
          ),
          caption = paste("Showing 5 of", totalRows, "entries")
        )
      }
      
      # Calibration Data preview
      if (!is.null(calibrationData[[i]])) {
        totalRows <- nrow(calibrationData[[i]])
        previewList[[length(previewList) + 1]] <- h4(paste("Calibration Data for Plate", i))
        previewList[[length(previewList) + 1]] <- datatable(
          head(calibrationData[[i]], 5), 
          options = list(
            pageLength = 5, 
            dom = 't', 
            info = TRUE, 
            lengthChange = FALSE
          ),
          caption = paste("Showing 5 of", totalRows, "entries")
        )
      }
    }
    
    do.call(tagList, previewList)
  })
  
  # Dropdown for selecting the blank bead column
  output$blankBeadColumn <- renderUI({
    req(input$confirmUpload)  # Only show after upload confirmation
    selectizeInput(inputId = "blankBead", 
                   label = "Select Blank Bead Column", 
                   choices = allColumnNames(),  # Provide choices after file upload
                   options = list(placeholder = 'Select'),  # Placeholder with no default value
                   selected = NULL)  # Ensure nothing is pre-selected
  })
  
  # Dropdown for selecting the sample ID column
  output$sampleIDColumn <- renderUI({
    req(input$confirmUpload)  # Only show after upload confirmation
    selectizeInput(inputId = "sampleID", 
                   label = "Select Sample ID Column", 
                   choices = allColumnNames(),  # Provide choices after file upload
                   options = list(placeholder = 'Select'),  # Placeholder with no default value
                   selected = NULL)  # Ensure nothing is pre-selected
  })
  
  # Dropdown for selecting calibration columns (dilution)
  output$calibrationColumns <- renderUI({
    req(input$confirmUpload)  # Only show after upload confirmation
    calibrationData <- calibrationDataList()[[1]]  # Use first calibration plate data
    req(calibrationData)
    
    selectizeInput("dilutionColumn", "Select Dilution Column", choices = colnames(calibrationData), selected = "")  # Blank initialized
    
    tagList(
      selectizeInput(
      inputId = "dilutionColumn", 
      label = "Select Dilution Column", 
      choices = colnames(calibrationData),  # Provide choices after file upload
      options = list(placeholder = 'Select'),  # Placeholder with no default value
      selected = NULL  # Ensure nothing is pre-selected
    ),
    selectInput(
      inputId = "dilutionScale", 
      label = "Select Dilution Scale", 
      choices = c("log10", "log2", "base"), 
      selected = NULL  # Ensure no default value is selected
    )    )
  })
  
}

# Create Shiny app
shinyApp(ui = ui, server = server)
