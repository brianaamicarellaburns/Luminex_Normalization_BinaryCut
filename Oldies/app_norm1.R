# Load required packages
library(shiny)
library(DT)
library(ggplot2)
library(shinyjs)

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
  useShinyjs(),  # Include shinyjs
  
  # Title
  titlePanel("Luminex Data Normalization and Clustering App"),
  
  # Sidebar layout
  sidebarLayout(
    
    # Sidebar layout and input section
    sidebarPanel(
      conditionalPanel(
        condition = "input.confirmUpload == 0",
        numericInput("numPlates", "How many plates were run?", value = NULL, min = 1),
        uiOutput("fileInputs"),
        actionButton("confirmUpload", "All Data is Uploaded")
      ),
      
      # Show column input fields only when Step 1 hasn't started
      conditionalPanel(
        condition = "input.confirmUpload > 0 && input.startNormalization == 0",
        h4("Preview of Uploaded Data"),
        uiOutput("previewTables"),
        textInput("targetBeads", "Enter the Target Bead Column Names (comma separated)", ""),
        textInput("negativeControlBeads", "Enter the Negative Control Bead Column Name(s) (comma separated)", ""),
        uiOutput("blankBeadColumn"),
        uiOutput("sampleIDColumn"),
        uiOutput("calibrationColumns"),
        actionButton("startNormalization", "Begin Data Pre-Processing")
      ),
      
      # Show transformation options after Step 1 begins
      conditionalPanel(
        condition = "input.startNormalization > 0",
        h4("Transform Data to Resemble Normal Distribution (as close as possible)"),
        uiOutput("blankTransformation"),
        uiOutput("negativeControlTransformation"),
        actionButton("confirmNormalization", "Run Normalization")
      )
    ),
    
    # Main panel for output
    mainPanel(
      conditionalPanel(
        condition = "input.confirmUpload > 0 && input.startNormalization == 0",
        h4("Preview of Uploaded Data"),
        uiOutput("previewTables")
      ),
      
      conditionalPanel(
        condition = "input.startNormalization > 0",
        h4("Distribution Plot"),
        uiOutput("chooseBead"),
        plotOutput("beadPlot")
      )
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
    
    tagList(
      selectizeInput(
        inputId = "dilutionColumn", 
        label = "Select Dilution Column", 
        choices = colnames(calibrationData),  # Provide choices after file upload
        options = list(placeholder = 'Select'),  # Placeholder with no default value
        selected = NULL  # Ensure nothing is pre-selected
      ),
      
      # Dropdown for selecting dilution scale
      selectInput(
        inputId = "dilutionScale", 
        label = "Select Dilution Scale", 
        choices = c("log10", "log2", "base"), 
        selected = NULL  # Ensure no default value is selected
      )
    )
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
  
  # Show preview tables after "All Data is Uploaded"
  observeEvent(input$confirmUpload, {
    shinyjs::show("previewTables")  # Show preview tables
  })
  
  # Hide the upload and column input section after 'All Data is Uploaded'
  observeEvent(input$confirmUpload, {
    shinyjs::show("previewTables")  # Show preview tables
  })
  
  # Reactive expression for transformed blank data
  transformedBlank <- reactive({
    req(input$blankTransform)
    blankData <- sampleDataList()[[1]][[input$blankBead]]  # Assuming blank data is from the selected column
    
    # Handle missing or zero values before transformation
    if (input$blankTransform == "Log Transformation") {
      return(log10(blankData + 1))  # Ensure no negative or zero values for log
    } else if (input$blankTransform == "Square Root Transformation") {
      return(sqrt(blankData))
    } else {
      return(blankData)
    }
  })
  
  # UI for blank transformation dropdown
  output$blankTransformation <- renderUI({
    req(input$startNormalization)
    selectInput("blankTransform", "Transformation for Blank Bead", 
                choices = c("No Transformation", "Log Transformation", "Square Root Transformation"),
                selected = "No Transformation")
  })
  
  # Reactive expression for transformed negative control data (one for each negative control bead)
  transformedNegControls <- reactive({
    negControlBeads <- clean_and_split(input$negativeControlBeads)
    negControlDataList <- list()
    
    # Iterate over each negative control bead and apply the selected transformation
    for (negControl in negControlBeads) {
      negControlData <- sampleDataList()[[1]][[negControl]]  # Data for each negative control bead
      
      # Check the transformation type for this specific bead
      transformType <- input[[paste0("transform_", negControl)]]
      
      if (transformType == "Log Transformation") {
        negControlDataList[[negControl]] <- log10(negControlData + 1)  # Ensure no negative or zero values for log
      } else if (transformType == "Square Root Transformation") {
        negControlDataList[[negControl]] <- sqrt(negControlData)
      } else {
        negControlDataList[[negControl]] <- negControlData
      }
    }
    
    return(negControlDataList)
  })
  
  # Dropdown to select which bead's plot to show (on the right side)
  output$chooseBead <- renderUI({
    req(input$startNormalization)
    selectInput("chosenBead", "Choose Bead", 
                choices = c("Blank Bead", clean_and_split(input$negativeControlBeads)), 
                selected = "Blank Bead")
  })
  
  # Reactive expression to determine which bead data to plot based on the dropdown
  selectedBeadData <- reactive({
    req(input$chosenBead)
    if (input$chosenBead == "Blank Bead") {
      return(transformedBlank())
    } else {
      negControlDataList <- transformedNegControls()  # List of transformed neg control data
      return(negControlDataList[[input$chosenBead]])  # Return the selected negative control's data
    }
  })
  
  # Plot for the selected bead
  output$beadPlot <- renderPlot({
    req(selectedBeadData())
    ggplot() +
      geom_histogram(aes(x = selectedBeadData()), bins = 30, fill = "lightblue", color = "black") +
      labs(title = paste("Distribution of", input$chosenBead), x = "Intensity", y = "Frequency") +
      theme_classic()
  })
  
  output$negativeControlTransformation <- renderUI({
    req(input$startNormalization)
    negControlBeads <- clean_and_split(input$negativeControlBeads)
    
    # Generate transformation dropdowns for each negative control bead
    lapply(negControlBeads, function(negControl) {
      selectInput(inputId = paste0("transform_", negControl), 
                  label = paste("Transformation for", negControl), 
                  choices = c("No Transformation", "Log Transformation", "Square Root Transformation"),
                  selected = "No Transformation")
    })
  })
  
  # Handle "My data looks normally distributed" button
  observeEvent(input$confirmNormalization, {
    shinyjs::hide("fileInputs")
    shinyjs::hide("confirmUpload")
    shinyjs::show("startNormalization")
  })
  
  # Show transformation options once Step 1 is initiated
  observeEvent(input$startNormalization, {
    shinyjs::show("blankTransformation")  # Show blank transformation dropdown
    shinyjs::show("negControlTransformations")  # Show negative control transformations
    shinyjs::show("chooseBead")  # Show bead selection for plots
    shinyjs::show("beadPlot")  # Show bead plot
    shinyjs::hide("previewTables")  # Hide preview of uploaded data
  })
  
}

# Create Shiny app
shinyApp(ui = ui, server = server)

