#### Load required packages ####
library(shiny)
library(DT)
library(ggplot2)
library(shinyjs)
library(pracma)
library(mgcv)

#### Define functions ####
# Function to clean and split comma-separated input values
clean_and_split <- function(input_string) {
  if (is.null(input_string) || nchar(input_string) == 0) {
    return(character(0))  # Return an empty character vector if input is empty
  } else {
    return(strsplit(gsub("\\s+", "", input_string), ",")[[1]])  # Split on commas after removing spaces
  }
}

#### Define the UI ####
## Define the UI
ui <- fluidPage(
  useShinyjs(),  # Include shinyjs
  
  # Title
  titlePanel("Luminex Data Normalization and Clustering App"),
  
  
  # Sidebar layout
  sidebarLayout(
    
    #### Define the UI - Side bar panel ####
    
    sidebarPanel(
      id = "sidebarPanel",
      
      conditionalPanel(
        condition = "input.confirmUpload == 0",
        numericInput("numPlates", "How many plates were run?", value = NULL, min = 1),
        uiOutput("fileInputs"),
        actionButton("confirmUpload", "All Data is Uploaded")
      ),
      
      conditionalPanel(
        condition = "input.confirmUpload > 0 && input.startTransformation == 0",
        h4("Preview of Uploaded Data"),
        uiOutput("previewTables"),
        textInput("targetBeads", "Enter the Target Bead Column Names (comma separated)", ""),
        textInput("negativeControlBeads", "Enter the Negative Control Bead Column Name(s) (comma separated)", ""),
        uiOutput("blankBeadColumn"),
        uiOutput("sampleIDColumn"),
        uiOutput("calibrationColumns"),
        uiOutput("dilutionFactorInput"),
        actionButton("startTransformation", "Begin Data Pre-Processing")
      ),
      
      div(
        id = "transformationPanel",  # Adding an ID to this section
        conditionalPanel(
          condition = "input.startTransformation > 0",
          h4("Transform Data to Resemble Normal Distribution (as close as possible)"),
          uiOutput("blankTransformation"),
          uiOutput("negativeControlTransformation"),
          actionButton("confirmNormalization", "Run Normalization")
        )),
      
      conditionalPanel(
        condition = "output.normalizedDataAvailable == true",
        actionButton("clusterButton", "Cluster Normalized Data")  # Cluster button
      ),
      
      div(
        id = "clusterTransformationPanel",  
        conditionalPanel(
          condition = "input.clusterButton > 0",
          h4("Transform Data to Resemble Normal Distribution (as close as possible)"),
          uiOutput("antigenTransformation"),
          actionButton("runClustering", "Run Clustering")
        )),
      
      div(
        id = "clusteringPanel",  
        conditionalPanel(
          condition = "input.runClustering > 0",
          h4("Customize Clustering"),
          uiOutput("chooseClusterAntigen"),
          uiOutput("chooseNumClusterGroups"),
          uiOutput("chooseNumNodeGroups"),
          actionButton("confirmClustering", "Confirm Clustering")
        )),
      
      div(
        id = "closeApp",
        conditionalPanel(
          condition = "output.clusteredDataAvailable == true",
          downloadButton("downloadFinalData", "Download Data")
          ))
    ),
    
    #### Define the UI - Main panel for output ####
    mainPanel(
      # View preview data
      conditionalPanel(
        condition = "input.confirmUpload > 0 && input.startTransformation == 0",
        h4("Preview of Uploaded Data"),
        uiOutput("previewTables")
      ),
      
      # Transform control data
      conditionalPanel(
        condition = "input.startTransformation > 0",
        div(id = "distributionPanel",  # Wrap the distribution plot section in a div with an ID
            h4("Distribution Plot"),
            uiOutput("chooseBead"),
            plotOutput("beadPlot")
        )),
      
      # Preview (and download) normalized data
      div(
        id = "normalizedDataPanel",  
        conditionalPanel(
          condition = "output.normalizedDataAvailable == true",
          h3("Normalized Data"),
          dataTableOutput("normalizedData"),  # This will show the preview of the data
          downloadButton("downloadData", "Download Normalized Data")  # Button to download the data
        )),
      
      # Transform normalized data
      conditionalPanel(
        condition = "input.clusterButton > 0",
        div(id = "transformationPlots",  
            h4("Distribution Plots"),
            uiOutput("chooseNormalizedAntigen"),
            plotOutput("transformationPlot1"),
            plotOutput("transformationPlot2")
        )),
      
      # Cluster normalized transformed data
      conditionalPanel(
        condition = "input.runClustering > 0",
        div(id = "clusteringPlots",  
            h4("Clustering Plots"),
            plotOutput("clusteredDensityPlot"),
            plotOutput("clusteredNodePlot"),
            plotOutput("originalDensityPlot")
        )),
      
      div(
        id = "finalDataPanel",  
        conditionalPanel(
          condition = "output.clusteredDataAvailable == true",
          h3("Normalized and Clustered Data"),
          dataTableOutput("finalData"),  # This will show the preview of the data
          actionButton("AllDone", "Close Application")  # Close
        ))
      )
  )
)

#### Define server logic ####
server <- function(input, output, session) {
  #### Upload Data ####
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
  
  #### Input column names ####
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
  
  # UI for selecting the dilution factor used for experimental values
  output$dilutionFactorInput <- renderUI({
    req(input$confirmUpload)  # Only show after upload confirmation
    textInput(inputId = "expDilutionFactor", 
                 label = "Enter the Dilution Factor Used for Experimental Values in Fraction Form (e.g. 1/500)", 
                 value = NULL)  # User enters a numeric dilution factor
  })
  

  
  #### Control transformations ####
  
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
    req(input$startTransformation)
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
    req(input$startTransformation)
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
    req(input$startTransformation)
    negControlBeads <- clean_and_split(input$negativeControlBeads)
    
    # Generate transformation dropdowns for each negative control bead
    lapply(negControlBeads, function(negControl) {
      selectInput(inputId = paste0("transform_", negControl), 
                  label = paste("Transformation for", negControl), 
                  choices = c("No Transformation", "Log Transformation", "Square Root Transformation"),
                  selected = "No Transformation")
    })
  })
  
  # Show transformation options once Step 1 is initiated
  observeEvent(input$startTransformation, {
    shinyjs::show("blankTransformation")  # Show blank transformation dropdown
    shinyjs::show("negControlTransformations")  # Show negative control transformations
    shinyjs::show("chooseBead")  # Show bead selection for plots
    shinyjs::show("beadPlot")  # Show bead plot
    shinyjs::hide("previewTables")  # Hide preview of uploaded data
  })
  
  #### Orthogonal Regression Function ####
  transformation <- function(value, method) {
    if (method == "Log Transformation") {
      return(log10(value + 1))
    } else if (method == "Square Root Transformation") {
      return(sqrt(value))
    } else {
      return(value)  # No transformation
    }
  }
  
  untransform <- function(value, method) {
    if (method == "Log Transformation") {
      return(10^value - 1)
    } else if (method == "Square Root Transformation") {
      return(value^2)
    } else {
      return(value)  # No transformation
    }
  }
  
  compute_corrected_values <- function(plate, antigens, negatives, blankTransform, negTransformations) {
    # Apply transformation to blank bead
    plate$transformedBlank <- transformation(plate[[input$blankBead]], blankTransform)
    
    # Perform orthogonal regression for each negative control
    for (neg in negatives) {
      # Apply the selected transformation for this negative control
      transformedNeg = paste0(neg, "t")
      
      # Use the transformation method specific to each negative control
      plate[[transformedNeg]] = transformation(plate[[neg]], negTransformations[[neg]])
      
      # Perform orthogonal regression between blank and transformed negative control
      oreg <- odregress(plate$transformedBlank, plate[[transformedNeg]])
      plate[[paste0(transformedNeg, "f")]] <- oreg$fitted
      
      # Untransform the fitted values
      plate[[paste0(neg, "u")]] = untransform(plate[[paste0(transformedNeg, "f")]], negTransformations[[neg]])
    }
    
    
    # Compute the median fitted value for background correction and rename it to Background
    plate$Background <- apply(plate[, paste0(negatives, "u")], 1, median, na.rm = TRUE)
    
    # Apply background correction to each target antigen
    for (antigen in antigens) {
      plate[[paste0(antigen, "_back")]] <- plate[[antigen]] - plate$Background
    }
    return(plate)
  }
  
  #### GAM function ####
  calculate_gam_pnf <- function(calibration_data, antigens, transform_cal, dilution_value, DilutionFactorCol) {
    pnf_values <- list()  # Store the PNF values for each plate
    
    for (antigen in antigens) {  # Loop over each antigen
      # Build the formula for each antigen using the user-selected dilution column
      formula_str <- paste0(antigen, " ~ s(log1p(", DilutionFactorCol, "))")  # Corrected line
      formula <- as.formula(formula_str)
      print(paste0("Formula for antigen ", antigen, ": ", formula_str))  # Debugging
      
      for (cal_plate in calibration_data) {
        print("Calibration plate structure before transformation:")
        print(str(cal_plate))  # Check if the plate is valid before transformation
        
        # Apply the transformation to the dilution column
        cal_plate[[DilutionFactorCol]] <- transform_cal(cal_plate[[DilutionFactorCol]])
        
        # Check for invalid transformations (like -Inf or NaN)
        print("Dilution column after transformation:")
        print(cal_plate[[DilutionFactorCol]])
        
        # Remove rows with NA, Inf, or -Inf values
        cal_plate <- cal_plate[is.finite(cal_plate[[DilutionFactorCol]]), ]
        
        # Check if there are any rows left after filtering
        print(paste("Number of rows after filtering:", nrow(cal_plate)))
        
        if (nrow(cal_plate) == 0) {
          print(paste("Error: Calibration plate has no valid data after removing NA/Inf values for antigen", antigen))
          showNotification(paste("Skipping plate due to no valid data for antigen", antigen), type = "warning")
          next  # Skip to the next plate if no valid data remains
        }
        
        # Fit the GAM model using the formula and cleaned calibration data
        gam_model <- tryCatch({
          gam(formula, data = cal_plate)
        }, error = function(e) {
          print(paste("Error fitting GAM for antigen:", antigen))
          print(e$message)  # Print the actual error message
          return(NULL)
        })
        
        if (is.null(gam_model)) next  # Skip if the model couldn't be fitted
        
        # Predict intensity for each row (using the Dilution column)
        log_dilution_values <- cal_plate[[DilutionFactorCol]]  # All dilution values from the plate
        pnf_values <- predict(gam_model, newdata = data.frame(Dilution = log_dilution_values))  # Should return a vector
        
        # Ensure pnf_values is a vector and matches the length of the plate rows
        if (length(pnf_values) != nrow(plate)) {
          print("Error: PNF values do not match the number of rows in the plate")
          next
        }
        
        # Store the PNF value for this antigen and plate
        pnf_for_plate[[antigen]] <- pnf_values
        
      }
    }
    
    return(pnf_values)  # Return the computed PNF values for each antigen
  }
  
  
  
  #### Perform Normalization ####
    # Use reactiveValues to store combined_normalized_data
    rv <- reactiveValues(normalized_data = NULL)

    # Normalization
    observeEvent(input$confirmNormalization, {
      shinyjs::hide("transformationPanel")  # Hide only transformation section
      shinyjs::hide("distributionPanel")    # Still hide the distribution plot section
      shinyjs::show("normalizedDataPanel")  # Ensure normalized data panel is visible
      
      # Step 1: Convert the dilution factor from fraction to numeric safely
      dilution_value <- tryCatch({
        # Evaluate the entered dilution factor as numeric (supports fraction input)
        as.numeric(eval(parse(text = input$expDilutionFactor)))
      }, error = function(e) {
        # If the input is invalid, show an error notification
        showNotification("Invalid dilution factor. Please enter a valid fraction like 1/500.", type = "error")
        return(NULL)  # Return NULL to stop further execution
      })
      
      # If dilution_value is successfully parsed and not NULL, continue processing
      if (!is.null(dilution_value)) {
        # Define the transformation function (log1p in this case)
        transform_cal <- function(x) { log1p(x) }
        
        # Collect user input for antigens, negatives, transformations
        plates <- sampleDataList()  # User uploaded sample plates
        antigens <- clean_and_split(input$targetBeads)  # User inputted antigen columns
        negatives <- clean_and_split(input$negativeControlBeads)
        blankTransform <- input$blankTransform
        negTransformations <- setNames(lapply(negatives, function(neg) input[[paste0("transform_", neg)]]), negatives)
        
        # Step 2: Correct values for each plate
        corrected_plates <- lapply(plates, function(plate) {
          compute_corrected_values(plate, antigens, negatives, blankTransform, negTransformations)
        })
        
        # Step 3: Compute PNF for calibration plates
        calibration_data <- calibrationDataList()  # User uploaded calibration plates
        DilutionFactorCol <- input$dilutionColumn  # User-selected dilution column
        pnf_values <- list()  # Store PNF values for each antigen
        
        for (antigen in antigens) {  # Loop over each antigen
          formula_str <- paste0(antigen, " ~ s(log1p(", DilutionFactorCol, "))")
          formula <- as.formula(formula_str)
          print(paste0("Formula for antigen ", antigen, ": ", formula_str))  # Debugging
          
          for (cal_plate in calibration_data) {
            print("Calibration plate structure before transformation:")
            print(str(cal_plate))  # Check if the plate is valid before transformation
            
            # Apply the transformation to the dilution column
            cal_plate[[DilutionFactorCol]] <- transform_cal(cal_plate[[DilutionFactorCol]])
            
            # Check for invalid transformations (like -Inf or NaN)
            print("Dilution column after transformation:")
            print(cal_plate[[DilutionFactorCol]])
            
            # Remove rows with NA, Inf, or -Inf values
            cal_plate <- cal_plate[is.finite(cal_plate[[DilutionFactorCol]]), ]
            
            # Check if there are any rows left after filtering
            print(paste("Number of rows after filtering:", nrow(cal_plate)))
            
            if (nrow(cal_plate) == 0) {
              print(paste("Error: Calibration plate has no valid data after removing NA/Inf values for antigen", antigen))
              showNotification(paste("Skipping plate due to no valid data for antigen", antigen), type = "warning")
              next  # Skip to the next plate if no valid data remains
            }
            
            # Ensure the necessary dilution column is present
            if (!(DilutionFactorCol %in% colnames(cal_plate))) {
              print(paste("Error: Dilution column", DilutionFactorCol, "not found in the cleaned calibration plate"))
              next
            }
            
            # Fit the GAM model using the formula and cleaned calibration data
            gam_model <- tryCatch({
              gam(formula, data = cal_plate)
            }, error = function(e) {
              print(paste("Error fitting GAM for antigen:", antigen))
              print(e$message)  # Print the actual error message
              return(NULL)
            })
            
            if (is.null(gam_model)) next  # Skip if the model couldn't be fitted
            
            # Predict intensity at the user-inputted dilution value
            log_dilution_value <- transform_cal(dilution_value)
            pnf_value <- predict(gam_model, newdata = data.frame(Dilution = log_dilution_value))
            
            # Store the PNF value for this plate and antigen
            pnf_values[[antigen]] <- pnf_value
          }
        }
        
        # Step 4: Normalize antigen values for each plate using PNF
        
        final_normalized_plates <- lapply(seq_along(corrected_plates), function(i) {
          plate <- corrected_plates[[i]]  # The corrected experimental plate
          pnf_for_plate <- pnf_values  # Corresponding PNF for the plate
          
          for (antigen in antigens) {
            antigen_norm <- paste0(antigen, "_norm")  # Column for normalized antigen values
            antigen_back <- paste0(antigen, "_back")  # Background-corrected antigen column
            
            # Normalize the antigen using the corresponding PNF for that antigen
            plate[[antigen_norm]] <- plate[[antigen_back]] / rep(pnf_for_plate[[antigen]], nrow(plate))
          }
          
          # Retain only SampleID and antigen_norm columns
          keep_cols <- c("SampleID", grep("_norm$", colnames(plate), value = TRUE))  # Keep only SampleID and columns ending with _norm
          plate <- plate[, keep_cols, drop = FALSE]
          
          return(plate)  # Return the fully normalized plate
        })
        
        # Combine all normalized experimental plates into one final data frame
        combined_normalized_data <- do.call(rbind, final_normalized_plates)
        
        # Store the result in reactiveValues
        rv$normalized_data <- combined_normalized_data  # Store the data in reactiveValues
        
        # Notify the user that the normalization is complete
        showNotification("Normalization process complete. Data is ready!", type = "message")
        
        shinyjs::show("normalizedDataPanel")
      }
    })
    
    
    # Check if normalized data is available
    output$normalizedDataAvailable <- reactive({
      !is.null(rv$normalized_data)  # Returns TRUE if normalized data is available
    })
    outputOptions(output, "normalizedDataAvailable", suspendWhenHidden = FALSE)
    output$normalizedData <- renderDataTable({
      head(rv$normalized_data, 10)  # Show the first 10 rows of normalized data
    })
    
    # Download handler for the normalized data
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("normalized_data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(rv$normalized_data, file, row.names = FALSE)
      }
    )
  
  #### Cluster Transformation ####
    # Reactive expression for transformed antigen data
    transformedAntigens <- reactive({
      req(rv$normalized_data)
      
      normalized_data <- rv$normalized_data
      antigens = colnames(normalized_data)[-1]
      antigenList <- list()
      
      # Iterate over each antigen bead and apply the selected transformation
      for (antigen in antigens) {
        antigenData <- normalized_data[[antigen]]  # Data for each antigen
        
        # Check the transformation type for this specific bead
        transformType <- input[[paste0("transform_", antigen)]]
        
        if (is.null(transformType) || length(transformType) == 0) {
          transformType <- "No Transformation"  # Default to no transformation
        }
        
        if (transformType == "Log Transformation") {
          antigenList[[antigen]] <- log10(antigenData - min(antigenData, na.rm = TRUE) + 1) 
        } else if (transformType == "Square Root Transformation") {
          antigenList[[antigen]] <- sqrt(pmax(0, antigenData - min(antigenData, na.rm = TRUE)))
        } else {
          antigenList[[antigen]] <- antigenData
        }
      }
      
      return(antigenList)
    })
    
    # Dropdown to select which bead's plot to show 
    output$chooseNormalizedAntigen <- renderUI({
      req(input$clusterButton, rv$normalized_data)
      antigens = colnames(rv$normalized_data)[-1]
      selectInput("chosenNormalizedAntigen", "Choose Antigen for Plot", 
                  choices = antigens,
                  selected = antigens[1])
    })
    
    # Reactive expression to determine which bead to plot based on the dropdown
    selectedAntigenData <- reactive({
      req(input$chosenNormalizedAntigen)
      antigenList <- transformedAntigens()  
      return(antigenList[[input$chosenNormalizedAntigen]])
    })
    
    # Plot for the selected bead
    output$transformationPlot1 <- renderPlot({
      req(selectedAntigenData())
      ggplot() +
        geom_histogram(aes(x = selectedAntigenData()), bins = 30, fill = "lightblue", color = "black") +
        labs(title = paste("Distribution of", input$chosenNormalizedAntigen), x = "Intensity", y = "Frequency") +
        theme_classic()
    })
    
    output$transformationPlot2 <- renderPlot({
      req(selectedAntigenData())
      ggplot() +
        geom_density(aes(x = selectedAntigenData()), fill = "lightblue", alpha = 0.7) +
        labs(title = paste("Distribution of", input$chosenNormalizedAntigen), x = "Intensity", y = "Frequency") +
        theme_classic()
    })
    
    
    output$antigenTransformation <- renderUI({
      req(input$clusterButton)
      antigens = colnames(rv$normalized_data)[-1]
      
      # Generate transformation dropdowns for each negative control bead
      lapply(antigens, function(antigen) {
        selectInput(inputId = paste0("transform_", antigen), 
                    label = paste("Transformation for", antigen), 
                    choices = c("No Transformation", "Log Transformation", "Square Root Transformation"),
                    selected = "No Transformation")
      })
    })
    
    # Show transformation options once Step 1 is initiated
    observeEvent(input$clusterButton, { 
      shinyjs::show("chooseNormalizedAntigen")  
      shinyjs::show("transformationPlots")  
      shinyjs::show("clusterTransformationPanel")
      shinyjs::hide("normalizedDataPanel")
      shinyjs::hide("clusterButton")
    })
    

    
  #### Clustering ####
    # Transform data
    transformedDataFrame <- reactive({
      req(rv$normalized_data) 
      
      transformed_df <- rv$normalized_data
      antigens <- colnames(transformed_df)[-1]  
      
      # Loop through each antigen and apply the chosen transformation
      for (antigen in antigens) {
        antigenData <- transformed_df[[antigen]]  # Retrieve data for the current antigen
        
        # Get the selected transformation type for this antigen
        transformType <- input[[paste0("transform_", antigen)]]
        
        # Apply the chosen transformation
        if (transformType == "Log Transformation") {
          transformed_df[[antigen]] <- log10(antigenData - min(antigenData, na.rm = TRUE) + 1)
        } else if (transformType == "Square Root Transformation") {
          transformed_df[[antigen]] <- sqrt(pmax(0, antigenData - min(antigenData, na.rm = TRUE)))
        } else {
          transformed_df[[antigen]] <- antigenData  # No transformation
        }
      }
      
      # Return the transformed data frame
      return(transformed_df)
    })
    
    # Run Clustering
    filteredDataFrame <- reactive({
      req(transformedDataFrame())
      transformed_df <- transformedDataFrame()
      antigens <- colnames(transformed_df)[-1]
      filtered_df <- transformed_df
      
      for (antigen in antigens) {
        cut_df <- transformed_df %>%
          select(.data[[input$sampleID]], .data[[antigen]])
        
        
        z_column <- paste0(antigen, "_z")
        cluster_column <- paste0(antigen, "_cluster")
        node_column <- paste0(antigen, "_node")
        
        #Create z-score column
        cut_df[[z_column]] <- scale(cut_df[[antigen]])
        
        #Filter outliers
        cut_df = cut_df %>% filter(abs(.data[[z_column]]) < 2.5)
        
        #Define user inputs
        G_value <- if (input[[paste0("chosenNumClusterGroups_", antigen)]] == "Default") NULL else as.numeric(input[[paste0("chosenNumClusterGroups_", antigen)]])
        node_threshold <- as.numeric(input[[paste0("chosenNumNodeGroups_", antigen)]])
        
        #Cluster
        clustering <- Mclust(as.numeric(cut_df[[antigen]]), G = G_value)
        cut_df[[cluster_column]] <- clustering$classification
        cut_df[[node_column]] <- ifelse(cut_df[[cluster_column]] > node_threshold, "Positive", "Negative")
        
        filtered_df = full_join(filtered_df, cut_df)
      }
      return(filtered_df)
    })
    
    # Choose antigen to cluster
    output$chooseClusterAntigen <- renderUI({
      req(input$runClustering, rv$normalized_data)
      antigens = colnames(rv$normalized_data)[-1]
      selectInput("chosenClusterAntigen", "Choose Antigen for Plot", 
                  choices = antigens,
                  selected = antigens[1])
    })
    # Number of cluster groups dropdown
    output$chooseNumClusterGroups <- renderUI({
      req(input$runClustering, rv$normalized_data)
      antigens <- colnames(rv$normalized_data)[-1]
      
      # Create a dropdown for each antigen
      lapply(antigens, function(antigen) {
        selectInput(inputId = paste0("chosenNumClusterGroups_", antigen), 
                    label = paste("Number of clusters for", antigen),
                    choices = c("Default", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
                    selected = "Default")
      })
    })    
    # Number of groups in node dropdown
    output$chooseNumNodeGroups <- renderUI({
      req(input$runClustering, rv$normalized_data)
      antigens <- colnames(rv$normalized_data)[-1]
      
      # Create a dropdown for each antigen
      lapply(antigens, function(antigen) {
        selectInput(inputId = paste0("chosenNumNodeGroups_", antigen), 
                    label = paste("Number of groups in the negative node for", antigen),
                    choices = 1:9,
                    selected = 1)
      })
    })    
    
    # Render the ggplot visuals
    output$clusteredDensityPlot <- renderPlot({
        filtered_data <- filteredDataFrame()  %>%
          filter(!is.na(.data[[input$chosenClusterAntigen]]), 
                 !is.na(.data[[paste0(input$chosenClusterAntigen, "_cluster")]]))  # Filter out NA values
        

        ggplot(filtered_data, aes(x = .data[[input$chosenClusterAntigen]], fill = as.factor(.data[[paste0(input$chosenClusterAntigen, "_cluster")]]))) + 
          geom_density(alpha = 0.5) + 
          geom_dotplot(dotsize = 0.25) +
          labs(x = "Transformed Antigen Intensity", 
               title = paste("Density Plot for", input$chosenClusterAntigen),
               fill = "Clusters") +
          theme_classic()
      })
      
    output$clusteredNodePlot <- renderPlot({
      filtered_data <- filteredDataFrame()  %>%
        filter(!is.na(.data[[input$chosenClusterAntigen]]), 
               !is.na(.data[[paste0(input$chosenClusterAntigen, "_cluster")]]))  # Filter out NA values
      
      
        ggplot(filtered_data, aes(x = .data[[input$chosenClusterAntigen]], fill = as.factor(.data[[paste0(input$chosenClusterAntigen, "_node")]]))) + 
          geom_density(alpha = 0.5) + 
          geom_dotplot(dotsize = 0.25) +
          labs(x = "Transformed Antigen Intensity", 
               title = paste("Node Plot for", input$chosenClusterAntigen),
               fill = "Nodes") +
          theme_classic()
      })
    
    output$originalDensityPlot <- renderPlot({
      filtered_data <- filteredDataFrame()  %>%
        filter(!is.na(.data[[input$chosenClusterAntigen]]), 
               !is.na(.data[[paste0(input$chosenClusterAntigen, "_cluster")]]))  # Filter out NA values
      
      
      ggplot(filtered_data, aes(x = .data[[input$chosenClusterAntigen]], fill = "All Data")) + 
        geom_density(alpha = 0.5, fill = "lightblue") + 
        geom_dotplot(dotsize = 0.25) +
        labs(x = "Transformed Antigen Intensity", 
             title = paste("Density", input$chosenClusterAntigen),
             fill = "All") +
        theme_classic()
    })
    
    # View
    observeEvent(input$runClustering, { 
      shinyjs::show("clusteringPlots")  
      shinyjs::show("clusteringPanel")
      shinyjs::hide("chooseNormalizedAntigen")
      shinyjs::hide("transformationPlots")
      shinyjs::hide("clusterTransformationPanel")
      
      shinyjs::hide("closeApp")
      shinyjs::hide("finalDataPanel")
      shinyjs::hide("downloadFinalData")
    })
    
  #### Download Clustered Data ####
   
    finalDataFrame <- reactive({
      filtered_df <- filteredDataFrame()
      transformed_df <- transformedDataFrame()
      
      final_df <- filtered_df
      
      antigens <- colnames(transformed_df)[-1]
      
      for (antigen in antigens) {
        node_column <- paste0(antigen, "_node")
        z_column <- paste0(antigen, "_z")

        final_df = final_df %>%
          mutate(
            !!node_column := case_when(
              !is.na(.data[[node_column]]) ~ .data[[node_column]],  # Keep existing values if not NA
              .data[[z_column]] >= 2.5 ~ "Positive",               # Set to Positive if z_column >= 2.5
              TRUE ~ "Negative"                          # Keep as is if none of the above apply
            ))
      }
      
      final_df <- final_df %>%
        select(-all_of(paste0(antigens, "_z")), -all_of(paste0(antigens, "_cluster")))
      
      return(final_df)
    }) 
    
    output$clusteredDataAvailable <- reactive({
      !is.null(finalDataFrame())  # Returns TRUE if final data is available
    })
    outputOptions(output, "clusteredDataAvailable", suspendWhenHidden = FALSE)
    
    # Render the final data table preview
    output$finalData <- renderDataTable({
      head(finalDataFrame(), 10)  # Show the first 10 rows of final data
    })
    
    # Download handler for the final data
    output$downloadFinalData <- downloadHandler(
      filename = function() {
        paste("clustered_normalized_data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(finalDataFrame(), file, row.names = FALSE)
      }
    )
    
    observeEvent(input$confirmClustering, { 
      
      shinyjs::hide("clusteringPlots")  
      shinyjs::hide("clusteringPanel")
      shinyjs::show("closeApp")
      shinyjs::show("finalDataPanel")
      shinyjs::show("downloadFinalData")
    })
    
}

#### Create Shiny app ####
shinyApp(ui = ui, server = server)

