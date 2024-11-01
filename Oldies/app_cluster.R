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
      
      div(id = "clusterTransformationPanel",
          h4("Transform Normalized Data for Clustering"),
          uiOutput("normalizedTransformation")  # This will be filled by the transformation dropdowns
      ),
      
      conditionalPanel(
        condition = "input.clusterButton > 0",  # This ensures it only shows after clusterButton is clicked
        h4("Transform Normalized Data for Clustering"),
        uiOutput("normalizedTransformation"),  # UI for the transformations (defined in server)
        actionButton("confirmClustering", "Run Clustering")  # Button to start clustering
      ),
      
      # Add customizeClusteringPanel in the sidebarPanel
      conditionalPanel(
        condition = "input.confirmClustering > 0",
        uiOutput("customizeClusteringPanel")
      )
    ),
    
    #### Define the UI - Main panel for output ####
    mainPanel(
      conditionalPanel(
        condition = "input.confirmUpload > 0 && input.startTransformation == 0",
        h4("Preview of Uploaded Data"),
        uiOutput("previewTables")
      ),
      
      conditionalPanel(
        condition = "input.startTransformation > 0",
        div(id = "distributionPanel",  # Wrap the distribution plot section in a div with an ID
            h4("Distribution Plot"),
            uiOutput("chooseBead"),
            plotOutput("beadPlot")
        )),
      
      div(
        id = "normalizedDataPanel",  # Add an id to the normalized data section
        conditionalPanel(
          condition = "output.normalizedDataAvailable == true",
          h3("Normalized Data"),
          dataTableOutput("normalizedData"),  # This will show the preview of the data
          downloadButton("downloadData", "Download Normalized Data")  # Button to download the data
        )),
      
      # Add antigen selection to the main panel
      conditionalPanel(
        condition = "input.clusterButton > 0",  # Ensure this shows up after pressing Cluster Data
        h4("Choose Antigen for Plot"),
        uiOutput("chosenClusterAntigen")  # This moves the antigen dropdown to the main panel
      ),
      
      div(id = "transformationPlots",  # Add an ID to the section with transformation plots
          h4("Transformation Plots"),
          plotOutput("transformationPlot1"),
          plotOutput("transformationPlot2")
      ),
      
      # After Clustering, show density and node plots
      conditionalPanel(
        condition = "input.confirmClustering > 0",  # Show only after "Run Clustering" is pressed
        h4("Cluster Plot"),
        plotOutput("clusteredDensityPlot"),  # Output for the density plot
        plotOutput("clusteredNodePlot")  # Output for the node plot
      )
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
  

  
  #### Data transformations ####
  
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
    shinyjs::hide("clusterTransformationPanel")
    shinyjs::hide("transformationPlots")
    
    # Reactive expression for transformed normalized data
    transformedNormalizedAntigen <- reactive({
      req(input$chosenClusterAntigen)  # Ensure inputs exist
      
      # Retrieve the normalized data and selected antigen
      normalized_data <- rv$normalized_data
      antigen <- input$chosenClusterAntigen
      antigen_data <- normalized_data[[antigen]]
      
      # Apply the chosen transformation to the selected antigen
      # Handle edge cases for log and sqrt transformations
      if (input[[paste0("transform_", antigen)]] == "Log Transformation") {
        transformed_data <- log10(antigen_data - min(antigen_data, na.rm = TRUE) + 1)  # Shift the data and apply log
        transformed_data[!is.finite(transformed_data)] <- NA  # Replace non-finite values (e.g., -Inf) with NA
      } else if (input[[paste0("transform_", antigen)]] == "Square Root Transformation") {
        transformed_data <- sqrt(pmax(0, antigen_data - min(antigen_data, na.rm = TRUE)))  # Ensure non-negative values
        transformed_data[!is.finite(transformed_data)] <- NA  # Handle any non-finite values
      } else {
        transformed_data <- antigen_data  # No transformation
      }
  
      return(transformed_data)
    })
    
    # Dropdown to select which normalized antigen to plot
    output$chooseNormalizedAntigen <- renderUI({
      req(rv$normalized_data)
      antigen_cols <- colnames(rv$normalized_data)[-1]  # Exclude SampleID
      
      selectInput("chosenClusterAntigen", "Choose Antigen for Plot", 
                  choices = antigen_cols,  # Populate with available normalized antigens
                  selected = antigen_cols[1])  # Default selection
    })
    
    # Trigger when the "Cluster Data" button is pressed
    observeEvent(input$clusterButton, {
      shinyjs::hide("normalizedDataPanel")  # Hide the normalized data table
      shinyjs::hide("clusterButton")  # Hide the "Cluster Normalized Data" button
      shinyjs::show("clusterTransformationPanel")  # Show the transformation panel
      shinyjs::show("transformationPlots")
      
      rv$transformationComplete <- TRUE
      
      # Trigger the transformation dropdowns for each antigen
      output$normalizedTransformation <- renderUI({
        req(rv$normalized_data)  # Ensure the normalized data is available
        
        # Generate dropdowns for each normalized antigen
        antigen_cols <- colnames(rv$normalized_data)[-1]  # Exclude SampleID column
        lapply(antigen_cols, function(antigen) {
          selectInput(
            inputId = paste0("transform_", antigen),  # Unique input ID for each antigen
            label = paste("Transformation for", antigen),
            choices = c("No Transformation", "Log Transformation", "Square Root Transformation"),
            selected = "No Transformation"
          )
        })
      })
      
      # Show transformation plots based on chosen antigen
      shinyjs::show("transformationPlots")
      
    })
    
    # Render the plot based on the chosen antigen and transformations
    output$transformationPlot1 <- renderPlot({
      req(transformedNormalizedAntigen())  # Ensure transformed data is available
      
      # Plot histogram of the transformed data
      ggplot() +
        geom_histogram(aes(x = transformedNormalizedAntigen()), bins = 30, fill = "lightblue", color = "black") +
        labs(title = paste("Transformation Plot 1 for", input$chosenClusterAntigen), x = "Intensity", y = "Frequency") +
        theme_classic()
    })
    
    output$transformationPlot2 <- renderPlot({
      req(transformedNormalizedAntigen())  # Ensure transformed data is available
      
      # Plot density of the transformed data
      ggplot() +
        geom_density(aes(x = transformedNormalizedAntigen()), fill = "lightblue", alpha = 0.7) +
        labs(title = paste("Transformation Plot 2 for", input$chosenClusterAntigen), x = "Intensity", y = "Density") +
        theme_classic()
    })

    
  #### Clustering ####
    # Create reactiveValues to store filtered_df
    rv_clustering <- reactiveValues(filtered_df = NULL)
    
    # Observe event for Run Clustering button
    observeEvent(input$confirmClustering, {
      # Ensure the necessary data exists and the button was pressed
      req(rv$normalized_data, input$chosenClusterAntigen)
      
      # Get the transformed data for the chosen antigen and add it to the filtered data frame
      transformed_data <- transformedNormalizedAntigen()  # Get the transformed antigen data
      rv_clustering$filtered_df$transformed_antigen <- transformed_data
  
      output$customizeClusteringPanel <- renderUI({
        tagList(
          sliderInput("numGroups", "Customize the number of groups for clustering:", min = 2, max = 10, value = 2),
          sliderInput("negativeNode", "How many clustered groups should be in the negative node:", min = 1, max = 9, value = 1)
        )
      })
      
      # Perform clustering on the normalized antigen
      clusterless <- rv$normalized_data
      clusterless$zscore <- scale(clusterless[[input$chosenClusterAntigen]])  # Z-score on selected antigen
      
      # Filter the data based on z-score threshold
      rv_clustering$filtered_df <- clusterless %>%
        filter(zscore < abs(2.5))
      
      # Run Mclust with the default number of clusters (G = NULL)
      clustering <- Mclust(as.numeric(rv_clustering$filtered_df[[input$chosenClusterAntigen]]), G = NULL)
      rv_clustering$filtered_df$antigen_cluster <- clustering$classification  # Assign cluster classification
      
      # Apply condition for node with safe handling for input$negativeNode
      negativeNodeValue <- ifelse(is.null(input$negativeNode), 1, input$negativeNode)
      rv_clustering$filtered_df$antigen_node <- ifelse(rv_clustering$filtered_df$antigen_cluster > negativeNodeValue, "Positive", "Negative")
      
      # Render the ggplot visuals
      output$clusteredDensityPlot <- renderPlot({
        transformed_data <- transformedNormalizedAntigen()  # Get the transformed antigen data
        
        ggplot(rv_clustering$filtered_df, aes(x = transformed_antigen, fill = as.factor(antigen_cluster))) + 
          geom_density(alpha = 0.5) + 
          geom_dotplot() +
          labs(x = "Transformed Antigen Intensity", title = paste("Density Plot for", input$chosenClusterAntigen)) +
          theme_classic()
      })
      
      output$clusteredNodePlot <- renderPlot({
        ggplot(rv_clustering$filtered_df, aes(x = transformed_antigen, fill = as.factor(antigen_node))) + 
          geom_density(alpha = 0.5) + 
          geom_dotplot(dotsize = 0.25) +
          labs(x = "Transformed Antigen Intensity", title = paste("Node Plot for", input$chosenClusterAntigen)) +
          theme_classic()
      })
    })
    
}

#### Create Shiny app ####
shinyApp(ui = ui, server = server)

