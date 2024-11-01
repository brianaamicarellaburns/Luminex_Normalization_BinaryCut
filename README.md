# Luminex Normalization and Binary Classification

## Description
This Shiny app was developed to accompany the paper titled "Comprehensive Normalization and Binary Classification  Methods for Enhanced Sensitivity and Reproducibility in Luminex Assay Quantitation." The app carries out the published method including the normalization and clustering. 

## Features
- Normalize data using calibration curves, blank bead, and negative control beads
- Customize clustering with visualization to classify binary data

## Data Requirements
The input data should be structured as follows:
- **Sample Data**: Should contain a column with unique Sample IDs and corresponding columns with measured intensities of blank, negative control(s), and target antigen beads. Each plate should be a separate file.

- **Calibration Data**: Separate file with calibration data run on plate (every Sample Data plate should have a corresponding calibration curve). If dilution factor was used, create a column with the calculated ratio to act as your dilution column (ie 1:500 = 1/500). In addition to the dilution column, calibration data should have columns with the measured intensity corresponding to the dilution for the blank, negative control(s), and target antigen beads. 
**Important**: names of measured intensity columns must match the measured intensity columns in the sample data.

-**Format**: Files should be in CSV format with consistent column names as specified above.

## Getting Started
You can access the app online at: http://brianaamicarellaburns.shinyapps.io/luminex_normcut

To run the app locally:

1. Install [R](https://cran.r-project.org/) and [RStudio](https://rstudio.com/).
2. Install the required packages (listed below) in R:
   ```R
   install.packages(c("shiny", "DT", "ggplot2", "shinyjs", "pracma", "mgcv"))
