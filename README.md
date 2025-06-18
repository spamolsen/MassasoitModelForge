# Massasoit Model Forge

An R Shiny application that integrates Python functionality using the `reticulate` package. This application allows users to load and analyze Excel files with both R and Python.

## Features

- Load and display Excel files
- View data in an interactive table
- Display summary statistics
- Python integration via `reticulate`

## Prerequisites

- R (>= 3.6.0)
- RStudio (recommended)
- Python (>= 3.6)

## Installation

1. Install the required R packages:
   ```R
   install.packages(c("shiny", "reticulate", "DT", "readxl"))
   ```

2. Install the required Python packages:
   ```
   pip install -r requirements.txt
   ```

## Usage

1. Place your Excel files in the `Base_Data_Files` directory
2. Run the app by opening `app.R` in RStudio and clicking "Run App" or by running:
   ```R
   shiny::runApp()
   ```
3. Use the file uploader to load an Excel file
4. Explore the data in the Data and Summary tabs

## Project Structure

```
MassasoitModelForge/
├── app.R                 # Main Shiny application
├── README.md            # Project documentation
├── requirements.txt     # Python dependencies
└── Base_Data_Files/     # Directory for Excel files
```

## License

This project is licensed under the MIT License - see the LICENSE file for details.
