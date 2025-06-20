#!/bin/bash

# Activate the virtual environment
source venv/bin/activate

# Set Python path for reticulate
export RETICULATE_PYTHON="$(pwd)/venv/bin/python"

# Check if R is installed
if ! command -v Rscript &> /dev/null; then
    echo "R is not installed. Please install R from https://cran.r-project.org/"
    exit 1
fi

# Install required R packages if not installed
echo "Checking for required R packages..."
Rscript -e "if(!require('shiny')) install.packages('shiny', repos='https://cran.rstudio.com/')"
Rscript -e "if(!require('reticulate')) install.packages('reticulate', repos='https://cran.rstudio.com/')"
Rscript -e "if(!require('DT')) install.packages('DT', repos='https://cran.rstudio.com/')"
Rscript -e "if(!require('readxl')) install.packages('readxl', repos='https://cran.rstudio.com/')"

# Run the Shiny app
echo "Starting Shiny app..."
R -e "shiny::runApp(port=8080, launch.browser=TRUE)"
