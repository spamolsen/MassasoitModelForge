# Check if R is installed
$rPath = ""
$possiblePaths = @(
    "C:\Program Files\R\R-4.4.2\bin\x64",
    "C:\Program Files\R\R-4.4.1\bin\x64",
    "C:\Program Files\R\R-4.4.0\bin\x64",
    "C:\Program Files\R\R-4.3.2\bin\x64",
    "C:\Program Files\R\R-4.3.1\bin\x64",
    "C:\Program Files\R\R-4.3.0\bin\x64"
)

foreach ($path in $possiblePaths) {
    if (Test-Path $path) {
        $rPath = $path
        break
    }
}

if (-not $rPath) {
    Write-Host "R is not found in the default locations. Please ensure R is installed and add it to your PATH." -ForegroundColor Red
    exit 1
}

# Add R to PATH
$env:Path = "$rPath;" + $env:Path

# Activate Python virtual environment
if (Test-Path ".\venv\Scripts\Activate.ps1") {
    .\venv\Scripts\Activate.ps1
} else {
    Write-Host "Virtual environment not found. Please run setup_venv.ps1 first." -ForegroundColor Red
    exit 1
}

# Set Python path for reticulate
$env:RETICULATE_PYTHON = ".\venv\Scripts\python.exe"

# Install required R packages if not already installed
Write-Host "Checking for required R packages..." -ForegroundColor Yellow
$packages = @("shiny", "reticulate", "DT", "readxl")

foreach ($pkg in $packages) {
    $checkCmd = "if(!require('$pkg', quietly=TRUE)) install.packages('$pkg', repos='https://cran.rstudio.com/')"
    Rscript -e $checkCmd
    if ($LASTEXITCODE -ne 0) {
        Write-Host "Failed to install/load package: $pkg" -ForegroundColor Red
        exit 1
    }
}

# Run the Shiny app
Write-Host "Starting Shiny app..." -ForegroundColor Green
Rscript -e "shiny::runApp('app.R', port=8080, launch.browser=TRUE)"

if ($LASTEXITCODE -ne 0) {
    Write-Host "Failed to start the Shiny app. Please check the error messages above." -ForegroundColor Red
    exit 1
}
