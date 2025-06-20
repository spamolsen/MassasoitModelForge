# Check if Python is installed
$pythonCmd = ""
$pythonVersions = @("python3.11", "python3.10", "python3.9", "python3", "python")

foreach ($py in $pythonVersions) {
    if (Get-Command $py -ErrorAction SilentlyContinue) {
        $pythonCmd = $py
        break
    }
}

if (-not $pythonCmd) {
    Write-Host "Python not found. Please install Python 3.8 or higher and ensure it's in your PATH." -ForegroundColor Red
    exit 1
}

# Check Python version
$pythonVersion = & $pythonCmd -c "import sys; print(f'{sys.version_info.major}.{sys.version_info.minor}.{sys.version_info.micro}')"
$majorVersion = [int]($pythonVersion -split '\\.')[0]
$minorVersion = [int]($pythonVersion -split '\\.')[1]

if ($majorVersion -lt 3 -or ($majorVersion -eq 3 -and $minorVersion -lt 8)) {
    Write-Host "Python 3.8 or higher is required. Found Python $pythonVersion" -ForegroundColor Red
    exit 1
}

Write-Host "Using Python $pythonVersion" -ForegroundColor Green

# Create virtual environment
Write-Host "Creating Python virtual environment..." -ForegroundColor Yellow
& $pythonCmd -m venv venv

if (-not $?) {
    Write-Host "Failed to create virtual environment" -ForegroundColor Red
    exit 1
}

# Activate the virtual environment
Write-Host "Activating virtual environment..." -ForegroundColor Yellow
.\venv\Scripts\Activate.ps1

if (-not $?) {
    Write-Host "Failed to activate virtual environment" -ForegroundColor Red
    exit 1
}

# Upgrade pip
Write-Host "Upgrading pip..." -ForegroundColor Yellow
python -m pip install --upgrade pip

if (-not $?) {
    Write-Host "Failed to upgrade pip" -ForegroundColor Red
    exit 1
}

# Install requirements
Write-Host "Installing Python dependencies..." -ForegroundColor Yellow
pip install -r requirements.txt

if (-not $?) {
    Write-Host "Failed to install requirements" -ForegroundColor Red
    exit 1
}

Write-Host "`nVirtual environment setup complete!" -ForegroundColor Green
Write-Host "To activate the virtual environment in the future, run:"
Write-Host "    .\venv\Scripts\Activate.ps1" -ForegroundColor Cyan
Write-Host "`nTo run the application, use:"
Write-Host "    .\run_app.ps1" -ForegroundColor Cyan
