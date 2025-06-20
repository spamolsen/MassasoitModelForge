# Massasoit Model Forge

An R Shiny application that integrates Python functionality using the `reticulate` package. This application allows users to load and analyze Excel files with both R and Python.

## Features

- Load and display Excel files
- View data in an interactive table
- Display summary statistics
- Python integration via `reticulate`
- Pre-configured Python virtual environment

## Prerequisites

- [R](https://cran.r-project.org/) (>= 4.0.0 recommended)
- [RStudio](https://www.rstudio.com/products/rstudio/download/) (recommended)
- [Python](https://www.python.org/downloads/) (>= 3.8)
- [Git](https://git-scm.com/downloads) (for cloning the repository)

## Quick Start

### 1. Clone the Repository
```bash
git clone https://github.com/spamolsen/MassasoitModelForge.git
cd MassasoitModelForge
```

### 2. Set Up the Python Virtual Environment

#### Windows (PowerShell):
```powershell
# Navigate to the project directory
cd MassasoitModelForge

# Run the setup script (Windows)
.\setup_venv.ps1
```

#### macOS/Linux:
```bash
# Navigate to the project directory
cd MassasoitModelForge

# Make the setup script executable
chmod +x setup_venv.sh

# Run the setup script
./setup_venv.sh
```

### 3. Install Required R Packages

Open R or RStudio and run:
```R
install.packages(c("shiny", "reticulate", "DT", "readxl"))
```

### 4. Run the Application

#### Using the provided script (recommended):
```powershell
# Windows
.\run_app.ps1
```

```bash
# macOS/Linux
chmod +x run_app.sh
./run_app.sh
```

#### Or manually in R:
```R
# Set Python to use the virtual environment
reticulate::use_virtualenv("venv")

# Run the app
shiny::runApp()
```

## Project Structure

```
MassasoitModelForge/
├── app.R                 # Main Shiny application
├── README.md             # Project documentation
├── requirements.txt      # Python dependencies
├── setup_venv.ps1        # Windows setup script
├── setup_venv.sh         # macOS/Linux setup script
├── run_app.ps1           # Windows run script
├── run_app.sh            # macOS/Linux run script
├── .gitignore           # Git ignore file
└── Base_Data_Files/     # Directory for Excel files
   └── .gitkeep         # Keeps the directory in version control
```

## Troubleshooting

1. **Python Not Found**
   - Ensure Python is installed and added to your system PATH
   - The app looks for Python in the virtual environment first

2. **Package Installation Issues**
   - Make sure you have the latest version of pip: `python -m pip install --upgrade pip`
   - If you encounter permission errors, try adding `--user` to the pip install command

3. **Reticulate Issues**
   - If R can't find Python, set the path manually in R:
     ```R
     library(reticulate)
     use_python("path/to/your/python")
     ```

## License

This project is licensed under the MIT License - see the LICENSE file for details.
