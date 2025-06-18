library(shiny)
library(reticulate)
library(DT)

# Check if Python is available and load required Python modules
use_python("C:\\Users\\TEKOWNER\\AppData\\Local\\Programs\\Python\\Python313\\python.exe")

# Import Python utilities
py_utils <- import_from_path("python_utils", path = ".")
data_utils <- py_utils$data_utils

# UI definition with custom CSS
ui <- tagList(
  tags$head(
    tags$style(HTML("
      .well {
        background-color: #f9f9f9;
        border: 1px solid #ddd;
        border-radius: 4px;
        padding: 10px;
        margin-bottom: 20px;
      }
      .btn-primary {
        background-color: #337ab7;
        border-color: #2e6da4;
      }
      .btn-primary:hover {
        background-color: #286090;
        border-color: #204d74;
      }
      .shiny-input-container {
        margin-bottom: 10px;
      }
    "))
  ),

  fluidPage(
    titlePanel("Massasoit Model Forge"),
    sidebarLayout(
      sidebarPanel(
        radioButtons("dataSource", "Choose data source:",
                     choices = c("Use base file" = "base",
                                 "Upload your own file" = "upload"),
                     selected = "base"),

        # Conditional panel for base file selection
        conditionalPanel(
          condition = "input.dataSource == 'base'",
          selectInput(
            "baseFile",
            "Select base file:",
            choices = list.files(
              "Base_Data_Files",
              pattern = "\\.xlsx$",
              full.names = FALSE),
            selected = NULL)
        ),

        # Conditional panel for file upload
        conditionalPanel(
          condition = "input.dataSource == 'upload'",
          fileInput("file1", "Choose Excel File", accept = ".xlsx")
        ),

        actionButton("loadData", "Load Data"),

        # Analysis type selection
        selectInput(
          "analysisType",
          "Select Analysis Type:",
          choices = list(
            "-- Select Analysis Type --" = "",
            "Parametric/Semi-parametric" = list(
              "GAM(M)s" = "gamm",
              "GLM(M)s" = "glmm",
              "Logistic Regression" = "logistic",
              "ANOVA" = "anova",
              "Linear Regression" = "linear",
              "Generalized Estimating Equations" = "gee",
              "Negative Binomial Regression" = "negbin"
            ),
            "Non-parametric" = list(
              "GWR (Geographically Weighted Regression)" = "gwr",
              "Goodness of Fit, Chi-squared test" = "chisq",
              "Mann-Whitney U test" = "mannwhitney",
              "Kruskal-Wallis test" = "kruskal",
              "Zero Inflated Model" = "zeroinfl",
              "Hurdle Model" = "hurdle",
              "Sign test" = "signtest",
              "Wilcoxon Signed-Rank test" = "wilcoxon",
              "Spearman's Rank Correlation" = "spearman",
              "Permutation signed rank test" = "permtest"
            )
          ),
          selected = ""
        ),

        # Dynamic UI for analysis parameters
        uiOutput("analysisParams"),

        # Action button to run the selected analysis
        actionButton("runAnalysis", "Run Analysis", class = "btn-primary")
      ),
      mainPanel(
        tabsetPanel(
          id = "mainTabs",
          tabPanel("Data", DTOutput("dataTable")),
          tabPanel("Summary", verbatimTextOutput("summary")),
          tabPanel("Analysis Results", 
                   verbatimTextOutput("analysisResults"),
                   plotOutput("analysisPlot"))
        )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Reactive value to store analysis results
  analysis_results <- reactiveValues(
    result = NULL,
    plot = NULL
  )
  
  # Dynamic UI for analysis parameters
  output$analysisParams <- renderUI({
    req(input$analysisType)
    
    if (input$analysisType == "") return(NULL)
    
    tagList(
      # Common parameters for most analyses
      selectInput("responseVar", "Response Variable:", 
                  choices = names(data())),
      
      # Conditional parameters based on analysis type
      if (input$analysisType %in% c("linear", "logistic", "glmm", "gamm", "negbin")) {
        selectInput("predictorVars", "Predictor Variables:", 
                    choices = names(data()),
                    multiple = TRUE)
      },
      
      if (input$analysisType %in% c("anova", "kruskal")) {
        selectInput("groupVar", "Grouping Variable:", 
                    choices = names(data()))
      },
      
      if (input$analysisType %in% c("chisq")) {
        tagList(
          selectInput("observedVar", "Observed Variable:", 
                      choices = names(data())),
          numericInput("expectedProbs", "Expected Probabilities (comma-separated):", 
                      value = "", 
                      placeholder = "Leave empty for uniform distribution")
        )
      } else if (input$analysisType %in% c("spearman", "pearson")) {
        tagList(
          selectInput("var1", "Variable 1:", choices = names(data())),
          selectInput("var2", "Variable 2:", choices = names(data()))
        )
      }
    )
  })
  
  # Run analysis when the run button is clicked
  observeEvent(input$runAnalysis, {
    req(data(), input$analysisType)
    
    tryCatch({
      # This is where we'll implement the actual analysis functions
      # For now, we'll just show a message
      showNotification(paste("Running", input$analysisType, "analysis..."), 
                      type = "message")
      
      # Store a simple result for demonstration
      analysis_results$result <- paste("Results for", input$analysisType, "analysis will appear here.")
      
    }, error = function(e) {
      showNotification(paste("Error in analysis:", e$message), 
                      type = "error")
    })
  })
  
  # Display analysis results
  output$analysisResults <- renderPrint({
    req(analysis_results$result)
    cat(analysis_results$result)
  })
  
  # Display analysis plot
  output$analysisPlot <- renderPlot({
    # Placeholder for analysis plots
    if (!is.null(analysis_results$plot)) {
      analysis_results$plot
    } else {
      plot(1, 1, type = "n", xlab = "", ylab = "", axes = FALSE)
      text(1, 1, "Plot will appear here", cex = 1.5)
    }
  })
  
  # Reactive value to store the loaded data
  data <- reactiveVal(NULL)
  
  # Update base file dropdown when files change
  observe({
    updateSelectInput(session, "baseFile", 
                      choices = list.files("Base_Data_Files", pattern = "\\.xlsx$", full.names = FALSE))
  })
  
  # Load data when button is clicked
  observeEvent(input$loadData, {
    tryCatch({
      if (input$dataSource == "base" && !is.null(input$baseFile)) {
        # Read from base file
        file_path <- file.path("Base_Data_Files", input$baseFile)
        if (file.exists(file_path)) {
          df <- readxl::read_excel(file_path)
          data(df)
          showNotification(paste("Loaded base file:", input$baseFile), type = "message")
        } else {
          stop("Selected base file not found")
        }
      } else if (input$dataSource == "upload" && !is.null(input$file1)) {
        # Read from uploaded file
        df <- readxl::read_excel(input$file1$datapath)
        data(df)
        showNotification("Uploaded file loaded successfully!", type = "message")
      } else {
        stop("Please select a file or upload one")
      }
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # Display the data table
  output$dataTable <- renderDT({
    req(data())
    datatable(data())
  })
  
  # Display summary statistics using Python
  output$summary <- renderPrint({
    req(data())
    
    # Convert R data to Python pandas DataFrame
    py_run_string("import pandas as pd")
    py$df <- r_to_py(data())
    
    # Get summary using Python function
    summary_result <- data_utils$get_data_summary(py$df)
    
    # Format and display the summary
    cat("Data Summary\n")
    cat("===========\n\n")
    
    cat("Number of rows:", summary_result$num_rows, "\n")
    cat("Number of columns:", summary_result$num_columns, "\n\n")
    
    cat("Column names:\n")
    cat(paste(" - ", summary_result$column_names, collapse = "\n"), "\n\n")
    
    cat("Data types:\n")
    for (col in names(summary_result$data_types)) {
      cat(" - ", col, ": ", summary_result$data_types[[col]], "\n")
    }
    cat("\n")
    
    if (length(summary_result$missing_values) > 0) {
      cat("Missing values:\n")
      for (col in names(summary_result$missing_values)) {
        if (summary_result$missing_values[[col]] > 0) {
          cat(" - ", col, ": ", summary_result$missing_values[[col]], "\n")
        }
      }
    } else {
      cat("No missing values found.\n")
    }
  })
}



# Run the application
shinyApp(ui = ui, server = server)
