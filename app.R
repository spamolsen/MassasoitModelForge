  library(shiny)
suppressPackageStartupMessages({
  library(shinyjs) # For JavaScript operations in Shiny
  library(reticulate)
  library(DT)
  library(readxl) # For reading Excel files
  library(lme4)   # For GLMMs
  library(mgcv)   # For GAMs/GAMMs
  library(MASS)   # For Negative Binomial Regression (glm.nb)
  library(pscl)   # For Zero-Inflated and Hurdle models
  library(geepack) # For GEE
  library(spgwr) # For GWR
  library(readr)
  library(readxl)
  library(ggplot2)
  library(BSDA)
  library(openmeteo)
})
message("\n---\n***Starting Shiny app\n")
# Set up Python environment
env_name <- "MassasoitModelForge_env"

# Create/check Conda environment
message(paste("\n***Checking for", env_name, "environment:"))

if (!(env_name %in% reticulate::conda_list()$name)) {
  reticulate::conda_create(
    envname = env_name,
    packages = c(paste0("python=", "3.10"))
  )
  message(paste("\tEnvironment created.\n"))
} else {
  message(paste("\tEnvironment present.\n"))
}

installed_packages <- py_list_packages(env_name)[[3]]

required_packages <- readLines("requirements.txt")

missing_packages <- setdiff(gsub("([>=]).*", "", required_packages), gsub("([>=]).*", "", installed_packages))

#Print list of required and missing packages if any
if (length(missing_packages) > 0) {
  message(paste("***Packages required:\n\t", paste(required_packages, collapse = "\n\t")))
  message(paste("***Packages missing:\n\t", paste(missing_packages, collapse = "\n\t")))
}

# Install Python packages
if (length(missing_packages) > 0) {
  for (pkg in missing_packages) {
    reticulate::py_install(
      pkg,
      envname = env_name,
      pip = TRUE,
      ignore_installed = FALSE
    )
  }
}

# Initialize environment
reticulate::use_condaenv(env_name, required = TRUE)

# Check Python availability and load modules
tryCatch({
  if (!py_available(initialize = TRUE)) {
    stop("Python is not available. Please install Python and ensure it's in your PATH.")
  }

  if (!file.exists("python_utils")) {
    stop("python_utils directory not found. Please ensure it exists in the app directory.")
  }
  
  py_utils <- import_from_path("python_utils", path = ".")
  data_utils <- py_utils$data_utils
  # noaa_ncei <- import("noaa_ncei")
}, error = function(e) {
  stop("Error initializing Python: ", conditionMessage(e))
})




##########################################################################

######################     Analysis Functions           ###################

##########################################################################



# Function to read and clean data (Can be expanded for more than logistic regression)
read_data_file <- function(file_path, file_name) {
  # ... existing code ...

  # After loading and cleaning data
  df <- clean_and_convert(df)

  # Identify suitable logistic response variables
  suitable_logistic_vars <- sapply(names(df), function(col) {
    is_logistic_response(df, col)
  })

  # Store suitable variables in reactive value
  suitable_response_vars <- reactiveVal()
  suitable_response_vars(names(df)[suitable_logistic_vars])

  return(df)
}

# Linear Regression Analysis
run_linear_analysis <- function(df, response_var, predictor_vars) {
  formula_str <- paste(response_var, "~", paste(predictor_vars, collapse = " + "))
  model <- lm(as.formula(formula_str), data = df)

  list(
    summary = summary(model),
    plot = function() {
      if (length(predictor_vars) == 1) {
        plot(df[[predictor_vars[1]]], df[[response_var]],
             xlab = predictor_vars[1],
             ylab = response_var,
             main = paste("Linear Regression:", response_var, "vs", predictor_vars[1]))
        abline(model, col = "blue", lwd = 2)
      } else {
        par(mfrow = c(2, 2))
        plot(model)
        par(mfrow = c(1, 1))
      }
    }
  )
}

# Function to identify and categorize suitable logistic response variables
is_logistic_response <- function(df, col_name) {
  col <- df[[col_name]]

  # Check if column is numeric or logical
  if (!is.numeric(col) && !is.logical(col)) {
    return("unsuitable")
  }

  # For numeric columns
  if (is.numeric(col)) {
    # Remove NA values for checking
    col <- na.omit(col)

    # Check if all values are either 0 or 1
    if (all(col %in% c(0, 1))) {
      return("binary")
    }

    # Check if all values are proportions (0 <= x <= 1)
    if (all(col >= 0 & col <= 1)) {
      return("proportion")
    }

    # Check if column has only non-zero values of same sign
    non_zero <- col[col != 0]
    zero <- col[col == 0]
    if (length(non_zero) > 0 && length(zero) > 0) {
      # Check if all non-zero values are positive or all negative
      if (all(non_zero > 0) || all(non_zero < 0)) {
        return("convertible")
      }
    }

    return("unsuitable")
  }

  # For logical columns, they're automatically suitable
  return("binary")
}

# Function to convert convertible variables to binary
convert_to_binary <- function(df, col_name) {
  col <- df[[col_name]]

  # Convert non-zero values to 1, keep zeros as 0
  df[[col_name]] <- ifelse(col != 0, 1, 0)
  return(df)
}

#Logistic Regression Analysis
run_logistic_analysis <- function(df, response_var, predictor_vars, family = "binomial") {
  # Convert convertible variables before analysis
  var_type <- is_logistic_response(df, response_var)
  if (var_type == "convertible") {
    df <- convert_to_binary(df, response_var)
  }

  # Prepare formula
  formula_str <- paste(response_var, "~", paste(predictor_vars, collapse = " + "))

  # Run logistic regression with proper error handling
  tryCatch({
    model <- glm(as.formula(formula_str), family = family, data = df)

    # Create summary and plot
    model_summary <- summary(model)

    # Create plot function that will be called by Shiny
    plot_func <- function() {
      if (length(predictor_vars) > 0) {
        predictor_to_plot <- predictor_vars[1]
        plot_data <- data.frame(
          x = df[[predictor_to_plot]],
          y = df[[response_var]],
          predicted = predict(model, type = "response")
        )

        # Create base R plot
        plot(
          plot_data$x,
          plot_data$predicted,
          type = "l",
          col = "blue",
          lwd = 2,
          xlab = predictor_to_plot,
          ylab = "Predicted Probability",
          main = "Logistic Regression: Predicted Probabilities"
        )
        points(plot_data$x, plot_data$y, col = "red", pch = 16)
      } else {
        plot(1, 1, type = "n",
             main = "No plot available for this configuration",
             xlab = "", ylab = "")
      }
    }

    list(
      summary = model_summary,
      plot = plot_func
    )
  }, error = function(e) {
    showNotification(paste("Error running logistic regression:", e$message), type = "error")
    NULL
  })
}

# ANOVA Analysis
run_anova_analysis <- function(df, response_var, group_var) {
  formula_str <- paste(response_var, "~", group_var)
  model <- aov(as.formula(formula_str), data = df)

  list(
    summary = summary(model),
    plot = function() {
      boxplot(as.formula(formula_str), data = df,
              main = paste("ANOVA: ", response_var, " by ", group_var),
              xlab = group_var,
              ylab = response_var)
    }
  )
}
# Chi-squared test
run_chisq_analysis <- function(df, response_var, group_var) {
  tbl <- table(df[[response_var]], df[[group_var]])
  test <- chisq.test(tbl)
  list(
    summary = test,
    plot = function() {
      barplot(tbl, beside = TRUE, legend = TRUE,
              main = "Contingency Table",
              xlab = group_var, ylab = "Count")
    }
  )
}

# Mann-Whitney U test (Wilcoxon rank sum test)
run_mannwhitney_analysis <- function(df, response_var, group_var) {
  x <- df[[response_var]][df[[group_var]] == unique(df[[group_var]])[1]]
  y <- df[[response_var]][df[[group_var]] == unique(df[[group_var]])[2]]
  test <- wilcox.test(x, y)
  list(
    summary = test,
    plot = function() {
      boxplot(df[[response_var]] ~ df[[group_var]],
              main = "Mann-Whitney U Test",
              xlab = group_var, ylab = response_var)
    }
  )
}
# Kruskal-Wallis test
run_kruskal_analysis <- function(df, response_var, group_var) {
  test <- kruskal.test(df[[response_var]] ~ df[[group_var]])
  list(
    summary = test,
    plot = function() {
      boxplot(df[[response_var]] ~ df[[group_var]],
              main = "Kruskal-Wallis Test",
              xlab = group_var, ylab = response_var)
    }
  )
}

# Hurdle model using pscl::hurdle
run_hurdle_analysis <- function(df, response_var, predictor_vars) {
  formula_str <- paste(response_var, "~", paste(predictor_vars, collapse = " + "))
  model <- pscl::hurdle(as.formula(formula_str), data = df)
  list(
    summary = summary(model),
    plot = function() {
      plot(model, main = "Hurdle Model Diagnostics")
    }
  )
}

# Sign test (paired data)
run_signtest_analysis <- function(df, response_var, group_var) {
  library(BSDA)
  levels <- unique(df[[group_var]])
  if (length(levels) != 2) {
    stop("Sign test requires exactly two groups.")
  }
  x <- df[[response_var]][df[[group_var]] == levels[1]]
  y <- df[[response_var]][df[[group_var]] == levels[2]]
  n <- min(length(x), length(y))
  test <- BSDA::SIGN.test(x[1:n], y[1:n])
  list(
    summary = test,
    plot = function() {
      boxplot(x[1:n], y[1:n], names = levels,
              main = "Sign Test",
              ylab = response_var)
    }
  )
}

# Wilcoxon signed-rank test (paired)
run_wilcoxon_analysis <- function(df, response_var, group_var) {
  levels <- unique(df[[group_var]])
  if (length(levels) != 2) {
    stop("Wilcoxon signed-rank test requires exactly two groups.")
  }
  x <- df[[response_var]][df[[group_var]] == levels[1]]
  y <- df[[response_var]][df[[group_var]] == levels[2]]
  n <- min(length(x), length(y))
  test <- wilcox.test(x[1:n], y[1:n], paired = TRUE)
  list(
    summary = test,
    plot = function() {
      boxplot(x[1:n], y[1:n], names = levels,
              main = "Wilcoxon Signed-Rank Test",
              ylab = response_var)
    }
  )
}

# Spearman's rank correlation
run_spearman_analysis <- function(df, response_var, predictor_var) {
  test <- cor.test(df[[response_var]], df[[predictor_var]], method = "spearman")
  list(
    summary = test,
    plot = function() {
      plot(df[[predictor_var]], df[[response_var]],
           main = "Spearman Correlation",
           xlab = predictor_var, ylab = response_var)
      abline(lm(df[[response_var]] ~ df[[predictor_var]]), col = "blue")
    }
  )
}

# Permutation test for difference in means (simple version)
run_permtest_analysis <- function(df, response_var, group_var) {
  levels <- unique(df[[group_var]])
  if (length(levels) != 2) {
    stop("Permutation test requires exactly two groups.")
  }
  x <- df[[response_var]][df[[group_var]] == levels[1]]
  y <- df[[response_var]][df[[group_var]] == levels[2]]
  obs_diff <- mean(x, na.rm = TRUE) - mean(y, na.rm = TRUE)
  n_perm <- 1000
  perm_diffs <- replicate(n_perm, {
    perm <- sample(c(x, y))
    mean(perm[1:length(x)], na.rm = TRUE) - mean(perm[(length(x)+1):(length(x)+length(y))], na.rm = TRUE)
  })
  p_value <- mean(abs(perm_diffs) >= abs(obs_diff))
  result <- list(
    statistic = obs_diff,
    p.value = p_value,
    method = "Permutation test for difference in means",
    alternative = "two.sided"
  )
  class(result) <- "htest"
  list(
    summary = result,
    plot = function() {
      hist(perm_diffs, breaks = 30, main = "Permutation Test Null Distribution",
           xlab = "Difference in Means")
      abline(v = obs_diff, col = "red", lwd = 2)
      legend("topright", legend = "Observed diff", col = "red", lwd = 2)
    }
  )
}

# GLMM Analysis
run_glmm_analysis <- function(df, response_var, predictor_vars, random_effect, family = "poisson") {
  formula_str <- paste0(response_var, " ~ ",
                       paste(predictor_vars, collapse = " + "),
                       " + (1|", random_effect, ")")

  model <- lme4::glmer(as.formula(formula_str),
                      family = family,
                      data = df)

  list(
    summary = summary(model),
    plot = function() {
      plot(model, main = "GLMM Residuals vs. Fitted")
    }
  )
}

run_gwr_analysis <- function(df, response_var, predictor_vars, bandwidth = NULL) {
  # Check for coordinates column
  if (!"coordinates" %in% names(df)) {
    stop("Data must contain a 'coordinates' column with spatial coordinates.")
  }

  # Extract coordinates (assume list-column or character "lat,lon")
  coords <- df$coordinates
  if (is.list(coords)) {
    coords_mat <- do.call(rbind, coords)
  } else if (is.character(coords)) {
    coords_mat <- do.call(rbind, strsplit(coords, ","))
    coords_mat <- apply(coords_mat, 2, as.numeric)
  } else if (is.matrix(coords) || is.data.frame(coords)) {
    coords_mat <- as.matrix(coords)
  } else {
    stop("Unrecognized format for 'coordinates' column.")
  }
  if (ncol(coords_mat) != 2) stop("Coordinates must have two columns (lat, lon).")
  colnames(coords_mat) <- c("lat", "lon")

  # Prepare formula
  formula_str <- paste(response_var, "~", paste(predictor_vars, collapse = " + "))
  gwr_formula <- as.formula(formula_str)

  # Set bandwidth if not provided
  if (is.null(bandwidth)) {
    bandwidth <- spgwr::gwr.sel(gwr_formula, data = df, coords = coords_mat)
  }

  # Run GWR
  gwr_result <- spgwr::gwr(
    gwr_formula,
    data = df,
    coords = coords_mat,
    bandwidth = bandwidth,
    hatmatrix = TRUE,
    se.fit = TRUE
  )

  list(
    summary = gwr_result$SDF,
    plot = function() {
      # Plot the spatial distribution of the fitted values
      plot(
        coords_mat,
        col = heat.colors(100)[cut(gwr_result$SDF$pred, 100)],
        pch = 19,
        xlab = "Longitude",
        ylab = "Latitude",
        main = "GWR Fitted Values"
      )
      legend("topright", legend = "Fitted", pch = 19, col = "red")
    }
  )
}

# GAMM Analysis
run_gamm_analysis <- function(df, response_var, predictor_vars, random_effect = NULL, linear_terms = NULL, family = "gaussian") {
  tryCatch({
    # Convert family to function if it's a string
    if (is.character(family)) {
      family <- get(family, mode = "function", envir = parent.frame())
    }

    # Create formula for smooth terms (non-linear predictors)
    smooth_terms <- if (!is.null(predictor_vars)) {
      paste0("s(", predictor_vars, ")", collapse = " + ")
    } else {
      ""
    }

    # Add linear terms if specified
    if (!is.null(linear_terms) && length(linear_terms) > 0) {
      linear_terms_str <- paste(linear_terms, collapse = " + ")
      if (nzchar(smooth_terms)) {
        smooth_terms <- paste(smooth_terms, linear_terms_str, sep = " + ")
      } else {
        smooth_terms <- linear_terms_str
      }
    }

    # Create the formula
    formula_str <- if (nzchar(smooth_terms)) {
      paste(response_var, "~", smooth_terms)
    } else {
      paste(response_var, "~ 1")  # Intercept-only model if no predictors
    }

    # Remove any double plusses from the formula
    formula_str <- gsub("\\+\\s*\\+", "+", formula_str)

    # Prepare random effects if specified
    random_effect_list <- NULL
    if (!is.null(random_effect) && length(random_effect) > 0) {
      # Create a named list with the random effect formula
      random_effect_list <- list(as.formula(paste0("~ 1 | ", random_effect[1])))
      names(random_effect_list) <- random_effect[1]
    }

    # Create the model call
    if (!is.null(random_effect_list)) {
      # If we have random effects, use gamm with the random parameter
      model <- gamm(
        as.formula(formula_str),
        random = random_effect_list,
        family = family,
        data = df,
        method = "REML"
      )
    } else {
      # If no random effects, just use gam for better performance
      model <- list(
        gam = mgcv::gam(
          as.formula(formula_str),
          family = family,
          data = df,
          method = "REML"
        ),
        lme = NULL
      )
    }

    # Return summary and plot function
    list(
      summary = summary(model$gam),
      plot = function() {
        par(mfrow = c(2, 2))
        plot(model$gam, pages = 1, residuals = TRUE, pch = 1, cex = 1, seWithMean = TRUE)
        par(mfrow = c(1, 1))
      }
    )
  }, error = function(e) {
    stop(paste("Error in GAMM analysis:", e$message))
  })
}


###################################################################

######################     UI Definition           ##################

#####################################################################


# UI definition with custom CSS
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    # Prevent caching of CSS
    HTML('<meta http-equiv="Cache-Control" content="no-cache, no-store, must-revalidate" />
         <meta http-equiv="Pragma" content="no-cache" />
         <meta http-equiv="Expires" content="0" />
         <meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>'),
    # Include the external CSS file with version parameter
    tags$link(rel = "stylesheet", type = "text/css",
             href = paste0("app_design.css?", as.integer(Sys.time()))),
    # Add favicon
    tags$link(rel = "icon", type = "image/png", href = "favicon.ico")
  ),

  # Landing Page
  div(
    id = "landingPage",
    class = "landing-page",
    div(
      id = "landingContent",
      class = "landing-content",
      h1("Welcome to the Massasoit Model Forge", class = "landing-title"),
      div(id = "landingButtonContainer", class = "button-container",
        actionButton("enterAppBtn", "Enter Application", class = "app-btn"),
        actionButton("aboutBtn", "About Us", class = "app-btn")
      )
    )
  ),

  # About Page
  div(
    id = "aboutPage",
    class = "about-page-hidden about-background",
    # Dark overlay
    div(id = "aboutPageOverlay", class = "page-overlay"),
    # Content wrapper
    div(id = "aboutPageContentWrapper", class = "page-content-wrapper",
      # Header with title and logo
      div(id = "aboutPageHeader", class = "app-header",
      # The position: fixed style is now handled by the .app-header class in CSS
        div(id = "aboutHeaderLeft", class = "header-left",
          actionLink(
            "appTitleLink_about",
            "Massasoit Model Forge",
            class = "app-title-link"
          )
        ),
        div(id = "aboutHeaderRight", class = "header-right",
          a(
            href = "https://massasoitstem.com/",
            target = "_blank",
            img(
              src = "STEMlogowithBackground.png",
              class = "stem-logo",
              alt = "Massasoit STEM"
            )
          )
        )
      ),
      # Main content
      div(
        id = "aboutContainer",
        class = "about-container",
        # About Us Section
        h2("Who We Are", id = "whoWeAreHeading"),
        p("  \t     We're ",
          tags$a(id = "sammyLink", onclick = "event.preventDefault();", "Sammy Olsen", class = "profile-link"),
          " and ",
          tags$a(id = "ianLink", onclick = "event.preventDefault();", "Ian Handy", class = "profile-link"),
          ", data scientists who got \
        our start at Massasoit Community College. This app began as a \
        tool for a very specific purpose: to help make sense of over a \
        decade's worth of wild bee research in preparation for the \
        national Ecological Society of America conference in 2025."),

        # Popup Modals
        tags$div(id = "sammyModal", class = "modal",
          tags$div(class = "modal-content",
            tags$span(class = "close", "×"),
            div(id = "sammyModalProfileLayout", class = "modal-profile-layout",
              img(src = "sammy_bio_image.jpg",
                  class = "profile-image",
                  alt = "Sammy Olsen"),
              div(id = "sammyProfileDetails", class = "profile-details",
                h3("Sammy Olsen"),
                p(HTML("<a href='https://github.com/spamolsen' target='_blank'>GitHub: spamolsen</a>"))
              )
            )
          )
        ),

        tags$div(id = "ianModal", class = "modal",
          tags$div(class = "modal-content",
            tags$span(class = "close", "×"),
            div(id = "ianModalProfileLayout", class = "modal-profile-layout",
              img(src = "ian_bio_placeholder.png",
                  class = "profile-image ian-profile-image", # Added specific class for Ian's image
                  alt = "Ian Handy"),
              div(id = "ianProfileDetails", class = "profile-details",
                h3("Ian Handy"),
                p(HTML("<a href='https://github.com/ian-handy' target='_blank'>GitHub: ian-handy</a>"))
              )
            )
          )
        ),

        # JavaScript for modals
        tags$script(HTML(
          "// Get the modals
          var sammyModal = document.getElementById('sammyModal');
          var ianModal = document.getElementById('ianModal');

          // Get the links that open the modals
          var sammyLink = document.getElementById('sammyLink');
          var ianLink = document.getElementById('ianLink');

          // Get the <span> elements that close the modals
          var spans = document.getElementsByClassName('close');

          // When the user clicks on a link, open the corresponding modal
          sammyLink.onclick = function() { sammyModal.style.display = 'block'; }
          ianLink.onclick = function() { ianModal.style.display = 'block'; }

          // When the user clicks on <span> (x), close the modal
          for (var i = 0; i < spans.length; i++) {
            spans[i].onclick = function() {
              sammyModal.style.display = 'none';
              ianModal.style.display = 'none';
            }
          }

          // When the user clicks anywhere outside of the modal, close it
          window.onclick = function(event) {
            if (event.target == sammyModal || event.target == ianModal) {
              sammyModal.style.display = 'none';
              ianModal.style.display = 'none';
            }
          }
          ")),
        p("   \t    As interns in the Massasoit STEM Research Program, \
        we worked with field data that was messy, complex, and deeply \
        important. We wanted to build a tool that not only helped us run \
        our own statistical models, but also made advanced data science \
        techniques accessible to researchers like us— community college \
        students, interns, field biologists, and anyone working with data \
        outside of a traditional research institution. We saw how messy and \
        overwhelming data could be, \
        especially when you’re just getting started. \
        Our goal was to make something that not \
        only handles the complexity, but \
        actually helps people ", em("understand"), " it."),
        p("  \t    We believe in open science, \
        transparency, and user developed \
        software. Massasoit Model Forge reflects that belief. Over time, \
        that idea grew into the tool you're using now. It’s built by students, \
        for students, but designed to be powerful enough for anyone."),

        # Why We Built This Section
        h3("Why We Built This", id = "whyWeBuiltThisHeading"),
        p("As community college students, we found that the tools for advanced \
        statistical analysis were either too expensive, opaque, or complex for \
        many in education and research. We \
        built this app to show that real science \
        can happen anywhere, when you give people the tools to do it."),

        h3(" ", id = "missionStatementHeading"), # Empty heading for spacing, can be styled with CSS
        p("Our mission is twofold:"),
        tags$ul(id = "missionList",
          tags$li("To create transparent, open-source tools that bring \
          the power of modern statistical modeling to everyone, across \
          disciplines."),
          tags$li("To legitimize and elevate the research of community college \
          students, whose work is often undervalued and overlooked, despite \
          its scientific rigor.")
        ),
        p("Massasoit Model Forge is a reflection of the values we hold dear: \
        accessibility, reproducibility, and \
        scientific curiosity. We built this \
        for our peers, our mentors, and anyone doing research without a huge \
        lab budget or institutional access. We’re proud of where we came from, \
        and excited about where this project can go."),

        # What It Does Section
        h3("What It Does", id = "whatItDoesHeading"),
        p("Massasoit Model Forge is an open-source statistical modeling \
          application built with R Shiny and hosted through Posit Connect. \
          It integrates both R and Python \
          (via the ",
          span(
            "reticulate",
            class = "code-package-name" # New class for code/package names
          ),
          " package) to \
          give users access to a broad set of tools for analyzing datasets— \
          without needing advanced programming skills or thousand dollar \
          software."),
        p("The app was originally built to support our lab’s ongoing \
          research on wild bee populations, where we needed a flexible tool \
          that could accommodate non-parametric, real-world ecological data. \
          It has since evolved into a general purpose modeling environment \
          that allows users to:"),
        tags$ul(id = "capabilitiesList",
          tags$li("Upload and examine structured data files. (CSV, Excel)"),
          tags$li("Run both parametric (e.g., GLMs, linear regression, ANOVA) \
          and non-parametric (e.g., \
          mixed models, chi-squared tests) analyses."),
          tags$li("Explore and export model diagnostics, summaries, and data \
          visualizations without writing code.")
        ),
        h4("All through a guided interface with built in interpretability \
        and error-checking!", id = "guidedInterfaceHeading"),
        p("We’ll continue to expand the app’s capabilities, documentation, \
        and educational use cases. If you're working with data in an \
        under-resourced setting, this tool was built with you in mind. \
        We’re still learning. We’re still \
        building. And we’re glad you’re here."),

        # Contact Information
        h3("Contact Information", id = "contactInfoHeading"),
        p("Click on our names above to learn more about us or reach out through GitHub."),
        tags$ul(id = "contactList",
          tags$li(tags$a(href = "https://github.com/spamolsen", target = "_blank", "GitHub: spamolsen")),
          tags$li(tags$a(href = "https://github.com/ian-handy", target = "_blank", "GitHub: ian-handy"))
        )
      )
    )
  ),

  # Main Application Content
  div(
    id = "mainApp",
    class = "main-app-hidden main-background",
    # Dark overlay
    div(id = "mainAppOverlay", class = "page-overlay"),
    # Content wrapper
    div(id = "mainAppContentWrapper", class = "page-content-wrapper",
      # Header with title and logo
      div(id = "mainAppHeader", class = "app-header",
        div(id = "mainHeaderLeft", class = "header-left",
          actionLink(
            "appTitleLink",
            "Massasoit Model Forge",
            class = "app-title-link"
          )
        ),
        div(id = "mainHeaderRight", class = "header-right",
          a(href = "https://massasoitstem.com/", target = "_blank",
            img(
              src = "STEMlogowithBackground.png",
              class = "stem-logo",
              alt = "Massasoit STEM"
            )
          )
        )
      ),

      # Transparent spacer div for layout
      div(id = "mainContentSpacer", class = "content-spacer"),

      div(id = "mainAppContainerFluid", class = "container-fluid",
        div(id = "mainAppRow", class = "row",
          # Sidebar with data/analysis controls (left side)
          div(id = "sidebarCol", class = "col-md-4",
            div(id = "sidebarPanel", class = "sidebar-panel",
              tabsetPanel(
                id = "sidebarTabs",
                tabPanel(
                  "Data",
                  id = "dataTabPanel", # Unique ID for tabPanel
                  div(id = "dataSourceGroup", class = "form-group",
                    radioButtons(
                      "dataSource",
                      "Choose data source:",
                      choices = c(
                        "Use base file" = "base",
                        "Upload your own file" = "upload",
                        "Online Databases" = "api"
                      ),
                      selected = "base"
                    )
                  ),

                  # Conditional panel for base file selection
                  conditionalPanel(
                    condition = "input.dataSource == 'base'",
                    id = "baseFileConditionalPanel",
                    div(id = "baseFileGroup", class = "form-group",
                      selectInput(
                        "baseFile",
                        "Select base file:",
                        choices = list.files(
                          "Base_Data_Files",
                          pattern = "\\.(xlsx|csv)$",
                          full.names = FALSE
                        ),
                        selected = NULL
                      )
                    )
                  ),

                  # Conditional panel for file upload
                  conditionalPanel(
                    condition = "input.dataSource == 'upload'",
                    id = "fileUploadConditionalPanel",
                    div(id = "fileUploadGroup", class = "form-group",
                      fileInput(
                        "file1",
                        label = span("Choose File(s)", class = "file-input-label"),
                        multiple = TRUE,
                        accept = c(".xlsx", ".xls", ".csv"),
                        buttonLabel = "Browse..."
                      ),
                      div(
                        "Select one or more Excel (.xlsx, .xls) or CSV (.csv) files",
                        class = "file-input-info"
                      )
                    )
                  ),

                  # Conditional panel for online databases
                  conditionalPanel(
                    condition = "input.dataSource == 'api'",
                    id = "apiSourceConditionalPanel",
                    div(id = "apiSourceGroup", class = "form-group",
                      selectInput(
                        "apiSource",
                        "Select Data Source:",
                        choices = c("Traffic", "Weather")
                      ),
                      # Placeholder for API-specific parameters
                      uiOutput("apiParams")
                    )
                  ),

                  actionButton("loadData", "Load Data", class = "btn-primary load-data-btn"), # Added specific class
                  # The margin-top style is moved to CSS under .load-data-btn

                  # Add JavaScript to switch to Analyze tab when Load Data is clicked
                  tags$script(HTML("
                    $(document).on('shiny:inputchanged', function(event) {
                      if (event.name === 'loadData' && event.value > 0) {
                        setTimeout(function() {
                          $('a[data-value=\"Analyze\"]').tab('show');
                        }, 300);
                      }
                    });
                  "))
                ),

                  tabPanel(
                    "API",
                    id = "apiTabPanel",
                    div(id = "apiControlsGroup", class = "form-group",
                      selectInput(
                        "apiTabSource",
                        "Select Data Source:",
                        choices = c("Traffic", "Weather")
                      ),
                      uiOutput("apiTabParams"),
                      actionButton("callTabApi", "Call API", class = "btn-primary call-api-btn")
                    )
                  ),

                tabPanel(
                  "Analyze",
                  id = "analyzeTabPanel", # Unique ID for tabPanel
                  div(id = "analysisTypeGroup", class = "form-group",
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
                    )
                  ),

                  # Dynamic UI for analysis parameters
                  uiOutput("analysisParams"),

                  # Action button to run the selected analysis
                  actionButton("runAnalysis", "Run Analysis", class = "btn-primary run-analysis-btn") # Added specific class
                )
              )
            )
          ),

          # Main content area with results tabs (right side)
          div(id = "mainContentCol", class = "col-md-8",
            div(id = "mainPanel", class = "main-panel",
              tabsetPanel(
                id = "mainTabs",
                tabPanel(
                  "View File",
                  id = "viewFileTabPanel", # Unique ID for tabPanel
                  div(id = "dataTableContainer", class = "table-responsive",
                    DTOutput("dataTable")
                  )
                ),

                tabPanel("Data Summary",
                  id = "dataSummaryTabPanel", # Unique ID for tabPanel
                  div(id = "summaryOutput", class = "summary-output",
                    verbatimTextOutput("summary")
                  )
                ),

                tabPanel("API Data",
                  id = "apiDataTabPanel",
                  div(id = "apiDataContainer", class = "api-data-container",
                    h3("API Data Integration"),
                    DTOutput("apiDataTable"),
                    verbatimTextOutput("apiCallSummary")
                  )
                ),

                tabPanel("Analysis Results",
                  id = "analysisResultsTabPanel", # Unique ID for tabPanel
                  conditionalPanel(
                    condition = "output.analysisPlot_available == true",
                    id = "analysisPlotConditional",
                    div(id = "analysisPlotContainer", class = "plot-container",
                      plotOutput("analysisPlot", height = "500px")
                    )
                  ),

                  conditionalPanel(
                    condition = "output.analysisPlot_available == false",
                    id = "noPlotAvailableConditional",
                    div(id = "noPlotAlert", class = "alert alert-info",
                      "No plot available for this analysis type or an error occurred."
                    )
                  ),

                  div(id = "analysisResultsOutput", class = "results-output",
                    verbatimTextOutput("analysisResults")
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Function to safely convert columns to appropriate types
  clean_and_convert <- function(df) {
    # Function to guess and convert column types
    convert_column <- function(x) {
      # Remove any non-numeric characters from potential numeric columns
      clean_x <- gsub("[^0-9.-]", "", x)

      # Try to convert to numeric if possible
      num_x <- suppressWarnings(as.numeric(clean_x))
      if (!all(is.na(num_x)) && !all(is.na(x) | x == "")) {
        return(num_x)
      }

      # Check for logical values
      if (all(tolower(x) %in% c("true", "false", "t", "f", "", NA))) {
        return(as.logical(x))
      }

      # Check for dates (simple check)
      if (any(grepl("\\d{1,4}[-/]\\d{1,2}[-/]\\d{1,4}", x, ignore.case = TRUE))) {
        date_x <- as.Date(x, optional = TRUE)
        if (!all(is.na(date_x))) {
          return(date_x)
        }
      }

      # Return as character if no other type fits
      return(x)
    }

    # Apply conversion to each column
    df[] <- lapply(df, function(col) {
      # Skip if column is already in a good format
      if (is.numeric(col) || is.logical(col) || inherits(col, "Date")) {
        return(col)
      }
      convert_column(col)
    })

    return(df)
  }

  # Reactive value to store suitable response variables for logistic regression
  suitable_response_vars <- reactiveVal(NULL)

  # Update suitable response variables when data is loaded
  observeEvent(input$loadData, {
    if (!is.null(data())) {
      df <- data()
      suitable_response_vars(names(df)[sapply(names(df), function(col) {
        is_logistic_response(df, col)
      })])
    }
  })

  # Reactive value to store suitable variables
  suitable_vars <- reactiveVal(NULL)
  suitable_vars_types <- reactiveVal(NULL)

  # Update suitable variables when data is loaded
  observeEvent(input$loadData, {
    if (!is.null(data())) {
      df <- data()
      
      # Get all variable types
      var_types <- lapply(names(df), function(col) {
        is_logistic_response(df, col)
      })
      names(var_types) <- names(df)
      suitable_vars_types(var_types)
      
      # Get only suitable variables
      suitable_vars(names(df)[sapply(var_types, function(x) x != "unsuitable")])
    }
  })

  # Function to format variable names with conversion info
  format_variable_name <- function(var_name, var_type) {
    if (var_type == "convertible") {
      return(paste0(var_name, " <span style='color: red;'>Will be Converted</span>"))
    }
    return(var_name)
  }

  # Function to format variable names with conversion info
  format_variable_name <- function(var_name, var_type) {
    if (var_type == "convertible") {
      return(paste0(var_name, " <span style='color: red;'>Will be Converted</span>"))
    }
    return(var_name)
  }

  # Render response variable selector with conversion info
  output$responseVarSelector <- renderUI({
    req(data(), input$analysisType)
    
    if (input$analysisType == "logistic") {
      # Get all variable types
      var_types <- lapply(names(data()), function(col) {
        is_logistic_response(data(), col)
      })
      names(var_types) <- names(data())
      
      # Get suitable variables
      suitable_vars_list <- names(data())[sapply(var_types, function(x) x != "unsuitable")]
      
      # Create choices with conversion info
      choices <- setNames(
        sapply(suitable_vars_list, function(var) {
          format_variable_name(var, var_types[[var]])
        }),
        suitable_vars_list
      )
      
      selectInput(
        "responseVar",
        "Response Variable:",
        choices = choices,
        selected = NULL
      )
    } else if (input$analysisType != "") {
      selectInput(
        "responseVar",
        "Response Variable:",
        choices = names(data()),
        selected = NULL
      )
    }
  })

  # ... rest of existing server code ...
  # Initialize app - show only landing page initially
  shinyjs::runjs("$('#landingPage').addClass('page').show();")
  shinyjs::runjs("$('#mainApp, #aboutPage').addClass('page').hide();")

  # Navigation button handlers
  observeEvent(input$aboutBtn, {
    navigateToPage("about")
  })

  observeEvent(input$enterAppBtn, {
    navigateToPage("app")
  })

  observeEvent(input$appTitleLink_about, {
    if (appState$currentPage != "landing") {
      navigateToPage("landing")
    }
  })

  # Initialize app state
  appState <- reactiveValues(
    currentPage = "landing" # Can be "landing", "app", or "about"
  )

  # Navigation functions
  navigateToPage <- function(page) {
    # Hide all pages
    shinyjs::runjs("$('.page').hide();")

    # Show the selected page
    if (page == "app") {
      shinyjs::runjs("$('#mainApp').show();")
      appState$currentPage <- "app"
    } else if (page == "about") {
      shinyjs::runjs("$('#aboutPage').show();")
      appState$currentPage <- "about"
    } else {
      shinyjs::runjs("$('#landingPage').show();")
      appState$currentPage <- "landing"
    }

    # Force a redraw to ensure the background image loads
    shinyjs::runjs(
      "setTimeout(function() { 
        $(window).trigger('resize'); 
      }, 50);
      "
    )
  }

  # Navigation observers
  observeEvent(input$enterAppBtn, {
    navigateToPage("app")
  })

  observeEvent(input$aboutBtn, {
    navigateToPage("about")
  })

  # Handle title click to navigate back
  # to the landing page
  observeEvent(input$appTitleLink, {
    navigateToPage("landing")
  }, ignoreInit = TRUE)

  # Reactive values
  analysis_results <- reactiveValues(
    result = NULL,
    plot = NULL
  )

  # Store merged data from multiple files
  merged_data <- reactiveVal(NULL)

  # API parameters UI
output$apiTabParams <- renderUI({
  req(input$apiTabSource)

  if (input$apiTabSource == "Traffic") {
    tagList(
      textInput("trafficLocation", "Location:", placeholder = "e.g., Boston, MA"),
      dateRangeInput("trafficDates", "Date Range:",
                     start = Sys.Date() - 30,
                     end = Sys.Date())
    )
    } else if (input$apiTabSource == "Weather") {
    tagList(
      textInput("tabApi_weatherLocation", "Location (Latitude,Longitude):", 
               placeholder = "e.g., 42.36,-71.06"),
      dateRangeInput("tabApi_weatherDates", "Date Range:",
                    start = Sys.Date() - 7,
                    end = Sys.Date()),
      selectInput("tabApi_weatherVars", "Weather Variables:",
                 choices = c(
                   "Min. Temperature" = "temperature_2m_min",
                   "Max. Temperature" = "temperature_2m_max",
                   "Mean Temperature" = "temperature_2m_mean",
                   "Precipitation" = "precipitation",
                   "Wind Speed" = "wind_speed_10m",
                   "Humidity" = "relative_humidity_2m",
                   "Cloud Cover" = "cloud_cover",
                   "Pressure" = "pressure_msl"
                 ),
                 selected = "temperature_2m",
                 multiple = TRUE),
      # selectInput("tabApi_weatherUnit", "Unit System:",
      #            choices = c("Metric" = "metric", "Imperial" = "imperial"),
      #            selected = "metric")
    )
  } else if (input$apiSource == "Visual Crossing") {
    tagList(
      textInput("vcLocation", "Location:", placeholder = "e.g., Boston, MA"),
      dateRangeInput("vcDates", "Date Range:",
                     start = Sys.Date() - 30,
                     end = Sys.Date()),
      selectInput("vcUnitGroup", "Unit System:",
                  choices = c("Metric" = "metric", "US" = "us"))
    )
  }
})

  # Reactive value to store the loaded data
  data <- reactiveVal(NULL)

  # Helper function to read different file types with robust type handling
  read_data_file <- function(file_path, file_name) {
    # Function to safely convert columns to appropriate types
    clean_and_convert <- function(df) {
      # Convert all columns to character first to avoid type coercion warnings
      df[] <- lapply(df, as.character)

      # Function to guess and convert column types
      convert_column <- function(x) {
        # Remove any non-numeric characters from potential numeric columns
        clean_x <- gsub("[^0-9.-]", "", x)

        # Try to convert to numeric if possible
        num_x <- suppressWarnings(as.numeric(clean_x))
        if (!all(is.na(num_x)) && !all(is.na(x) | x == "")) {
          return(num_x)
        }

        # Check for logical values
        if (all(tolower(x) %in% c("true", "false", "t", "f", "", NA))) {
          return(as.logical(x))
        }

        # Check for dates (simple check)
        if (any(grepl("\\d{1,4}[-/]\\d{1,2}[-/]\\d{1,4}", x, ignore.case = TRUE))) {
          date_x <- as.Date(x, optional = TRUE)
          if (!all(is.na(date_x))) {
            return(date_x)
          }
        }

        # Return as character if no other type fits
        return(x)
      }

      # Apply conversion to each column
      df[] <- lapply(df, function(col) {
        # Skip if column is already in a good format
        if (is.numeric(col) || is.logical(col) || inherits(col, "Date")) {
          return(col)
        }
        convert_column(col)
      })

      return(df)
    }

    # Read the file with appropriate function
    df <- tryCatch({
      if (grepl("\\.xlsx?$", file_name, ignore.case = TRUE)) {
        # For Excel files, read all as text first to avoid type guessing issues
        suppressWarnings({
          df <- readxl::read_excel(file_path, col_types = "text")
        })
      } else if (grepl("\\.csv$", file_name, ignore.case = TRUE)) {
        # For CSV files, read all as character first
        df <- readr::read_csv(file_path, col_types = cols(.default = col_character()), 
                             show_col_types = FALSE)
      } else {
        stop("Unsupported file format. Please use .xlsx, .xls, or .csv files.")
      }

      # Clean and convert column types
      df <- clean_and_convert(df)

      # Process with Python utilities if available
      if (exists("data_utils")) {
        py_df <- r_to_py(df)
        df <- data_utils$append_coord(py_df)
        py_df <- r_to_py(df)
        df <- data_utils$clean_column_names(py_df)
      }

      df
    },
    error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
      return(NULL)
    })
    return(df)
  }

  # Load data when button is clicked
  observeEvent(input$loadData, {
    tryCatch({
      if (input$dataSource == "base" && !is.null(input$baseFile)) {
        # Load single base file
        file_path <- file.path("Base_Data_Files", input$baseFile)
        
        # Read the file with appropriate function
        if (grepl("\\.xlsx?$", input$baseFile, ignore.case = TRUE)) {
          # For Excel files, read all as text first to avoid type guessing issues
          df <- readxl::read_excel(file_path, col_types = "text")
        } else if (grepl("\\.csv$", input$baseFile, ignore.case = TRUE)) {
          # For CSV files, read all as character first
          df <- readr::read_csv(file_path, col_types = cols(.default = col_character()), 
                               show_col_types = FALSE)
        } else {
          stop("Unsupported file format. Please use .xlsx, .xls, or .csv files.")
        }

        # Append coordinates
        py_df <- r_to_py(df)
        df <- data_utils$append_coord(py_df)
        
        # Clean and convert column types
        df <- clean_and_convert(df)
        
        # Store the data
        data(df)
        
        # Update suitable variables and types
        var_types <- lapply(names(df), function(col) {
          is_logistic_response(df, col)
        })
        names(var_types) <- names(df)
        suitable_vars_types(var_types)
        suitable_vars(names(df)[sapply(var_types, function(x) x != "unsuitable")])
      } else if (input$dataSource == "upload" && !is.null(input$file1)) {
        # Load uploaded files
        uploaded_files <- input$file1
        df_list <- list()
        
        for (file in uploaded_files) {
          if (grepl("\\.xlsx?$", file$name, ignore.case = TRUE)) {
            df <- readxl::read_excel(file$datapath, col_types = "text")
          } else if (grepl("\\.csv$", file$name, ignore.case = TRUE)) {
            df <- readr::read_csv(file$datapath, col_types = cols(.default = col_character()), 
                                 show_col_types = FALSE)
          } else {
            stop("Unsupported file format. Please use .xlsx, .xls, or .csv files.")
          }
          df_list[[file$name]] <- df
        }
        
        # Merge all uploaded files
        if (length(df_list) > 0) {
          df <- Reduce(function(x, y) merge(x, y, all = TRUE), df_list)
          
          # Clean and convert column types after merge
          df <- clean_and_convert(df)
          
          # Store the data
          data(df)
          
          # Update suitable variables and types
          var_types <- lapply(names(df), function(col) {
            is_logistic_response(df, col)
          })
          names(var_types) <- names(df)
          suitable_vars_types(var_types)
          suitable_vars(names(df)[sapply(var_types, function(x) x != "unsuitable")])
        }
      } else if (input$dataSource == "api") {
        # Load from API
        if (input$apiSource == "Traffic") {
          # Traffic API implementation
          df <- data.frame(
            Location = input$trafficLocation,
            StartDate = input$trafficDates[1],
            EndDate = input$trafficDates[2]
          )
        } else if (input$apiSource == "Visual Crossing") {
          # Visual Crossing API implementation
          df <- data.frame(
            Location = input$vcLocation,
            StartDate = input$vcDates[1],
            EndDate = input$vcDates[2],
            UnitGroup = input$vcUnitGroup
          )
        }
        
        # Clean and convert column types
        df <- clean_and_convert(df)
        
        # Store the data
        data(df)
        
        # Update suitable variables and types
        var_types <- lapply(names(df), function(col) {
          is_logistic_response(df, col)
        })
        names(var_types) <- names(df)
        suitable_vars_types(var_types)
        suitable_vars(names(df)[sapply(var_types, function(x) x != "unsuitable")])
      }
    }, error = function(e) {
      showNotification(paste("Error loading data:", e$message), 
                      type = "error")
    })
  })

  # Reactive value for API data
  apiData <- reactiveVal(NULL)

  observeEvent(input$callTabApi, {
    req(input$apiTabSource == "Weather")
    
    # Validate location format
    location <- input$tabApi_weatherLocation
    if (!grepl("^\\s*-?\\d+\\.?\\d*\\s*,\\s*-?\\d+\\.?\\d*\\s*$", location)) {
      showNotification("Invalid location format. Use 'latitude,longitude' (e.g., 42.36,-71.06)", type = "error")
      return()
    }
    
    # Parse coordinates
    coords <- as.numeric(strsplit(trimws(location), ",")[[1]])
    lat <- coords[1]
    lon <- coords[2]
    
    tryCatch({
      # Fetch weather data from OpenMeteo
      weather_df <- openmeteo::weather_history(
        location = c(lat, lon),
        start = input$tabApi_weatherDates[1],
        end = input$tabApi_weatherDates[2],
        daily = input$tabApi_weatherVars,
        # unit = input$tabApi_weatherUnit
      )
      
      # Store API data separately
      apiData(weather_df)
      showNotification("Successfully retrieved weather data!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("API Error:", e$message), type = "error")
    })
  })

  output$apiDataTable <- renderDT({
    req(apiData())
    datatable(apiData(),
              options = list(
                scrollX = TRUE,
                pageLength = 10,
                autoWidth = TRUE,
                columnDefs = list(list(className = 'dt-center', targets = "_all"))
              ))
  })

  # Data table output
  output$dataTable <- renderDT({
    df <- data()
    req(df)
    datatable(df, 
              options = list(scrollX = TRUE, 
                           pageLength = 10,
                           lengthMenu = c(5, 10, 15, 20)))
  })

  output$summary <- renderPrint({
    req(data())
    df <- data()
    cat("Data Summary\n")
    cat("===========\n\n")
      cat("Number of rows:", nrow(df), "\n")
      cat("Number of columns:", ncol(df), "\n\n")
      cat("Column names:\n")
      cat(paste(" -", names(df)), sep = "\n")
      cat("\n\n")
      cat("Data types:\n")
      for (col in names(df)) {
        cat(" -", col, ":", class(df[[col]])[1], "\n")
      }
      cat("\n")
      missing_vals <- sapply(df, function(x) sum(is.na(x)))
      if (any(missing_vals > 0)) {
        cat("Missing values:\n")
        for (col in names(missing_vals)) {
          if (missing_vals[[col]] > 0) {
        cat(" -", col, ":", missing_vals[[col]], "\n")
          }
        }
      } else {
        cat("No missing values found.\n")
      }
      cat("\n")
      cat("First few rows of data:\n")
      print(utils::head(df, 5))
})

  output$analysisParams <- renderUI({
    req(input$analysisType)
    req(data()) 

    if (input$analysisType == "") return(NULL)

    # Get data and calculate non-NA counts
    df <- data()
    non_na_counts <- sapply(df, function(x) sum(!is.na(x)))
    total_rows <- nrow(df)
    
    # This is my attempt at right-aligning da N values
    css_rules <- lapply(seq_along(non_na_counts), function(i) {
      glue::glue(
        ".selectize-dropdown-content .option[data-value='{names(non_na_counts)[i]}']::after {{
          content: 'N = {non_na_counts[i]} / {total_rows}';
          float: right;
          color: #777;
          margin-left: 10px;
        }}
        .selectize-dropdown-content .option[data-value='{names(non_na_counts)[i]}']:hover::after {{
          color: #000;
        }}
        .selectize-dropdown-content .option[data-value='{names(non_na_counts)[i]}'].active::after {{
          color: #000;
        }}
        .selectize-dropdown-content .option[data-value='{names(non_na_counts)[i]}'].selected::after {{
          color: #fff;
        }}"
      )
    }) %>% paste(collapse = "\n")



##########################################################################

####################### Function to handle variable conversion before analysis
prepare_response_variable <- function(df, var_name) {
  var_type <- suitable_vars_types()[[var_name]]
  
  if (var_type == "convertible") {
    df <- convert_to_binary(df, var_name)
  }
  
  return(df)
}

##########################################################################

######################     Code for analysis           ###################

##########################################################################



    # Format variable names (without N values in the text, they'll be added via CSS)
    format_vars <- function(vars) {
      setNames(vars, vars)
    }

    all_data_cols <- format_vars(names(df))
    num_data_cols <- format_vars(names(df)[sapply(df, is.numeric)])
    char_data_cols <- format_vars(names(df)[sapply(df, is.character)])
    logistic_cols <- format_vars(names(df)[sapply(names(df), function(col) is_logistic_response(df, col) != "unsuitable")])
    
    # For negative binomial, filter numeric variables that are counts (non-negative integers)
    nb_cols <- format_vars(names(df)[sapply(names(df), function(x) {
      var <- df[[x]]
      is.numeric(var) && all(!is.na(var) & var >= 0 & var == floor(var))
    })])

    # Include the CSS in the UI
    tagList(
      tags$head(tags$style(HTML(css_rules))),
      if (input$analysisType == "glmm" || input$analysisType == "gee") {
        selectizeInput("glmmFamily", "Family for GLMM/GEE:",
                     choices = c("binomial", "poisson", "gaussian", "Gamma", "inverse.gaussian", "quasibinomial", "quasipoisson"),
                     selected = "poisson")
      } else if (input$analysisType == "gamm") {
  tagList(
    selectizeInput("gammFamily", "Family for GAMM:",
                 choices = c("gaussian", "binomial", "poisson", "Gamma", "inverse.gaussian"),
                 selected = "gaussian"),
    selectizeInput("linearTerms", "Linear Terms:",
                 choices = all_data_cols,  # This will show all available variables
                 multiple = TRUE,  # Allow multiple selections
                 options = list(
                   render = I('{
                     item: function(item, escape) { 
                       return "<div>" + escape(item.label) + "</div>"; 
                     }
                   }')
                 ))
  )
},
      # Common parameters for most analyses
      if (input$analysisType %in% c("linear", "glmm", "gamm", "anova", "kruskal",
                                    "gee", "zeroinfl", "hurdle", "wilcoxon", "signtest", "mannwhitney", "gwr")) {
        selectizeInput("responseVar", "Response Variable:",
                     choices = num_data_cols,
                     options = list(render = I(
                       '{
                         item: function(item, escape) { 
                           return "<div>" + escape(item.label) + "</div>"; 
                         }
                       }'
                     )))
      },

      if (input$analysisType == "negbin") {
        selectizeInput("responseVar", "Response Variable:",
                     choices = nb_cols,
                     options = list(render = I(
                       '{
                         item: function(item, escape) { 
                           return "<div>" + escape(item.label) + "</div>"; 
                         }
                       }'
                     )))
      },

      if (input$analysisType == "logistic") {
        selectizeInput("responseVar", "Response Variable:",
                     choices = logistic_cols,
                     options = list(render = I(
                       '{
                         item: function(item, escape) { 
                           return "<div>" + escape(item.label) + "</div>"; 
                         }
                       }'
                     )))
      },

      if (input$analysisType %in% c("linear","logistic", "glmm", "gamm", "negbin", "gee", "zeroinfl", "hurdle", "gwr")) {
        selectizeInput("predictorVars", "Predictor Variables:",
                     choices = all_data_cols,
                     multiple = TRUE,
                     options = list(
                       render = I('{
                         item: function(item, escape) { 
                           return "<div>" + escape(item.label) + "</div>"; 
                         }
                       }')
                     ))
      },

      if (input$analysisType %in% c("glmm", "gamm")) {
        selectizeInput("randomEffect", "Random Effects:",
                       choices = char_data_cols,
                       multiple = TRUE,
                       options = list(
                         render = I('{
                           item: function(item, escape) { 
                             return "<div>" + escape(item.label) + "</div>"; 
                           }
                         }'),
                         delimiter = "+"
                       ))
      },

      if (input$analysisType %in% c("anova", "kruskal", "mannwhitney", "wilcoxon", "signtest")) {
        selectizeInput("groupVar", "Grouping Variables:",
                     choices = char_data_cols,
                     multiple = TRUE,
                     options = list(
                       render = I('{
                         item: function(item, escape) { 
                           return "<div>" + escape(item.label) + "</div>"; 
                         }
                       }'),
                       delimiter = "+"
                     ))
      },

      if (input$analysisType == "logistic") {
        selectizeInput("logisticFamily", "Family for Logistic Regression:",
                     choices = c("binomial", "quasibinomial"),
                     selected = "binomial")
      },

      if (input$analysisType == "chisq") {
        tagList(
          selectizeInput("chisqVar", "Variable for Chi-squared Test:",
                        choices = all_data_cols,
                        options = list(render = I(
                          '{
                            item: function(item, escape) { 
                              return "<div>" + escape(item.label) + "</div>"; 
                            }
                          }'
                        ))),
          textInput("expectedProbs", "Expected Probabilities (comma-separated, optional):",
                    value = "",
                    placeholder = "e.g., 0.25, 0.75"),
          helpText("Leave empty for uniform distribution, or provide probabilities matching levels.")
        )
      },

      if (input$analysisType %in% c("spearman", "pearson")) { # Pearson added as a common correlation
        tagList(
          selectizeInput("var1", "Variable 1:",
                       choices = all_data_cols,
                       options = list(render = I(
                         '{
                           item: function(item, escape) { 
                             return "<div>" + escape(item.label) + "</div>"; 
                           }
                         }'
                       ))),
          selectizeInput("var2", "Variable 2:",
                       choices = all_data_cols,
                       options = list(render = I(
                         '{
                           item: function(item, escape) { 
                             return "<div>" + escape(item.label) + "</div>"; 
                           }
                         }'
                       )))
        )
      },
      # Add more specific parameters as needed for other models
      if (input$analysisType == "permtest") {
        helpText("Permutation signed rank test requires a specific implementation (e.g., from 'coin' package).")
      }
    )
  })


  # Run analysis when the run button is clicked
observeEvent(input$runAnalysis, {
  req(data(), input$analysisType)
  
  # Reset results and plot on new analysis run
  analysis_results$result <- NULL
  analysis_results$plot <- NULL
  
  df <- data()
  
  tryCatch({
    showNotification(paste("Running", input$analysisType, "analysis..."), 
                    type = "message")
    
    # Dispatch to the appropriate analysis function
    model_result <- switch(
      input$analysisType,
      "linear" = run_linear_analysis(
        df, 
        input$responseVar, 
        input$predictorVars
      ),
      "logistic" = run_logistic_analysis(
        df, 
        input$responseVar, 
        input$predictorVars,
        input$logisticFamily
      ),
      "anova" = run_anova_analysis(
        df,
        input$responseVar,
        input$groupVar
      ),
      "glmm" = run_glmm_analysis(
        df,
        input$responseVar,
        input$predictorVars,
        input$randomEffect,
        input$glmmFamily
      ),
      "gee" = run_gee_analysis(
        df,
        input$responseVar,
        input$predictorVars,
        input$clusterID,
        input$glmmFamily
      ),
      "chisq" = run_chisq_analysis(
        df,
        input$responseVar,
        input$groupVar
      ),
      "mannwhitney" = run_mannwhitney_analysis(
        df,
        input$responseVar,
        input$groupVar
      ),
      "kruskal" = run_kruskal_analysis(
        df,
        input$responseVar,
        input$groupVar
      ),
      "hurdle" = run_hurdle_analysis(
        df,
        input$responseVar,
        input$predictorVars
      ),
      "signtest" = run_signtest_analysis(
        df,
        input$responseVar,
        input$groupVar
      ),
      "wilcoxon" = run_wilcoxon_analysis(
        df,
        input$responseVar,
        input$groupVar
      ),
      "spearman" = run_spearman_analysis(
        df,
        input$var1,
        input$var2
      ),
      "permtest" = run_permtest_analysis(
        df,
        input$responseVar,
        input$groupVar
      ),
      "gwr" = {
        req(input$responseVar, input$predictorVars)
        run_gwr_analysis(
          df,
          input$responseVar,
          input$predictorVars
        )
      },
      "negbin" = {
        # For negative binomial, ensure we have a count variable
        var <- df[[input$responseVar]]
        if (!(is.numeric(var) && all(!is.na(var) & var >= 0 & var == floor(var)))) {
          stop("Response variable must be a count variable (non-negative integers)")
        }
        
        # Run negative binomial regression
        formula_str <- paste0(input$responseVar, " ~ ", paste(input$predictorVars, collapse = " + "))
        model <- MASS::glm.nb(as.formula(formula_str), data = df)
        
        list(
          summary = summary(model),
          plot = function() {
            if (length(input$predictorVars) > 0) {
              predictor_to_plot <- input$predictorVars[1]
              plot_data <- data.frame(
                x = df[[predictor_to_plot]],
                y = df[[input$responseVar]],
                predicted = predict(model, type = "response")
              )
              
              plot(
                plot_data$x,
                plot_data$predicted,
                xlab = predictor_to_plot,
                ylab = "Predicted Counts",
                main = "Negative Binomial Regression",
                pch = 16,
                col = "blue"
              )
              abline(h = 0, lty = 2)
            }
          }
        )
      },
      "gamm" = {
        # Run GAMM analysis
        run_gamm_analysis(
          df = df,
          response_var = input$responseVar,
          predictor_vars = input$predictorVars,
          random_effect = input$randomEffect,
          linear_terms = input$linearTerms,
          family = input$gammFamily
        )
      },
      # Add other analysis types here
      NULL
    )
    
    # Store results if successful
    if (!is.null(model_result)) {
      analysis_results$result <- model_result$summary
      analysis_results$plot <- model_result$plot
      
      # Switch to results tab after successful analysis
      updateTabsetPanel(session, "mainTabs", selected = "Analysis Results")
    } else {
      stop("Unsupported analysis type or missing parameters. Please ensure you have selected all required variables for the chosen analysis type.")
    }
    
  }, error = function(e) {
    showNotification(paste("Error in analysis:", e$message), 
                    type = "error")
  })
})

  # Reactive to track if plot is available
  output$analysisPlot_available <- reactive({
    !is.null(analysis_results$plot)
  })
  outputOptions(output, "analysisPlot_available", suspendWhenHidden = FALSE)

  # Display analysis results
output$analysisResults <- renderPrint({
  if (is.null(analysis_results$result)) {
    return("Running analysis...")
  }
  analysis_results$result
})

  # Display analysis plot
  output$analysisPlot <- renderPlot({
    req(analysis_results$plot)
    tryCatch({
      analysis_results$plot()
    }, error = function(e) {
      plot(1, 1, type = "n", main = "Error generating plot",
           xlab = "", ylab = "", axes = FALSE)
      text(1, 1, paste("Plot error:", e$message), cex = 1, col = "red")
    })
  })
}

message("\n***Starting Shiny app\n---\n")
# Run the application
shinyApp(ui = ui, server = server)
