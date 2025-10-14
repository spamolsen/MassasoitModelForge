Posit Connect deployment guide for Massasoit Model Forge

Overview

This document explains how to publish the `MassasoitModelForge` Shiny app to Posit Connect when the app uses Python via `reticulate`.

Summary of recommended steps

- Record R package dependencies with `renv` and commit `renv.lock`.
- Provide a declarative Python environment for Connect: either `requirements.txt` (pip) or `environment.yml` (conda). This repo contains `requirements.txt` and the added `environment.yml` as an option.
- Do not create or modify Python environments at runtime on the server. Let Connect build the environment and set `RETICULATE_PYTHON`.
- Use the Deploy button in RStudio or `rsconnect::deployApp()`.

Quick notes

- If Connect supports pip-based builds, it will install from `requirements.txt` automatically.
- If you prefer conda-managed builds, `environment.yml` is provided and will instruct Connect (or your admin) to build a conda env with Python 3.10 and then install pip requirements.

Debugging tips

- If Python imports fail after deployment, check the Connect deployment logs and the app's startup logs. You can temporarily enable `reticulate::py_config()` output in `app.R` to see which Python interpreter and environment reticulate detected.

More details are available in the project's README. This file is intentionally concise and focused on the Connect workflow.
