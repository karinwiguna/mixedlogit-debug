##############################################################
# R01_MNL_Baseline_Wide.R
# ------------------------------------------------------------
# Project   : Mode Choice Modelling with Apollo
# Researcher: Karina
# Date      : 30 October 2025 (revised wide-format version)
#
# Description:
# Baseline Multinomial Logit (MNL) using Apollo,
# with ORIGINAL WIDE data:
#   ID, RP, SP, RP_journey, SP_task,
#   av_car, av_bus, av_air, av_rail,
#   time_car, cost_car, ...,
#   female, business, income, choice
#
# One row = one choice situation (RP or SP) for one individual.
##############################################################

### ==========================================================
### 0. INITIAL SETUP
### ==========================================================

library(apollo)
library(readr)
library(dplyr)

apollo_initialise()

apollo_control <- list(
  modelName  = "MNL_baseline_wide",
  modelDescr = "MNL baseline on original wide Apollo sample",
  indivID    = "ID",      # panel ID (banyak RP/SP per orang)
  panelData  = TRUE,      # IMPORTANT: panel product over rows per ID
  mixing     = FALSE,     # no random parameters here
  nCores     = 1
)

### ==========================================================
### 1. LOAD WIDE DATA (NO RESHAPING)
### ==========================================================

get_script_path <- function(){
  p <- NULL

  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    if (tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE)) {
      p_try <- try(rstudioapi::getActiveDocumentContext()$path, silent = TRUE)
      if (!inherits(p_try, "try-error") && is.character(p_try) && nzchar(p_try)) {
        p <- p_try
      }
    }
  }

  if (is.null(p) || !nzchar(p)) {
    args <- commandArgs(trailingOnly = FALSE)
    fileArg <- grep("^--file=", args, value = TRUE)
    if (length(fileArg) > 0) p <- sub("^--file=", "", fileArg[1])
  }

  if (is.null(p) || !nzchar(p)) {
    p <- normalizePath(".", winslash = "/", mustWork = FALSE)
  }

  normalizePath(p, winslash = "/", mustWork = FALSE)
}

.script_path  <- get_script_path()
.script_dir   <- if (dir.exists(.script_path)) .script_path else dirname(.script_path)
.project_root <- normalizePath(file.path(.script_dir, ".."), winslash = "/", mustWork = FALSE)

cat(".script_dir   : ", .script_dir, "\n")
cat(".project_root : ", .project_root, "\n")

candidate_paths <- c(
  file.path(.script_dir,   "DATA", "apollo_modeChoiceData.csv"),
  file.path(.project_root, "DATA", "apollo_modeChoiceData.csv"),
  file.path(.script_dir,   "data", "apollo_modeChoiceData.csv"),
  file.path(.project_root, "data", "apollo_modeChoiceData.csv")
)

hit <- which(file.exists(candidate_paths))
if (length(hit) == 0) {
  cat("Checked these paths:\n"); print(candidate_paths)
  stop("apollo_modeChoiceData.csv not found. Please put it in DATA/ next to the script.")
}

data_path <- candidate_paths[hit[1]]
cat("Using data file:\n", data_path, "\n")

database <- readr::read_csv(data_path, show_col_types = FALSE)

cat("\nColumns available in wide dataset:\n"); print(names(database))
cat("\nMissing values summary:\n"); print(colSums(is.na(database)))

### ==========================================================
### 2. DEFINE PARAMETERS (STARTING VALUES)
### ==========================================================

# Car as base:
# choice code: 1=car, 2=bus, 3=air, 4=rail
apollo_beta <- c(
  asc_bus   = -1.5,
  asc_air   = -0.3,
  asc_rail  =  0.3,
  b_time    = -0.05,   # generic time sensitivity
  b_cost    = -0.02,   # generic cost sensitivity
  b_access  =  0.01,   # for modes with 'access_*'
  b_service =  0.20    # for modes with 'service_*'
)

apollo_fixed <- c()  # everything estimated

### ==========================================================
### 3. VALIDATE INPUTS
### ==========================================================

apollo_inputs <- apollo_validateInputs()

### ==========================================================
### 4. DEFINE PROBABILITIES – MNL ON WIDE DATA
### ==========================================================

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate"){

  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs), add = TRUE)

  # Utility functions, using WIDE columns like time_car, cost_car, ...
  V <- list()
  V[["car"]]  = 0 +
    b_time * time_car +
    b_cost * cost_car

  V[["bus"]]  = asc_bus +
    b_time   * time_bus +
    b_cost   * cost_bus +
    b_access * access_bus

  V[["air"]]  = asc_air +
    b_time    * time_air +
    b_cost    * cost_air +
    b_access  * access_air +
    b_service * service_air

  V[["rail"]] = asc_rail +
    b_time    * time_rail +
    b_cost    * cost_rail +
    b_access  * access_rail +
    b_service * service_rail

  # Availability from av_car, av_bus, ...
  avail_list <- list(
    car  = av_car,
    bus  = av_bus,
    air  = av_air,
    rail = av_rail
  )

  mnl_settings <- list(
    alternatives = c(car = 1, bus = 2, air = 3, rail = 4),
    avail        = avail_list,
    choiceVar    = choice,   # already 1..4 in this dataset
    V            = V
  )

  P <- list()
  P[["model"]] <- apollo_mnl(mnl_settings, functionality)

  # panel product over multiple RP/SP rows per ID
  P <- apollo_panelProd(P, apollo_inputs, functionality)

  # final preparation
  P <- apollo_prepareProb(P, apollo_inputs, functionality)

  return(P)
}

# Quick validation
invisible(apollo_probabilities(apollo_beta, apollo_inputs, functionality = "validate"))
cat("Validation passed for MNL wide model.\n")

### ==========================================================
### 5. ESTIMATE MODEL
### ==========================================================

mnl_model <- apollo_estimate(apollo_beta, apollo_fixed,
                             apollo_probabilities, apollo_inputs)

apollo_modelOutput(mnl_model)
apollo_saveOutput(mnl_model)

cat("\nMNL (wide) estimation completed successfully.\n")
##############################################################
