##############################################################
# R02_MMNL_Independent_Wide.R
# ------------------------------------------------------------
# Project   : Mode Choice Modelling with Apollo
# Researcher: Karina
# Date      : 30 October 2025 (revised wide-format version)
#
# Description:
# Mixed Multinomial Logit (MMNL) with independent normal
# random coefficients for travel time and cost,
# using the ORIGINAL WIDE Apollo sample.
##############################################################

### ==========================================================
### 0. LOAD PACKAGES & INITIALISE APOLLO
### ==========================================================

library(apollo)
library(readr)
library(dplyr)

apollo_initialise()

apollo_control <- list(
  modelName  = "MMNL_independent_wide",
  modelDescr = "Mixed Logit with independent normals (wide data)",
  indivID    = "ID",
  panelData  = TRUE,
  mixing     = TRUE,   # IMPORTANT: we have random coefficients
  nCores     = 1
)

### ==========================================================
### 1. LOAD WIDE DATA (SAME AS R01)
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
### 2. STARTING VALUES (MEAN & SD OF RANDOM COEFFS)
### ==========================================================

apollo_beta <- c(
  asc_bus    = -1.2,
  asc_air    = -0.2,
  asc_rail   =  0.3,
  mu_time    = -0.05,   # mean time sensitivity
  mu_cost    = -0.02,   # mean cost sensitivity
  sigma_time =  0.03,   # SD of time coefficient
  sigma_cost =  0.02,   # SD of cost coefficient
  b_access   =  0.01,
  b_service  =  0.20
)

apollo_fixed <- c()

### ==========================================================
### 3. SPECIFY DRAWS FOR RANDOM COEFFS
### ==========================================================

apollo_draws <- list(
  interDrawsType = "sobol",
  interNDraws    = 200,                        # boleh dinaikkan nanti
  interNormDraws = c("draws_time", "draws_cost")  # ~ N(0,1)
)

### ==========================================================
### 4. RANDOM COEFFICIENTS (PER INDIVIDU)
### ==========================================================

apollo_randCoeff <- function(apollo_beta, apollo_inputs){
  with(as.list(c(apollo_beta, apollo_inputs$draws)), {
    b_time <- mu_time + sigma_time * draws_time   # N(mu_time, sigma_time^2)
    b_cost <- mu_cost + sigma_cost * draws_cost   # N(mu_cost, sigma_cost^2)
    return(list(
      b_time = b_time,
      b_cost = b_cost
    ))
  })
}

### ==========================================================
### 5. PROBABILITIES – MIXED LOGIT ON WIDE DATA
### ==========================================================

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate"){

  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs), add = TRUE)

  # Utility functions, now using random b_time & b_cost
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

  avail_list <- list(
    car  = av_car,
    bus  = av_bus,
    air  = av_air,
    rail = av_rail
  )

  mnl_settings <- list(
    alternatives = c(car = 1, bus = 2, air = 3, rail = 4),
    avail        = avail_list,
    choiceVar    = choice,
    V            = V
  )

  P <- list()
  P[["model"]] <- apollo_mnl(mnl_settings, functionality)

  # Panel product across RP/SP rows per ID
  P <- apollo_panelProd(P, apollo_inputs, functionality)

  # Average over inter-individual draws (mixed logit)
  P <- apollo_avgInterDraws(P, apollo_inputs, functionality)

  # Final preparation
  P <- apollo_prepareProb(P, apollo_inputs, functionality)

  return(P)
}

### ==========================================================
### 6. VALIDATE INPUTS & PROBABILITIES
### ==========================================================

apollo_inputs <- apollo_validateInputs()

invisible(apollo_probabilities(apollo_beta, apollo_inputs, functionality = "validate"))
cat("Validation passed for MMNL (wide) model.\n")

### ==========================================================
### 7. ESTIMATE MIXED LOGIT
### ==========================================================

mmnl_model <- apollo_estimate(apollo_beta, apollo_fixed,
                              apollo_probabilities, apollo_inputs)

apollo_modelOutput(mmnl_model)
apollo_saveOutput(mmnl_model)

cat("\nMMNL (wide) estimation completed successfully.\n")
##############################################################
