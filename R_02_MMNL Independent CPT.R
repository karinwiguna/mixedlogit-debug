##############################################################
# R02_MMNL Independent CPT.R
# ------------------------------------------------------------
# Project   : Mode Choice Modelling with Apollo
# Researcher: Karina
# Date      : 03 December 2025
#
# Description:
# Mixed Multinomial Logit (MMNL) with *independent* random
# coefficients for travel time and cost + CPT on generalised cost,
# using Apollo example data:
#   apollo_modeChoiceData.csv
#
# Alternatives: car (1), bus (2), air (3), rail (4)
#
# Random coefficients (independent normals):
#   b_time ~ Normal(mu_time,  sigma_time^2)
#   b_cost ~ Normal(mu_cost,  sigma_cost^2)
#
# Heterogeneous Value of Time per draw:
#   VOT = - b_time / b_cost
#
# Generalised cost (for estimation, no SDLATE & R yet):
#   GC_{nj}^{(r)} = cost_{nj} + VOT^{(n,r)} * time_{nj}
#
# Reference point in estimation:
#   RP_{nt}^{(r)} = GC of chosen alternative j* in that choice task,
#                   so ΔGC is defined against the *status-quo choice*.
#
# CPT value function (no probability weighting yet):
#   v(ΔGC) = -( -ΔGC )^alpha        if ΔGC <= 0   (gain: cheaper than RP)
#          = -lambda * (ΔGC)^alpha  if ΔGC >  0   (loss: more expensive)
#
# Utility:
#   V_{nj}^{(r)} = ASC_j + b_cost^{(n,r)} * v(ΔGC_{nj}^{(r)})
#
##############################################################

### ==========================================================
### STEP 0 – Load packages and initialise Apollo
### ==========================================================

library(apollo)
library(readr)
library(dplyr)

apollo_initialise()

apollo_control <- list(
  modelName  = "MMNL_independent_CPT",
  modelDescr = "Mixed Logit (time & cost, VOT heterogeneity + CPT on GC, WIDE data)",
  indivID    = "ID",
  panelData  = TRUE,
  mixing     = TRUE,
  nCores     = 4
)

### ==========================================================
### STEP 1 – Locate data file and read WIDE dataset
### ==========================================================

get_script_path <- function(){
  p <- NULL
  
  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    if (tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE)) {
      p <- tryCatch(rstudioapi::getActiveDocumentContext()$path,
                    error = function(e) NULL)
    }
  }
  
  if (is.null(p) || !nzchar(p)) {
    args    <- commandArgs(trailingOnly = FALSE)
    fileArg <- grep("^--file=", args, value = TRUE)
    if (length(fileArg) > 0){
      p <- sub("^--file=", "", fileArg[1])
    }
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
  file.path(.script_dir,   "apollo_modeChoiceData.csv"),
  file.path(.project_root, "apollo_modeChoiceData.csv"),
  file.path(.script_dir,   "DATA", "apollo_modeChoiceData.csv"),
  file.path(.project_root, "DATA", "apollo_modeChoiceData.csv")
)

hit <- which(file.exists(candidate_paths))
if (length(hit) == 0) {
  cat("Checked these paths for apollo_modeChoiceData.csv:\n")
  print(candidate_paths)
  stop("apollo_modeChoiceData.csv not found. Please place it in the repo root or DATA/ folder.")
}

data_path <- candidate_paths[hit[1]]
cat("Using data file:\n", data_path, "\n")

database <- readr::read_csv(data_path, show_col_types = FALSE)

cat("\nColumns in WIDE dataset:\n"); print(names(database))
cat("\nMissing values summary:\n"); print(colSums(is.na(database)))

stopifnot("choice" %in% names(database))

### ==========================================================
### STEP 2 – Define parameters & fixed params
### ==========================================================

# ASC_car = 0 (base)
# Random coefficients: mu_time, mu_cost, sigma_time, sigma_cost
# CPT parameters:
#   alpha_raw   -> alpha in (0,1)
#   lambda_raw  -> lambda > 1
# We also keep b_cost as the scale on CPT value of GC.

apollo_beta <- c(
  asc_bus        = -2.0,
  asc_air        = -1.0,
  asc_rail       = -0.7,
  
  mu_time        = -0.012,   # starting values: from your MMNL results
  mu_cost        = -0.061,
  
  sigma_time     =  0.003,
  sigma_cost     =  0.030,
  
  alpha_raw      =  0.0,      # alpha = plogis(alpha_raw) in (0,1)
  lambda_raw     =  log(2)    # lambda = 1 + exp(lambda_raw) > 1
  
  # --- LATER (when already have SDLATE & R from MATSim) ---
  # theta_L       =  0.0,    # schedule delay penalty coefficient
  # theta_R       =  0.0     # unreliability penalty coefficient
)

apollo_fixed <- c()   # semua di-estimate

### ==========================================================
### STEP 3 – Simulation draws (independent normals)
### ==========================================================

apollo_draws <- list(
  interDrawsType = "sobol",
  interNDraws    = 200,
  interNormDraws = c("draws_time", "draws_cost")   # z1,z2 ~ N(0,1) independent
)

### ==========================================================
### STEP 4 – Random coefficients (independent)
### ==========================================================

apollo_randCoeff <- function(apollo_beta, apollo_inputs){
  
  par   <- as.list(apollo_beta)
  draws <- apollo_inputs$draws
  
  mu_time    <- par$mu_time
  mu_cost    <- par$mu_cost
  sigma_time <- par$sigma_time
  sigma_cost <- par$sigma_cost
  
  z1 <- draws$draws_time   # N(0,1)
  z2 <- draws$draws_cost   # N(0,1)
  
  b_time <- mu_time + sigma_time * z1
  b_cost <- mu_cost + sigma_cost * z2
  
  return(list(
    b_time = b_time,
    b_cost = b_cost
  ))
}

### ==========================================================
### STEP 5 – Utility functions & probabilities (CPT on GC)
### ==========================================================

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs), add = TRUE)
  
  # ---- 5.0 Transform CPT parameters ----
  alpha  <- plogis(alpha_raw)        # (0,1)
  lambda <- 1 + exp(lambda_raw)      # (1, +inf)
  
  # ---- 5.1 Compute heterogeneous VOT per draw ----
  # VOT = - b_time / b_cost
  # Tambah epsilon kecil untuk menghindari pembagian 0 (secara praktis jarang terjadi).
  eps  <- 1e-6
  VOT  <- - b_time / (b_cost + eps)
  
  # ---- 5.2 Generalised cost per alternative (no SDLATE & R yet) ----
  # GC_j = cost_j + VOT * time_j
  gc_car  <- cost_car  + VOT * time_car
  gc_bus  <- cost_bus  + VOT * time_bus
  gc_air  <- cost_air  + VOT * time_air
  gc_rail <- cost_rail + VOT * time_rail
  
  # ---- 5.3 Reference point per observation: GC of chosen alternative ----
  # RP = GC of status-quo (chosen) alternative in that task, per draw
  gc_chosen <- ifelse(choice == 1, gc_car,
                      ifelse(choice == 2, gc_bus,
                             ifelse(choice == 3, gc_air, gc_rail)))
  
  # Deviation from reference (ΔGC)
  dgc_car  <- gc_car  - gc_chosen
  dgc_bus  <- gc_bus  - gc_chosen
  dgc_air  <- gc_air  - gc_chosen
  dgc_rail <- gc_rail - gc_chosen
  
  # ---- 5.4 CPT value function on ΔGC ----
  cpt_value <- function(dx, alpha, lambda){
    v <- numeric(length(dx))
    gain <- dx <= 0   # GC <= RP: cheaper or equal → gain
    # Gain: dx <= 0
    v[gain]  <- -((-dx[gain])^alpha)
    # Loss: dx > 0
    v[!gain] <- -lambda * ((dx[!gain])^alpha)
    return(v)
  }
  
  v_car  <- cpt_value(dgc_car,  alpha, lambda)
  v_bus  <- cpt_value(dgc_bus,  alpha, lambda)
  v_air  <- cpt_value(dgc_air,  alpha, lambda)
  v_rail <- cpt_value(dgc_rail, alpha, lambda)
  
  # ---- 5.5 Utility functions (V_j) ----
  # Scale of CPT value is b_cost (unit: utility per unit GC).
  V <- list()
  
  V[["car"]]  <- 0 +
    b_cost * v_car
  
  V[["bus"]]  <- asc_bus +
    b_cost * v_bus
  
  V[["air"]]  <- asc_air +
    b_cost * v_air
  
  V[["rail"]] <- asc_rail +
    b_cost * v_rail
  
  # ---- 5.6 Availability ----
  avail <- list(
    car  = av_car,
    bus  = av_bus,
    air  = av_air,
    rail = av_rail
  )
  
  # ---- 5.7 MNL kernel inside Mixed Logit ----
  mnl_settings <- list(
    alternatives = c(car = 1, bus = 2, air = 3, rail = 4),
    avail        = avail,
    choiceVar    = choice,
    V            = V,
    componentName = "MNL_CPT_GC"
  )
  
  P <- list()
  P[["model"]] <- apollo_mnl(mnl_settings, functionality)
  
  # Panel product over repeated choices per ID
  P <- apollo_panelProd(P, apollo_inputs, functionality)
  
  # Average over inter-individual draws (Mixed Logit)
  P <- apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  # Prepare final probabilities
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  
  return(P)
}

### ==========================================================
### STEP 6 – Validate inputs
### ==========================================================

apollo_inputs <- apollo_validateInputs()

invisible(apollo_probabilities(apollo_beta, apollo_inputs, functionality = "validate"))
cat("Validation passed for MMNL_independent_CPT_GC.\n")

### ==========================================================
### STEP 7 – Estimate & save output
### ==========================================================

mmnl_indep_cpt_gc_model <- apollo_estimate(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs
)

apollo_modelOutput(mmnl_indep_cpt_gc_model)
apollo_saveOutput(mmnl_indep_cpt_gc_model)

cat("\nMMNL_independent_CPT_GC estimation completed.\n")
##############################################################
