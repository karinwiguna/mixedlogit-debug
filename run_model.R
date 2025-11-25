#!/usr/bin/env Rscript

## STEP 0 – Initialise environment
suppressPackageStartupMessages({
  library(utils)
})

## STEP 1 – Helper utilities ---------------------------------------------------

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  script_path <- args[startsWith(args, file_arg)]
  if (length(script_path) == 0) {
    return(getwd())
  }
  normalizePath(dirname(sub(file_arg, "", script_path[1])), winslash = "/")
}

check_packages <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(missing) > 0) {
    stop(sprintf("Missing required packages: %s", paste(missing, collapse = ", ")))
  }
}

## STEP 2 – Main orchestration --------------------------------------------------

main <- function() {
  ## STEP 2.1 – Set working directory to repo root
  repo_dir <- get_script_dir()
  setwd(repo_dir)

  ## STEP 2.2 – Check required packages
  required_pkgs <- c("apollo", "readr", "dplyr")
  check_packages(required_pkgs)

  ## STEP 2.3 – Ensure data file exists in expected location
  data_dir <- file.path(repo_dir, "DATA")
  if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

  raw_root   <- file.path(repo_dir, "apollo_modeChoiceData.csv")
  raw_inDATA <- file.path(data_dir, "apollo_modeChoiceData.csv")

  if (!file.exists(raw_root) && !file.exists(raw_inDATA)) {
    stop("Unable to locate 'apollo_modeChoiceData.csv' ",
         "in repo root or DATA/. Please add the file first.")
  }

  # If the main file exists in the root but is not yet in DATA, copy it as a backup.
  if (file.exists(raw_root) && !file.exists(raw_inDATA)) {
    file.copy(raw_root, raw_inDATA, overwrite = FALSE)
  }

  ## STEP 2.4 – Run baseline MNL (wide data)
  message("==> Running R01_MNL_Baseline.R")
  source("R01_MNL_Baseline.R", local = FALSE)

  ## STEP 2.5 – Run MMNL independent (wide data)
  message("==> Running R02_MMNL_Independent.R")
  source("R02_MMNL_Independent.R", local = FALSE)

  ## STEP 2.6 – Run MMNL dependent (wide data)
  message("==> Running R03_MMNL_Dependent.R")
  source("R03_MMNL_Dependent_Wide.R", local = FALSE)

  ## STEP 2.7 – Done
  message("==> All models (R01, R02, R03) finished running.")
}

## STEP 3 – Execute pipeline ----------------------------------------------------
main()
