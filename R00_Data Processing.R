##############################################################
# R00_DataProcessing.R
# ------------------------------------------------------------
# Project   : Mode Choice Modelling with Apollo
# Researcher: Karina
# Date      : 30 October 2025
#
# Description:
# This script reshapes the original wide-format mode choice data
# into long-format data compatible with the Apollo package.
# The final output is saved as:
#   data/processed/modechoice_long.csv
#
# The sample data structure (wide):
# ------------------------------------------------------------
# ID, RP, SP, RP_journey, SP_task,
# av_car, av_bus, av_air, av_rail,
# time_car, cost_car, time_bus, cost_bus, ...
# female, business, income, choice
#
# After processing, each individual (ID) will have 4 rows
# (one per alternative), with variables:
# ID, choice_id, alt, time, cost, access, service,
# avail, female, business, income, choice
##############################################################

### ==========================================================
### 0. INITIAL SETUP
### ==========================================================

library(readr)
library(dplyr)
library(tidyr)

# Set your input and output file paths
input_file  <- "DATA/apollo_modeChoiceData.csv"
output_file <- "DATA/processed/modechoice_long.csv"

# Load the raw wide-format dataset
data_wide <- read_csv(input_file, show_col_types = FALSE)
data_wide <- data_wide %>% mutate(row_id = row_number())

cat("\n✅ Data loaded successfully. Columns:\n")
print(names(data_wide))


### ==========================================================
### 1. DEFINE ALTERNATIVES
### ==========================================================

alts <- c("car", "bus", "air", "rail")


### ==========================================================
### 2. RESHAPE ATTRIBUTES (time, cost, access, service)
### ==========================================================

# Pivot all mode-specific variables into long format
attr_long <- data_wide %>%
  pivot_longer(
    cols = matches("^(time|cost|access|service)_"),
    names_to = c(".value", "alt"),
    names_sep = "_"
  )%>%
  select(ID, row_id, alt, time, cost, access, service, female, business, income)

cat("\n✅ Attributes reshaped to long format.\n")
print(head(attr_long))


### ==========================================================
### 3. RESHAPE AVAILABILITY (av_car, av_bus, ...)
### ==========================================================

avail_long <- data_wide %>%
  pivot_longer(
    cols = matches("^av_"),
    names_to = "alt",
    names_prefix = "av_",
    values_to = "avail"
  )%>%
  select(ID, row_id, alt, avail)

### ==========================================================
### 4. CREATE CHOICE IDENTIFIERS
### ==========================================================

# mapping alternatif → angka (biar bisa dipakai untuk dummy 0/1)
alt_map <- c(car=1L, bus=2L, air=3L, rail=4L)

# buat lookup untuk pilihan aktual
choice_lookup <- data_wide %>%
  transmute(
    ID,
    row_id,   # pakai row_id yg sudah dibuat di STEP 0
    choice_num = case_when(
      is.character(choice) ~ unname(alt_map[tolower(choice)]), # kalau kolom choice berupa teks
      is.numeric(choice)   ~ as.integer(choice),                # kalau sudah angka
      TRUE ~ NA_integer_
    )
  )

# cek sanity
stopifnot(all(choice_lookup$choice_num %in% 1:4, na.rm = TRUE))


### ==========================================================
### 5. JOIN ALL COMPONENTS
### ==========================================================

long <- attr_long %>%
  mutate(choice_id = row_id) %>% 
  left_join(avail_long, by = c("ID", "row_id", "alt")) %>%
  left_join(choice_lookup, by = c("ID", "row_id"))

cat("\n✅ Joined data successfully.\n")
print(head(long))


### ==========================================================
### 6. CLEAN AND RECODE VARIABLES
### ==========================================================

long <- long %>%
  mutate(
    # Replace missing access/service with zero
    access  = ifelse(is.na(access), 0, access),
    service = ifelse(is.na(service), 0, service),
    # Binary choice variable (1 if chosen alt, 0 otherwise)
    choice  = if_else(as.integer(unname(alt_map[alt])) == choice_num, 1L, 0L),
    # Convert availability to 1/0
    avail   = ifelse(is.na(avail), 0L, as.integer(avail)),
    # Optional socio-demographic variables
    female  = as.integer(female),
    business= as.integer(business),
    income= as.integer(income)
  )


### ==========================================================
### 7. VALIDATION CHECKS
### ==========================================================

cat("\n✅ Data summary:\n")
print(summary(long))

cat("\nUnique alternatives:\n")
print(unique(long$alt))

cat("\nChoice distribution (should sum to number of individuals):\n")
print(table(long$choice))

# Optional: check NA count
cat("\nNumber of NA values per column:\n")
print(colSums(is.na(long)))

cat("\nChoice per (ID, choice_id) should be 1:\n")
print(
  long %>% 
    group_by(ID, choice_id) %>% 
    summarise(sel = sum(choice), n=n(), .groups="drop") %>%
    count(sel, name = "n_sets")
)

cat("\nRows per (ID, choice_id):\n")
print(
  long %>% 
    count(ID, choice_id, name="n_rows") %>%
    count(n_rows, name="n_sets")
)

### ==========================================================
### 8. SAVE PROCESSED LONG FORMAT
### ==========================================================

# Create output folder if it doesn’t exist
if(!dir.exists("data/processed")) dir.create("data/processed", recursive = TRUE)

# Save to CSV
write_csv(long, output_file)

cat(paste0("\n✅ Long-format data saved to: ", output_file, "\n"))


### ==========================================================
### 9. NOTES
### ==========================================================
# - Ensure that column names in the raw file strictly follow
#   patterns: time_car, cost_bus, access_air, service_rail, etc.
# - The resulting 'long' dataset is fully compatible with Apollo.
# - Next step: run R01_MNL_Baseline.R to estimate MNL.
##############################################################
