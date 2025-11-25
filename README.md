# mixedlogit-using-Apollo-Independent-and-Dependent-randcoeff

This repository is a Mixed Logit (MMNL) debugging playground built around the Apollo R package. It uses the original wide-format example dataset from Apollo and implements:
 - a baseline Multinomial Logit (MNL) model,
 - a Mixed Logit with independent random coefficients, and
 - a Mixed Logit with correlated (dependent) random coefficients.
A small runner script (run_model.R) lets you execute all models from a single command.

## Dependencies

Please install R (version 4.5 or newer recommended) and the following packages:
- `apollo`
- `dplyr`
- `readr`
- `tidyr` (optional; depending on your extensions)
- `utils` (base helper, used in run_model.R)

## File structure
Core files in this repo:
- `apollo_modeChoiceData.csv` – Original wide-format example dataset from the Apollo manual.
- `R_01_MNL Base Line.R` – Baseline Multinomial Logit (MNL) specification using the wide-format data.
  1) Reads DATA/apollo_modeChoiceData.csv (or copies from repo root if needed).
  2) Estimates the non-mixed model for four alternatives: car, bus, air, rail.
- `R_02_MMNL Independent.R` – Mixed Logit (MMNL) with independent random coefficients (e.g. random time and cost).
  1) Uses wide-format data, in line with the Apollo manual.
  2) Sets up apollo_draws + apollo_randCoeff for independent normals.
  3) Estimates the model via apollo_estimate() and writes Apollo outputs.
- `R_03_MMNL Dependent.R` – Mixed Logit (MMNL) with correlated random coefficients (full covariance).
  1) Extends the independent case to allow correlation between random parameters.
  2) Follows the structure of the MMNL example in the Apollo v0.3.6 manual.
- `run_model.R` – Orchestration script that:
  1) Aligns the working directory with the repo root.
  2) Checks that required packages are installed.
  3) Ensures DATA/apollo_modeChoiceData.csv exists (copies from root if needed)
  4) Sequwntially runs:
    - R01_MNL_Baseline.R
    - R02_MMNL_Independent.R
    - R03_MMNL_Dependent.R
- `DATA/` - DATA/apollo_modeChoiceData.csv – working copy of the original wide data used by all models.
- `output/` – (created at runtime) - Folder where Apollo writes estimation logs, model outputs, and summaries (depending on how you save them in each script).

## How to run

### Run the baseline MNL model directly

```bash
Rscript 'R_01_MNL Base Line.R'
```
This Will:
- Ensure DATA/apollo_modeChoiceData.csv is available (if your script includes that step), and
- Estimate the basic MNL model.

### Run the MMNL independent random coefficients

```bash
Rscript 'R_02_MMNL Independent.R'
```
This Script:
- Reads the Apollo wide-format data,
- Defines apollo_draws and apollo_randCoeff with independent normals,
- Builds the utility functions and probabilities,
- Calls apollo_estimate() and saves the results.

### Run the MMNL with correlated random coefficients

```bash
Rscript 'R_03_MMNL Dependent.R'
```
This Script:
- Uses the same wide-format dataset,
- Specifies a full covariance structure for the random coefficients (via Cholesky or similar, as in the Apollo manual),
- Estimates the model and writes the Apollo outputs.

### Run the full preprocessing + MMNL pipeline

```bash
Rscript run_model.R
```

The runner will:
- Set the working directory to the repo root.
- Check that required packages (apollo, readr, dplyr, etc.) are installed.
- Ensure DATA/apollo_modeChoiceData.csv exists (copying from root if needed).
- Source:
  1) `R_01_MNL Base Line.R`,
  2) `R_02_MMNL Independent.R`,
  3) `R_03_MMNL Dependent.R`.
- Print a completion message when all models have finished.

### Running from RStudio

1. Open the `mixedlogit-using-Apollo-Independent-and-Dependent-randcoeff` folder as an RStudio project (or set working directory to the repo root).
2. To run everything:
   - Open `run_model.R` and click Source, or
   - In the Console, run:
     ```bash
     source("run_model.R")
     ```
3. To experiment with a single model:
   - Open `R_01_MNL Base Line.R`, `R_02_MMNL Independent.R`, or `R_03_MMNL Dependent.R`.
   - Edit/ experiment with the specification,
   - Then run the script or selected chunks interactively.
