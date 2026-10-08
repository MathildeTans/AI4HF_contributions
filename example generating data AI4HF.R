# ------------------------------------------------------------------------------
# Generating Data.R
#
# Purpose:  Example of generating required data files for data preprocessing
# Project:  AI4HF
# Branch:   Cohort construction/data preprocessing
# Version:  1.0
# Author:   Mathilde Tans
# Created:  2026-10-08
# Updated:  2026-10-08
#
# Description: This script shows how the data required for data preprocessing is 
#              generated. This example takes files supplied in MyDre.
# ------------------------------------------------------------------------------

# ---- (1) Functions -----------------------------------------------------------

#' @title Loading parquet data
#'
#' @description
#' Loading parquet data. Some minor data cleaning steps are performed.
#'
#' @param file_path   [character] a file_path where the data is stored
#' @return [data.table] the data set
#' 
#' @keywords internal
#' @export
load_data <- function(file_path) {
  # Load parquet file. Each path contains only one file.
  file_loc <- list.files(file_path, pattern = "\\.parquet$", full.names = TRUE)
  file     <- as.data.table(open_dataset(file_loc, format = "parquet"))
  rm(file_loc); gc()
  
  setDT(file)
  
  file[, `:=`(
    patient_demographics_gender = fifelse(patient_demographics_gender == "male", 1L, 0L),
	encounters_admissionYear    = as.integer(encounters_admissionYear)
  )]
  
  # cleaning column types. We only want continuous (numeric) and binary (0/1)
  column_classes <- data.table(col = names(file), class = sapply(file, class))
  
  # transform logical columns
  cols <- column_classes[class == "logical"]$col
  file[, (cols) := lapply(.SD, as.integer), .SDcols = cols]
  rm(cols); gc()
  
  return(file)
}

#' @title Splitting up predictor names
#'
#' @description
#' Split up (predictor) columns into binary or continuous sets, unless not numeric.
#'
#' @param dt       a datatable containing the predictor columns
#' @param cols     a collection of column names to consider
#' @return a nested list of binary and continuous predictor names
#' 
#' @keywords internal
#' @export
classify_cols <- function(dt, cols) {
  type <- sapply(cols, function(cl) {
    x <- dt[[cl]]
    if (!is.numeric(x)) return(class(x)[1])
    u <- unique(x[!is.na(x)])
    if (length(u) <= 2 && all(u %in% c(0, 1))) "binary" else "continuous"
  })
  split(cols, type)
}

# ---- (2) Load data -----------------------------------------------------------
dt_file_path <- file.path("Z:/my_path")
dt           <- load_data(dt_file_path)

# ---- (3) Generate required data files ----------------------------------------
# Groups of columns. Make sure `primary_outcome` is subset of `outcomes`
idx_names       <- c("pid", "encounterId")
outcomes        <- c("patient_demographics_deceased_in_12_months_f",
                     "patient_demographics_deceased_in_24_months_f",
                     "patient_demographics_deceased_in_36_months_f",
                     "patient_demographics_deceased_in_48_months_f")
primary_outcome <- c("patient_demographics_deceased_in_12_months_f")
maggic          <- c("maggic_total_score")

# Other columns not used
other_cols <- c(
  "referenceTimePoint",
  "eventTime",
  "exitTime",
  "rank",
  "nextReferenceTimePoint",
  "lastRecordDate",
  "previousReferenceTimePoint",
  "encounters_encounterClass",
  "encounters_admissionDate",
  "encounters_dischargeDate"
)

# Delete other columns not used
dt[, (other_cols) := NULL]
rm(other_cols); gc()

# Collect remaining columns as predictors
predictors <- setdiff(names(dt), c(idx_names, outcomes, maggic))

# Separate column types into continuous (numeric) and binary (0/1)
predictor_list        <- classify_cols(dt, predictors)
predictors_continuous <- predictor_list$continuous
predictors_binary     <- predictor_list$binary

rm(predictor_list); gc()

# ---- (4) Save required data files --------------------------------------------
saveRDS(dt,                    file.path(input_dir, sprintf("data.rds")))
saveRDS(predictors,            file.path(input_dir, sprintf("predictors.rds")))
saveRDS(predictors_binary,     file.path(input_dir, sprintf("predictors_binary.rds")))
saveRDS(predictors_continuous, file.path(input_dir, sprintf("predictors_continuous.rds")))
saveRDS(outcomes,              file.path(input_dir, sprintf("outcomes.rds")))
saveRDS(primary_outcome,       file.path(input_dir, sprintf("primary_outcome.rds")))
