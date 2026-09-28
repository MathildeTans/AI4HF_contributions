#' @title Data preprocessing for a structured EHR data set
#'
#' @description
#' The preprocessing steps for structured EHR data sets within the DT4H or AI4HF
#' consortia. The steps include:
#' 1. Splitting data into train and test sets
#' 2. Discarding features with missingness (continuous) or low prevalence (binary)
#' 3. Discarding one feature in a pair of highly correlated features
#' 4. Discarding features with only one unique value
#' 5. Imputing missing values in continuous features
#' 6. Standardizing/scaling the data
#' Further explanations are included per section.
#'
#' @details
#' The function loads the required data and saves the preprocessed data.
#' The required data is:
#' * \code{dt}                    - a data.table with the data. Make sure the data contains no more than the 
#' predictors, outcomes, and identifier names (e.g., \code{Pseudo_id})
#' * \code{predictors}            - a vector with the names of the predictors
#' * \code{predictors_binary}     - a subset of \code{predictors} with the names of the binary predictors
#' * \code{predictors_continuous} - a subset of \code{predictors} with the names of the continuous predictors
#' * \code{outcomes}              - a vector with the names of the outcomes
#' * \code{primary_outcome}       - a subset of \code{outcomes} with the name of the primary outcome
#' 
#' @param input_dir             [character] Directory from where required data is loaded.
#' @param output_dir            [character] Directory where the preprocessed data is saved.
#' @param train_perc            [numeric]   Percentage of entries to be assigned to the train set.
#' @param required_prevalence   [numeric]   Percentage of entries as lowerbound for prevalence
#' @param allowed_missingness   [numeric]   Percentage of entries as upperbound for missingness
#' @param override_missingness  [character] Predictor names that are included regardless of missingness
#' @param tolerated_correlation [numeric]   Absolute correlation value that is tolerated
#' @param splitting_seed        [integer]   Seed for splitting into train and test sets
#' @param dropping_seed         [integer]   Seed for randomly dropping one in a pair of correlated features
#' @param imputation_seed       [integer]   Seed for imputation
#' @param m                     [integer]   Number of imputed data sets to be generated
#' @param imputation_maxit      [integer]   Maximum number of iterations for imputation
#'
#' @return Invisibly returns NULL; preprocessed data is saved at \code{output_dir}. These include
#' * \code{train_dt_impX}   - train data.tables; imputation version indicated by \code{_impX}
#' * \code{test_dt_impX}    - test data.tables; imputation version indicated by \code{_impX}
#' * \code{std_params_impX} - standardization parameters; imputation version indicated by \code{_impX}
#' * \code{predictors_used} - all predictor names that should be used for the model analyses
#'
#' @export
data_preprocessing <- function(
    input_dir,
    output_dir,
    train_perc            = 0.8,
    required_prevalence   = 0.01,
    allowed_missingness   = 0.50,
    override_missingness  = c("echo_lvef"),
    tolerated_correlation = 0.95,
    splitting_seed        = 321L,
    dropping_seed         = 95L,
    imputation_seed       = 42L,
    m                     = 10L,
    imputation_maxit      = 20L
) {

  # ---- (0) Load packages -------------------------------------------------------
  suppressPackageStartupMessages({
    library(data.table)
    library(mice)
    library(rsample)
  })
  
  # ---- (0) Load data -----------------------------------------------------------
  # Make sure the data contains no more than the predictors, outcomes, and idx_names
  
  dt                    <- readRDS(file.path(input_dir, sprintf("data.rds")))
  predictors            <- readRDS(file.path(input_dir, sprintf("predictors.rds")))
  predictors_binary     <- readRDS(file.path(input_dir, sprintf("predictors_binary.rds")))
  predictors_continuous <- readRDS(file.path(input_dir, sprintf("predictors_continuous.rds")))
  outcomes              <- readRDS(file.path(input_dir, sprintf("outcomes.rds")))
  primary_outcome       <- readRDS(file.path(input_dir, sprintf("primary_outcome.rds")))
  
  idx_names  <- c("Pseudo_id", "PatientContactId")
  
  # ---- (1) Split data 80/20 ----------------------------------------------------
  # In some cases, data sets contain multiple encounter entries per patient. To ensure
  # a patient stays contained in only the train or test set (to avoid data leakage), 
  # we split at the patient level.
  # 
  # In this case, the outcomes are binary: 1 if event occurred, 0 if it did not.
  # Here, patients are grouped by ever having experience the primary outcome.
  # Namely, "max(outcome_col)" only returns 1 if the patient experienced the outcome
  # for at least one encounter.
  # 
  # After this section, all decision will be made using the train set.
  
  patient_strata <- dt[, .(stratum_outcome = max(outcome_col)), 
                       by = Pseudo_id,
                       env = list(outcome_col = primary_outcome)]
  
  set.seed(splitting_seed)
  patient_split <- rsample::initial_split(
    patient_strata,
    prop   = train_perc,
    strata = stratum_outcome
  )
  
  train_ids <- as.data.table(rsample::training(patient_split))$Pseudo_id
  test_ids  <- as.data.table(rsample::testing(patient_split))$Pseudo_id
  
  # Ensure train_ids and test_ids do not overlap
  stopifnot(length(intersect(train_ids, test_ids)) == 0)
  
  train <- dt[Pseudo_id %in% train_ids]
  test  <- dt[Pseudo_id %in% test_ids]
  
  rm(patient_strata, patient_split, train_ids, test_ids); gc()
  
  # ---- (2.1) Require lowerbound prevalence for binary variables ----------------
  # Te prevalence boundary indicates the prevalence that should be present for binary
  # features to be included in the analyses. This reflects the number of encounters where, e.g., 
  # a condition is registered or a medication is used. If less than, say, 1% of 
  # encounters has a registration for a condition, we discard this condition as a feature.
  
  prev_boundary <- ceiling(required_prevalence*nrow(train))
  
  tmp       <- train[, lapply(.SD, \(x) sum(x, na.rm=T)), .SDcols = predictors_binary]
  result    <- data.table(variable = names(tmp), prev = unlist(tmp))
  drop_cols <- result[prev < prev_boundary, variable]
  
  if (length(drop_cols) > 0) {
    train[, (drop_cols) := NULL]
    test[,  (drop_cols) := NULL]
    predictors_binary <- setdiff(predictors_binary, drop_cols)
    predictors        <- setdiff(predictors,        drop_cols)
  }
  
  rm(prev_boundary, tmp, result, drop_cols);gc()
  
  # ---- (2.2) Drop variables with >50% missingness ------------------------------
  # Continuous features may contain missingness. If a feature has more than, say, 50%
  # missingness, we discard this feature. One may indicate features that should be included
  # regardless.
  
  missingness <- train[, lapply(.SD, \(x) mean(is.na(x))), .SDcols = predictors_continuous]
  drop_cols   <- names(missingness)[missingness > allowed_missingness]
  
  # Override drop_cols here for clinically essential features to keep
  drop_cols <- setdiff(drop_cols, override_missingness)
  
  if (length(drop_cols) > 0) {
    train[, (drop_cols) := NULL]
    test[,  (drop_cols) := NULL]
    predictors_continuous <- setdiff(predictors_continuous, drop_cols)
    predictors            <- setdiff(predictors,            drop_cols)
  }
  
  rm(missingness, drop_cols); gc()
  
  # ---- (3) Discard highly correlated features ----------------------------------
  # Structured EHR data is often quite correlated. Algorithm computations tend to fail
  # if predictors are too correlated. We randomly discard one feature in a pair
  # of highly correlated features.
  
  cor_matrix <- cor(dt[, ..predictors], 
                    use    = "pairwise.complete.obs", 
                    method = "pearson")
  cor_matrix[lower.tri(cor_matrix, diag=TRUE)] <- NA
  
  cor_pairs <- as.data.table(as.table(cor_matrix))
  setnames(cor_pairs, c("col1", "col2", "correlation"))
  cor_pairs <- cor_pairs[!is.na(correlation) & abs(correlation) >= tolerated_correlation]
  cor_pairs[order(-abs(correlation))]
  
  set.seed(dropping_seed)
  drop <- character(0)
  
  for (i in seq_len(nrow(cor_pairs))) {
    c1 <- cor_pairs$col1[i]
    c2 <- cor_pairs$col2[i]
    
    # Pair already resolved by an earlier (stronger) correlation
    if (c1 %in% drop || c2 %in% drop) next
    
    drop <- c(drop, sample(c(c1, c2), 1L))
  }
  
  if (length(drop) > 0) {
    train[, (drop) := NULL]
    test[,  (drop) := NULL]
    predictors_continuous <- setdiff(predictors_continuous, drop)
    predictors_binary     <- setdiff(predictors_binary,     drop)
    predictors            <- setdiff(predictors,            drop)
  }
  
  rm(cor_matrix, cor_pairs, drop); gc()
  
  # ---- (4) Check for >1 unique values per feature ------------------------------
  # Features should contain at least 2 unique values in order to (possibly) inform
  # the outcome. Namely, a feature with only 1 unique value would act as the intercept.
  # Since a model can only contain one intercept, we must discard features with only
  # one unique value.
  
  tmp       <- train[, lapply(.SD, \(x) uniqueN(x, na.rm=T)), .SDcols = predictors]
  result    <- data.table(variable = names(tmp), num_unique = unlist(tmp))
  drop_cols <- result[num_unique == 1, variable]
  
  if (length(drop_cols) > 0) {
    train[, (drop_cols) := NULL]
    test[,  (drop_cols) := NULL]
    predictors_continuous <- setdiff(predictors_continuous, drop_cols)
    predictors_binary     <- setdiff(predictors_binary,     drop_cols)
    predictors            <- setdiff(predictors,            drop_cols)
  }
  
  rm(tmp, result, drop_cols); gc()
  
  # ---- (5) Impute with MICE ----------------------------------------------------
  # Perform MICE. It's important that the equations for MICE are only based on the
  # train entries. After that, they will be applied to the test entries. MICE requires
  # a prediction matrix and a method.
  # 
  # If the data contains p columns, the prediction matrix is a (p x p) binary matrix,
  # where cell (i,j) shows 1 if feature j is used to impute feature i, and 0 if not.
  # The function called from mice, "quickpred", makes a prediction matrix where it assigns
  # a 1 to a cell if the corresponding features are sufficiently correlated. Namely,
  # if they are not correlated, feature j will not carry enough information to impute 
  # feature i. The default absolute correlation threshold is 0.1. This can be updated
  # (see the mice documentation). 
  # It is also possible to exclude variables. It is extremely important that the 
  # outcomes are excluded. These should not be used to impute missing values. However, 
  # a study from 2006 showed that it will decrease bias is the primary outcome is used
  # for the imputation. Hence, note that below the primary outcome is not excluded. 
  # See https://doi.org/10.1016/j.jclinepi.2006.01.009. 
  # 
  # The method is a vector with an element for each feature and indicates which method
  # will be used, if any, to impute the missing values of said feature. There are many
  # options. See https://search.r-project.org/CRAN/refmans/mice/html/mice.html. 
  
  train_for_mice <- copy(train)
  test_for_mice  <- copy(test)
  
  ignore_vec <- c(rep(FALSE, nrow(train_for_mice)),
                  rep(TRUE,  nrow(test_for_mice)))
  
  combined <- rbindlist(list(train_for_mice, test_for_mice), use.names = TRUE)
  mice_input <- copy(combined)
  mice_input[, (idx_names) := NULL]
  
  rm(train_for_mice, test_for_mice); gc()
  
  # Configure prediction matrix and method
  pred <- mice::quickpred(mice_input, exclude = setdiff(outcomes, primary_outcome))
  meth <- mice::make.method(mice_input)
  
  # Fit
  imp <- mice::mice(
    as.data.frame(mice_input),
    m               = m,
    method          = meth,
    predictorMatrix = pred,
    maxit           = imputation_maxit,
    ignore          = ignore_vec,
    seed            = imputation_seed,
    printFlag       = F
  )
  
  # Apply imputation equations to test set and extract m data sets
  completed <- lapply(seq_len(m), \(i) {
    dt <- as.data.table(mice::complete(imp, i))
    dt[, `:=`(
      Pseudo_id        = combined$Pseudo_id,
      PatientContactid = combined$PatientContactId
    )]
    list(
      train = dt[!ignore_vec],
      test  = dt[ignore_vec]
    )
  })
  
  rm(ignore_vec, combined, mice_input, pred, meth, imp); gc()
  
  # ---- (6) Standardization -----------------------------------------------------
  # Data standardization is an essential step of data preprocessing. Here, binary
  # features (coded 0/1) are only scaled to ensure 0's remain untouched.
  
  scaled <- lapply(completed, \(cs) {
    train_dt <- copy(cs$train)
    test_dt  <- copy(cs$test)
    
    mu    <- train_dt[, lapply(.SD, mean), .SDcols = predictors]
    sigma <- train_dt[, lapply(.SD, sd),   .SDcols = predictors]
    # Standardize unless binary, then scale
    mu[, (predictors_binary) := 0]
    
    params <- list()
    
    scale_cols <- \(x, m, s) (x - m) / s
    
    train_dt[, (predictors) := Map(scale_cols, .SD, mu, sigma), .SDcols = predictors]
    test_dt[,  (predictors) := Map(scale_cols, .SD, mu, sigma), .SDcols = predictors]
    
    params <- Map(\(m, s) list(mean = m, sd = s), mu, sigma)
    
    list(train = train_dt, test = test_dt, params = params)
  })
  
  # ---- (7) Save ----------------------------------------------------------------
  for (i in 1:m) {
    saveRDS(scaled[[i]]$train,  file.path(output_dir, sprintf("train_dt_imp%d.rds",   i)))
    saveRDS(scaled[[i]]$test,   file.path(output_dir, sprintf("test_dt_imp%d.rds",    i)))
    saveRDS(scaled[[i]]$params, file.path(output_dir, sprintf("std_params_imp%d.rds", i)))
  }
  
  saveRDS(predictors, file.path(output_dir, sprintf("predictors_used.rds")))
}