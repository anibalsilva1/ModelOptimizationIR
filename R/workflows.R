#' XGBoost workflow
#'
#' @description Workflow for XGBoost.
#'
#' @param formula \code{formula} object.
#' @param train \code{data.frame} or \code{tibble} object with the training set.
#' @param test \code{data.frame} or \code{tibble} object with the test set.
#' @param ... Additional parameters which can be passed into internal model.
#'
#' @return A \code{list} containing trues, predictions and execution time (in seconds).
#' @export
#'
#' @examples
#' \dontrun{
#'
#' library(dplyr)
#' library(xgboost)
#'
#' data("NO2Emissions")
#'
#' n <- nrow(NO2Emissions)
#' s <- sample(1:n, size = n*0.8)
#'
#' formula <- LNO2 ~ .
#' train <- NO2Emissions %>% dplyr::slice(s)
#' test <- NO2Emissions %>% dplyr::slice(-s)
#'
#' nrounds = 100
#'
#'
#' res <- wf.XGBoost(formula, train, test, nrounds=nrounds)
#' res
#' }

wf.XGBoost <- function(formula, train, test, ...){

  t <- formula[[2]]

  xgb_train_m <- data.matrix(dplyr::select(train, -t))
  xgb_test_m <- data.matrix(dplyr::select(test, -t))

  label_train <- dplyr::pull(train, dplyr::all_of(t))
  label_test <- dplyr::pull(test, dplyr::all_of(t))

  xgb_train <- xgboost::xgb.DMatrix(data = xgb_train_m, label = label_train)
  xgb_test <-  xgboost::xgb.DMatrix(data = xgb_test_m, label = label_test)

  start_train_time <- Sys.time()

  m <- xgboost::xgboost(data = xgb_train,
                        verbose = 0,
                        booster = "dart",
                        ...
  )
  end_train_time <- Sys.time()

  start_test_time <- Sys.time()

  preds <- stats::predict(m, xgb_test)

  end_test_time <- Sys.time()

  train_time <- as.numeric(difftime(end_train_time, start_train_time, units = "sec"))
  test_time <- as.numeric(difftime(end_test_time, start_test_time, units = "sec"))

  time <-  c(train_time, test_time)

  r <- list("trues" = as.vector(performanceEstimation::responseValues(formula, test)),
            "preds" = preds,
            "time" = time)

  return(r)

}

#' XGBoost(SERA) workflow
#'
#' @description Workflow for XGBoost optimised with SERA.
#'
#' @param formula A \code{formula} object.
#' @param train \code{data.frame} or \code{tibble} object with the training set.
#' @param test \code{data.frame} or \code{tibble} object with the test set.
#' @param ... Additional parameters which can be passed into internal model.
#'
#' @return A \code{list} containing trues, predictions and execution time (in seconds).
#' @export
#'
#' @details Calls \code{\link{xgboost.sera}} as \code{objective}.
#'
#' @examples
#' \dontrun{
#'
#' library(dplyr)
#' library(IRon)
#' library(xgboost)
#' library(robustbase)
#'
#' data("NO2Emissions")
#'
#' n <- nrow(NO2Emissions)
#' s <- sample(1:n, size = n*0.8)
#'
#' formula <- LNO2 ~ .
#' train <- NO2Emissions %>% dplyr::slice(s)
#' test <- NO2Emissions %>% dplyr::slice(-s)
#'
#' nrounds = 100
#'
#'
#' res <- wf.XGBoost_SERA(formula, train, test, nrounds=nrounds)
#' res
#' }

wf.XGBoost_SERA <- function(formula, train, test,...){

  t <- formula[[2]]

  xgb_train_m <- data.matrix(dplyr::select(train, -dplyr::all_of(t)))
  xgb_test_m <- data.matrix(dplyr::select(test, -dplyr::all_of(t)))

  label_train <- dplyr::pull(train, dplyr::all_of(t))
  label_test <-  dplyr::pull(test, dplyr::all_of(t))

  xgb_train <- xgboost::xgb.DMatrix(data = xgb_train_m, label = label_train)
  xgb_test <- xgboost::xgb.DMatrix(data = xgb_test_m, label = label_test)

  start_train_time <- Sys.time()

  m <- xgboost::xgboost(data = xgb_train,
                        verbose = 0,
                        booster = "dart",
                        objective = xgboost.sera,
                        ...
  )

  end_train_time <- Sys.time()
  start_test_time <- Sys.time()

  preds <- stats::predict(m, xgb_test)

  end_test_time <- Sys.time()

  train_time <- as.numeric(difftime(end_train_time, start_train_time, units = "sec"))
  test_time <- as.numeric(difftime(end_test_time, start_test_time, units = "sec"))

  time <- c("train" = train_time, "test" = test_time)

  r <- list("trues" = as.vector(performanceEstimation::responseValues(formula, test)),
            "preds" = preds,
            "time" = time)
  return(r)

}

#' Gradient Boosting Regression Trees (SERA)
#'
#' @description Workflow for Gradient Boosting Regression Trees optimised with SERA.
#'
#' @param formula A \code{formula} object.
#' @param train \code{data.frame} or \code{tibble} object with the training set.
#' @param test \code{data.frame} or \code{tibble} object with the test set.
#' @param ... Additional parameters which can be passed into internal model.
#'
#' @return A \code{list} containing trues, predictions and execution time (in seconds).
#' @export
#'
#' @examples
#' \dontrun{
#'
#' library(dplyr)
#' library(IRon)
#' library(rpart)
#' library(treeClust)
#'
#' data("NO2Emissions")
#'
#' n <- nrow(NO2Emissions)
#' s <- sample(1:n, size = n*0.8)
#'
#' formula <- LNO2 ~ .
#' train <- NO2Emissions %>% dplyr::slice(s)
#' test <- NO2Emissions %>% dplyr::slice(-s)
#'
#' res <- wf.SERAGradientTreeBoost(formula, train, test)
#' res
#' }

wf.SERAGradientTreeBoost <- function(formula, train, test,...){

  preds <- SERAGradientTreeBoost(formula = formula, train, test, ...)

  r <- list("trues" = performanceEstimation::responseValues(formula, test),
            "preds" = preds$preds,
            "time" = preds$time)
  return(r)
}


#' Gradient Boosting Regression Trees
#'
#' @description Workflow for Gradient Boosting Regression Trees.
#'
#' @param formula A \code{formula} object.
#' @param train \code{data.frame} or \code{tibble} object with the training set.
#' @param test \code{data.frame} or \code{tibble} object with the test set.
#' @param ... Additional parameters which can be passed into internal model.
#'
#' @return A \code{list} containing trues, predictions and execution time (in seconds).
#' @export
#'
#' @examples
#' \dontrun{
#'
#' library(IRon)
#' library(dplyr)
#' library(rpart)
#' library(treeClust)
#'
#' data("NO2Emissions")
#'
#' n <- nrow(NO2Emissions)
#' s <- sample(1:n, size = n*0.8)
#'
#' formula <- LNO2 ~ .
#' train <- NO2Emissions %>% dplyr::slice(s)
#' test <- NO2Emissions %>% dplyr::slice(-s)
#'
#' res <- wf.GradientTreeBoost(formula, train, test)
#' res
#' }


wf.GradientTreeBoost <- function(formula, train, test,...){

  preds <- GradientTreeBoost(formula = formula, train, test, ...)

  r <- list("trues" = performanceEstimation::responseValues(formula, test),
            "preds" = preds$preds,
            "time" = preds$time)
  return(r)
}

#' LGB
#'
#' @description Workflow for Light Gradient Boosting Machines.
#'
#' @param formula A \code{formula} object.
#' @param train \code{data.frame} or \code{tibble} object with the training set.
#' @param test \code{data.frame} or \code{tibble} object with the test set.
#' @param ... Additional parameters which can be passed into internal model.
#'
#' @return A \code{list} containing trues, predictions and execution time (in seconds).
#' @export
#'
#' @examples
#' \dontrun{
#'
#' library(dplyr)
#' library(lightgbm)
#'
#' data("NO2Emissions")
#'
#' n <- nrow(NO2Emissions)
#' s <- sample(1:n, size = n*0.8)
#'
#' formula <- LNO2 ~ .
#' train <- NO2Emissions %>% dplyr::slice(s)
#' test <- NO2Emissions %>% dplyr::slice(-s)
#'
#' nrounds <- 100
#'
#' res <- wf.LGBM(formula, train, test, nrounds=nrounds)
#' res
#' }

wf.LGBM <- function(formula, train, test, ...){

  t <- formula[[2]]

  if(t == "C_peptide")
    min_data = 15
  else
    min_data = 20

  lgbm_train_m <- data.matrix(dplyr::select(train, -dplyr::all_of(t)))
  lgbm_test_m <- data.matrix(dplyr::select(test, -dplyr::all_of(t)))

  label_train <- dplyr::pull(train, dplyr::all_of(t))

  lgbm_train <- lightgbm::lgb.Dataset(data = lgbm_train_m, label = label_train)

  pars <- list(...)

  start_train_time <- Sys.time()

  m <- lightgbm::lightgbm(data = lgbm_train,
                          label = label_train,
                          obj = "regression",
                          verbose = -1,
                          boosting = "dart",
                          params = pars,
                          min_data = min_data)

  end_train_time <- Sys.time()
  start_test_time <- Sys.time()

  preds <- stats::predict(m, lgbm_test_m)

  end_test_time <- Sys.time()

  train_time <- as.numeric(difftime(end_train_time, start_train_time, units = "sec"))
  test_time <- as.numeric(difftime(end_test_time, start_test_time, units = "sec"))

  time <- c("train" = train_time, "test" = test_time)

  r <- list("trues" = as.vector(performanceEstimation::responseValues(formula, test)),
            "preds" = preds,
            "time" = time)
  return(r)

}

#' LGB(SERA)
#'
#' @description Workflow for Light Gradient Boosting optimised with SERA.
#'
#' @param formula A \code{formula} object.
#' @param train \code{data.frame} or \code{tibble} object with the training set.
#' @param test \code{data.frame} or \code{tibble} object with the test set.
#' @param ... Additional parameters which can be passed into internal model.
#'
#' @return A \code{list} containing trues, predictions and execution time (in seconds).
#' @export
#'
#' @details Calls \code{\link{lgbm.sera}} as \code{objective}.
#'
#' @examples
#' \dontrun{
#'
#' library(IRon)
#' library(dplyr)
#' library(lightgbm)
#'
#' data("NO2Emissions")
#'
#' n <- nrow(NO2Emissions)
#' s <- sample(1:n, size = n*0.8)
#'
#' formula <- LNO2 ~ .
#' train <- NO2Emissions %>% dplyr::slice(s)
#' test <- NO2Emissions %>% dplyr::slice(-s)
#'
#' nrounds <- 100
#'
#' res <- wf.LGBM_SERA(formula, train, test, nrounds=nrounds)
#' res
#' }

wf.LGBM_SERA <- function(formula, train, test, ...){

  t <- formula[[2]]

  if(t == "C_peptide")
    min_data = 15
  else
    min_data = 20

  lgbm_train_m <- data.matrix(dplyr::select(train, -dplyr::all_of(t)))
  lgbm_test_m <- data.matrix(dplyr::select(test, -dplyr::all_of(t)))

  label_train <- dplyr::pull(train, dplyr::all_of(t))

  lgbm_train <- lightgbm::lgb.Dataset(data = lgbm_train_m, label = label_train)

  pars <- list(...)

  start_train_time <- Sys.time()

  m <- lightgbm::lightgbm(data = lgbm_train,
                          label = label_train,
                          obj = lgbm.sera,
                          verbose = -1,
                          boosting = "dart",
                          params = pars,
                          min_data = min_data)

  end_train_time <- Sys.time()

  start_test_time <- Sys.time()

  preds <- stats::predict(m, lgbm_test_m)

  end_test_time <- Sys.time()



  train_time <- as.numeric(difftime(end_train_time, start_train_time, units = "sec"))
  test_time <- as.numeric(difftime(end_test_time, start_test_time, units = "sec"))

  time <- c("train" = train_time, "test" = test_time)

  r <- list("trues" = as.vector(performanceEstimation::responseValues(formula, test)),
            "preds" = preds,
            "time" = time)
  return(r)

}
