#' Custom SERA loss function adaptation for XGBoost.
#'
#' @description Determines the gradient statistics of SERA to be used in
#' XGBoost algorithm as a custom loss function.
#'
#' @param preds Numeric \code{vector} of predictions.
#' @param dtrain \code{xgb.DMatrix} object.
#'
#' @return Returns a \code{list} containing the gradient and the hessian of SERA.
#'
#' @references https://github.com/dmlc/xgboost/blob/master/R-package/demo/custom_objective.R
#'
#' @export
#'
#' @examples
#'
#' library(IRon)
#' library(xgboost)
#' library(dplyr)
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
#' train_m <- data.matrix(dplyr::select(train, -LNO2))
#' test_m <- data.matrix(dplyr::select(test, -LNO2))
#'
#' labels_train <- train$LNO2
#' labels_test <- test$LNO2
#'
#' I <- 1000
#' phi <- phi(labels_train)
#'
#' steps <- seq(0, 1, 1/I)
#' sigma <- sigma(phis=phi, steps=steps)
#'
#' xgb_dtrain <- xgb.DMatrix(data = train_m, label = labels_train, weight = sigma)
#' xgb_dtest <- xgb.DMatrix(data = test_m, label = labels_test)
#'
#' params <- list(objective=custom.xgboost.sera, max_depth=5, eta=10^{-1})
#'
#' xgb.model <- xgboost(data=xgb_dtrain, nrounds = 100, params = params, verbose = 0)
#' preds.xgb <- predict(xgb.model, xgb_dtest)
#'

custom.xgboost.sera <- function(preds, dtrain){

  labels <- xgboost::getinfo(dtrain, "label")
  sigma <- xgboost::getinfo(dtrain, "weight")
  if(is.null(sigma)) stop("Weights must be provided.")

  grad <- sigma*(preds-labels)
  hess <- sigma

  return(list(grad = grad, hess = hess))
}


#' Custom SERA loss function adaptation for LightGBM.
#'
#' @description Determines the gradient statistics of SERA to be used in
#' LGBM algorithm as a custom loss function.
#'
#' @param preds Numeric \code{vector} of predictions.
#' @param dtrain A \code{lgb.Dataset} object.
#'
#' @return  Returns a \code{list} containing the gradient and the hessian of SERA.
#' @export
#' @references https://github.com/microsoft/LightGBM/blob/master/R-package/tests/testthat/test_custom_objective.R
#'
#' @examples
#' library(IRon)
#' library(lightgbm)
#' library(dplyr)
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
#' train_m <- data.matrix(dplyr::select(train, -LNO2))
#' test_m <- data.matrix(dplyr::select(test, -LNO2))
#'
#' labels_train <- train$LNO2
#'
#' I <- 1000
#' phi <- phi(labels_train)
#'
#' steps <- seq(0, 1, 1/I)
#' sigma <- sigma(phis=phi, steps=steps)
#'
#' lgbm_dtrain <- lgb.Dataset(data = train_m, label = labels_train, weight = sigma)
#'
#' params <- list(objective=custom.lgbm.sera, max_depth=5, eta=10^{-1}, num_leaves=5)
#'
#' lgbm.model <- lightgbm(data=lgbm_dtrain, nrounds = 100, params = params, verbose = -1)
#' preds.lgbm <- predict(lgbm.model, test_m)
#'
custom.lgbm.sera <- function(preds, dtrain){

  labels <- lightgbm::get_field(dtrain, "label")
  sigma <- lightgbm::get_field(dtrain, "weight")
  if(is.null(sigma)) stop("Weights must be provided.")

  grad <- sigma*(preds-labels)
  hess <- sigma

  return(list(grad = grad, hess = hess))
}

