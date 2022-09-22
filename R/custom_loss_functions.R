#' Custom SERA loss function adaptation for XGBoost.
#'
#' @description Determines the gradient statistics of SERA to be used in
#' XGBoost algorithm as a custom loss function.
#'
#' @param preds Numeric \code{vector} of predictions.
#' @param dtrain \code{xgb.DMatrix} object.
#'
#' @return Returns a list containing the gradient and the hessian of SERA.
#' @export
#'
#' @examples
#'
#' @references
#' https://github.com/dmlc/xgboost/blob/master/R-package/demo/custom_objective.R
#' @importFrom xgboost getinfo
#' @importFrom IRon phi
xgboost.sera <- function(preds, dtrain){

  s <- 0.001
  labels <- xgboost::getinfo(dtrain, "label")

  step <- seq(0,1,s)

  p.extrm <- get_extreme_type(labels)
  phi.ctrl <- IRon::phi.control(labels, extr.type = p.extrm)

  phi.trues <- IRon::phi(labels, phi.ctrl)
  sigmas <- sigma(phis=phi.trues, steps=step)

  grad <- 2*sigmas*(preds-labels)
  hess <- 2*sigmas

  return(list(grad = grad, hess = hess))
}


#' Custom SERA loss function adaptation for Light GBM.
#'
#' @description Determines the gradient statistics of SERA to be used in
#' LGBM algorithm as a custom loss function.
#'
#' @param preds Numeric \code{vector} of predictions.
#' @param dtrain A \code{lgb.Dataset} object.
#'
#' @return  Returns a list containing the gradient and the hessian of SERA.
#' @export
#' @references
#' https://github.com/microsoft/LightGBM/blob/master/R-package/tests/testthat/test_custom_objective.R
#'
#' @importFrom lightgbm get_field
#' @importFrom IRon phi.control
#' @importFrom IRon phi
#' @examples
lgbm.sera <- function(preds, dtrain){

  labels <- lightgbm::get_field(dtrain, "label")

  s <- 0.001
  step <- seq(0,1,s)

  p.extrm <- get_extreme_type(labels)
  phi.ctrl <- IRon::phi.control(labels, extr.type = p.extrm)

  phi.trues <- IRon::phi(labels, phi.ctrl)
  sigmas <- sigma(phis=phi.trues, steps=step)

  grad <- 2*sigmas*(preds-labels)
  hess <- 2*sigmas

  return(list(grad = grad, hess = hess))
}

