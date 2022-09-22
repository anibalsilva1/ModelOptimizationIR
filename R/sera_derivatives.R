#' SERA numerical first derivative based on sigma.
#'
#' @param trues A vector with the true values of a variable.
#' @param preds A vector with the prediction values of a model.
#' @param sigmas A vector with the sigmas returned from calling \code{\link{sigma}}.
#'
#' @return A vector with SERA first derivative for each prediction.
#' @export
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(rpart)
#' library(IRon)
#'
#' data("NO2Emissions")
#'
#' n <- nrow(NO2Emissions)
#' s <- sample(1:n, size = n*0.8)
#'
#' formula <- LNO2 ~ .
#' train <- NO2Emissions %>% slice(s)
#' test <- NO2Emissions %>% slice(-s)
#'
#' trues <- train$LNO2
#' phi <- phi(trues)
#' steps <- seq(0, 1, 0.001)
#' sigmas <- sigma(phi, steps)
#'
#' model <- rpart(formula, train)
#' preds <- predict(model, test)
#'
#' sera_first_deriv <- sera_first_deriv_num(trues, preds, sigmas)
#' }
sera_first_deriv_num <- function(trues, preds, sigmas){
  return (2*sigmas*(preds - trues))
}

#'
#' @importFrom IRon phi
#' @importFrom rpart rpart
NULL

#' SERA numerical second derivative based on sigma.
#'
#' @param sigmas A vector with the sigmas returned from calling \code{\link{sigma}}.
#'
#' @return A vector with SERA second derivative for each prediction.
#' @export
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(IRon)
#'
#' data("NO2Emissions")
#'
#' n <- nrow(NO2Emissions)
#' s <- sample(1:n, size = n*0.8)
#'
#' formula <- LNO2 ~ .
#' train <- NO2Emissions %>% slice(s)
#' test <- NO2Emissions %>% slice(-s)
#'
#' trues <- train$LNO2
#' phi <- phi(trues)
#' steps <- seq(0, 1, 0.001)
#' sigmas <- sigma(phi, steps)
#'
#' sera_second_deriv <- sera_second_deriv_num(sigmas)
#' }
sera_second_deriv_num <- function(sigmas){
  return (2*sigmas)
}

#' @importFrom IRon phi
#' @importFrom rpart rpart

NULL

#' SERA first derivative
#' @description Calculates SERA first derivative using the trapezoidal rule.
#'
#' @param trues Target values from a test set of a given data set. Should be a vector and have the same size as the variable preds
#' @param preds Predicted values given a certain test set of a given data set. Should be a vector and have the same size
#' @param phi Relevance of the values in the parameter trues.
#' @param step Relevance intervals between 0 (min) and 1 (max). Default 0.001.
#'
#' @return A vector with SERA first derivative for each prediction.
#' @export
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(rpart)
#' library(IRon)
#'
#' data("NO2Emissions")
#'
#' n <- nrow(NO2Emissions)
#' s <- sample(1:n, size = n*0.8)
#'
#' formula <- LNO2 ~ .
#' train <- NO2Emissions %>% slice(s)
#' test <- NO2Emissions %>% slice(-s)
#'
#' trues <- train$LNO2
#' phi <- phi(trues)
#'
#' model <- rpart(formula, train)
#' preds <- predict(model, test)
#'
#' sera_deriv <- sera_first_deriv(trues, preds, phi)
#' }
sera_first_deriv <- function(trues, preds, phi, step = 0.001){

  th <- c(seq(0, 1, step))

  errors <- lapply(1:length(trues), FUN = function(i) sapply(th, FUN = function(x) if(phi[i] >= x) preds[i] - trues[i] else 0))
  areas <- sapply(errors, FUN = function(err) step * sapply(2:length(th), FUN=function(x) (err[x-1] + err[x])/2))

  y_deriv <- 2*colSums(areas)

  return(y_deriv)
}

#' @importFrom IRon phi
#' @importFrom rpart rpart

NULL

#' SERA second derivative
#'
#' @description Calculates SERA second derivative using the trapezoidal rule with Riemann's sum.
#' @param trues Target values from a test set of a given data set. Should be a vector and have the same size as the variable preds
#' @param phi Relevance of the values in the parameter trues.
#' @param step Relevance intervals between 0 (min) and 1 (max). Default 0.001.
#'
#' @return A vector with SERA second derivative for each prediction.
#' @export
#' @examples
#'
#' library(dplyr)
#' library(IRon)
#'
#' data("NO2Emissions")
#'
#' n <- nrow(NO2Emissions)
#' s <- sample(1:n, size = n*0.8)
#'
#' formula <- LNO2 ~ .
#' train <- NO2Emissions %>% slice(s)
#' test <- NO2Emissions %>% slice(-s)
#'
#' trues <- train$LNO2
#' phi <- phi(trues)
#'
#' sera_second_deriv <- sera_second_deriv(trues, phi)
#'
sera_second_deriv <- function(trues, phi, step = 0.001){

  th <- c(seq(0, 1, step))

  errors <- lapply(1:length(trues), FUN = function(i) sapply(1:length(th), FUN = function(x) if(phi[i] >= th[x]) 1 else 0))
  areas <- sapply(errors, FUN = function(i) step * sapply(2:length(th), FUN=function(x) (i[x-1] + i[x])/2))

  y_deriv <- 2*colSums(areas)

  return(y_deriv)
}

#' Calculates the value for which SERA is minimum.
#'
#' @param trues A vector of true values.
#' @param phi.trues A vector of the relevance function w.r.t. true values.
#' @param ph Control points.
#' @param step The step used in the trapezoidal rule. Defaults to 0.001.
#'
#' @return A numeric value for which SERA is minimum.
#' @export
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(IRon)
#'
#' data("NO2Emissions")
#'
#' n <- nrow(NO2Emissions)
#' s <- sample(1:n, size = n*0.8)
#'
#' formula <- LNO2 ~ .
#' train <- NO2Emissions %>% slice(s)
#' test <- NO2Emissions %>% slice(-s)
#'
#' trues <- train$LNO2
#' phi <- phi(trues)
#'
#' sera_min <- sera_min(trues, phi)
#' }
sera_min <- function(trues, phi.trues = NULL, ph = NULL, step = 0.001){

  if(is.null(phi.trues)) phi.trues <- phi(trues,ph)

  th <- c(seq(0,1,step))

  num = sapply(th, FUN = function(x)  sum(trues[phi.trues >= x]))
  num_areas = sum(sapply(2:length(th), FUN = function(x) step * (num[x-1] + num[x])/2))

  den = sapply(th, FUN = function(x) length(trues[phi.trues >=x]))
  den_areas = sum(sapply(2:length(th), FUN = function(x) step * (den[x-1] + den[x])/2))

  min = num_areas/den_areas

  return(min)

}

