#' SERA numerical approximation based on sigma.
#'
#' @param trues A vector with the true values of a variable.
#' @param preds A vector with the prediction values of a model.
#' @param sigmas A vector with the sigmas returned from calling \code{\link{sigma}}.
#'
#' @return A scalar with SERA error.
#' @export
#'
#' @examples
#'
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
#' sera <- sera_numerical(trues, preds, sigmas)
#'
#'
sera_numerical <- function(trues, preds, sigmas){

  return(sum(sigmas*(trues-preds)^2))
}
#' @importFrom IRon phi

