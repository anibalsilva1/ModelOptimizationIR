#' A wrapper that calls \code{xgboost} with weights returned from \code{sigma}.
#'
#' @param formula A \code{formula} object.
#' @param train A \code{data.frame} or \code{tibble} with the training data.
#' @param test A \code{data.frame} or \code{tibble} with the test data.
#' @param sigma A \code{vector} with the weights assigned to each instance.
#' @param I A number. It is the number of intervals taken to discretize SERA.
#' @param nrounds A number. Number of boosters to train.
#' @param parameters Additional parameters that are passed to \code{xgboost}. Check \link[xgboost]{xgboost} for more information.
#' @param verbose Prints the error for each iteration. The error printed defaults to the root mean squared error (RMSE).
#' @param ... Additional parameters to be passed into the model. Check \link[xgboost]{xgboost} for more information.
#'
#' @return A \code{list} with the following elements:
#' \itemize{
#'   \item \code{trues} a \code{vector} with the true values of the test set.
#'   \item \code{preds} a \code{vector} with the predictions obtained from the test set.
#'   \item \code{time} a \code{vector} with the train and test time (in seconds).
#' }
#' @export
#'
#' @description A wrapper that calls \link[xgboost]{xgboost} with weights returned from \code{\link{sigma}}.
#'
#' @details
#'
#' Ideally, \code{sigma} should be provided based on domain knowledge of the target variable.
#'
#' When \code{sigma} is not provided, an automatic approach based on adjusted box plot statistics
#' is used to retrieve the type of imbalance a given target variable has:
#'
#' \itemize{
#'  \item \code{high} if there are outliers above the upper fence of the adjusted box plot;
#'  \item \code{low} if there are outliers below the lower fence of the adjusted box plot;
#'  \item \code{both} if there are outliers below/above the lower/upper fences of the adjusted box plot.
#'
#' }
#'
#' The number of intervals, \code{I}, is set as 1000 by default.
#' When the weights are not provided, this parameter is used to calculate them.
#' Note that this parameter can influence the weights assigned to each instance.
#' Changing its value may lead to a better generalization problem.
#'
#'
#'
#' @references
#'
#' Silva A., Ribeiro R., Moniz N., Model Optimization in Imbalanced Regression,
#' \url{https://arxiv.org/abs/2206.09991}
#'
#' Ribeiro R., Moniz N., Imbalanced regression and extreme value prediction,
#' \url{https://link.springer.com/article/10.1007/s10994-020-05900-9}
#'
#' @seealso \link[xgboost]{xgboost}, \code{\link{sigma}}, \link[IRon]{phi}
#'
#'
#' @examples
#'
#' ## A simple call to the wrapper where sigma are generated automatically:
#'
#' library(xgboost)
#' library(dplyr)
#'
#' data("NO2Emissions")
#' n <- nrow(NO2Emissions)
#' s <- sample(1:n, size = n*0.8)
#'
#' formula <- LNO2 ~ .
#' train <- NO2Emissions %>% dplyr::slice(s)
#' test <- NO2Emissions %>% dplyr::slice(-s)
#'
#' res <- XGBoost.sera(formula, train, test)
#' res
#'
#' ## Ideally, there should be domain knowledge about the target variable:
#'
#' library(xgboost)
#' library(dplyr)
#' library(IRon)
#'
#' data("NO2Emissions")
#' n <- nrow(NO2Emissions)
#' s <- sample(1:n, size = n*0.8)
#'
#' I = 1000
#' steps <- seq(0, 1, 1/I)
#'
#' formula <- LNO2 ~ .
#' train <- NO2Emissions %>% dplyr::slice(s)
#' test <- NO2Emissions %>% dplyr::slice(-s)
#'
#' y <- train$LNO2
#'
#' control.points <- matrix(c(1.1, 0, 0, 3.7, 0, 0, 5, 1, 0), byrow = TRUE, ncol=3)
#' ph.ctrl <- phi.control(y = y, method = "range", control.pts = control.points)
#' phi <- phi(y = y, phi.parms = ph.ctrl)
#' sigma <- sigma(phis = phi, steps = steps)
#'
#' # If you want to add some model parameters
#' params <- list(max_depth=5, eta = 10^{-1})
#'
#' res <- XGBoost.sera(formula = formula,
#'                     train = train,
#'                     test = test,
#'                     sigma = sigma,
#'                     parameters = params)
#' res
#'
#'
XGBoost.sera <- function(formula,
                         train,
                         test,
                         sigma=NULL,
                         I=1000,
                         nrounds = 100,
                         parameters = list(),
                         verbose=0,
                         ...
){
  target <- formula[[2]]

  X_train <- data.matrix(dplyr::select(train, -target))
  label_train <- dplyr::pull(train, target)

  X_test <- data.matrix(dplyr::select(test, -target))
  label_test <- dplyr::pull(test, target)

  start_train_time <- Sys.time()

  #Validation checks
  if(is.null(sigma)){
    warning("weights were not provided. They were generated automatically.")

    steps = seq(0, 1, 1/I)
    extr.type <- get_extreme_type(y = label_train)
    phi.ctrl <- IRon::phi.control(y = label_train, extr.type = extr.type)

    phi_train <- IRon::phi(y = label_train, phi.parms = phi.ctrl)
    sigma <- sigma(phis = phi_train, steps = steps)

  }

  # Set objective, if provided as inputs in parameters it is changed accordingly.
  params <- list(objective = "reg:squarederror")
  if(length(parameters) != 0){
    if(is.null(parameters$objective)){
      params <- c(parameters, params)
    }else{
      params <- parameters
    }
  }

  xgb_dtrain <- xgboost::xgb.DMatrix(data = X_train, label = label_train, weight=sigma)
  xgb_dtest <- xgboost::xgb.DMatrix(data = X_test, label = label_test)


  model <- xgboost::xgboost(
    params = params,
    data = xgb_dtrain,
    nrounds = nrounds,
    verbose = verbose,
    ...
  )

  end_train_time <- Sys.time()

  start_test_time <- Sys.time()

  preds <- stats::predict(model, xgb_dtest)

  end_test_time <- Sys.time()

  train_time <- as.numeric(difftime(end_train_time, start_train_time, units = "sec"))
  test_time <- as.numeric(difftime(end_test_time, start_test_time, units = "sec"))

  time <- c("train" = train_time, "test" = test_time)

  res <- list("trues" = label_test,
              "preds" = preds,
              "time" = time)
  return(res)
}
