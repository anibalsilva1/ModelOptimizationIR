#' Get sigma.
#'
#' @description Calculates the number of steps up to a given relevance phi.
#'
#' @param phi A number with the relevance of the target variable.
#' @param steps A numeric vector with steps.
#'
#' @return A number with the number of occurrences of a given observation.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' library(IRon)
#'
#' y <- rnorm(100)
#' phis <- phi(y)
#' steps <- seq(0, 1, 0.001)
#'
#' phi <- phis[1]
#' sigmas <- sigma_one(phi, steps)
#' sigmas <- sigma_one(phi, steps)
#' }
sigma_one <- function(phi, steps){

  t = length(steps) - 1
  v <- 1/2 ## phi=0

  for(i in 2:t){
    if(phi >= steps[i]){
      v <- v + 1
    }
  }
  if(phi == 1){
    v <- v + 1/2
  }
  return(v)
}

#' Calculates the number of steps up to a given relevance phi, for all phis.
#'
#' @param phis A vector with the relevance of the target variable.
#' @param steps A numeric vector with steps.
#'
#' @return A numeric vector with sigma values.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' library(IRon)
#'
#' y <- rnorm(100)
#' phi <- phi(y)
#' steps <- seq(0, 1, 0.001)
#' sigmas <- sigma(phi, steps)
#'
#' sigmas <- sigma(phi, steps)
#' }

sigma <- function(phis, steps){

  t = length(steps) - 1
  N <- sapply(phis, FUN = function(i) sigma_one(phi=i, steps=steps))
  return(N/t)
}
