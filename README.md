
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ModelOptimizationIR

<!-- badges: start -->
<!-- badges: end -->

The goal of ModelOptimizationIR is to …

## Installation

You can install the development version of ModelOptimizationIR like so:

``` r
install(devtools)
devtools::install_github(anibalsilva1/ModelOptimizationIR)
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(ModelOptimizationIR)
#> 
#> Attaching package: 'ModelOptimizationIR'
#> The following object is masked from 'package:stats':
#> 
#>     sigma

library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(rpart)
library(IRon)

data("NO2Emissions")

n <- nrow(NO2Emissions)
s <- sample(1:n, size = n*0.8)

formula <- LNO2 ~ .
train <- NO2Emissions %>% slice(s)
test <- NO2Emissions %>% slice(-s)

trues <- train$LNO2
phi <- phi(trues)
#> The default of 'doScale' is FALSE now for stability;
#>   set options(mc_doScale_quiet=TRUE) to suppress this (once per session) message
steps <- seq(0, 1, 0.001)
sigmas <- sigma(phi, steps)

model <- rpart(formula, train)
preds <- predict(model, test)

sera <- sera_numerical(trues, preds, sigmas)
sera
#> [1] 155.7345
```
