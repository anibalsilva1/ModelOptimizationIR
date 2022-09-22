
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ModelOptimizationIR

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

library(IRon)
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
library(treeClust)
#> Loading required package: cluster

data("NO2Emissions")

n <- nrow(NO2Emissions)
s <- sample(1:n, size = n*0.8)


formula <- LNO2 ~ .
train <- NO2Emissions %>% dplyr::slice(s)
test <- NO2Emissions %>% dplyr::slice(-s)

maxIter <- 50

res <- SERAGradientTreeBoost(formula, train, test, maxIter=maxIter)
#> The default of 'doScale' is FALSE now for stability;
#>   set options(mc_doScale_quiet=TRUE) to suppress this (once per session) message
res
#> $preds
#>   [1] 4.411149 4.436566 4.411149 4.411149 4.411149 4.411499 4.420270 4.411149
#>   [9] 4.436566 4.410877 4.411149 4.436300 4.435949 4.411499 4.445071 4.435678
#>  [17] 4.411149 4.410877 4.411499 4.490828 4.411149 4.411149 4.519389 4.435949
#>  [25] 4.410877 4.445071 4.410877 4.411149 4.435949 4.410877 4.510859 4.410877
#>  [33] 4.410877 4.513772 4.436566 4.410877 4.411149 4.411149 4.433526 4.411149
#>  [41] 4.411149 4.411149 4.359508 4.411499 4.410877 4.435949 4.510588 4.411149
#>  [49] 4.435678 4.410877 4.436710 4.411149 4.411149 4.410877 4.445071 4.411499
#>  [57] 4.410877 4.411149 4.411149 4.411149 4.411149 4.410877 4.411149 4.410877
#>  [65] 4.411149 4.433526 4.509354 4.411149 4.411149 4.419678 4.445071 4.411149
#>  [73] 4.411499 4.411149 4.410877 4.411149 4.411149 4.411149 4.509003 4.410877
#>  [81] 4.410877 4.410877 4.410877 4.410877 4.510859 4.500221 4.411499 4.419678
#>  [89] 4.435949 4.410877 4.905904 4.509354 4.435949 4.411499 4.411149 4.435949
#>  [97] 4.411149 4.411149 4.411149 4.359508
#> 
#> $time
#>    train     test 
#> 9.649602 0.115809
```
