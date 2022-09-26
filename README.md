
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ModelOptimizationIR

## Installation

You can install the development version of ModelOptimizationIR like so:

``` r
install(devtools)
devtools::install_github("anibalsilva1/ModelOptimizationIR")
```

## Example

Usage example on SERA evaluation:

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

p.ctrl <- phi.control(train$LNO2)
#> The default of 'doScale' is FALSE now for stability;
#>   set options(mc_doScale_quiet=TRUE) to suppress this (once per session) message
steps <- seq(0, 1, 0.001)

model <- rpart(formula, train)
preds <- predict(model, test)


trues <- test$LNO2

phi <- phi(trues, p.ctrl)
sigma <- sigma(phi, steps)


sera_num <- sera_numerical(trues=trues, preds=preds, sigmas=sigma)
sera_num
#> [1] 13.68711

sera_trap <- sera(trues=trues, preds=preds, phi.trues=phi)
sera_trap
#>    preds 
#> 13.68711
```

## Citation

``` bib
@misc{https://doi.org/10.48550/arxiv.2206.09991,
  doi = {10.48550/ARXIV.2206.09991},
  url = {https://arxiv.org/abs/2206.09991},
  author = {Silva, Aníbal and Ribeiro, Rita P. and Moniz, Nuno},
  title = {Model Optimization in Imbalanced Regression},
  publisher = {arXiv},
  year = {2022},
  copyright = {Creative Commons Attribution 4.0 International}
}
```
