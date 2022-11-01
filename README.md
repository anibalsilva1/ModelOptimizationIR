
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ModelOptimizationIR

## Installation

You can install the development version of ModelOptimizationIR like so:

``` r
install(devtools)
devtools::install_github("anibalsilva1/ModelOptimizationIR")
```

## Example

Usage example of SERA as a loss function to infer extreme values:

``` r
library(ModelOptimizationIR)
#> 
#> Attaching package: 'ModelOptimizationIR'
#> The following object is masked from 'package:stats':
#> 
#>     sigma
library(xgboost)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following object is masked from 'package:xgboost':
#> 
#>     slice
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(IRon)

data("NO2Emissions")
n <- nrow(NO2Emissions)
s <- sample(1:n, size = n*0.8)

I = 1000
steps <- seq(0, 1, 1/I)

formula <- LNO2 ~ .
train <- NO2Emissions %>% dplyr::slice(s)
test <- NO2Emissions %>% dplyr::slice(-s)

y <- train$LNO2

control.points <- matrix(c(1.1, 0, 0, 3.7, 0, 0, 5, 1, 0), byrow = TRUE, ncol=3)
ph.ctrl <- phi.control(y = y, method = "range", control.pts = control.points)
phi <- phi(y = y, phi.parms = ph.ctrl)
sigma <- sigma(phis = phi, steps = steps)

# If you want to add some model parameters
params <- list(max_depth=5, eta = 10^{-1})

res <- XGBoost.sera(formula = formula,
                    train = train,
                    test = test,
                    sigma = sigma,
                    parameters = params)
res
#> $trues
#>   [1] 3.10009 3.23080 3.67377 2.28238 2.21920 4.53475 3.31782 2.66723 4.35543
#>  [10] 3.73290 4.26127 2.57261 4.23120 3.99268 2.00148 3.19458 3.71601 2.95491
#>  [19] 4.12066 2.54945 3.57235 4.33860 3.56105 3.16548 3.01062 4.40672 3.68888
#>  [28] 4.58802 2.53370 3.23868 2.44235 1.62924 2.73437 4.89933 4.07414 4.02356
#>  [37] 4.26549 4.47506 3.69387 4.19117 2.72785 4.27528 2.79728 4.55703 2.96527
#>  [46] 3.50255 4.03954 4.14630 3.87743 3.91202 4.17746 4.26549 3.80444 4.40305
#>  [55] 4.49647 3.42426 4.33336 3.54385 4.00186 4.14472 1.70475 2.87920 3.72569
#>  [64] 4.90602 4.16044 4.08092 4.32678 4.20320 4.17285 4.10594 3.28466 3.74950
#>  [73] 3.81551 3.79549 2.54160 2.42480 3.83945 4.56954 4.24133 2.69463 3.21888
#>  [82] 4.18205 4.07923 2.76632 4.51852 4.01096 2.40695 3.92197 3.62966 4.42485
#>  [91] 4.21804 4.18205 4.62399 2.73437 2.98062 3.92790 4.04830 4.06732 3.86073
#> [100] 2.56495
#> 
#> $preds
#>   [1] 4.263956 3.887488 4.315934 4.009439 4.156706 4.274750 4.250453 3.868563
#>   [9] 4.005608 3.865511 4.560306 3.937967 4.060581 4.055746 3.954560 4.082080
#>  [17] 4.064754 3.889043 4.222600 3.824662 3.959940 4.489875 4.198874 4.091762
#>  [25] 4.188568 4.270676 3.907565 4.428837 4.146828 3.869177 3.963143 3.775789
#>  [33] 3.924203 4.590806 4.310972 4.273203 4.025193 4.156253 4.196452 4.268201
#>  [41] 4.052545 4.432015 3.802379 4.420926 3.873926 4.214679 4.320316 4.616881
#>  [49] 4.008936 4.191723 4.201665 4.338653 4.482992 4.274928 4.423419 4.064307
#>  [57] 4.369845 4.194212 4.453813 4.246326 3.882254 4.057608 4.125885 4.419963
#>  [65] 4.132915 4.132671 4.665098 4.462840 4.260127 3.975104 4.227847 4.199425
#>  [73] 4.116467 4.190015 3.945343 4.030990 4.164529 4.497624 4.117437 3.941414
#>  [81] 4.041115 4.565991 4.349231 4.239834 4.565394 4.304387 3.881599 4.436210
#>  [89] 4.100194 4.237485 4.083336 4.179106 4.282740 3.982482 4.048569 3.969805
#>  [97] 4.192683 4.275868 3.805453 3.907289
#> 
#> $time
#>       train        test 
#> 0.088861227 0.002364874
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
