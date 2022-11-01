
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
library(xgboost)
library(dplyr)
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
#>   [1] 3.31419 4.34640 3.23080 3.67377 3.65584 3.93183 3.77963 3.75420 3.45632
#>  [10] 3.94546 3.48431 4.13035 4.01998 2.62467 4.23120 1.33500 4.58190 4.39198
#>  [19] 3.01062 2.64617 4.62399 3.02529 3.66356 3.48431 4.30271 3.85862 3.84802
#>  [28] 4.02535 4.21656 2.44235 4.07414 4.02356 4.25135 4.41280 3.08649 3.10459
#>  [37] 4.25561 4.27528 2.78501 4.36055 2.79728 3.59182 3.02042 4.24850 3.21487
#>  [46] 3.36730 4.03954 4.14630 4.07073 3.91202 4.28496 3.15274 4.40305 3.34990
#>  [55] 4.49647 3.54385 3.17388 4.22244 4.23700 3.31419 4.18662 3.81331 5.02651
#>  [64] 3.72569 4.31348 3.48431 3.88156 2.42480 3.71601 3.27714 4.10594 3.28466
#>  [73] 3.81551 3.97218 4.92435 2.89037 3.32504 3.79098 3.70130 4.57883 2.95491
#>  [82] 3.37417 4.76217 4.01458 1.36098 4.40916 2.85071 3.92197 4.09101 3.99268
#>  [91] 3.66099 3.65584 4.02535 4.21804 4.47050 3.44042 2.98062 3.99083 3.98155
#> [100] 2.95491
#> 
#> $preds
#>   [1] 3.899232 4.163350 3.871713 4.228779 4.263709 4.768865 4.194416 4.013278
#>   [9] 4.135949 3.823129 4.290694 4.197311 4.285467 4.086702 4.140828 3.859711
#>  [17] 4.353094 4.237541 4.261664 3.871822 4.680878 4.518871 4.207847 4.238189
#>  [25] 4.232188 4.139825 4.195663 4.895604 5.097433 4.016120 4.281458 4.366767
#>  [33] 4.260844 4.169632 3.910839 4.169408 4.179049 4.563083 4.137344 4.400470
#>  [41] 3.836796 3.961806 4.531759 4.680306 4.083981 4.339976 4.244241 4.498428
#>  [49] 4.692109 4.092527 4.251806 3.792355 4.253544 4.097158 4.334836 4.162409
#>  [57] 4.196109 4.097247 4.198833 3.973488 4.214164 3.818469 4.475795 4.117335
#>  [65] 4.206036 4.033762 4.311665 4.023857 4.129992 3.822489 4.070420 4.297572
#>  [73] 4.097683 4.222573 4.495485 4.224400 4.201775 4.206081 4.272772 4.224648
#>  [81] 4.285144 4.281821 4.197711 4.106202 3.772960 4.438107 4.109416 4.223305
#>  [89] 4.276146 4.098306 4.374069 4.267839 4.319815 4.166670 4.748231 3.899360
#>  [97] 4.014729 4.285117 4.251223 4.097764
#> 
#> $time
#>       train        test 
#> 0.086755037 0.002198935
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
