
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
#>   [1] 2.73437 3.45632 2.21920 4.58190 4.03247 3.21084 3.81551 2.70805 3.45632
#>  [10] 4.27667 4.04655 4.43438 3.95316 5.11500 4.54223 3.19458 1.79176 4.10759
#>  [19] 3.57235 1.72277 3.35341 2.99072 4.23700 4.58190 3.01062 4.29729 3.68888
#>  [28] 3.25424 3.61092 3.45947 3.46574 2.83321 4.22244 4.02356 3.16969 3.87536
#>  [37] 4.19117 4.39198 3.79549 3.07269 4.31080 3.83945 3.21487 4.51196 4.07073
#>  [46] 3.05871 4.28496 4.38701 4.13357 2.71469 3.34990 4.20767 4.18662 4.14472
#>  [55] 5.02651 1.70475 2.87920 3.72086 4.90602 4.31348 2.36085 4.17131 2.42480
#>  [64] 4.31080 4.31749 4.01818 3.20680 3.69883 4.92435 3.97406 1.74047 3.83945
#>  [73] 4.75875 4.57883 4.88129 4.34251 3.14845 3.37417 3.72086 4.02892 3.88156
#>  [82] 4.01096 3.99452 3.91202 2.63906 3.99268 2.82731 4.94876 3.74242 3.65584
#>  [91] 4.02535 3.94352 3.25810 3.39786 3.65066 4.06732 3.98155 3.86073 2.94444
#> [100] 4.03247
#> 
#> $preds
#>   [1] 3.922813 4.190429 4.239620 4.144163 4.014591 4.054470 4.061478 3.906379
#>   [9] 3.819582 4.104140 4.311373 4.238539 4.233750 4.935888 4.552957 3.930067
#>  [17] 4.008838 4.380265 4.086124 3.932400 3.873571 3.857566 4.174876 4.133500
#>  [25] 4.258648 4.195934 3.879132 3.824203 4.115803 4.244298 3.844401 3.918823
#>  [33] 4.225871 4.334474 4.291545 3.905314 4.206137 4.561018 4.235489 4.091348
#>  [41] 4.322418 3.763173 4.071218 4.443281 4.792434 3.933431 4.328525 4.487648
#>  [49] 4.239257 4.239121 4.164370 4.231658 4.243369 4.301308 4.521836 3.892262
#>  [57] 4.003306 3.884422 4.487023 4.166811 4.140322 4.026401 4.020616 4.422408
#>  [65] 4.253711 4.479919 4.064075 4.130778 4.371256 4.264965 3.842689 4.418734
#>  [73] 4.207073 4.206431 4.210759 4.334798 4.398820 4.258324 4.271913 4.039575
#>  [81] 4.277452 4.172477 4.244800 4.170662 3.826421 4.238345 3.796037 4.225103
#>  [89] 4.132465 4.262747 4.154908 4.026832 3.986108 3.897809 4.209404 4.235796
#>  [97] 4.237741 3.770255 3.847173 4.109945
#> 
#> $time
#>       train        test 
#> 0.076480865 0.002153873
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
