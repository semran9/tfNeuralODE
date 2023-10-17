
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tfNeuralODE

<!-- badges: start -->

[![R-CMD-check](https://github.com/semran9/tfNeuralODE/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/semran9/tfNeuralODE/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of tfNeuralODE is to provide an easy framework surrounding the
use of Neural Ordinary Differential Equations in R. This package does so
by building on top of R Tensorflow, to help wth auto-differentiation and
the construction of neural network models with Keras. Examples of this
package in use can be found on the project website.

## Notes

Please note that in its current state, tfNeuralODE is in not an
optimized state, with programs running quite slowly at the moment.
Improvements to speed are a priority on the development agenda.

## Installation

You can install the development version of tfNeuralODE like so:

``` r
devtools::install_github("https://github.com/semran9/tfNeuralODE.git")
```

A release version can be installed from CRAN.

``` r
install.packages("tfNeuralODE")
```
