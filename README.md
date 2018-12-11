
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dycon: Dyadic Models of Consonance

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

`dycon` is an R package that implements three dyadic models of
consonance:

  - Hutchinson & Knopoff (1978)
  - Sethares (1993)
  - Vassilakis (2001)

## Installation

You can install the current version of dycon from Github as follows:

``` r
if (!require(devtools)) install.packages("devtools")
devtools::install_github("dycon")
```

## Usage

These functions take two vectors as input:

  - a vector of frequencies, each corresponding to a spectral component;
  - a vector of amplitudes, each corresponding to a spectral component.

For analysing musical chords, it’s necessary to expand the notes of the
musical chord into their implied spectral components. The `incon`
package provides a useful wrapper for these functions that automatically
applies this expansion.

For analysing real audio, it’s necessary to compute the acoustic
spectrum (using e.g. a Fourier Transform) and then identify the spectral
components using a peak-picking algorithm. See the MIR Toolbox or the
Essentia library for such implementations.

``` r
library(dycon)

f <- c(440, 460, 520) # frequencies
a <- c(1, 1.5, 1) # amplitudes

roughness_hutch(f, a)
#> [1] 0.4819797
roughness_seth(f, a)
#> [1] 0.3645045
roughness_vass(f, a)
#> [1] 0.2232312
```
