
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dycon: Dyadic Models of Consonance

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

`dycon` is an R package that implements three dyadic models of
consonance:

  - Hutchinson & Knopoff (1978)
  - Sethares (1993)
  - Vassilakis (2001)

## Installation

You can install the current version of `dycon` from Github as follows:

``` r
if (!require(devtools)) install.packages("devtools")
devtools::install_github("dycon")
```

## Usage

By default, input sonorities are interpreted as vectors of MIDI note
numbers. These notes are expanded into their implied harmonics using the
`hrep` package (see `?hrep::sparse_fr_spectrum`).

``` r
library(dycon)

# Major triad
roughness_hutch(c(60, 64, 67)) 
#> [1] 0.1202426

# Minor triad
roughness_hutch(c(60, 63, 67)) 
#> [1] 0.130083

# Diminished triad
roughness_hutch(c(60, 63, 66)) 
#> [1] 0.2005575

# Sethares, major triad minus minor triad
roughness_seth(c(60, 64, 67)) - 
  roughness_seth(c(60, 63, 67))
#> [1] -0.01876388

# Vassilakis, major triad minus minor triad
roughness_vass(c(60, 64, 67)) - 
  roughness_vass(c(60, 63, 67))
#> [1] -0.0423007
```

Alternatively, it is possible to provide a custom input frequency
spectrum as a list of two numeric vectors: frequency and amplitude.

``` r
freq <- c(440, 480, 520)
amp <- c(1, 1, 2)
roughness_hutch(list(freq, amp))
#> [1] 0.3990136
```

These representation formats can be formalised using classes from the
`hrep` package, in particular those created by `hrep::pi_chord()` and
`hrep::sparse_fr_spectrum()`.

## References

Hutchinson, W., & Knopoff, L. (1978). The acoustic component of Western
consonance. Journal of New Music Research, 7(1), 1–29.
<https://doi.org/10.1080/09298217808570246>

Sethares, W. A. (1993). Local consonance and the relationship between
timbre and scale. The Journal of the Acoustical Society of America,
94(3), 1218–1228.

Vassilakis, P. N. (2001). Perceptual and physical properties of
amplitude fluctuation and their musical significance. University of
California, Los Angeles, CA.
