#' Spectral roughness (Vassilakis)
#'
#' Gets the roughness of a sonority according to the model of
#' \insertCite{Vassilakis2001;textual}{dycon}
#' \insertCite{Villegas2010;textual}{dycon}
#' @param x Object to analyse, which is coerced to the class
#' \code{\link[hrep]{sparse_fr_spectrum}}.
#' * Numeric vectors will be treated as vectors of MIDI note numbers,
#' and expanded into their implied harmonics.
#' * Two-element lists will be treated as finalised spectra,
#' with the first element being a numeric vector of frequencies,
#' and the second element being a numeric vector of amplitudes.
#' @return Estimated roughness, as a numeric scalar.
#' @references
#' \insertAllCited{}
#' @rdname roughness_vass
#' @md
#' @export
roughness_vass <- function(x, ...) {
  UseMethod("roughness_vass")
}

#' @param ... Further arguments to pass to \code{\link[hrep]{sparse_fr_spectrum}}.
#' @rdname roughness_vass
#' @export
roughness_vass.default <- function(x, ...) {
  x <- hrep::sparse_fr_spectrum(x, ...)
  roughness_vass(x)
}

#' @rdname roughness_vass
#' @export
roughness_vass.sparse_fr_spectrum <- function(x, ...) {
  frequency <- hrep::freq(x)
  amplitude <- hrep::amp(x)
  n <- length(frequency)
  if (n < 2) 0 else {
    # Roughness is computed by summing over all dyadic roughnesses.
    # Noting that the formula for dyadic roughness is symmetric,
    # we can instead only compute dyadic roughnesses for pairs
    # where i < j, and then double the resulting sum.
    df <- expand.grid(j = seq_len(n), i = seq_len(n)) %>%
      (function(df) {
        df[df$i < df$j, ]
      })
    dyad_roughness_vass(
      f1 = frequency[df$i],
      f2 = frequency[df$j],
      a1 = amplitude[df$i],
      a2 = amplitude[df$j]
    ) %>% sum %>% magrittr::multiply_by(2)
  }
}

#' Dyad roughness (Vassilakis)
#'
#' Gets the roughness of a dyad according to the model of
#' \insertCite{Vassilakis2001;textual}{dycon}
#' \insertCite{Villegas2010;textual}{dycon}
#' @param f1 Frequency of tone 1 (Hz) (numeric vector).
#' @param f2 Frequency of tone 2 (Hz) (numeric vector).
#' @param a1 amplitude of tone 1 (numeric vector).
#' @param a2 amplitude of tone 2 (numeric vector).
#' @return Numeric vector of roughnesses.
#' @note The function is vectorised over all inputs.
#' @references
#' \insertAllCited{}
dyad_roughness_vass <- function(f1, f2, a1, a2) {
  assertthat::assert_that(
    is.numeric(f1), is.numeric(f2),
    is.numeric(a1), is.numeric(a2),
    length(f1) == length(f2),
    length(f1) == length(a1),
    length(f1) == length(a2)
  )
  ((a1 * a2) ^ 0.1) *
    0.5 *
    (((2 * pmin(a1, a2)) / (a1 + a2)) ^ 3.11) *
    (exp(- 3.5 * f_vass(f1, f2)) - exp(- 5.75 * f_vass(f1, f2)))
}

f_vass <- function(f1, f2) {
  s_vass(pmin(f1, f2)) *
    abs(f1 - f2)
}

s_vass <- function(f) {
  0.24 / (0.0207 * f + 18.96)
}
