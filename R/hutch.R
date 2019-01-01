#' Spectral roughness (Hutchinson & Knopoff)
#'
#' Gets the roughness of a sonority according to the model of
#' \insertCite{Hutchinson1978;textual}{dycon}.
#' @param x Object to analyse, which is coerced to the class
#' \code{\link[hrep]{sparse_fr_spectrum}}.
#' * Numeric vectors will be treated as vectors of MIDI note numbers,
#' and expanded into their implied harmonics.
#' * Two-element lists will be treated as finalised spectra,
#' with the first element being a numeric vector of frequencies,
#' and the second element being a numeric vector of amplitudes.
#' @param a Parameter passed to \code{\link{hutch_g}()}.
#' @param b Parameter passed to \code{\link{hutch_g}()}.
#' @param cbw_cut_off Parameter passed to \code{\link{hutch_g}()}.
#' @return Numeric scalar, identifying the roughness of the spectrum.
#' @references
#' \insertAllCited{}
#' @rdname roughness_hutch
#' @md
#' @export
roughness_hutch <- function(x, cbw_cut_off = 1.2, a = 0.25, b = 2, ...) {
  UseMethod("roughness_hutch")
}

#' @param ... Further arguments to pass to \code{\link[hrep]{sparse_fr_spectrum}}.
#' @rdname roughness_hutch
#' @export
roughness_hutch.default <- function(x, cbw_cut_off = 1.2, a = 0.25, b = 2, ...) {
  x <- hrep::sparse_fr_spectrum(x, ...)
  roughness_hutch(x, cbw_cut_off = cbw_cut_off, a = a, b = b)
}

#' @rdname roughness_hutch
#' @export
roughness_hutch.sparse_fr_spectrum <- function(x, cbw_cut_off = 1.2, a = 0.25, b = 2) {
  frequency <- hrep::freq(x)
  amplitude <- hrep::amp(x)
  n <- length(frequency)
  if (n < 2) 0 else {
    # Compute denominator
    denominator <- sum(amplitude ^ 2)
    # Compute numerator
    df <- expand.grid(j = seq_len(n), i = seq_len(n)) %>%
      (function(df) {
        df[df$i < df$j, ]
      })
    df$a_i_a_j <- amplitude[df$i] * amplitude[df$j]
    df$g_ij <- hutch_g(
      y = hutch_y(f1 = frequency[df$i],
                  f2 = frequency[df$j]),
      cbw_cut_off = cbw_cut_off,
      a = a,
      b = b
    )
    numerator <- sum(df$a_i_a_j * df$g_ij)
    numerator / denominator
  }
}

#' Critical bandwidth (Hutchison & Knopoff)
#'
#' Calculates the critical bandwidth given pairs of frequencies
#' \code{f1} and \code{f2},
#' according to the model of \insertCite{Hutchinson1978;textual}{dycon}.
#' @param f1 (Numeric vector) Frequency 1, Hz
#' @param f2 (Numeric vector) Frequency 2, Hz
#' @return (Numeric vector) Critical bandwidths.
#' @references
#'   \insertAllCited{}
#' @export
hutch_cbw <- function(f1, f2) {
  mean_f <- (f1 + f2) / 2
  1.72 * (mean_f ^ 0.65)
}

#' Critical bandwidth distance (Hutchison & Knopoff)
#'
#' Calculates the distance between pairs of frequencies in units of
#' critical bandwidths, according to the model of
#' \insertCite{Hutchinson1978;textual}{dycon}.
#' @param f1 (Numeric vector) Frequency 1, Hz
#' @param f2 (Numeric vector) Frequency 2, Hz
#' @return (Numeric vector) Unsigned distances in frequency bandwidths.
#' @references
#'   \insertAllCited{}
#' @export
hutch_y <- function(f1, f2) {
  abs_freq_diff <- abs(f1 - f2)
  critical_bandwidth <- hutch_cbw(f1, f2)
  abs_freq_diff / critical_bandwidth
}

#' Dissonance factor (Hutchinson & Knopoff)
#'
#' Computes dissonance factors given frequency distances in units of
#' critical bandwidths, after Mashinter's implementation of
#' Hutchinson & Knopoff's model
#' \insertCite{Mashinter2006,Hutchinson1978}{dycon}.
#' This function corresponds to an approximation of the
#' look-up table in \insertCite{Plomp1965;textual}{dycon}.
#' @param y (Numeric vector) Frequency distance in units of critical bandwidths.
#' @param cbw_cut_off (Numeric scalar)
#' If not \code{NULL}, then should be a number
#' corresponding to the variable CBWcutoff in Mashinter's own implementation.
#' If \code{y >= cbw_cut_off}, then the dissonance factor will be approximated as 0.
#' Setting \code{cbw_cut_off} to 1.2 is necessary for replicating Mashinter's results.
#' A cut-off of 1.2 was also used by \insertCite{Bigand1996;textual}{dycon}.
#' @param a (Numeric scalar, default = 0.25)
#' Parameter from \insertCite{Mashinter2006;textual}{dycon}.
#' @param b (Numeric scalar, default = 2)
#' Parameter from \insertCite{Mashinter2006;textual}{dycon}.
#' @return (Numeric vector) Dissonance factors.
#' @references
#'   \insertAllCited{}
hutch_g <- function(y, cbw_cut_off = 1.2, a = 0.25, b = 2) {
  assertthat::assert_that(
    is.numeric(y), all(y >= 0),
    is.null(cbw_cut_off) || (assertthat::is.scalar(cbw_cut_off) && is.numeric(cbw_cut_off)),
    is.numeric(a), is.numeric(b),
    assertthat::is.scalar(a), assertthat::is.scalar(b)
  )
  res <- ((y / a) * exp(1 - (y / a))) ^ b
  if (!is.null(cbw_cut_off)) {
    res[y > cbw_cut_off] <- 0
  }
  res
}
