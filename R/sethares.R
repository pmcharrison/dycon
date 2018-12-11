#' Spectral roughness (Sethares)
#'
#' Gets the roughness of a spectrum according to the model of Sethares (1993).
#' By default, the algorithm is modified according to
#' \insertCite{Sethares2005;textual}{dycon} and
#' \insertCite{Weisser2013;textual}{dycon}:
#' roughness is proportional to the minimum amplitude of each pair of partials,
#' not the product of their amplitudes.
#' This behaviour can be disabled by setting \code{min_amplitude = FALSE}.
#' @param frequency Numeric vector of frequencies.
#' @param amplitude Numeric vector of amplitudes.
#' @param ... Further parameters to be passed to \code{\link{dyad_roughness_seth}}.
#' @return Numeric vector of roughnesses.
#' @note The function assumes that any input complex tones have already
#' been expanded into their constituent pure tones.
#' @note \insertCite{Sethares2005;textual}{dycon}
#' suggests using loudnesses instead of amplitudes.
#' However, he acknowledges that loudness is difficult to calculate
#' for arbitrary timbres.
#' Furthermore, if we replace amplitude with roughness,
#' we lose the original model's invariance to multiplicative
#' scaling of the original signal.
#' In this implementation, we therefore stay with amplitude,
#' consistent with \insertCite{Sethares1993;textual}{dycon}.
#' @references
#' \insertAllCited{}
#' @export
roughness_seth <- function(frequency,
                           amplitude,
                           ...) {
  assertthat::assert_that(
    length(frequency) == length(amplitude)
  )
  n <- length(frequency)
  if (n < 2) 0 else {
    # The formula given in Sethares (1993) iterates over all pairs of [i, j],
    # but because roughness is symmetric we can save time by only considering pairs.
    # This gets rid of the 'divide by two' component in the original equation.
    df <- expand.grid(j = seq_len(n), i = seq_len(n)) %>%
      (function(df) {
        df[df$i < df$j, ]
      })
    dyad_roughness_seth(
      f1 = frequency[df$i],
      f2 = frequency[df$j],
      a1 = amplitude[df$i],
      a2 = amplitude[df$j],
      ...
    ) %>% sum
  }
}

#' Dyad roughness (Sethares)
#'
#' Gets the roughness of a dyad according to the model of Sethares (1993).
#' @param f1 Frequency of tone 1 (Hz) (numeric vector).
#' @param f2 Frequency of tone 2 (Hz) (numeric vector). Must be greater than \code{f1}.
#' @param a1 amplitude of tone 1 (numeric vector).
#' @param a2 amplitude of tone 2 (numeric vector).
#' @param a Numeric scalar parameter, optimised to 3.5 (default) in Sethares (1993).
#' @param b Numeric scalar parameter, optimised to 5.75 (default) in Sethares (1993).
#' @param s1 Numeric scalar parameter from Sethares (1993).
#' @param s2 Numeric scalar parameter from Sethares (1993).
#' @param d_star Numeric scalar parameter from Sethares (1993).
#' @return Numeric vector of roughnesses.
#' \insertRef{Sethares1993}{dycon}
dyad_roughness_seth <- function(f1, f2, a1, a2,
                                ensure_f1_is_less_than_f2 = TRUE,
                                min_amplitude = TRUE,
                                a = 3.5,
                                b = 5.75,
                                s1 = 0.021,
                                s2 = 19,
                                d_star = 0.24) {
  assertthat::assert_that(
    is.numeric(f1), is.numeric(f2),
    is.numeric(a1), is.numeric(a2),
    is.logical(min_amplitude), length(min_amplitude) == 1L,
    length(f1) == length(f2),
    length(f1) == length(a1),
    length(f1) == length(a2)
  )
  if (ensure_f1_is_less_than_f2) {
    need_reversal <- f1 > f2
    dyad_roughness_seth(
      f1 = ifelse(need_reversal, f2, f1),
      f2 = ifelse(need_reversal, f1, f2),
      a1 = ifelse(need_reversal, a2, a1),
      a2 = ifelse(need_reversal, a1, a2),
      ensure_f1_is_less_than_f2 = FALSE,
      a = a, b = b, s1 = s1, s2 = s2, d_star = d_star
    )
  } else {
    s <- d_star / (s1 * f1 + s2)
    A <- if (min_amplitude) pmin(a1, a2) else a1 * a2
    A * (
      exp(- a * s * (f2 - f1)) -
        exp(- b * s * (f2 - f1))
    )
  }
}
