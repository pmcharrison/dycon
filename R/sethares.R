#' Spectral roughness (Sethares)
#'
#' Gets the roughness of a spectrum according to the model of Sethares (1993).
#' @param frequency Numeric vector of frequencies
#' @param amplitude Numeric vector of amplitudes
#' @return Numeric vector of roughnesses
#' @note The function assumes that any input complex tones have already
#' been expanded into their constituent pure tones.
#' \insertRef{Sethares1993}{dycon}
get_roughness_seth <- function(frequency, amplitude, seth_params = get_seth_params()) {
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
      seth_params = seth_params
    ) %>% sum
  }
}

#' Dyad roughness (Sethares)
#'
#' Gets the roughness of a dyad according to the model of Sethares (1993).
#' @param f1 Frequency of tone 1 (Hz) (numeric vector)
#' @param f2 Frequency of tone 2 (Hz) (numeric vector). Must be greater than \code{f1}.
#' @param a1 amplitude of tone 1 (numeric vector)
#' @param a2 amplitude of tone 2 (numeric vector)
#' @param a Numeric scalar parameter, optimised to 3.5 (default) in Sethares (1993).
#' @param b Numeric scalar parameter, optimised to 5.75 (default) in Sethares (1993).
#' @return Numeric vector of roughnesses
#' \insertRef{Sethares1993}{dycon}
dyad_roughness_seth <- function(f1, f2, a1, a2,
                                ensure_f1_is_less_than_f2 = TRUE,
                                seth_params = get_seth_params()) {
  assertthat::assert_that(
    is.numeric(f1), is.numeric(f2),
    is.numeric(a1), is.numeric(a2),
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
      seth_params = seth_params
    )
  } else {
    s <- s_seth(f1 = f1, seth_params = seth_params)
    a1 * a2 * (
      exp(- seth_params$a * s * (f2 - f1)) -
        exp(- seth_params$b * s * (f2 - f1))
    )
  }
}

s_seth <- function(f1, seth_params) {
  seth_params$d_star /
    (seth_params$s1 * f1 + seth_params$s2)
}
