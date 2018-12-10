context("hutch")

library(magrittr)

test_that(
  "hutch_g", {
    expect_equal(
      hutch_g(
        1, cbw_cut_off = 1.2
      ) %>% round(digits = 4),
      0.0397
    )
    expect_equal(
      hutch_g(
        0.5, cbw_cut_off = 1.2
      ) %>% round(digits = 4),
      0.5413
    )
    expect_equal(
      hutch_g(
        1.5, cbw_cut_off = NULL
      ) %>% round(digits = 4),
      0.0016
    )
    expect_equal(
      hutch_g(
        1.5, cbw_cut_off = 1.2
      ) %>% round(digits = 4),
      0
    )
  }
)

test_that(
  "hutch_cbw", {
    expect_equal(
      hutch_cbw(400, 440) %>% round(digits = 3),
      87.225
    )
    expect_equal(
      hutch_cbw(400, 380) %>% round(digits = 3),
      83.123
    )
  }
)

test_that("get_roughness_hutch", {
  test_midi <- function(midi, expect, num_harmonics, tolerance = 1e-3) {
    midi %>%
      (hrep::pi_chord) %>%
      {hrep::fr_sparse_spectrum(., num_harmonics = num_harmonics)} %>%
      {roughness_hutch(frequency = hrep::freq(.),
                       amplitude = hrep::amp(.))} %>%
      expect_equal(expect, tolerance = tolerance)
  }
  test_midi("60 61", 0.499, num_harmonics = 1)
  test_midi("69 70", 0.491, num_harmonics = 1)
  test_midi("60 61", 0.484, num_harmonics = 11)
  test_midi("60 64 67", 0.120, num_harmonics = 11)
  test_midi("60 63 67", 0.130, num_harmonics = 11)
  test_midi("60 63 67", 0.130, num_harmonics = 11)
})
