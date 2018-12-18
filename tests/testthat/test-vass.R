context("test-vass")

library(magrittr)
library(tibble)

test_that("Comparing model outputs to Vassilakis (2001, p. 210)", {
  # This table comes from p. 208
  .f <- tribble(
    ~ f1, ~ f2, ~ f3, ~ f4, ~ f5, ~ f6,
    262,  526,  790,  1049, 1318, 1573,
    277,  554,  837,  1118, 1398, 1677,
    294,  590,  886,  1180, 1473, 1772,
    311,  624,  932,  1244, 1569, 1873,
    330,  663,  995,  1323, 1654, 1994,
    349,  701,  1053, 1408, 1751, 2107,
    370,  741,  1118, 1482, 1852, 2235,
    392,  783,  1179, 1570, 1973, 2373,
    415,  834,  1250, 1670, 2093, 2499,
    440,  884,  1329, 1768, 2200, 2666,
    466,  937,  1400, 1874, 2345, 2799,
    494,  990,  1484, 1985, 2476, 2973,
    524,  1052, 1573, 2110, 2634, 3154
  )
  .a <- 1 / 1:6
  get_tone <- function(pc) {
    hrep::.fr_sparse_spectrum(frequency = as.numeric(.f[pc + 1, ]),
                              amplitude = .a)
  }
  get_dyad <- function(pc_1, pc_2) {
    c(get_tone(pc_1),
      get_tone(pc_2))
  }
  get_dyad_roughness <- function(pc_1, pc_2) {
    get_dyad(pc_1, pc_2) %>%
      {roughness_vass(list(hrep::freq(.), hrep::amp(.)))}
  }

  # These results come from p. 210
  res <- tibble(int = 2:12,
                old = 40.383 /
                  c(27.617, 18.117, 16.002,
                    11.446, 12.826, 6.17877,
                    10.103, 5.782, 6.214,
                    6.996, 1.589))
  res$new <- vapply(res$int, function(x) {
    get_dyad_roughness(0, 1) / get_dyad_roughness(0, x)
  }, numeric(1))
  res

  expect_gt(cor(res$old, res$new), 0.998)
})
