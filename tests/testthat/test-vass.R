context("test-vass")

library(magrittr)
library(tibble)

test_that("simple categorical examples", {
  expect_gt(roughness_vass(c(60, 63, 66)),
            roughness_vass(c(60, 64, 67)))
})

test_that("Vassilakis (2001, p. 208), ", {
  f <- tribble(
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
  a <- 1 / 1:6
  get_tone <- function(pc) as.numeric(f[pc + 1, ])
  get_tones <- function(pc_1, pc_2) {

  }
  roughness_vass(get_tone(0), a)
  roughness_vass(get_tone(1), a)
})
