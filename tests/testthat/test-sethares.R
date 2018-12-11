context("test-sethares")

test_that("Regression tests generated from Sethares's implementation", {
  # See below for MATLAB implementation of Sethares's (1993) model,
  # sourced from http://sethares.engr.wisc.edu/comprog.html.
  # To reproduce the exact results, it's necessary to adjust the parameters slightly.
  # Note that Sethares's implementation also introduces an arbitrary
  # scaling factor, which we need to compensate for in our testing.
  f <- function(frequency, amplitude, ref = TRUE) {
    x <- roughness_seth(frequency = frequency,
                        amplitude = amplitude,
                        s1 = 0.0207,
                        s2 = 18.96,
                        a = 3.51)
    if (ref) {
      x / f(frequency = c(440, 460), amplitude = c(1, 1), ref = FALSE)
    } else x
  }

  # MATLAB:
  # dissmeasure([440, 460, 480], [1, 1, 1]) / dissmeasure([440, 460], [1, 1])
  expect_equal(f(c(440, 460, 480), c(1, 1, 1)), 2.9194, tolerance = 1e-3)

  expect_equal(f(c(440, 460, 480), c(1, 2, 3)), 3.9161, tolerance = 1e-3)
  expect_equal(f(c(440, 460, 480), c(3, 2, 1)), 3.9194, tolerance = 1e-3)
  expect_equal(f(c(300, 250, 275, 425), c(1.5, 2, 9, 4)), 4.8657, tolerance = 1e-3)
})

# Sethares's MATLAB code:
# http://sethares.engr.wisc.edu/comprog.html

# function d=dissmeasure(fvec,amp)
# %
# % given a set of partials in fvec,
# % with amplitudes in amp,
# % this routine calculates the dissonance
# %
# Dstar=0.24; S1=0.0207; S2=18.96; C1=5; C2=-5;
# A1=-3.51; A2=-5.75; firstpass=1;
# N=length(fvec);
# [fvec,ind]=sort(fvec);
# ams=amp(ind);
# D=0;
# for i=2:N
# Fmin=fvec(1:N-i+1);
# S=Dstar./(S1*Fmin+S2);
# Fdif=fvec(i:N)-fvec(1:N-i+1);
# a=min(ams(i:N),ams(1:N-i+1));
# Dnew=a.*(C1*exp(A1*S.*Fdif)+C2*exp(A2*S.*Fdif));
# D=D+Dnew*ones(size(Dnew))';
# end
# d=D;
