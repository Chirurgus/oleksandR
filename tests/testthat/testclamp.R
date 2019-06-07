context("clamp")
library(oleksandR)

.test_clamp <- function(x, lwr, upr, rate) {
  clamped.x <- clamp(x, lwr= lwr, upr=upr, rate=rate)
  in.interval <- (lwr < clamped.x) & (clamped.x < upr)

  # Clamped variables should all be inside clamp interval
  expect_true(all(in.interval), paste("lwr=", lwr, ",upr=", upr, ",rate=", rate, sep=""));

  unclamped.x <- unclamp(x, lwr= lwr, upr=upr, rate=rate);
  # Unclamped.x should be equal to x
  expect_identical(x, unclamped.x)
}

.x <- -100:100
.lower.bound <- c(-1000, -500, -100, -50, -10, -1, -0.5, 0, 0.5, 1, 10, 50, 100, 500, 1000);
.upper.bound <- .lower.bound;
.interval.length <- c(1, 10, 100, 1000);
.rate = 0.01

test_that("clamp on bounded interval clamps", {
  for (lwr in .lower.bound) {
    for (len in .interval.length) {
      upr <- lwr + len;
      .test_clamp(x, lwr=lwr, upr=upr, rate=.rate)
    }
  }
});

test_that("clamp on lower unbounded interval clamps", {
  for (upr in .upper.bound) {
    .test_clamp(x, lwr=-Inf, upr=upr, rate=.rate)
  }
});

test_that("clamp on upper unbounded interval clamps", {
  for (lwr in .lower.bound) {
    .test_clamp(x, lwr=lwr, upr=Inf, rate=.rate)
  }
});
