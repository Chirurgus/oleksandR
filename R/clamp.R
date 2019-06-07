# Created by Oleksandr SOROCHYNSKYI
# On 23 02 2019

.bounded.clamp <- function(x, lwr=0, upr=1, rate=1, middle=0) {
  stopifnot(rate != 0,
            is.finite(lwr),
            is.finite(upr),
            lwr < upr);

  # (upr-lwr) / (1 + exp(-rate*x)) + lwr;
  #( -rate*(x - (a+b)/2) )/( (x-lwr)*(x-upr) );
  (upr-lwr) * (1/pi * atan( rate*(x-middle) ) + .5) + lwr;
}

.inv.bounded.clamp <- function(x, lwr=0, upr=1, rate=1, middle=0) {
  stopifnot(rate != 0,
            is.finite(lwr),
            is.finite(upr),
            lwr < upr);

  #-1/rate * log(upr-x) - log(x-lwr)
  #x <- -1/rate * x;
  #(-1 * (2*lwr*upr*x + 1) + 2 * sqrt( ((lwr*upr)^2 - lwr*upr)*x^2 + (lwr*upr - (lwr+upr)/2)*x + 1/4 ) )/(2*x);
  x <- (x-lwr)/(upr-lwr);
  1/rate * tan( pi*(x-.5) ) + middle;
}

.unbounded.clamp <- function(x, lwr=0, rate=1, middle=0) {
  stopifnot(rate != 0,
            is.finite(lwr))

  exp(rate*(x - middle)) + lwr
}

.inv.unbounded.clamp <- function(x, lwr=0, rate=1, middle=0) {
  stopifnot(rate != 0,
            is.finite(lwr))

  1/rate * log(x - lwr) + middle
}

clamp <- function(x, lwr, upr, rate= 0.01) {
  # If bounded
  if (is.finite(upr) && is.finite(lwr)) {
    return(.bounded.clamp(x, lwr=lwr, upr=upr, rate= rate));
  }
  # if bounded on left
  if (is.finite(lwr)) {
    return(.unbounded.clamp(x, lwr= lwr, rate= rate));
  }
  # if bounded on right
  if (is.finite(upr)) {
    return(-.unbounded.clamp(x, lwr= -upr, rate= rate));
  }
  # if we got here, both bounds are Inf
  return(x);
}

unclamp <- function(x, lwr, upr, rate= 0.01) {
  # If bounded
  if (is.finite(upr) && is.finite(lwr)) {
    return(.inv.bounded.clamp(x, lwr=lwr, upr=upr, rate= rate));
  }
  # if bounded on left
  if (is.finite(lwr)) {
    return(.inv.unbounded.clamp(x, lwr=lwr, rate= rate));
  }
  # if bounded on right
  if (is.finite(upr)) {
    return(.inv.unbounded.clamp(-x, lwr= -upr, rate= rate));
  }
  # if we got here, both bounds are Inf
  return(x);
}