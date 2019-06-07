# Created by Oleksandr Sorochynskyi
# On 05 March 2019

#' Inverse Cumilative Density Function
#'
#' Generate the inverse cdf, just as as 'cdf' function generates the 
#' cumilative density function.
#' 
#' 
#' @param x Vector of ordered numerical data to generate the inv.cdf from
#' @keywords ecdf, cdf
#' @export
#' @references
#'
#' @examples
#' x <- rnorm(100)
#' F <- ecdf(x)
#' F.inf <- inverse.ecdf(x)
#' all(x == F.inv(F(x)))# == TRUE
inverse.ecdf <- function(x) {
  # Break points
  u <- unique(c(0,cumsum(table(x)/length(x))))
  x <- sort(unique(x));

  function(p) {
    index <- findInterval(p, u,
                          left.open= TRUE,
                          rightmost.closed= TRUE,
                          all.inside= TRUE)
    x[index]
  }
}

#' Quantile function with confidence intervals
#'
#' Calculate quantiles, together with binomial-based confidence intervals.
#' 
#' 
#' @param x Vectored of ordered numerical data to calculate quntiles of
#' @param alpha The probabilites at which to calculate the quantiles
#' @param beta
#' @param 
#' @keywords ecdf, cdf
#' @export
#' @references
inverse.ecdf.conf.int <- function(x, alpha, beta) {
  q <- quantile(x, probs= alpha, type=1);
  k.plus <- qbinom(1-beta/2, size= length(x), prob= alpha);
  k.moins <- qbinom(beta/2, size= length(x), prob= alpha);
  ret <- cbind(q, sort(x)[k.moins],sort(x)[k.plus]);
  names(ret) <- c("quantile", "conf.inf", "conf.sup")
}
