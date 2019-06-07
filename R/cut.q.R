# Created by Oleksandr Sorochynskyi
# On 01/06/19

#' Cut variable into quantile
#'
#' Transform a quantitative variable into a factor by cutting it into
#' intervals. The breaks in those intervals are chosen by quantiles, at
#' regular probabilies, or at 'probs', if provided.
#' 
#' @param x Vector of quantitative data to transform
#' @param num.q Numer of categories to cut the variable into. Ignored if
#'              probs is provided.
#' @param probs The probabilites at  wchich to cut the data into quantiles.
#' @value A factor with either 'num.q' or 'length(probs) - 1' levels of
#'        intervals generted by cut, with breaks at quantiles. 
#' @keywords quantile, ecdf
#' @export
#' @references
#'
#' @examples
#' x <- rnorm(100)
#' categorical <- cut.q(x, num.q=2) # catgorical is now a factor with 2 levels:
#'                              #  "[min(x), median(x)]", "[median(x), max(x)]"
cut.q <- function(x, num.q = 4, probs) {
  if (missing(probs)) {
    probs <- seq(0, 1, length.out=n+1)
  }
  cuts <- quantile(x, probs= probs, type=8)
  cut(x, unique(cuts), include.lowest=TRUE)
}
