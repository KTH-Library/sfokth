#' Bootstrap to imitate sampling distribution for a mean
#'
#' @param x a vector with the sample to bootstrap over
#' @param samples the number of samples to use, default 1000
#' @returns a vector of sample means
#' @export
bootstrap <- function(x, samples = 1000) {
  valid_x <- x[!is.na(x)]
  n <- length(valid_x)
  samps <- replicate(samples, sample(valid_x, n, replace = TRUE))
  return(colMeans(samps))
}

#' Stability interval from bootstrapping
#'
#' @param x a vector of sample means
#' @param lower the lower percentile to return, default 0.05
#' @param upper the upper percentile to return, default 0.95
#' @returns a list of two quantiles
#' @export
stability_interval <- function(x, lower = 0.05, upper = 0.95){
  list(quantile(x, lower), quantile(x, upper))
}
