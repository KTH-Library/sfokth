#' Bootstrap to imitate sampling distribution for a mean
#'
#' @param x a vector with the sample to bootstrap over
#' @param samples the number of samples to generate, default 1000
#' @returns a vector of sample means
#' @export
bootstrap <- function(x, samples = 1000) {
  valid_x <- x[!is.na(x)]
  n <- length(valid_x)
  samps <- replicate(samples, sample(valid_x, n, replace = TRUE))
  return(colMeans(samps))
}

#' Bootstrap by publication year
#'
#' Run \code{\link{bootstrap}} for an indicator for each publication year
#' and keep the resulting samples in a data frame with one column per year
#'
#' @param data a data frame including a year column and an indicator column
#' @param indicator a string, the name of the indicator column
#' @param yearcolumn a string, the name of the year column (default "Publication_Year")
#' @param samples the number of samples to generate per year, default 1000
#' @import dplyr
#' @export
bootstrap_by_year <- function(data,
                              indicator,
                              yearcolumn = "Publication_Year",
                              samples = 1000) {

  data <- data |>
    rename(year = !!yearcolumn,
           indic = !!indicator) |>
    filter(!is.na(indic))

  firstyear = min(data$year)
  lastyear = max(data$year)

  sapply(as.character(firstyear:lastyear),
         function(y) {
           values <- data  |>
             filter(year == y) |>
             pull(indic)
           bootstrap(values, samples)
         },
         USE.NAMES = TRUE) |> as.data.frame()
}

#' Stability interval from bootstrapping
#'
#' @param x a vector of sample means
#' @param lower the lower percentile to return, default 0.05
#' @param upper the upper percentile to return, default 0.95
#' @returns a list of two quantiles
#' @export
stability_interval <- function(x, lower = 0.05, upper = 0.95){
  list(lower = quantile(x, lower), upper = quantile(x, upper))
}

#' Stability interval per year
#'
#' @param data a matrix with bootstrap samples
#' @param lower the lower percentile to return, default 0.05
#' @param upper the upper percentile to return, default 0.95
#' @returns a list of two quantiles
#' @export
stability_by_year <- function(data,
                              lower = .05,
                              upper = .95) {

  df <- sapply(colnames(data), \(y) stability_interval(data[, y], lower, upper)) |>
    t() |>
    as.data.frame() |>
    mutate(interval = sprintf("%.2f - %.2f", lower, upper))

  df$year <- row.names(df)
  row.names(df) <- NULL

  df |> select(year, interval)
}
