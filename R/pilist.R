#' Get years by person for PI list
#'
#' @param pilist data frame with list of PI:s and columns of years 'X2010' etc
#' @param startyear the first publication year to consider
#' @param stopyear the last publication year to consider
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_replace
#' @export
pi_years <- function(pilist, startyear, stopyear) {

  name <- value <- Year <- NULL

  names(pilist)[1] <- "Id"

  pilist |>
    pivot_longer(cols = starts_with("X")) |>
    mutate(Year = as.integer(str_replace(name, 'X', ''))) |>
    filter(value != '') |>
    select(Id, Year)
}
