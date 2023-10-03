#' Make SQL WHERE clause friendly list out of R vector of strings
#'
#' @param items vector of items to list (e.g. UT numbers, KTH-ids ...)
#' @returns character
#' @export
sql_list <- function(items) {
  paste(paste0("'", items, "'"), collapse = ",")
}

#' Get intervals for given years and width
#'
#' @param years the years to create intervals from
#' @param width the width of the intervals to create (default 3)
#' @param from_highest if true, start from the highest year, else from lowest (default TRUE)
#' @importFrom dplyr mutate
#' @export
intervalify <- function(years, width = 3, from_highest = TRUE) {

  tmp <- data.frame(year = years)

  minyear = min(tmp)
  maxyear = max(tmp)

  if(from_highest) {
    tmp <- tmp |>
      mutate(upper = year + (maxyear - year) %% width,
             lower = upper - width + 1)
  } else {
    tmp <- tmp |>
      mutate(lower = year - (year - minyear) %% width,
             upper = lower + width - 1)
  }

  tmp |>
    mutate(interval = paste(ifelse(minyear < lower, lower, minyear), '-',
                            ifelse(maxyear > upper, upper, maxyear))) |>
    select(year, interval)
}
