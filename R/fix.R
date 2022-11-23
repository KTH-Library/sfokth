#' Make SQL WHERE clause friendly list of UT numbers
#'
#' @param uts vector of UT numbers (15 chars each)
#' @returns character
#' @export
utlist_sql <- function(uts) {
  paste(paste0("'", uts, "'"), collapse = ",")
}
