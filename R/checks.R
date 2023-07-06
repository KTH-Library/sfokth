#' List full name and address for list of UT:s, a last name and an org_id
#'
#' @param con a connection to Bibmet
#' @param uts list of UT numbers
#' @param lastname Last name of the person to check
#' @param org_id Unified_org_id of the org of interest, if null no limitation on org
#'
#' @import dplyr
#' @importFrom DBI dbGetQuery
#'
#' @export

name_address_check <- function(con, uts, lastname, org_id = NULL){

  uts_string <- paste0("'", uts, "'", collapse = ", ")

  org_string <- if_else(is.null(org_id), "", paste("AND bra.Unified_org_id = ", org_id))

  q <- sprintf(
  "SELECT bra.UT, auth.Full_name, bra.Full_address
  FROM BIBMET.dbo.BestResAddrAuthorid bra
  INNER JOIN BIBMET.dbo.Author auth ON (auth.Author_id = bra.Author_id AND auth.Last_name = '%s')
  WHERE bra.UT IN (%s)
  %s
    ",
  lastname,
  uts_string,
  org_string
  )

  dbGetQuery(con, q)
}
