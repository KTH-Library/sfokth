#' List full name and address for list of UT:s, a last name and an org_id
#'
#' @param con a connection to Bibmet
#' @param uts list of UT numbers
#' @param lastname Last name of the person to check
#' @param org_id Unified_org_id of the org of interest (default 8 = KTH)
#'
#' @importFrom DBI dbGetQuery
#'
#' @export

name_address_check <- function(con, uts, lastname, org_id = 8){

  uts_string <- paste0("'", uts, "'", collapse = ", ")

  q <- sprintf(
  "SELECT bra.UT, auth.Full_name, bra.Full_address
  FROM BIBMET.dbo.BestResAddrAuthorid bra
  INNER JOIN BIBMET.dbo.Author auth ON (auth.Author_id = bra.Author_id)
  WHERE bra.UT IN (%s)
    AND bra.Unified_org_id = %d
    AND auth.Last_name = '%s'",
  uts_string,
  org_id,
  lastname)

  dbGetQuery(con, q)
}
