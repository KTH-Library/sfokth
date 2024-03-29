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

  org_string <- if_else(is.null(org_id), "", paste("AND bra.Unified_org_id = ", org_id))

  q <- sprintf(
  "SELECT bra.UT, auth.Full_name, bra.Full_address
  FROM BIBMET.dbo.BestResAddrAuthorid bra
  INNER JOIN BIBMET.dbo.Author auth ON (auth.Author_id = bra.Author_id AND auth.Last_name = '%s')
  WHERE bra.UT IN (%s)
  %s
    ",
  lastname,
  sql_list(uts),
  org_string
  )

  dbGetQuery(con, q)
}

#' Get PMID from Bibmet for list of UT
#'
#' @param uts list of UT numbers
#' @param con a connection to Bibmet
#'
#' @import dplyr
#' @importFrom DBI dbGetQuery
#'
#' @export
pmid_from_ut <- function(uts, con) {

  if(missing(con)){
    con <- con_bibmet()
    on.exit(dbDisconnect(con))
  }

  q <- sprintf("SELECT doc.UT, sd.PMID
FROM BIBMET.dbo.Document doc
INNER JOIN BIBMET.dbo.Source_Doc sd ON (sd.Doc_id = doc.Doc_id)
WHERE UT IN (%s) AND sd.PMID IS NOT NULL", sql_list(uts))

  dbGetQuery(con, q)
}
