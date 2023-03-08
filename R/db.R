#' Get connection to Bibmet database server
#'
#' @param db the database to connect to, default BIBMET
#' @param host the host database server, default from Sys.getenv
#' @param user the database user to connect with, default from Sys.getenv
#' @param pass the password for the user to connect, default from Sys.getenv
#' @import odbc
#' @export
con_bibmet <- function(db = "BIBMET",
                       host = Sys.getenv("BIBMET_HOST"),
                       user = Sys.getenv("BIBMET_USER"),
                       pass = Sys.getenv("BIBMET_PASS")){

  dbConnect(
    odbc(),
    driver = "ODBC Driver 17 for SQL Server",
    Port = 1433,
    server = host,
    database = db,
    UID = user,
    PWD = pass,
    encoding = "Windows-1252")
}

#' Make SQL WHERE clause friendly list out of R vector of strings
#'
#' @param items vector of items to list (e.g. UT numbers, KTH-ids ...)
#' @returns character
#' @export
sql_list <- function(items) {
  paste(paste0("'", items, "'"), collapse = ",")
}
