#' Get connection to Bibmet database server
#'
#' @param db the database to connect to, default BIBMET
#' @param host the host database server, default from Sys.getenv
#' @param user the database user to connect with, default from Sys.getenv
#' @param pass the password for the user to connect, default from Sys.getenv
#' @import odbc
#' @export
con_bibmet <- function(db = "BIBMET",
                       host = Sys.getenv("DBHOST"),
                       user = Sys.getenv("DBUSER"),
                       pass = Sys.getenv("DBPASS")){

  sqldriver <- ifelse(Sys.getenv("SQL_SERVER_DRIVER") == "",
                      "ODBC Driver 17 for SQL Server",
                      Sys.getenv("SQL_SERVER_DRIVER"))

  dbConnect(
    odbc(),
    driver = sqldriver,
    Port = 1433,
    server = host,
    database = db,
    UID = user,
    PWD = pass,
    Encrypt = "No")
}
