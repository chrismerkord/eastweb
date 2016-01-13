#' Get information on all projects in an EASTWeb database
#'
#' @param db_name Name of the database.
#' @param user Username
#' @param password Password
#' @param host Host name. Defaults to "localhost".
#' @param port Port number. Defaults to the postgreSQL default of 5432
#'
#' @return Returns a list of data frames, one for each table in the specified
#' database.
#'
#' @export
#'
#' @examples
#'
eastweb_global_schema <- function(db_name, user, password,
                                  host = "localhost", port = 5432) {

  # open a connection to the database
  db_con <- RPostgreSQL::dbConnect(
    drv = DBI::dbDriver("PostgreSQL"),
    dbname = db_name, host = host, port = port,
    user = user, password = password)

  global_schema <- read_global_schema(db_con)

  # close the connection to the database
  RPostgreSQL::dbDisconnect(db_con)

  global_schema

}

#' Read global schema using an open connection to an EASTWeb database
#'
#' @param db_con A connection to an EASTWeb database
#'
#' @return Returns a list of data frames, one for each table in the specified
#' database.
#'
#' @importFrom magrittr "%>%"
#'
#' @examples
#'
read_global_schema <- function(db_con) {

  # name of the global eastweb schema
  global_schema_name <- "EASTWeb"

  # names of the tables within the eastweb schema
  global_schema_tables <- c(
    "DateGroup",
    "Download",
    "DownloadExpectedTotalOutput",
    "DownloadExtra",
    "GlobalDownloader",
    "Index",
    "IndicesExpectedTotalOutput",
    "Plugin",
    "ProcessorExpectedTotalOutput",
    "Project",
    "ProjectSummary",
    "SummaryExpectedTotalOutput",
    "TemporalSummaryCompositionStrategy"
  )

  # get a list of dataframes, one for each table
  sapply(
    global_schema_tables,
    function(x) {
      RPostgreSQL::dbReadTable(db_con, c("EASTWeb", x)) %>%
        dplyr::tbl_df()
    } )

}
