

#' Get information on a particular project in an EASTWeb database
#'
#' @param project_name Name of the project. Does not need underscores.
#' @param db_name Name of the database.
#' @param user Username
#' @param password Password
#' @param host Host name. Defaults to "localhost".
#' @param port Port number. Defaults to the postgreSQL default of 5432
#'
#' @return A list with values describing the project
#' @export
#'
#' @examples
#'
eastweb_project_summary <- function(project_name,
                                    db_name, user, password,
                                    host = "localhost",
                                    port = 5432) {

  # open a connection to the database
  db_con <- RPostgreSQL::dbConnect(
    drv = DBI::dbDriver("PostgreSQL"),
    dbname = db_name, host = host, port = port,
    user = user, password = password)

  project_summary <- read_project_summary(db_con, project_name)

  # close the connection to the database
  RPostgreSQL::dbDisconnect(db_con)

  project_summary

}

#' Read information for a particular project using an open connection to an
#' EASTWeb database
#'
#' @param db_con A connection to an EASTWeb database
#' @param project_name Name of the project to extract.
#'
#' @return A list with values describing the project.
#' @importFrom magrittr "%>%"
#'
#' @examples
#'
read_project_summary <- function(db_con, project_name) {

  # read projects from database
  projects <- read_projects(db_con)

  project_name <- format_project_name(project_name)

  # extract information for one project
  projects %>%
    dplyr::filter(proj_name == project_name) %>%
    as.list()

}
