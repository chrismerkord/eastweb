
#' Get information on all projects in an EASTWeb database
#'
#' @param db_name Name of the database.
#' @param user Username
#' @param password Password
#' @param host Host name. Defaults to "localhost".
#' @param port Port number. Defaults to the postgreSQL default of 5432
#'
#' @return Returns a dataframe of the projects in the specified database.
#'
#' @export
#'
#' @examples
#'
eastweb_projects <- function(db_name, user, password,
                             host = "localhost",
                             port = 5432) {

  # open a connection to the database
  db_con <- RPostgreSQL::dbConnect(
    drv = DBI::dbDriver("PostgreSQL"),
    dbname = db_name, host = host, port = port,
    user = user, password = password)

  projects <- read_projects(db_con)

  # close the connection to the database
  RPostgreSQL::dbDisconnect(db_con)

  projects

}

#' Read all project information using an open connection to an EASTWeb database
#'
#' @param db_con A connection to an EASTWeb database
#'
#' @return Returns a dataframe of the projects in the specified database.
#'
#' @importFrom magrittr "%>%"
#'
#' @examples
#'
read_projects <- function(db_con) {

  eastweb <- read_global_schema(db_con)

  # return this
  eastweb$ProjectSummary %>%
    dplyr::left_join(eastweb$Project,
                     by = "ProjectID") %>%
    dplyr::left_join(eastweb$DateGroup,
                     by = c("StartDate_DateGroupID" = "DateGroupID")) %>%
    dplyr::left_join(eastweb$TemporalSummaryCompositionStrategy,
                     by = c("TemporalSummaryCompositionStrategyID")) %>%
    dplyr::transmute(proj_summ_id = ProjectSummaryID,
                     proj_name = Name.x,
                     proj_start_date = as.Date(paste(Year, DayOfYear),
                                               format = "%Y %j"),
                     temporal_summ = Name.y,
                     proj_summ_id_num = SummaryIDNum,
                     area_name_field = AreaNameField,
                     area_code_field = AreaCodeField,
                     shapefile = ShapeFile)

}
