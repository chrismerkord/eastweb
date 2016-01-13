
#' Format a project name as it appears in the Project table in an EASTWeb
#' database
#'
#' @param project_name The name of the project. Case sensitive.
#'
#' @return The formatted named of the project. Spaces are converted to
#' underscores.
#'
#' @examples
#' eastweb_proj_name("Project Name")
#'
eastweb_proj_name <- function(project_name) {
  stringr::str_replace_all(project_name, " ", "_")
}

#' Get global schema tables in an EASTWeb database
#'
#' @param db_name Name of the database.
#' @param user Username
#' @param password Password
#' @param host Host name. Defaults to "localhost".
#' @param port Port number. Defaults to the postgreSQL default of 5432
#'
#' @return Returns a list of data frames, one for each table in the specified
#' database.
#' @importFrom magrittr "%>%"
#' @export
#'
#' @examples
#'
eastweb_global_tables <- function(db_name, user, password,
                                  host = "localhost",
                                  port = 5432) {

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

  # open a connection to the database
  db_con <- RPostgreSQL::dbConnect(
    drv = DBI::dbDriver("PostgreSQL"),
    dbname = db_name, host = host, port = port,
    user = user, password = password)

  # get a list of dataframes, one for each table
  eastweb <- sapply(
    global_schema_tables,
    function(x) {
      RPostgreSQL::dbReadTable(db_con, c("EASTWeb", x)) %>%
        dplyr::tbl_df()
    } )

  # close the connection to the database
  RPostgreSQL::dbDisconnect(db_con)

  # return this
  eastweb

}

#' Get information on all projects in an EASTWeb database
#'
#' @param db_name Name of the database.
#' @param user Username
#' @param password Password
#' @param host Host name. Defaults to "localhost".
#' @param port Port number. Defaults to the postgreSQL default of 5432
#'
#' @return Returns a dataframe of the projects in the specified database.
#' @importFrom magrittr "%>%"
#' @export
#'
#' @examples
#'
eastweb_projects <- function(db_name, user, password,
                             host = "localhost",
                             port = 5432) {

  eastweb <- eastweb_global_tables(db_name, user, password, host, port)

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
#' @importFrom magrittr "%>%"
#' @export
#'
#' @examples
#'
eastweb_project_info <- function(project_name,
                                 db_name, user, password,
                                 host = "localhost",
                                 port = 5432) {

  # format the project name as it appears in the Project table
  project_name <- eastweb_proj_name(project_name)

  # get the info for the specified project
  eastweb_projects(db_name, user, password, host, port) %>%
    dplyr::filter(proj_name == project_name) %>%
    as.list()

}
