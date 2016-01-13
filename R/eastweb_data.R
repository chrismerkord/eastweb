
#' Get data from an EASTWeb database
#'
#' @param plugin Name of the plugin. Must be one of "TRMM3B42", "TRMM3b42RT",
#' "ModisLST", "ModisNBAR", "NldasNOAH", or "NldasForcing".
#' @param project_name Name of the project. Does not need underscores.
#' @param db_name Name of the database.
#' @param user Username.
#' @param password Password.
#' @param host Host name. Defaults to "localhost".
#' @param port Port number. Defaults to the postgreSQL default of 5432.
#'
#' @return A data frame containing the zonal statistics table for the given
#' plugin in an EASTWeb project. Even if a project has multiple plugins, each
#' one must be queried seperately.
#'
#' @importFrom magrittr "%>%"
#' @export
#'
#' @examples
#'
eastweb_data <- function(plugin = c("TRMM3B42", "TRMM3b42RT",
                                    "ModisLST", "ModisNBAR",
                                    "NldasNOAH", "NldasForcing"),
                         project_name,
                         db_name, user, password,
                         host = "localhost", port = 5432) {

  db_con <- RPostgreSQL::dbConnect(
    drv = DBI::dbDriver("PostgreSQL"),
    dbname = db_name, host = host, port = port,
    user = user, password = password)

  # get project summary
  project_summary <- read_project_summary(db_con, project_name)

  # get tables from global schema
  eastweb <- read_global_schema(db_con)

  # format schema name
  schema_name <- format_schema_name(project_name, plugin)

  zonal_stats <- RPostgreSQL::dbReadTable(
    db_con, c(schema_name, "ZonalStat")) %>%
    dplyr::tbl_df() %>%
    dplyr::select(-ZonalStatID, -ProjectSummaryID, -FilePath) %>%
    dplyr::left_join(eastweb$Index, by = "IndexID") %>%
    dplyr::left_join(eastweb$DateGroup, by = "DateGroupID") %>%
    dplyr::transmute(Index_Name = Name,
                     AreaCode,  AreaName,
                     Year, DayOfYear,
                     Count, Max, Min, SqrSum, StdDev) %>%
    dplyr::arrange(Index_Name, AreaName, Year, DayOfYear) %>%
    dplyr::rename_(
      .dots = setNames(list("AreaCode", "AreaName"),
                       c(project_info$area_code_field,
                         project_info$area_name_field)))

  RPostgreSQL::dbDisconnect(db_con)

  zonal_stats

}
