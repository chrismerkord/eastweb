
#' Format a schema name as it appears in the an EASTWeb
#' database
#'
#' @param project_name The name of the project. Case sensitive.
#' @param plugin The name of the plugin. Case sensitive.
#'
#' @return The formatted named of the schema. The project name is pasted
#' together with the plugin name and everything is converted to lower case.
#'
#' @importFrom magrittr "%>%"
#'
#' @examples
#' eastweb_proj_name("Project Name", "PluginName")
#'
eastweb_schema_name <- function(project_name,
                                plugin = c("TRMM3B42", "TRMM3b42RT",
                                           "ModisLST", "ModisNBAR",
                                           "NldasNOAH", "NldasForcing")) {
  eastweb_proj_name(project_name) %>%
    paste(plugin, sep = "_") %>%
    stringr::str_to_lower()

}



#' Get data  from an EASTWeb database
#'
#' @param project_name Name of the project. Does not need underscores.
#' @param plugin Name of the plugin. Must be one of "TRMM3B42", "TRMM3b42RT",
#' "ModisLST", "ModisNBAR", "NldasNOAH", or "NldasForcing".
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
eastweb_data <- function(project_name,
                         plugin = c("TRMM3B42", "TRMM3b42RT",
                                    "ModisLST", "ModisNBAR",
                                    "NldasNOAH", "NldasForcing"),
                         db_name = "epidemiadb",
                         user, password,
                         host = "localhost", port = 5432) {

  # get the data frame of eastweb projects
  project_info <- eastweb_project_info(
    project_name, db_name, user, password, host, port)

  eastweb <- eastweb_global_tables(
    db_name, user, password, host, port)

  schema_name <- eastweb_schema_name(project_name, plugin)

  db_con <- RPostgreSQL::dbConnect(
    drv = DBI::dbDriver("PostgreSQL"),
    dbname = db_name, host = host, port = port,
    user = user, password = password)

  zonal_stats <- RPostgreSQL::dbReadTable(
    db_con, c(schema_name, "ZonalStat")) %>%
    dplyr::tbl_df() %>%
    dplyr::select(-ZonalStatID, -ProjectSummaryID, -FilePath) %>%
    dplyr::left_join(eastweb$Index, by = "IndexID") %>%
    dplyr::left_join(eastweb$DateGroup,
                     by = "DateGroupID") %>%
    dplyr::transmute(
      var = Name,
      AreaCode,  AreaName,
      Date = as.Date(paste(Year, DayOfYear),
                     format = "%Y %j"),
      n_values = Count,
      max_value = Max,
      min_value = Min,
      sqrsum_value = SqrSum,
      sd_value = StdDev
    ) %>%
    dplyr::rename_(
      .dots = setNames(list("AreaCode", "AreaName"),
                       c(project_info$area_code_field,
                         project_info$area_name_field))) %>%
    dplyr::arrange(var, woreda, Date)

  RPostgreSQL::dbDisconnect(db_con)

  zonal_stats

}
