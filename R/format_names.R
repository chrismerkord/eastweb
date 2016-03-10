
#' Format a project name as it appears in the Project table in an EASTWeb
#' database
#'
#' @param project_name The name of the project. Case sensitive.
#'
#' @return The formatted named of the project. Spaces are converted to
#' underscores.
#'
#' @examples
#' format_project_name("Project Name")
#'
format_project_name <- function(project_name) {

  stringr::str_replace_all(project_name, " ", "_")

}

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
#' format_schema_name("Project Name", "PluginName")
#'
format_schema_name <- function(project_name,
                               plugin = c("TRMM3B42", "TRMM3b42RT",
                                          "ModisLST", "ModisNBAR",
                                          "NldasNOAH", "NldasForcing")) {
  format_project_name(project_name) %>%
    paste(plugin, sep = "_") %>%
    stringr::str_to_lower()

}
