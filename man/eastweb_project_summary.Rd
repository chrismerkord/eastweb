% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/project_summary.R
\name{eastweb_project_summary}
\alias{eastweb_project_summary}
\title{Get information on a particular project in an EASTWeb database}
\usage{
eastweb_project_summary(project_name, db_name, user, password,
  host = "localhost", port = 5432)
}
\arguments{
\item{project_name}{Name of the project. Does not need underscores.}

\item{db_name}{Name of the database.}

\item{user}{Username}

\item{password}{Password}

\item{host}{Host name. Defaults to "localhost".}

\item{port}{Port number. Defaults to the postgreSQL default of 5432}
}
\value{
A list with values describing the project
}
\description{
Get information on a particular project in an EASTWeb database
}
\examples{

}

