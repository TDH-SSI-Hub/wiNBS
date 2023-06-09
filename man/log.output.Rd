% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc_functions.R
\name{log.output}
\alias{log.output}
\title{Log OOS processing to a SQL table}
\usage{
\method{log}{output}(df, odbc_name, table_name)
}
\arguments{
\item{df}{Output data frame from one of the OOS scripts}

\item{odbc_name}{Name of the ODBC connection}

\item{table_name}{Name of the table to log results in}
}
\value{
Nothing
}
\description{
Log OOS processing to a SQL table
}
