% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nbs_investigation_functions.R
\name{nbs_field_get}
\alias{nbs_field_get}
\title{Retrieve field from an investigation page}
\usage{
nbs_field_get(id, page_source = NA, note = F, ...)
}
\arguments{
\item{id}{String. NBS question_identifier for the field.}

\item{page_source}{Page html. If NA, will pull current browser page (slower). For longer queries, use remDr$getPageSource() to pull the html once.}

\item{note}{T/F. Is the field a note field (with repeating note blocks)}

\item{...}{Additional arguments passed on to get_note() which determine what info is returned for notes. By default returns the text of the last note.}
}
\value{
string
}
\description{
Retrieve field from an investigation page
}
