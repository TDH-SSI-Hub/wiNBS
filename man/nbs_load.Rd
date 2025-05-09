% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nbs_login_functions.R
\name{nbs_load}
\alias{nbs_load}
\title{Load NBS home screen}
\usage{
nbs_load(
  u = "",
  environment = "NBS Production",
  url = "https://hssi.tn.gov/auth/login"
)
}
\arguments{
\item{u}{NBS Username}

\item{environment}{Environment to enter}

\item{url}{url for NBS login page - e.g., 'https://hssi.tn.gov/auth/login'}
}
\value{
Nothing
}
\description{
Navigate to the NBS login page, enter username and password
, and enter the specified environment - defaults to production
}
