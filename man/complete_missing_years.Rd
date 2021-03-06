% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper_fxns.R
\name{complete_missing_years}
\alias{complete_missing_years}
\title{Fill in the missing values}
\usage{
complete_missing_years(data, expected_years = 1700:2500)
}
\arguments{
\item{data}{a datatable emissions or concentration data}

\item{expected_years}{the number of years to ensure there data, default set to 1700 to 2500}
}
\value{
a data table with interpolated data
}
\description{
Fill in the missing values
}
