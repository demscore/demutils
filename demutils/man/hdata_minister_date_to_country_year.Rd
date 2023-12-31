% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hdata_aggregation_functions.R
\name{hdata_minister_date_to_country_year}
\alias{hdata_minister_date_to_country_year}
\title{Aggregation hdata_minister_date_to_country_year}
\usage{
hdata_minister_date_to_country_year(df)
}
\arguments{
\item{df}{a data.frame}
}
\value{
data.frame aggregated to hdata_country_year.
}
\description{
Sequentially aggregates from minister_date to country_date, then country_date to country_year
}
\examples{
hdata_minister_date_to_country_year(df)

}
