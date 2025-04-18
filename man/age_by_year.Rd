% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/age_by_year.R
\name{age_by_year}
\alias{age_by_year}
\title{Get age information from ID number}
\usage{
age_by_year(id)
}
\arguments{
\item{id}{A vector of ID numbers.}
}
\value{
Age vector obtained from ID numbers.
}
\description{
Get age information from ID number, only by year, not the specific date.
}
\examples{
id = c(
  "653127198503161793",
  "652801197305161555",
  "130206202202291545", 
  "110101841125178",
  "12345678",
  "65312a198204181793"
)
age_by_year(id)

}
