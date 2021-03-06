% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fd_utilities.R
\name{fd_addresses}
\alias{fd_addresses}
\title{Extract addresses of houses in a list of html documents.}
\usage{
fd_addresses(house_htmls)
}
\arguments{
\item{house_htmls}{list of html documents as read with function create_house_htmls().}
}
\value{
addresses of houses (character vector)
examples
house_htmls <- fd_create_house_htmls("Doesburg" )
x <- fd_addresses(house_htmls)
}
\description{
Extract addresses of houses in a list of html documents.
}
