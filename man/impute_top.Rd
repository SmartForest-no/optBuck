% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{impute_top}
\alias{impute_top}
\title{impute_top}
\usage{
impute_top(tt)
}
\arguments{
\item{tt}{matrix of log segments which maximize cumulative value}
}
\value{
new matrix which includes the tree top as waste
}
\description{
Impute unused top of stem into result matrix of optBuck (waste)
}
\author{
Lennart Noordermeer \email{lennart.noordermeer@nmbu.no}
}
