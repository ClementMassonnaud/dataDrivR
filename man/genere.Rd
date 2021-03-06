% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{genere}
\alias{genere}
\title{Make multiple clones of the dataset, and rbind them to create an artificial dataset}
\usage{
genere(data, i)
}
\arguments{
\item{data}{the data.table}

\item{i}{the approximate number of clones to create.}
}
\description{
Make multiple clones of the dataset, and rbind them to create an artificial dataset
}
\details{
For optimization purposes, the function is not row-binding the dataset i times, but rather row-binding the dataset to itself on each iteration. So the length of the dataset is doubling at each iteration. To have a final dataset of approximately 'i' times the original, we must double it 'n = log(i, base = 2)' times.
}
