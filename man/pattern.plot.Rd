% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extra.R
\name{pattern.plot}
\alias{pattern.plot}
\title{Plot pattern of missing values by class}
\usage{
pattern.plot(df, class)
}
\arguments{
\item{df}{dataframe with missing values}

\item{class}{name of the variable containing classes}
}
\value{
A plot with the patter of missing values by class, by variable
}
\description{
This function returns a dataframe with a multilevel structure. It generates a dataframe using a varying
intercepts/varying slopes linear regression with a single target variable y.
}
\examples{
data(data.example)
pattern.plot(data.example, "class")
}
