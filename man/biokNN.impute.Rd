% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/impute.R
\name{biokNN.impute}
\alias{biokNN.impute}
\title{Impute multilevel dataset}
\usage{
biokNN.impute(
  data,
  className,
  nIter = 10,
  weight = 0.5,
  k = 10,
  distance = "gower"
)
}
\arguments{
\item{data}{A dataframe with missing values}

\item{className}{name of the variable that contains the classes}

\item{nIter}{number of iterations, default = 10}

\item{weight}{weight of the kNN values in the objective function, default = 0.5}

\item{k}{number of nearest neighbours, default = 10}

\item{distance}{distance function used to get the k-nearest neighbors}
}
\value{
A dataframe with the imputed data
}
\description{
This function returns a dataframe with a complete dataset, where the missing values are imputed
using a bi-objective kNN method. It assumes that the class variable is complete and its name is known, and the rest
of the variables are numerical.
}
\examples{
data(data.example)
complete.data <- biokNN.impute(data.example,
               className = "class",
               nIter = 10,
               weight = 0.9,
               k = 15,
               distance = "gower")
}
