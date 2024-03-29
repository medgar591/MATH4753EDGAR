% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myboot2.R
\name{myboot2}
\alias{myboot2}
\title{myboot2}
\usage{
myboot2(iter = 10000, x, fun = "mean", alpha = 0.05, cx = 1.5, ...)
}
\arguments{
\item{iter}{Number of iterations to perform bootstrapping}

\item{x}{Vector containing the sample to bootstrap}

\item{fun}{The name of the function to use (ie "mean" or "var")}

\item{alpha}{Quantile representing the value in (1-a)*100% confidence value}

\item{cx}{Extra value used in graphs}

\item{...}{}
}
\value{
Produces a graph and retuns extra values for usage
}
\description{
myboot2
}
