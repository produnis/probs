% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{urnsamples.default}
\alias{urnsamples.default}
\title{Sampling from Urns
This function creates a sample space associated with the experiment of sampling distinguishable objects from an urn.}
\usage{
\method{urnsamples}{default}(x, size, replace = FALSE, ordered = FALSE, ...)
}
\arguments{
\item{x}{a vector or data frame from which sampling should take place.}

\item{size}{number indicating the sample size.}

\item{replace}{logical indicating whether sampling should be done with replacement.}

\item{ordered}{logical indicating whether order among samples is important.}

\item{...}{further arguments to be passed to or from other methods.}
}
\value{
A data frame if \code{urn} is a vector, and a list if \code{urn} is a data frame.
}
\description{
Sampling from Urns
This function creates a sample space associated with the experiment of sampling distinguishable objects from an urn.
}
\details{
The function operates on the indices of the urn (or rows, in the case \code{urn} is a data frame).  It then takes those samples and substitutes back into \code{urn} to generate the entries of the data frame (or list, respectively).  In the case that \code{urn} has repeated values, the result will be repeated values in the output. Note that \code{urnsamples} strips \code{x} of any existing \code{probs} column before sampling.
}
\examples{
urnsamples(1:10, size = 5)
S <- cards()
 urnsamples(S, size = 2)
}
\author{
G. Jay Kerns \email{gkerns@ysu.edu}.
}
