% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{isrep.default}
\alias{isrep.default}
\title{Is Repeated in a Vector
Tests for a certain number of repetitions of \code{vals} in a given vector \code{x}.}
\usage{
\method{isrep}{default}(x, vals = unique(x), nrep = 2, ...)
}
\arguments{
\item{x}{an object with potential repeated values.}

\item{vals}{values that may be repeated.}

\item{nrep}{exact number of repeats desired, defaults to pairs.}

\item{...}{further arguments to be passed to or from other methods.}
}
\value{
Logical.
}
\description{
Is Repeated in a Vector
Tests for a certain number of repetitions of \code{vals} in a given vector \code{x}.
}
\details{
This is a generic function, with methods supplied for data frames and vectors. The default behavior tests for existence of pairs of elements of \code{x}.  One can test existence of triples, etc., by changing the \code{nrep} argument.  If there are specific values for which one is looking for repeats, these can be specified with the \code{vals} argument.  Note that the function only checks for \emph{exactly} \code{nrep} instances, so two pairs of a specific element would be counted as 0 pairs and 1 quadruple. See the examples.
The data frame method uses \code{apply} to apply \code{isrep.default} to each row of the data frame.
}
\examples{
x <- c(3,3,2,2,3,3,4,4)
isrep(x)  # one pair each of 2s and 4s
isrep(x, nrep = 4)
isrep(x, vals = 4) # one pair of 4s

}
\seealso{
\code{\link[probs]{countrep}}
}
\author{
G. Jay Kerns \email{gkerns@ysu.edu}.
}
