% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{setdiff}
\alias{setdiff}
\title{Set Difference of Subsets
Calculates the (nonsymmetric) set difference of subsets of a probability space.}
\usage{
setdiff(x, ...)
}
\arguments{
\item{x}{vectors, data frames, or \code{ps} objects containing a sequence of items (conceptually).}

\item{...}{further arguments to be passed to or from other methods.}
}
\value{
A data frame or subset of a probability space of the same type as its arguments.
}
\description{
Set Difference of Subsets
Calculates the (nonsymmetric) set difference of subsets of a probability space.
}
\details{
This function operates row-wise on dataframes, and element-wise among the outcomes of \code{ps} objects.  The elements of \code{setdiff(x,y)} are those elements in \code{x} but not in \code{y}. The definition is taken to match the version in the \code{base} package.
}
\examples{
S <- cards()
A <- subset(S, suit == "Heart")
B <- subset(S, rank == "A" )
setdiff(B, A)

}
\seealso{
\code{\link[probs]{intersect}}, \code{\link[probs]{union}}
}
\author{
G. Jay Kerns \email{gkerns@ysu.edu}, essentially verbatim from a suggestion made by Brian Ripley on \code{R}-devel, 12/11/07
}
