% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{euchredeck}
\alias{euchredeck}
\title{A Deck of Playing Cards for Euchre}
\usage{
euchredeck(benny = FALSE, makespace = FALSE)
}
\arguments{
\item{benny}{logical. Include a Joker if \code{TRUE}.}

\item{makespace}{logical. Include a column of equally likely probabilities if \code{TRUE}.}
}
\value{
A data frame with columns \code{value} and \code{suit}, and optionally a column of equally likely \code{probs}.
}
\description{
The title says it all.
}
\details{
This is a conventional Euchre deck which uses a deck of 24 standard playing cards consisting of Ace, King, Queen, Jack, 10, and 9 of each of the four suits. If \code{benny = TRUE} then a Joker is added to the deck.
}
\examples{
euchredeck()
euchredeck(benny = TRUE, makespace = TRUE)

}
\seealso{
\code{\link[probs]{cards}}, \code{\link[probs]{tosscoin}}, and \code{\link[probs]{roulette}}
}
\keyword{data}
