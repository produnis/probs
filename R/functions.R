####################################################
# In order to let roxygen2() create all man pages, #
# you must run the following command:              #
#       roxygen2::roxygenise()                     #
####################################################


#' A Standard Set of Playing Cards
#'
#' The title says it all.
#'
#' @param jokers logical. Include jokers in the deck.
#' @param makespace logical. Include a column of equally likely probabilities.
#'
#' @details This generates a data frame sample space of a standard deck of 52 playing cards. Optionally, the user can specify that Jokers be included, which have a \code{rank} but with \code{suit} a missing value.
#'
#' @return A data frame with columns \code{rank} and \code{suit}, and optionally a column of equally likely \code{probs}.
#'
#' @seealso \code{\link[probs]{rolldie}}, \code{\link[probs]{tosscoin}}, and \code{\link[probs]{roulette}}
#' @author G. Jay Kerns \email{gkerns@ysu.edu}.
#' @examples
#' cards()
#' cards(makespace = TRUE)
#'
#' @keywords data
#' @name cards
#' @export
#-------------------------------------------------------------------------
`cards` <- function (jokers = FALSE, makespace = FALSE){
  x <- c(2:10, "J", "Q", "K", "A")
  y <- c("Club", "Diamond", "Heart", "Spade")
  res <- expand.grid(rank = x, suit = y)
  if (jokers) {
    levels(res$rank) <- c(levels(res$rank), "Joker")
    res <- rbind(res, data.frame(rank = c("Joker", "Joker"),
                                 suit = c(NA, NA)))
  }
  if (makespace) {
    res$probs <- rep(1, dim(res)[1])/dim(res)[1]
  }
  return(res)
}




#' A Deck of Playing Cards for Euchre
#'
#' The title says it all.
#'
#' @param benny logical. Include a Joker if \code{TRUE}.
#' @param makespace logical. Include a column of equally likely probabilities if \code{TRUE}.
#'
#' @details This is a conventional Euchre deck which uses a deck of 24 standard playing cards consisting of Ace, King, Queen, Jack, 10, and 9 of each of the four suits. If \code{benny = TRUE} then a Joker is added to the deck.
#'
#' @return A data frame with columns \code{value} and \code{suit}, and optionally a column of equally likely \code{probs}.
#'
#' @seealso \code{\link[probs]{cards}}, \code{\link[probs]{tosscoin}}, and \code{\link[probs]{roulette}}
#'
#' @examples
#' euchredeck()
#' euchredeck(benny = TRUE, makespace = TRUE)
#'
#' @keywords data
#' @name euchredeck
#' @export
#-----------------------------------------------
`euchredeck` <- function (benny = FALSE, makespace = FALSE){
  x <- c(9:10, "J", "Q", "K", "A")
  y <- c("Club", "Diamond", "Heart", "Spade")
  res <- expand.grid(value = x, suit = y)
  if (benny) {
    levels(res$value) <- c(levels(res$value), "Joker")
    res <- rbind(res, data.frame(value = c("Joker"), suit = NA))
  }
  if (makespace) {
    res$probs <- rep(1, dim(res)[1])/dim(res)[1]
  }
  return(res)
}


#' Rolling a Die
#'
#' Sets up a sample space for the experiment of rolling a die repeatedly.
#'
#' @param times number of rolls.
#' @param nsides number of sides on the die.
#' @param makespace logical. Include a column of equally likely probabilities if \code{TRUE}.
#'
#' @details The function uses \code{expand.grid()} to generate all possible rolls resulting from the experiment of rolling a die. Sides on the die are \code{1:nsides}. Columns of the data frame are called \code{X1}, \code{X2}, up to \code{Xtimes}.
#'
#' @return A data frame, with an equally likely \code{probs} column if \code{makespace} is \code{TRUE}.
#'
#' @author G. Jay Kerns \email{gkerns@ysu.edu}.
#'
#' @seealso \code{\link[probs]{tosscoin}}
#'
#' @examples
#' rolldie(2)
#' rolldie(3, nsides = 4)
#' rolldie(3, nsides = 4, makespace = TRUE)
#'
#' @keywords misc
#' @name rolldie
#' @export
#----------------------------------------------------------
`rolldie` <- function (times, nsides = 6, makespace = FALSE){
  temp = list()
  for (i in 1:times) {
    temp[[i]] <- 1:nsides
  }
  res <- expand.grid(temp, KEEP.OUT.ATTRS = FALSE)
  names(res) <- c(paste(rep("X", times), 1:times, sep = ""))
  if (makespace)
    res$probs <- rep(1, nsides^times)/nsides^times
  return(res)
}


#' Roulette
#'
#' Sets up a sample space for the experiment of spinning a roulette wheel once.
#'
#' @param european logical. Use a European roulette wheel with 37 pockets if \code{TRUE}, otherwise use a standard US roulette wheel with 38 pockets.
#' @param makespace logical. Include a column of equally likely probabilities if \code{TRUE}.
#'
#' @details If \code{european} is \code{TRUE}, then a traditional EU roulette wheel with 37 pockets is used, otherwise, a standard US roulette wheel with 38 pockets is used. Entries in the data frame are ordered in the customary way to facilitate the calculation probabilities regarding called bets.
#'
#' @return A data frame, with columns \code{num} and \code{color}, and an equally likely \code{probs} column if \code{makespace} is \code{TRUE}.
#'
#' @author G. Jay Kerns \email{gkerns@ysu.edu}.
#'
#' @seealso \code{\link[probs]{cards}}
#'
#' @examples
#' roulette()
#' roulette(european = TRUE, makespace = TRUE)
#'
#' @keywords misc
#' @name roulette
#' @export
#--------------------------------------------------------
`roulette` <- function (european = FALSE, makespace = FALSE){
  if (european) {
    num = c("0", "26", "3", "35", "12", "28", "7", "29",
            "18", "22", "9", "31", "14", "20", "1", "33", "16",
            "24", "5", "10", "23", "8", "30", "11", "36", "13",
            "27", "6", "34", "17", "25", "2", "21", "4", "19",
            "15", "32")
    color <- c("Green", rep(c("Black", "Red"), 18))
  }
  else {
    num = c("27", "10", "25", "29", "12", "8", "19", "31",
            "18", "6", "21", "33", "16", "4", "23", "35", "14",
            "2", "0", "28", "9", "26", "30", "11", "7", "20",
            "32", "17", "5", "22", "34", "15", "3", "24", "36",
            "13", "1", "00")
    color <- c(rep(c("Red", "Black"), 9), "Green", rep(c("Black",
                                                         "Red"), 9), "Green")
  }
  res <- data.frame(num = num, color = color)
  if (makespace) {
    res$probs <- rep(1, length(num))/length(num)
  }
  return(res)
}


#' Tossing a Coin
#'
#' Sets up a sample space for the experiment of tossing a coin repeatedly with the outcomes "H" or "T".
#'
#' @param times number of tosses.
#' @param makespace logical. Include a column of equally likely probabilities if \code{TRUE}.
#'
#' @details The function uses \code{expand.grid()} to generate all possible sequences of flips resulting from the experiment of tossing a coin. Columns of the dataframe are denoted \code{toss1}, \code{toss2}, up to \code{tosstimes}.
#'
#' @return A data frame, with an equally likely \code{probs} column if \code{makespace} is \code{TRUE}.
#'
#' @author G. Jay Kerns \email{gkerns@ysu.edu}.
#'
#' @seealso \code{\link[probs]{rolldie}}
#'
#' @examples
#' tosscoin(2)
#' tosscoin(3, makespace = TRUE)
#'
#' @keywords misc
#' @name tosscoin
#' @export

#---------------------------------------------
`tosscoin` <- function (times, makespace = FALSE){
  temp <- list()
  for (i in 1:times) {
    temp[[i]] <- c("H", "T")
  }
  res <- expand.grid(temp, KEEP.OUT.ATTRS = FALSE)
  names(res) <- c(paste(rep("toss", times), 1:times, sep = ""))
  if (makespace)
    res$probs <- rep(1, 2^times)/2^times
  return(res)
}


#' Sampling from Urns
#' This function creates a sample space associated with the experiment of sampling distinguishable objects from an urn.
#' @param x a vector or data frame from which sampling should take place.
#' @param ... further arguments to be passed to or from other methods.
#' @details The function operates on the indices of the urn (or rows, in the case \code{urn} is a data frame).  It then takes those samples and substitutes back into \code{urn} to generate the entries of the data frame (or list, respectively).  In the case that \code{urn} has repeated values, the result will be repeated values in the output. Note that \code{urnsamples} strips \code{x} of any existing \code{probs} column before sampling.
#' @author G. Jay Kerns \email{gkerns@ysu.edu}.
#' @importFrom stats aggregate
#' @importFrom utils combn
#' @return  A data frame if \code{urn} is a vector, and a list if \code{urn} is a data frame.
#' @examples
#' urnsamples(1:10, size = 5)
#' S <- cards()
#'  urnsamples(S, size = 2)
#' @export
#--------------------------------------------------------------------------------
`urnsamples` <- function (x, ...)
  UseMethod("urnsamples")


#' Sampling from Urns
#' This function creates a sample space associated with the experiment of sampling distinguishable objects from an urn.
#' @param x a vector or data frame from which sampling should take place.
#' @param size number indicating the sample size.
#' @param replace logical indicating whether sampling should be done with replacement.
#' @param ordered logical indicating whether order among samples is important.
#' @param ... further arguments to be passed to or from other methods.
#' @details The function operates on the indices of the urn (or rows, in the case \code{urn} is a data frame).  It then takes those samples and substitutes back into \code{urn} to generate the entries of the data frame (or list, respectively).  In the case that \code{urn} has repeated values, the result will be repeated values in the output. Note that \code{urnsamples} strips \code{x} of any existing \code{probs} column before sampling.
#' @author G. Jay Kerns \email{gkerns@ysu.edu}.
#' @return  A data frame if \code{urn} is a vector, and a list if \code{urn} is a data frame.
#' @importFrom stats aggregate
#' @importFrom utils combn
#' @examples
#' urnsamples(1:10, size = 5)
#' S <- cards()
#'  urnsamples(S, size = 2)
#' @export
#--------------------------------------------------------------------------------
`urnsamples.data.frame` <- function (x, size, replace = FALSE, ordered = FALSE, ...){
  nurn <- dim(x)[1]
  if (isTRUE(replace)) {
    if (isTRUE(ordered)) {
      temp <- list()
      for (i in 1:size) {
        temp[[i]] <- 1:nurn
      }
      ind <- t(as.matrix(expand.grid(temp, KEEP.OUT.ATTRS = FALSE)))
    }
    else {
      temp <- list()
      for (i in 1:size) {
        temp[[i]] <- 1:nurn
      }
      res <- as.matrix(expand.grid(temp, KEEP.OUT.ATTRS = FALSE))
      ind <- t(unique(t(apply(res, 1, sort))))
    }
  }
  else {
    if (size > nurn)
      stop("cannot take a sample larger than the urn size when 'replace = FALSE'")
    if (isTRUE(ordered)) {
      ind <- permsn(1:nurn, size)
    }
    else {
      ind <- combn(1:nurn, size)
    }
  }
  if (!is.null(x$probs))
    x$probs <- NULL
  nss <- dim(ind)[2]
  out <- list()
  for (i in 1:nss) {
    out[[i]] <- x[ind[, i], ]
  }
  return(out)
}



#' Sampling from Urns
#' This function creates a sample space associated with the experiment of sampling distinguishable objects from an urn.
#' @param x a vector or data frame from which sampling should take place.
#' @param size number indicating the sample size.
#' @param replace logical indicating whether sampling should be done with replacement.
#' @param ordered logical indicating whether order among samples is important.
#' @param ... further arguments to be passed to or from other methods.
#' @details The function operates on the indices of the urn (or rows, in the case \code{urn} is a data frame).  It then takes those samples and substitutes back into \code{urn} to generate the entries of the data frame (or list, respectively).  In the case that \code{urn} has repeated values, the result will be repeated values in the output. Note that \code{urnsamples} strips \code{x} of any existing \code{probs} column before sampling.
#' @author G. Jay Kerns \email{gkerns@ysu.edu}.
#' @return  A data frame if \code{urn} is a vector, and a list if \code{urn} is a data frame.
#' @importFrom stats aggregate
#' @importFrom utils combn
#' @examples
#' urnsamples(1:10, size = 5)
#' S <- cards()
#'  urnsamples(S, size = 2)
#' @export
#--------------------------------------------------------------------------------
`urnsamples.default` <- function (x, size, replace = FALSE, ordered = FALSE, ...){
  nurn <- length(x)
  if (isTRUE(replace)) {
    if (isTRUE(ordered)) {
      temp = list()
      for (i in 1:size) {
        temp[[i]] <- 1:nurn
      }
      ind <- t(as.matrix(expand.grid(temp, KEEP.OUT.ATTRS = FALSE)))
    }
    else {
      temp = list()
      for (i in 1:size) {
        temp[[i]] <- 1:nurn
      }
      res <- as.matrix(expand.grid(temp, KEEP.OUT.ATTRS = FALSE))
      ind <- t(unique(t(apply(res, 1, sort))))
    }
  }
  else {
    if (size > nurn)
      stop("cannot take a sample larger than the urn size when 'replace = FALSE'")
    if (isTRUE(ordered)) {
      ind <- permsn(1:nurn, size)
    }
    else {
      ind <- combn(1:nurn, size)
    }
  }
  nss <- dim(ind)[2]
  out <- matrix(nrow = nss, ncol = size)
  for (i in 1:nss) {
    out[i, ] <- x[ind[, i]]
  }
  return(data.frame(out))
}



#' Generate All Permutations of x Elements Taken m at a Time
#'
#' Generate all permutations of the elements of x taken m at a time.
#' If x is a positive integer, returns all permutations of the elements of seq(x) taken m at a time.
#'
#' @param x vector source for permutations, or integer n for x <- seq(n).
#' @param m number of elements to permute.
#'
#' @return a list or array (in nondegenerate cases).
#'
#' @author G. Jay Kerns \email{gkerns@ysu.edu}, modified from the \code{combn} function in the package \code{utils}.
#'
#' @seealso \code{\link[combinat]{combn}}
#' @importFrom combinat permn
#'
#' @examples
#' permsn(3, 2)
#'
#' @keywords misc
#' @name permsn
#' @export
`permsn` <- function (x, m)
{

  # require(combinat)
  if (is.numeric(x) && length(x) == 1 && x > 0 && trunc(x) == x)

    x <- seq(x)
  temp <- combn(x, m)
  if ( isTRUE(all.equal(m,1)) ) {

    P <- temp
  } else if (isTRUE(all.equal(m, length(x)))) {

    temp <- matrix(x, ncol = 1)
    P <- array(unlist(permn(temp[, 1])), dim = c(m, factorial(m)))
  } else {
    k <- dim(temp)[1]
    n <- dim(temp)[2]
    P <- array(unlist(permn(temp[, 1])), dim = c(k, factorial(k)))
    for (i in 2:n) {
      a <- temp[, i]
      perms <- array(unlist(permn(a)), dim = c(k, factorial(k)))
      P <- cbind(P, perms)
    }


  }
  return(P)
}



#' Adding Random Variables to a Probability Space
#'
#' Adds a column to a data frame probability space containing the values of a random variable computed from the existing columns of the space.
#'
#' @param space a data frame with a \code{probs} column.
#' @param FUN a function to be applied to each row of outcomes in \code{space}
#' @param invars a character vector indicating input columns of \code{space}
#' @param name an (optional) name to give the defined random variable.
#' @param ... an expression defining a random variable.
#'
#' @details There are two ways to add a random variable to a data frame probability space; see the examples. The argument \code{FUN} has precedence and will be used if specified. If \code{name} is not specified, then the new random variable will be called \code{X}. Note that this function only works for data frames, as a method for objects of class \code{ps} has not been implemented.
#'
#' @return The input data frame with an additional column called \code{name}.
#'
#' @author G. Jay Kerns \email{gkerns@ysu.edu}.
#'
#' @seealso See \code{\link[base]{transform}} to add a column to a data frame of outcomes (not yet a probability space).
#'
#' @examples
#' S <- rolldie(3, makespace = TRUE)
#' addrv(S, sum, name = "Y")
#' addrv(S, Z = X3 - X2)
#'
#' @keywords manip
#' @name addrv
#' @export
`addrv` <- function (space, FUN = NULL, invars = NULL, name = NULL, ...){
  if (any(class(space) == "ps"))
    stop("not implemented for class 'ps'")
  if (!is.data.frame(space) | is.null(space$probs)) {
    message("'space' is not a proper probability space")
    stop("see ?probspace")
  }
  bnames <- names(space)[which(names(space) != "probs")]
  out <- subset(space, select = bnames)
  probs <- subset(space, select = probs)
  if (is.null(invars))
    invars <- bnames
  if (!is.character(invars))
    stop("vars should be a character vector")
  if (!is.null(FUN)) {
    if (is.null(name))
      name <- "X"
    temp <- apply(subset(space, select = invars), 1, FUN)
    val <- cbind(out, temp, probs)
    names(val) <- c(bnames, name, "probs")
  }
  else {
    val <- transform(out, ...)
    val$probs <- probs
  }
  return(val)
}


#' Marginal Distributions
#'
#' Computes the marginal distribution of a set of variables.
#'
#' @param space a data frame probability space or a subset of one.
#' @param vars an optional character vector of variable names in \code{space}.
#'
#' @details If \code{vars} is not specified, then \code{marginal()} will set \code{vars} to be all non-\code{probs} columns, which can be useful in the case that it is desired to aggregate duplicated rows.
#'
#' @return A data frame with a \code{probs} column.
#'
#' @author G. Jay Kerns \email{gkerns@ysu.edu}.
#'
#' @seealso See \code{\link[probs]{addrv}} for adding random variables to a data frame probability space.
#'
#' @examples
#' S <- rolldie(3, makespace = TRUE)
#' marginal(S, vars = c("X1", "X2"))
#'
#' @importFrom stats aggregate
#' @keywords manip
#' @name marginal
#' @export
`marginal` <- function (space, vars = NULL){
  if (!is.data.frame(space) | is.null(space$probs)) {
    message("'space' is not a proper probability space")
    stop("see ?probspace")
  }
  if (is.null(vars))
    vars <- names(space)[names(space) != "probs"]
  if (!is.character(vars)) {
    stop("'vars' must be a character vector")
  }
  if (length(vars) > 1) {
    res <- aggregate(space$probs, by = as.list(space[, vars]),
                     FUN = sum)
  }
  else {
    res <- aggregate(space$probs, by = list(space[, vars]),
                     FUN = sum)
  }
  names(res) <- c(vars, "probs")
  return(res)
}



#' Sort and Merge Probability Space Outcomes
#'
#' This function sorts the rows (outcomes) of a data frame probability space, effectively removing the original order present and aggregates the sorted rows into a new probability data frame with unique, sorted outcomes.
#'
#' @param space a data frame probability space or a subset of one.
#'
#' @details The data frame \code{space} must have at least two non-\code{probs} columns or an error will result.
#'
#' @return A data frame with a \code{probs} column and sorted, unique rows.
#'
#' @author G. Jay Kerns \email{gkerns@ysu.edu}.
#'
#' @seealso \code{\link[probs]{addrv}}, \code{\link[probs]{marginal}}
#'
#' @importFrom stats aggregate
#' @examples
#' S <- tosscoin(3, makespace = TRUE)
#' noorder(S)
#'
#' @keywords misc
#' @name noorder
#' @export
`noorder` <- function (space){
  if (!is.data.frame(space)) {
    message("'space' is missing a probs column")
    stop("see ?probspace")
  }
  if (is.null(space$probs)) {
    if (dim(space)[2] < 2)
      stop("'space' has only one column of outcomes; already unordered")
    n <- names(space)
    res <- unique(data.frame(t(apply(space, 1, sort))))
    names(res) <- n
  }
  else {
    if (dim(space)[2] < 3)
      stop("'space' has only one column of outcomes; already unordered")
    A <- subset(space, select = -probs)
    probs <- subset(space, select = probs)
    n <- names(A)
    sA <- data.frame(t(apply(A, 1, sort)))
    res <- cbind(sA, probs)
    res <- aggregate(res$probs, by = as.list(sA), sum)
    names(res) <- c(n, "probs")
  }
  return(res)
}



#' Number of Samples from an Urn
#'
#' Calculates the number of samples from an urn under different sampling scenarios.
#'
#' @param n an integer or integer vector.
#' @param k an integer or integer vector.
#' @param replace logical indicating whether sampling should be done with replacement.
#' @param ordered logical indicating whether order among samples is important.
#'
#' @details The \code{nsamp()} function will calculate the number of samples from an urn under assorted assumptions on the sampling procedure. The arguments are: \code{n}, the number of (distinguishable) objects in the urn, \code{k}, the sample size, and \code{replace}, \code{ordered} as documented in \code{\link[probs]{urnsamples}}.
#'
#' \code{nsamp()} is vectorized, so that entering vectors instead of numbers for \code{n}, \code{k}, \code{replace}, and \code{ordered} results in a vector of corresponding answers.
#'
#' The formulas used in the four possible combinations of \code{replace} and \code{ordered} are as follows:
#' \itemize{
#'   \item When \code{replace = TRUE} and \code{ordered = TRUE}, the value is \eqn{n^k}.
#'   \item When \code{replace = FALSE} and \code{ordered = TRUE}, the value is \eqn{n!/(n-k)!}.
#'   \item When \code{replace = FALSE} and \code{ordered = FALSE}, the value is \eqn{n!/[k!(n-k)!]}.
#'   \item When \code{replace = TRUE} and \code{ordered = FALSE}, the value is \eqn{(n-1+k)!/[(n-1)!k!]}.
#' }
#'
#' @return A number.
#'
#' @author G. Jay Kerns \email{gkerns@ysu.edu}.
#'
#' @seealso \code{\link[probs]{urnsamples}}
#'
#' @examples
#' nsamp(n = 3, k = 2, replace = TRUE, ordered = TRUE)
#' nsamp(n = 3, k = 2, replace = TRUE, ordered = FALSE)
#' nsamp(n = 3, k = 2, replace = FALSE, ordered = FALSE)
#' nsamp(n = 3, k = 2, replace = FALSE, ordered = TRUE)
#'
#' @keywords misc
#' @name nsamp
#' @export
`nsamp` <- function (n, k, replace = FALSE, ordered = FALSE){
  m <- length(n)
  if (length(k) != m)
    stop("number of urns doesn't equal number of sample sizes")
  if (length(replace) != m) {
    replace <- rep(replace, length.out = m)
  }
  if (length(ordered) != m) {
    ordered <- rep(ordered, length.out = m)
  }
  res <- c()
  for (i in 1:m) if (isTRUE(replace[i])) {
    if (isTRUE(ordered[i])) {
      res[i] <- n[i]^k[i]
    }
    else {
      res[i] <- choose(n[i] - 1 + k[i], k[i])
    }
  }
  else {
    if (isTRUE(ordered[i])) {
      res[i] <- factorial(n[i])/factorial(n[i] - k[i])
    }
    else {
      res[i] <- choose(n[i], k[i])
    }
  }
  return(res)
}










#' Independent Identical Experiments
#' Sets up a probability space corresponding to independent, identical experiments.
#'
#' @param x a vector of outcomes
#' @param ntrials number of times to perform the experiment.
#' @param probs vector of non-negative weights corresponding to \code{x}.
#'
#' @details The elementary experiment to be repeated consists of drawing an element of \code{x} according to the probabilities contained in \code{probs}.  The entries of \code{probs} need not sum to one, but they will be normalized before any computations. If \code{probs} is not specified, the equally likely model will be assumed.
#'
#' @author G. Jay Kerns \email{gkerns@ysu.edu}.
#' @return   A data frame, with a \code{probs} column, where \code{probs} is calculated to be the probability of observing the outcome in its row under the assumption of independence and identical distribution of draws from \code{x}.
#'
#' @examples
#' iidspace( 1:6, ntrials = 3) # same as rolldie(3)
#' iidspace( 1:6, ntrials = 3, probs = 3:8 ) # unbalanced die
#'
#' @seealso \code{\link[probs]{probspace}}
#' @export
`iidspace` <- function (x, ntrials, probs = NULL){
  temp = list()
  for (i in 1:ntrials) {
    temp[[i]] <- x
  }
  res <- expand.grid(temp, KEEP.OUT.ATTRS = FALSE)
  if (is.null(probs)) {
    res$probs <- rep(1/dim(res)[1], dim(res)[1])
  }
  else {
    if (!identical(length(x), length(probs))) {
      stop("'probs' is not the same length as 'outcomes'")
    }
    if (any(probs < 0)) {
      stop("'probs' contains negative values")
    }
    probs <- probs/sum(probs)
    ptemp = list()
    for (i in 1:ntrials) {
      ptemp[[i]] <- probs
    }
    pdf <- expand.grid(ptemp, KEEP.OUT.ATTRS = FALSE)
    pres <- apply(pdf, 1, prod)
    res$probs <- pres
  }
  names(res) <- c(paste(rep("X", ntrials), 1:ntrials, sep = ""),
                  "probs")
  return(res)
}



#' Testing for a Probability Space
#' Decides whether a given object is a probability space.
#'
#' @param x an object for which probability space status should be checked.
#'
#' @details It first checks if the class of the object includes \code{ps}, and if so \code{TRUE} is returned.  If not, then it checks that the object is a data frame and contains a \code{probs} column.  Lastly, it checks whether all entries of \code{probs} are nonnegative.   Note that it does not check whether the sum of \code{probs} is one, to allow for the possibility that the input object is a proper subset of a probability space.
#' @author G. Jay Kerns \email{gkerns@ysu.edu}.
#' @seealso \code{\link[probs]{probspace}}
#' @return Logical.
#' @examples
#' S <- rolldie(3, makespace = TRUE)
#' is.probspace(S)
#'
#' @export
`is.probspace` <- function (x){
  if (any(class(x) == "ps"))
    return(TRUE)
  if (!is.data.frame(x) | is.null(x$probs))
    return(FALSE)
  if (any(x$probs < 0))
    return(FALSE)
  return(TRUE)
}


#' Probability Spaces
#' Forms a probability space from a set of outcomes and (optional) vector of probabilities.
#'
#' @param x a vector, data frame, or list of outcomes.
#' @param ... further arguments to be passed to or from other methods.
#' @details The elements of \code{probs} will be normalized to ensure that their sum is one.  If \code{probs} is not specified, then the equally likely model is assumed in which every outcome has the same probability.
#' @author G. Jay Kerns \email{gkerns@ysu.edu}.
#' @return If \code{outcomes} is a vector or data frame, then the value is a data frame with an added \code{probs} column.  If \code{outcomes} is a list, then the value is a list with components \code{outcomes} (the supplied list) and a \code{probs} component.
#' @examples
#' R <- rolldie(3)
#' probspace(R)
#' @export
`probspace` <- function (x, ...)
  UseMethod("probspace")


#' Probability Spaces
#' Forms a probability space from a set of outcomes and (optional) vector of probabilities.
#'
#' @param x a vector, data frame, or list of outcomes.
#' @param probs a vector of non-negative weights of the same length as \code{outcomes}
#' @param ... further arguments to be passed to or from other methods.
#' @details The elements of \code{probs} will be normalized to ensure that their sum is one.  If \code{probs} is not specified, then the equally likely model is assumed in which every outcome has the same probability.
#' @author G. Jay Kerns \email{gkerns@ysu.edu}.
#' @return If \code{outcomes} is a vector or data frame, then the value is a data frame with an added \code{probs} column.  If \code{outcomes} is a list, then the value is a list with components \code{outcomes} (the supplied list) and a \code{probs} component.
#' @examples
#' R <- rolldie(3)
#' probspace(R)
#' @export
`probspace.default` <- function (x, probs, ...){
  y <- data.frame(x)
  if (missing(probs)) {
    y$probs <- rep(1, dim(y)[1])/dim(y)[1]
  }
  else {
    if (any(probs < 0)) {
      stop("'probs' contains negative values")
    }
    y$probs <- probs/sum(probs)
  }
  return(y)
}


#' Probability Spaces
#' Forms a probability space from a set of outcomes and (optional) vector of probabilities.
#'
#' @param x a vector, data frame, or list of outcomes.
#' @param probs a vector of non-negative weights of the same length as \code{outcomes}
#' @param ... further arguments to be passed to or from other methods.
#' @details The elements of \code{probs} will be normalized to ensure that their sum is one.  If \code{probs} is not specified, then the equally likely model is assumed in which every outcome has the same probability.
#' @return If \code{outcomes} is a vector or data frame, then the value is a data frame with an added \code{probs} column.  If \code{outcomes} is a list, then the value is a list with components \code{outcomes} (the supplied list) and a \code{probs} component.
#' @author G. Jay Kerns \email{gkerns@ysu.edu}.
#' @examples
#' R <- rolldie(3)
#' probspace(R)
#' @export
`probspace.list` <- function (x, probs, ...){
  y <- x
  if (missing(probs)) {
    probs <- rep(1, length(y))/length(y)
  }
  else {
    if (any(probs < 0)) {
      stop("'probs' contains negative values")
    }
    probs <- probs/sum(probs)
  }
  res <- list(outcomes = y, probs = probs)
  class(res) <- c("ps", "list")
  return(res)
}




#' Count Repetitions
#' Counts the number of repetitions of \code{vals} in a given vector \code{x}.
#'
#' @param x an object in which repeats should be counted.
#' @param ... further arguments to be passed to or from other methods.
#'
#' @details  This is a generic function, with methods supplied for data frames and vectors. The default behavior counts the number of pairs of elements of \code{x}.  One can find the number of triples, etc., by changing the \code{nrep} argument.  If there are specific values for which one is looking for repeats, these can be specified with the \code{vals} argument.  Note that the function only checks for \emph{exactly} \code{nrep} instances, so two pairs of a specific element would be counted as 0 pairs and 1 quadruple. See the examples.
#' @author G. Jay Kerns \email{gkerns@ysu.edu}.
#' @seealso \code{\link[probs]{isrep}}
#' @return If \code{x} is a vector, then the value is an integer.  If \code{x} is a data frame then the value is a vector, with entries the corresponding value for the respective rows of \code{x}
#' @examples
#' x <- c(3,3,2,2,3,3,4,4)
#' countrep(x)  # one pair each of 2s and 4s
#' countrep(x, nrep = 4)
#' countrep(x, vals = 4) # one pair of 4s
#' @export
`countrep` <- function (x, ...)
  UseMethod("countrep")

#' Count Repetitions
#' Counts the number of repetitions of \code{vals} in a given vector \code{x}.
#'
#' @param x an object in which repeats should be counted.
#' @param ... further arguments to be passed to or from other methods.
#'
#' @details  This is a generic function, with methods supplied for data frames and vectors. The default behavior counts the number of pairs of elements of \code{x}.  One can find the number of triples, etc., by changing the \code{nrep} argument.  If there are specific values for which one is looking for repeats, these can be specified with the \code{vals} argument.  Note that the function only checks for \emph{exactly} \code{nrep} instances, so two pairs of a specific element would be counted as 0 pairs and 1 quadruple. See the examples.
#' @author G. Jay Kerns \email{gkerns@ysu.edu}.
#' @seealso \code{\link[probs]{isrep}}
#' @return If \code{x} is a vector, then the value is an integer.  If \code{x} is a data frame then the value is a vector, with entries the corresponding value for the respective rows of \code{x}
#' @examples
#' x <- c(3,3,2,2,3,3,4,4)
#' countrep(x)  # one pair each of 2s and 4s
#' countrep(x, nrep = 4)
#' countrep(x, vals = 4) # one pair of 4s
#' @export
`countrep.data.frame` <- function (x, ...){
  apply(x, MARGIN = 1, FUN = countrep, ...)
}



#' Count Repetitions
#' Counts the number of repetitions of \code{vals} in a given vector \code{x}.
#'
#' @param x an object in which repeats should be counted.
#' @param vals values that may be repeated.
#' @param nrep exact number of repeats desired, defaults to pairs.
#' @param ... further arguments to be passed to or from other methods.
#'
#' @details  This is a generic function, with methods supplied for data frames and vectors. The default behavior counts the number of pairs of elements of \code{x}.  One can find the number of triples, etc., by changing the \code{nrep} argument.  If there are specific values for which one is looking for repeats, these can be specified with the \code{vals} argument.  Note that the function only checks for \emph{exactly} \code{nrep} instances, so two pairs of a specific element would be counted as 0 pairs and 1 quadruple. See the examples.
#' @author G. Jay Kerns \email{gkerns@ysu.edu}.
#' @seealso \code{\link[probs]{isrep}}
#' @return If \code{x} is a vector, then the value is an integer.  If \code{x} is a data frame then the value is a vector, with entries the corresponding value for the respective rows of \code{x}
#' @examples
#' x <- c(3,3,2,2,3,3,4,4)
#' countrep(x)  # one pair each of 2s and 4s
#' countrep(x, nrep = 4)
#' countrep(x, vals = 4) # one pair of 4s
#' @export
`countrep.default` <- function (x, vals = unique(x), nrep = 2, ...){
  res <- 0
  if (length(x) >= nrep) {
    for (i in 1:length(vals)) {
      if (sum(mapply(all.equal, x, vals[i]) == TRUE) ==
          nrep) {
        res <- res + 1
      }
    }
  }
  return(res)
}



#' Test Whether One Vector Is In Another Vector
#' @param x vectors
#' @param ... further arguments to be passed to or from other methods.
#' @details
#' The function will only return \code{TRUE} if every element of \code{y} is present in the vector \code{x}, counting multiplicity.  See the examples below.  Of \code{ordered = TRUE}, then elements must be in the vector \code{x} in the order specified in \code{y}.  Compare this to the behavior of the \code{\%in\%} function in the \code{base} package.
#' This is a generic function with a method for data frames, which applies \code{isin()} to each row of the data frame, with a vector as a result.
#' @author G. Jay Kerns \email{gkerns@ysu.edu}.
#' @seealso \code{\link[probs]{isrep}}
#' @return Logical, or a vector of logicals.
#' @examples
#' x <- 1:10
#' y <- 3:7
#' z <- c(3,3,7)
#' isin(x,y)
#' isin(x,z)
#' isin(x, c(3,4,5), ordered = TRUE)
#' isin(x, c(3,5,4), ordered = TRUE)
#'
#'S <- rolldie(4)
#'subset(S, isin(S, c(2,2,6), ordered = TRUE))
#'
#' @export
`isin` <- function (x, ...)
  UseMethod("isin")



#' Test Whether One Vector Is In Another Vector
#' @param x vectors
#' @param ... further arguments to be passed to or from other methods.
#' @details
#' The function will only return \code{TRUE} if every element of \code{y} is present in the vector \code{x}, counting multiplicity.  See the examples below.  Of \code{ordered = TRUE}, then elements must be in the vector \code{x} in the order specified in \code{y}.  Compare this to the behavior of the \code{\%in\%} function in the \code{base} package.
#' This is a generic function with a method for data frames, which applies \code{isin()} to each row of the data frame, with a vector as a result.
#' @author G. Jay Kerns \email{gkerns@ysu.edu}.
#' @seealso \code{\link[probs]{isrep}}
#' @return Logical, or a vector of logicals.
#' @examples
#' x <- 1:10
#' y <- 3:7
#' z <- c(3,3,7)
#' isin(x,y)
#' isin(x,z)
#' isin(x, c(3,4,5), ordered = TRUE)
#' isin(x, c(3,5,4), ordered = TRUE)
#'
#'S <- rolldie(4)
#'subset(S, isin(S, c(2,2,6), ordered = TRUE))
#'
#' @export
`isin.data.frame` <- function (x, ...){
  apply(x, MARGIN = 1, FUN = isin, ...)
}


#' Test Whether One Vector Is In Another Vector
#' @param x vectors
#' @param y vectors
#' @param ordered logical
#' @param ... further arguments to be passed to or from other methods.
#' @details
#' The function will only return \code{TRUE} if every element of \code{y} is present in the vector \code{x}, counting multiplicity.  See the examples below.  Of \code{ordered = TRUE}, then elements must be in the vector \code{x} in the order specified in \code{y}.  Compare this to the behavior of the \code{\%in\%} function in the \code{base} package.
#' This is a generic function with a method for data frames, which applies \code{isin()} to each row of the data frame, with a vector as a result.
#' @author G. Jay Kerns \email{gkerns@ysu.edu}.
#' @return Logical, or a vector of logicals.
#' @seealso \code{\link[probs]{isrep}}
#' @examples
#' x <- 1:10
#' y <- 3:7
#' z <- c(3,3,7)
#' isin(x,y)
#' isin(x,z)
#' isin(x, c(3,4,5), ordered = TRUE)
#' isin(x, c(3,5,4), ordered = TRUE)
#'
#'S <- rolldie(4)
#'subset(S, isin(S, c(2,2,6), ordered = TRUE))
#'
#' @export
`isin.default` <- function (x, y, ordered = FALSE, ...){
  res <- (length(y) <= length(x))
  if (res) {
    temp <- x
    for (i in 1:length(y)) {
      if (is.element(y[i], temp)) {
        if (!ordered) {
          temp <- temp[-which(temp %in% y[i])[1]]
        }
        else {
          temp <- temp[-(1:which(temp %in% y[i])[1])]
        }
      }
      else {
        res <- FALSE
        i <- length(y)
      }
    }
  }
  return(res)
}



#' Is Repeated in a Vector
#' Tests for a certain number of repetitions of \code{vals} in a given vector \code{x}.
#'
#' @param x an object with potential repeated values.
#' @param ... further arguments to be passed to or from other methods.
#'
#' @details   This is a generic function, with methods supplied for data frames and vectors. The default behavior tests for existence of pairs of elements of \code{x}.  One can test existence of triples, etc., by changing the \code{nrep} argument.  If there are specific values for which one is looking for repeats, these can be specified with the \code{vals} argument.  Note that the function only checks for \emph{exactly} \code{nrep} instances, so two pairs of a specific element would be counted as 0 pairs and 1 quadruple. See the examples.
#' The data frame method uses \code{apply} to apply \code{isrep.default} to each row of the data frame.
#'
#' @author G. Jay Kerns \email{gkerns@ysu.edu}.
#' @seealso \code{\link[probs]{countrep}}
#' @return Logical.
#' @examples
#' x <- c(3,3,2,2,3,3,4,4)
#' isrep(x)  # one pair each of 2s and 4s
#' isrep(x, nrep = 4)
#' isrep(x, vals = 4) # one pair of 4s
#'
#' @export
`isrep` <- function (x, ...)
  UseMethod("isrep")


#' Is Repeated in a Vector
#' Tests for a certain number of repetitions of \code{vals} in a given vector \code{x}.
#'
#' @param x an object with potential repeated values.
#' @param ... further arguments to be passed to or from other methods.
#'
#' @details   This is a generic function, with methods supplied for data frames and vectors. The default behavior tests for existence of pairs of elements of \code{x}.  One can test existence of triples, etc., by changing the \code{nrep} argument.  If there are specific values for which one is looking for repeats, these can be specified with the \code{vals} argument.  Note that the function only checks for \emph{exactly} \code{nrep} instances, so two pairs of a specific element would be counted as 0 pairs and 1 quadruple. See the examples.
#' The data frame method uses \code{apply} to apply \code{isrep.default} to each row of the data frame.
#'
#' @author G. Jay Kerns \email{gkerns@ysu.edu}.
#' @seealso \code{\link[probs]{countrep}}
#' @return Logical.
#' @examples
#' x <- c(3,3,2,2,3,3,4,4)
#' isrep(x)  # one pair each of 2s and 4s
#' isrep(x, nrep = 4)
#' isrep(x, vals = 4) # one pair of 4s
#'
#' @export
`isrep.data.frame` <- function (x, ...){
  apply(x, MARGIN = 1, FUN = isrep, ...)
}



#' Is Repeated in a Vector
#' Tests for a certain number of repetitions of \code{vals} in a given vector \code{x}.
#'
#' @param x an object with potential repeated values.
#' @param vals values that may be repeated.
#' @param nrep exact number of repeats desired, defaults to pairs.
#' @param ... further arguments to be passed to or from other methods.
#'
#' @details   This is a generic function, with methods supplied for data frames and vectors. The default behavior tests for existence of pairs of elements of \code{x}.  One can test existence of triples, etc., by changing the \code{nrep} argument.  If there are specific values for which one is looking for repeats, these can be specified with the \code{vals} argument.  Note that the function only checks for \emph{exactly} \code{nrep} instances, so two pairs of a specific element would be counted as 0 pairs and 1 quadruple. See the examples.
#' The data frame method uses \code{apply} to apply \code{isrep.default} to each row of the data frame.
#'
#' @author G. Jay Kerns \email{gkerns@ysu.edu}.
#' @seealso \code{\link[probs]{countrep}}
#' @return Logical.
#' @examples
#' x <- c(3,3,2,2,3,3,4,4)
#' isrep(x)  # one pair each of 2s and 4s
#' isrep(x, nrep = 4)
#' isrep(x, vals = 4) # one pair of 4s
#'
#' @export
`isrep.default` <- function (x, vals = unique(x), nrep = 2, ...){
  res <- FALSE
  if (length(x) >= nrep) {
    for (i in 1:length(vals)) {
      if (sum(mapply(all.equal, x, vals[i]) == TRUE) ==
          nrep) {
        res <- TRUE
        i <- length(vals)
      }
    }
  }
  return(res)
}












#' Intersection of Subsets
#' Calculates the intersection of subsets of a probability space.  Comparisons are made row-wise, so that in the data frame case, \code{intersect(A,B)} is a data frame with those rows that are both in \code{A} and in \code{B}.
#'
#' @param x vectors, data frames, or \code{ps} objects containing a sequence of elements (conceptually).
#' @param ... further arguments to be passed to or from other methods.
#' @details This is a generic function, extended from the \code{intersect} function in the \code{base} package.  The elements of \code{intersect(x,y)} are those elements in \code{x} and in \code{y}. The original definition is preserved in the case that \code{x} and \code{y} are vectors of the same mode.
#'
#' @author G. Jay Kerns \email{gkerns@ysu.edu}, based on a suggestion made by Brian Ripley in \code{R}-devel, 12/11/07.
#' @seealso \code{\link[probs]{union}}, \code{\link[probs]{setdiff}}
#' @return A vector, data frame, or subset of a probability space of the same type as its arguments.
#' @examples
#' S <- cards()
#' A <- subset(S, suit == "Heart")
#' B <- subset(S, rank == "A" )
#' intersect(A, B)
#' @export
`intersect` <- function (x, ...)
  UseMethod("intersect")


#' Intersection of Subsets
#' Calculates the intersection of subsets of a probability space.  Comparisons are made row-wise, so that in the data frame case, \code{intersect(A,B)} is a data frame with those rows that are both in \code{A} and in \code{B}.
#'
#' @param x vectors, data frames, or \code{ps} objects containing a sequence of elements (conceptually).
#' @param y vectors, data frames, or \code{ps} objects containing a sequence of elements (conceptually).
#' @param ... further arguments to be passed to or from other methods.
#' @details This is a generic function, extended from the \code{intersect} function in the \code{base} package.  The elements of \code{intersect(x,y)} are those elements in \code{x} and in \code{y}. The original definition is preserved in the case that \code{x} and \code{y} are vectors of the same mode.
#'
#' @author G. Jay Kerns \email{gkerns@ysu.edu}, based on a suggestion made by Brian Ripley in \code{R}-devel, 12/11/07.
#' @seealso \code{\link[probs]{union}}, \code{\link[probs]{setdiff}}
#' @return A vector, data frame, or subset of a probability space of the same type as its arguments.
#' @examples
#' S <- cards()
#' A <- subset(S, suit == "Heart")
#' B <- subset(S, rank == "A" )
#' intersect(A, B)
#' @export
`intersect.data.frame` <- function (x, y, ...){
  a <- do.call("paste", c(x, sep = "\r"))
  b <- do.call("paste", c(y, sep = "\r"))
  x[match(intersect(a, b), a), ]
}




#' Intersection of Subsets
#' Calculates the intersection of subsets of a probability space.  Comparisons are made row-wise, so that in the data frame case, \code{intersect(A,B)} is a data frame with those rows that are both in \code{A} and in \code{B}.
#'
#' @param x vectors, data frames, or \code{ps} objects containing a sequence of elements (conceptually).
#' @param y vectors, data frames, or \code{ps} objects containing a sequence of elements (conceptually).
#' @param ... further arguments to be passed to or from other methods.
#' @details This is a generic function, extended from the \code{intersect} function in the \code{base} package.  The elements of \code{intersect(x,y)} are those elements in \code{x} and in \code{y}. The original definition is preserved in the case that \code{x} and \code{y} are vectors of the same mode.
#'
#' @author G. Jay Kerns \email{gkerns@ysu.edu}, based on a suggestion made by Brian Ripley in \code{R}-devel, 12/11/07.
#' @seealso \code{\link[probs]{union}}, \code{\link[probs]{setdiff}}
#' @return A vector, data frame, or subset of a probability space of the same type as its arguments.
#' @examples
#' S <- cards()
#' A <- subset(S, suit == "Heart")
#' B <- subset(S, rank == "A" )
#' intersect(A, B)
#' @export
`intersect.default` <- function (x, y, ...){
  y <- as.vector(y)
  unique(y[match(as.vector(x), y, 0)])
}




#' Intersection of Subsets
#' Calculates the intersection of subsets of a probability space.  Comparisons are made row-wise, so that in the data frame case, \code{intersect(A,B)} is a data frame with those rows that are both in \code{A} and in \code{B}.
#'
#' @param x vectors, data frames, or \code{ps} objects containing a sequence of elements (conceptually).
#' @param y vectors, data frames, or \code{ps} objects containing a sequence of elements (conceptually).
#' @param ... further arguments to be passed to or from other methods.
#' @details This is a generic function, extended from the \code{intersect} function in the \code{base} package.  The elements of \code{intersect(x,y)} are those elements in \code{x} and in \code{y}. The original definition is preserved in the case that \code{x} and \code{y} are vectors of the same mode.
#'
#' @author G. Jay Kerns \email{gkerns@ysu.edu}, based on a suggestion made by Brian Ripley in \code{R}-devel, 12/11/07.
#' @seealso \code{\link[probs]{union}}, \code{\link[probs]{setdiff}}
#' @return A vector, data frame, or subset of a probability space of the same type as its arguments.
#' @examples
#' S <- cards()
#' A <- subset(S, suit == "Heart")
#' B <- subset(S, rank == "A" )
#' intersect(A, B)
#' @export
`intersect.ps` <- function (x, y, ...){
  a <- do.call("paste", c(x, sep = "\r"))
  b <- do.call("paste", c(y, sep = "\r"))
  e <- match(intersect(a, b), a)
  res <- list(outcomes = x$outcomes[e], probs = x$probs[e])
  class(res) <- c("ps", "list")
  return(res)
}


#' Set Difference of Subsets
#' Calculates the (nonsymmetric) set difference of subsets of a probability space.
#'
#' @param x vectors, data frames, or \code{ps} objects containing a sequence of items (conceptually).
#' @param ... further arguments to be passed to or from other methods.
#' @details This function operates row-wise on dataframes, and element-wise among the outcomes of \code{ps} objects.  The elements of \code{setdiff(x,y)} are those elements in \code{x} but not in \code{y}. The definition is taken to match the version in the \code{base} package.
#'
#' @author G. Jay Kerns \email{gkerns@ysu.edu}, essentially verbatim from a suggestion made by Brian Ripley on \code{R}-devel, 12/11/07
#' @seealso \code{\link[probs]{intersect}}, \code{\link[probs]{union}}
#' @return  A data frame or subset of a probability space of the same type as its arguments.
#' @examples
#' S <- cards()
#' A <- subset(S, suit == "Heart")
#' B <- subset(S, rank == "A" )
#' setdiff(B, A)
#'
#' @export
`setdiff` <- function (x, ...)
  UseMethod("setdiff")


#' Set Difference of Subsets
#' Calculates the (nonsymmetric) set difference of subsets of a probability space.
#'
#' @param x vectors, data frames, or \code{ps} objects containing a sequence of items (conceptually).
#' @param y vectors, data frames, or \code{ps} objects containing a sequence of items (conceptually).
#' @param ... further arguments to be passed to or from other methods.
#' @details This function operates row-wise on dataframes, and element-wise among the outcomes of \code{ps} objects.  The elements of \code{setdiff(x,y)} are those elements in \code{x} but not in \code{y}. The definition is taken to match the version in the \code{base} package.
#'
#' @author G. Jay Kerns \email{gkerns@ysu.edu}, essentially verbatim from a suggestion made by Brian Ripley on \code{R}-devel, 12/11/07
#' @seealso \code{\link[probs]{intersect}}, \code{\link[probs]{union}}
#' @return  A data frame or subset of a probability space of the same type as its arguments.
#' @examples
#' S <- cards()
#' A <- subset(S, suit == "Heart")
#' B <- subset(S, rank == "A" )
#' setdiff(B, A)
#'
#' @export
`setdiff.data.frame` <- function (x, y, ...){
  a <- do.call("paste", c(x, sep = "\r"))
  b <- do.call("paste", c(y, sep = "\r"))
  x[match(setdiff(a, b), a), ]
}

#' Set Difference of Subsets
#' Calculates the (nonsymmetric) set difference of subsets of a probability space.
#'
#' @param x vectors, data frames, or \code{ps} objects containing a sequence of items (conceptually).
#' @param y vectors, data frames, or \code{ps} objects containing a sequence of items (conceptually).
#' @param ... further arguments to be passed to or from other methods.
#' @details This function operates row-wise on dataframes, and element-wise among the outcomes of \code{ps} objects.  The elements of \code{setdiff(x,y)} are those elements in \code{x} but not in \code{y}. The definition is taken to match the version in the \code{base} package.
#'
#' @author G. Jay Kerns \email{gkerns@ysu.edu}, essentially verbatim from a suggestion made by Brian Ripley on \code{R}-devel, 12/11/07
#' @seealso \code{\link[probs]{intersect}}, \code{\link[probs]{union}}
#' @return  A data frame or subset of a probability space of the same type as its arguments.
#' @examples
#' S <- cards()
#' A <- subset(S, suit == "Heart")
#' B <- subset(S, rank == "A" )
#' setdiff(B, A)
#'
#' @export
`setdiff.default` <- function (x, y, ...){
  x <- as.vector(x)
  y <- as.vector(y)
  unique(if (length(x) || length(y))
    x[match(x, y, 0) == 0]
    else x)
}

#' Set Difference of Subsets
#' Calculates the (nonsymmetric) set difference of subsets of a probability space.
#'
#' @param x vectors, data frames, or \code{ps} objects containing a sequence of items (conceptually).
#' @param y vectors, data frames, or \code{ps} objects containing a sequence of items (conceptually).
#' @param ... further arguments to be passed to or from other methods.
#' @details This function operates row-wise on dataframes, and element-wise among the outcomes of \code{ps} objects.  The elements of \code{setdiff(x,y)} are those elements in \code{x} but not in \code{y}. The definition is taken to match the version in the \code{base} package.
#'
#' @author G. Jay Kerns \email{gkerns@ysu.edu}, essentially verbatim from a suggestion made by Brian Ripley on \code{R}-devel, 12/11/07
#' @seealso \code{\link[probs]{intersect}}, \code{\link[probs]{union}}
#' @return  A data frame or subset of a probability space of the same type as its arguments.
#' @examples
#' S <- cards()
#' A <- subset(S, suit == "Heart")
#' B <- subset(S, rank == "A" )
#' setdiff(B, A)
#'
#' @export
`setdiff.ps` <- function (x, y, ...){
  a <- do.call("paste", c(x, sep = "\r"))
  b <- do.call("paste", c(y, sep = "\r"))
  e <- match(setdiff(a, b), a)
  res <- list(outcomes = x$outcomes[e], probs = x$probs[e])
  class(res) <- c("ps", "list")
  return(res)
}



#' Subsets of Probability Spaces
#'  This is a method for \code{subset()} for the case when the input object is a probability space of class \code{ps}.
#' @param x a probability space.
#' @param subset logical expression indicating elements or rows of \code{space} to keep: missing values are taken as false.
#' @param ... further arguments to be passed to or from other methods.
#' @details This function simply extends the existing \code{subset()} function to \code{ps} objects.
#'
#' @author G. Jay Kerns \email{gkerns@ysu.edu}.
#' @return A \code{ps} object, a subset of a probability space.
#' @examples
#' L <- tosscoin(2)
#' M <- urnsamples(L, 3)
#' N <- probspace(M)
#' subset(N, all(toss1=="H"))
#' subset(N, any(toss2=="T"))
#' @export
`subset.ps` <- function (x, subset, ...){
  e <- substitute(subset)
  r <- sapply(x$outcomes, function(t) {
    eval(e, t)
  })
  if (!is.logical(r))
    stop("'subset' must be logical")
  res <- list(outcomes = x$outcomes[r & !is.na(r)], probs = x$probs[r &
                                                                      !is.na(r)])
  class(res) <- c("ps", "list")
  return(res)
}


#' Union of Subsets
#' Calculates the union of subsets of a probability space.
#'
#' @param x vectors, data frames, or \code{ps} objects containing a sequence of items (conceptually)
#' @param ... further arguments to be passed to or from other methods.
#' @details This function operates row-wise on dataframes, and element-wise among the outcomes of \code{ps} objects. The elements of \code{union(x,y)} are those elements in \code{x} or \code{y}, or both. The definition is taken to match the version in the \code{base} package.
#' @return A data frame or subset of a probability space of the same type as its arguments.
#' @author G. Jay Kerns \email{gkerns@ysu.edu}, based on a suggestion made by Brian Ripley in \code{R}-devel, 12/11/07.
#' @examples
#' S <- cards()
#' A <- subset(S, suit == "Heart")
#' B <- subset(S, rank == "A" )
#' union(A, B)
#' @export
`union` <- function (x, ...)
  UseMethod("union")


#' Union of Subsets
#' Calculates the union of subsets of a probability space.
#'
#' @param x vectors, data frames, or \code{ps} objects containing a sequence of items (conceptually)
#' @param y vectors, data frames, or \code{ps} objects containing a sequence of items (conceptually)
#' @param ... further arguments to be passed to or from other methods.
#' @details This function operates row-wise on dataframes, and element-wise among the outcomes of \code{ps} objects. The elements of \code{union(x,y)} are those elements in \code{x} or \code{y}, or both. The definition is taken to match the version in the \code{base} package.
#' @return A data frame or subset of a probability space of the same type as its arguments.
#' @author G. Jay Kerns \email{gkerns@ysu.edu}, based on a suggestion made by Brian Ripley in \code{R}-devel, 12/11/07.
#' @examples
#' S <- cards()
#' A <- subset(S, suit == "Heart")
#' B <- subset(S, rank == "A" )
#' union(A, B)
#' @export
`union.data.frame` <- function (x, y, ...){
  res <- unique(rbind(as.data.frame(y), x))
  res[order(as.numeric(rownames(res))), ]
}


#' Union of Subsets
#' Calculates the union of subsets of a probability space.
#'
#' @param x vectors, data frames, or \code{ps} objects containing a sequence of items (conceptually)
#' @param y vectors, data frames, or \code{ps} objects containing a sequence of items (conceptually)
#' @param ... further arguments to be passed to or from other methods.
#' @details This function operates row-wise on dataframes, and element-wise among the outcomes of \code{ps} objects. The elements of \code{union(x,y)} are those elements in \code{x} or \code{y}, or both. The definition is taken to match the version in the \code{base} package.
#' @return A data frame or subset of a probability space of the same type as its arguments.
#' @author G. Jay Kerns \email{gkerns@ysu.edu}, based on a suggestion made by Brian Ripley in \code{R}-devel, 12/11/07.
#' @examples
#' S <- cards()
#' A <- subset(S, suit == "Heart")
#' B <- subset(S, rank == "A" )
#' union(A, B)
#' @export
`union.default` <- function (x, y, ...)
  unique(c(as.vector(x), as.vector(y)))

#' Union of Subsets
#' Calculates the union of subsets of a probability space.
#'
#' @param x vectors, data frames, or \code{ps} objects containing a sequence of items (conceptually)
#' @param y vectors, data frames, or \code{ps} objects containing a sequence of items (conceptually)
#' @param ... further arguments to be passed to or from other methods.
#' @details This function operates row-wise on dataframes, and element-wise among the outcomes of \code{ps} objects. The elements of \code{union(x,y)} are those elements in \code{x} or \code{y}, or both. The definition is taken to match the version in the \code{base} package.
#' @return A data frame or subset of a probability space of the same type as its arguments.
#' @author G. Jay Kerns \email{gkerns@ysu.edu}, based on a suggestion made by Brian Ripley in \code{R}-devel, 12/11/07.
#' @examples
#' S <- cards()
#' A <- subset(S, suit == "Heart")
#' B <- subset(S, rank == "A" )
#' union(A, B)
#' @export
`union.ps` <- function (x, y, ...){
  na <- length(x$outcomes)
  nb <- length(y$outcomes)
  temp <- x
  for (i in 1:nb) {
    temp$outcomes[[na + i]] <- y$outcomes[[i]]
    temp$probs[[na + i]] <- y$probs[[i]]
  }
  a <- do.call("paste", c(temp, sep = "\r"))
  e <- !duplicated(a)
  res <- list(outcomes = temp$outcomes[e], probs = temp$probs[e])
  class(res) <- c("ps", "list")
  return(res)
}



#' Probability and Conditional Probability
#' Calculates probability and conditional probability of events.
#'
#' @param x a probability space or a subset of one.
#' @param ... further arguments to be passed to or from other methods.
#' @details   This function calculates the probability of events or subsets of a given sample space.
#' Conditional probability is also implemented.  In essence, the \code{Prob()} function operates by summing the \code{probs} column of its argument.  It will find subsets on the fly if desired.
#' The \code{event} argument is used to define a subset of \code{x}, that is, the only outcomes used in the probability calculation will be those that are elements of \code{x} and satisfy \code{event} simultaneously. In other words, \code{Prob(x,event)} calculates \code{Prob(intersect(x, subset(x, event)))}.  Consequently, \code{x} should be the entire probability space in the case that \code{event} is non-null.
#' There is some flexibility in the \code{given} argument in that it can be either a data frame or it can be a logical expression that defines the subset.  However, that flexibility is limited.  In particular, if \code{given} is a logical expression, then \code{event} must also be specified (also a logical expression).  And in this case, the argument \code{x} should be the entire sample space, not a subset thereof.
#' @return A number in the interval \code{[0,1]}.
#' @author G. Jay Kerns \email{gkerns@ysu.edu}.
#' @seealso \code{\link[probs]{probspace}}, \code{\link[probs]{iidspace}}
#' @examples
#' S <- rolldie(times = 3, makespace = TRUE )
#' Prob(S, X1+X2 > 9 )
#' Prob(S, X1+X2 > 9, given = X1+X2+X3 > 7 )
#' @export
`Prob` <- function (x, ...)
  UseMethod("Prob")


#' Probability and Conditional Probability
#' Calculates probability and conditional probability of events.
#'
#' @param x a probability space or a subset of one.
#' @param event logical expression indicating elements or rows of \code{space} to keep: missing values are taken as false.
#' @param given either a subset of a probability space or a logical expression indicating elements or rows of \code{space} to keep: missing values are taken as false.
#' @param ... further arguments to be passed to or from other methods.
#' @details   This function calculates the probability of events or subsets of a given sample space.
#' Conditional probability is also implemented.  In essence, the \code{Prob()} function operates by summing the \code{probs} column of its argument.  It will find subsets on the fly if desired.
#' The \code{event} argument is used to define a subset of \code{x}, that is, the only outcomes used in the probability calculation will be those that are elements of \code{x} and satisfy \code{event} simultaneously. In other words, \code{Prob(x,event)} calculates \code{Prob(intersect(x, subset(x, event)))}.  Consequently, \code{x} should be the entire probability space in the case that \code{event} is non-null.
#' There is some flexibility in the \code{given} argument in that it can be either a data frame or it can be a logical expression that defines the subset.  However, that flexibility is limited.  In particular, if \code{given} is a logical expression, then \code{event} must also be specified (also a logical expression).  And in this case, the argument \code{x} should be the entire sample space, not a subset thereof.
#' @return A number in the interval \code{[0,1]}.
#' @author G. Jay Kerns \email{gkerns@ysu.edu}.
#' @seealso \code{\link[probs]{probspace}}, \code{\link[probs]{iidspace}}
#' @examples
#' S <- rolldie(times = 3, makespace = TRUE )
#' Prob(S, X1+X2 > 9 )
#' Prob(S, X1+X2 > 9, given = X1+X2+X3 > 7 )
#' @export
`Prob.default` <- function (x, event = NULL, given = NULL, ...){
  if (is.null(x$probs)) {
    message("'space' is missing a probs column")
    stop("see ?probspace")
  }
  if (missing(event)) {
    r <- TRUE
  }
  else {
    e <- substitute(event)
    r <- eval(e, x, parent.frame())
    if (!is.logical(r))
      stop("'event' must evaluate to logical")
    r <- r & !is.na(r)
    if (!isTRUE(all.equal(sum(x$probs), 1)))
      warning("'space' does not have probability 1.")
  }
  A <- x[r, ]
  if (missing(given)) {
    p <- sum(A$probs)
  }
  else {
    f <- substitute(given)
    g <- eval(f, x, enclos = parent.frame())
    if (!is.logical(g)) {
      if (!is.data.frame(given))
        stop("'given' must be data.frame or evaluate to logical")
      B <- given
    }
    else {
      if (missing(event))
        stop("'event' must be specified when 'given' is an expression")
      g <- g & !is.na(g)
      B <- x[g, ]
    }
    if (sum(B$probs <= 0))
      stop("prob(given) must be positive")
    p <- sum(intersect(A, B)$probs)/sum(B$probs)
  }
  return(p)
}


#' Probability and Conditional Probability
#' Calculates probability and conditional probability of events.
#'
#' @param x a probability space or a subset of one.
#' @param event logical expression indicating elements or rows of \code{space} to keep: missing values are taken as false.
#' @param given either a subset of a probability space or a logical expression indicating elements or rows of \code{space} to keep: missing values are taken as false.
#' @param ... further arguments to be passed to or from other methods.
#' @details   This function calculates the probability of events or subsets of a given sample space.
#' Conditional probability is also implemented.  In essence, the \code{Prob()} function operates by summing the \code{probs} column of its argument.  It will find subsets on the fly if desired.
#' The \code{event} argument is used to define a subset of \code{x}, that is, the only outcomes used in the probability calculation will be those that are elements of \code{x} and satisfy \code{event} simultaneously. In other words, \code{Prob(x,event)} calculates \code{Prob(intersect(x, subset(x, event)))}.  Consequently, \code{x} should be the entire probability space in the case that \code{event} is non-null.
#' There is some flexibility in the \code{given} argument in that it can be either a data frame or it can be a logical expression that defines the subset.  However, that flexibility is limited.  In particular, if \code{given} is a logical expression, then \code{event} must also be specified (also a logical expression).  And in this case, the argument \code{x} should be the entire sample space, not a subset thereof.
#' @return A number in the interval \code{[0,1]}.
#' @author G. Jay Kerns \email{gkerns@ysu.edu}.
#' @seealso \code{\link[probs]{probspace}}, \code{\link[probs]{iidspace}}
#' @examples
#' S <- rolldie(times = 3, makespace = TRUE )
#' Prob(S, X1+X2 > 9 )
#' Prob(S, X1+X2 > 9, given = X1+X2+X3 > 7 )
#' @export
`Prob.ps` <- function (x, event = NULL, given = NULL, ...){
  if (is.null(x$probs)) {
    message("'space' is missing a probs component")
    stop("see ?probspace")
  }
  if (missing(event)) {
    A <- x
  }
  else {
    e <- substitute(event)
    r <- sapply(x$outcomes, function(t) {
      eval(e, t, enclos=parent.frame())
    })
    if (!is.logical(r))
      stop("'event' must evaluate to logical")
    r <- r & !is.na(r)
    if (!isTRUE(all.equal(sum(x$probs), 1)))
      warning("'space' does not have probability 1.")
    A <- list(outcomes = x$outcomes[r], probs = x$probs[r])
  }
  if (missing(given)) {
    p <- sum(A$probs)
  }
  else {
    f <- substitute(given)
    g <- sapply(x$outcomes, function(t) {
      eval(f, t, enclos=parent.frame())
    })
    if (!is.logical(g)) {
      if (!is.probspace(given))
        stop("'given' must be a probspace or evaluate to logical")
      B <- given
    }
    else {
      if (missing(event))
        stop("'event' must be specified when 'given' is an expression")
      g <- g & !is.na(g)
      B <- list(outcomes = x$outcomes[g], probs = x$probs[g])
    }
    if (sum(B$probs <= 0))
      stop("prob(given) must be positive")
    p <- sum(intersect(A, B)$probs)/sum(B$probs)
  }
  return(p)
}



#' Empirical Summary of a Simulation
#'
#' Calculates relative frequencies of the rows of a data frame.
#'
#' @param x a data frame.
#'
#' @details The function works by adding a \code{probs} column to \code{x} with equally likely entries of \eqn{1/n}, where \eqn{n} is the number of rows. Then it aggregates the duplicated rows of \code{x} while accumulating the probabilities associated with each.
#'
#' @return A data frame formed by aggregating the rows of \code{x}. A \code{probs} column is added giving the relative frequencies of each of the rows.
#'
#' @author G. Jay Kerns \email{gkerns@ysu.edu}.
#'
#' @seealso \code{\link[probs]{sim}}
#'
#' @examples
#' S <- tosscoin(2, makespace = TRUE)
#' sims <- sim(S, ntrials = 50000)
#' empirical(sims)
#'
#' @keywords manip
#' @name empirical
#' @export
`empirical` <- function (x){
  if (any(class(x) == "ps"))
    stop("not implemented for class 'ps'")
  if (!is.data.frame(x))
    message("'x' must be a data frame")
  temp <- x
  n <- dim(temp)[1]
  vars <- names(temp)
  temp$probs <- rep(1, n)/n
  return(marginal(temp))
}


#' Simulate Draws from a Sample Space
#'
#' Simulates the experiment of drawing from a sample space.
#'
#' @param x a probability space or a subset of one.
#' @param ... further arguments to be passed to or from other methods.
#'
#' @details The \code{sim()} function is a wrapper for \code{sample()}, except that it strips the \code{probs} component from the result and (if \code{x} is a data frame) renames the rownames of the data frame consecutively from \code{1:ntrials}.
#'
#' @return A data frame if \code{space} is a data frame, or a list if \code{space} is of class \code{ps}.
#'
#' @author G. Jay Kerns \email{gkerns@ysu.edu}.
#'
#' @seealso \code{\link[probs]{empirical}}
#'
#' @examples
#' S <- cards(makespace = TRUE)
#' sim(S, ntrials = 5)
#'
#' T <- urnsamples(S, 2)
#' U <- probspace(T)
#' sim(U, ntrials = 4)
#'
#' @keywords misc
#' @name sim
#' @export
`sim` <- function (x, ...)
  UseMethod("sim")

#' Simulate Draws from a Sample Space
#'
#' Simulates the experiment of drawing from a sample space.
#'
#' @param x a probability space or a subset of one.
#' @param ntrials number of times to repeat the experiment.
#' @param ... further arguments to be passed to or from other methods.
#'
#' @details The \code{sim()} function is a wrapper for \code{sample()}, except that it strips the \code{probs} component from the result and (if \code{x} is a data frame) renames the rownames of the data frame consecutively from \code{1:ntrials}.
#'
#' @return A data frame if \code{space} is a data frame, or a list if \code{space} is of class \code{ps}.
#'
#' @author G. Jay Kerns \email{gkerns@ysu.edu}.
#'
#' @seealso \code{\link[probs]{empirical}}
#'
#' @examples
#' S <- cards(makespace = TRUE)
#' sim(S, ntrials = 5)
#'
#' T <- urnsamples(S, 2)
#' U <- probspace(T)
#' sim(U, ntrials = 4)
#'
#' @keywords misc
#' @name sim
#' @export
`sim.default` <- function (x, ntrials, ...){
  out <- data.frame(x[, -which(names(x) == "probs")])
  names(out) <- names(x)[-which(names(x) == "probs")]
  p <- x$probs
  d <- dim(x)[1]
  ind <- sample(1:d, size = ntrials, replace = TRUE, prob = p)
  res <- as.data.frame(out[ind, ])
  names(res) <- names(out)
  rownames(res) <- 1:ntrials
  return(res)
}


#' Simulate Draws from a Sample Space
#'
#' Simulates the experiment of drawing from a sample space.
#'
#' @param x a probability space or a subset of one.
#' @param ntrials number of times to repeat the experiment.
#' @param ... further arguments to be passed to or from other methods.
#'
#' @details The \code{sim()} function is a wrapper for \code{sample()}, except that it strips the \code{probs} component from the result and (if \code{x} is a data frame) renames the rownames of the data frame consecutively from \code{1:ntrials}.
#'
#' @return A data frame if \code{space} is a data frame, or a list if \code{space} is of class \code{ps}.
#'
#' @author G. Jay Kerns \email{gkerns@ysu.edu}.
#'
#' @seealso \code{\link[probs]{empirical}}
#'
#' @examples
#' S <- cards(makespace = TRUE)
#' sim(S, ntrials = 5)
#'
#' T <- urnsamples(S, 2)
#' U <- probspace(T)
#' sim(U, ntrials = 4)
#'
#' @keywords misc
#' @name sim
#' @export
`sim.ps` <- function (x, ntrials, ...){
  out <- x$outcomes
  p <- x$probs
  d <- length(x$outcomes)
  ind <- sample(1:d, size = ntrials, replace = TRUE, prob = p)
  res <- out[ind]
  return(res)
}
