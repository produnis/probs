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
#' @seealso \code{\link[prob]{rolldie}}, \code{\link[prob]{tosscoin}}, and \code{\link[prob]{roulette}}
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
#' @seealso \code{\link[prob]{cards}}, \code{\link[prob]{tosscoin}}, and \code{\link[prob]{roulette}}
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
#' @seealso \code{\link[prob]{tosscoin}}
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
#' @seealso \code{\link[prob]{cards}}
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
#' @seealso \code{\link[prob]{rolldie}}
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
#' @seealso See \code{\link[prob]{addrv}} for adding random variables to a data frame probability space.
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
#' @seealso \code{\link[prob]{addrv}}, \code{\link[prob]{marginal}}
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
#' @details The \code{nsamp()} function will calculate the number of samples from an urn under assorted assumptions on the sampling procedure. The arguments are: \code{n}, the number of (distinguishable) objects in the urn, \code{k}, the sample size, and \code{replace}, \code{ordered} as documented in \code{\link[prob]{urnsamples}}.
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
#' @seealso \code{\link[prob]{urnsamples}}
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

