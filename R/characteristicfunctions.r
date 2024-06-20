####################################################
# In order to let roxygen2() create all man pages, #
# you must run the following command:              #
#       roxygen2::roxygenise()                     #
####################################################
#  Characteristic functions
#  Released under GPL 2 or greater
#  Copyright January 2009, G. Jay Kerns

#' Characteristic functions for selected probability distributions
#'
#' The characteristic functions for selected probability distributions supported
#' by \R. All base distributions are included, with the exception of
#' \code{wilcox} and \code{signedrank}. For more resources please see the
#' References, and for complete details and formulas see the \code{charfunc}
#' vignette, which can be accessed by \code{vignette("charfunc")} at the
#' command prompt. Only the simplest formulas are listed below.
#'
#' @name CharFunc
#' @aliases cfbeta cfbinom cfcauchy cfchisq cfexp cff cfgamma cfgeom cfhyper cflogis cflnorm cfnbinom cfnorm cfpois cfsignrank cft cfunif cfweibull cfwilcox
#' @concept characteristic function
#' @author G. Jay Kerns \email{gkerns@ysu.edu}.
#' @usage
#' cfbeta(t, shape1, shape2, ncp = 0)
#' cfbinom(t, size, prob)
#' cfcauchy(t, location = 0, scale = 1)
#' cfchisq(t, df, ncp = 0)
#' cfexp(t, rate = 1)
#' cff(t, df1, df2, ncp, kmax = 10)
#' cfgamma(t, shape, rate = 1, scale = 1/rate)
#' cfgeom(t, prob)
#' cfhyper(t, m, n, k)
#' cflnorm(t, meanlog = 0, sdlog = 1)
#' cflogis(t, location = 0, scale = 1)
#' cfnbinom(t, size, prob, mu)
#' cfnorm(t, mean = 0, sd = 1)
#' cfpois(t, lambda)
#' cfsignrank(t, n)
#' cft(t, df, ncp)
#' cfunif(t, min = 0, max = 1)
#' cfweibull(t, shape, scale = 1)
#' cfwilcox(t, m, n)
#'
#' @param t numeric value. Some of the above are vectorized functions.
#' @param df degrees of freedom (\eqn{> 0}, maybe non-integer).
#' @param df1 numerator degrees of freedom, must be positive.
#' @param df2 denominator degrees of freedom, must be positive.
#' @param k the number of balls drawn from the urn.
#' @param kmax the number of terms in the series.
#' @param lambda vector of (positive) means.
#' @param location location parameter; must be positive.
#' @param scale scale parameter; must be positive.
#' @param m the number of white balls in the urn.
#' @param meanlog mean of the distribution on the log scale (default is \code{0}).
#' @param sdlog standard deviation of the distribution on the log scale (default is \code{1}).
#' @param mean vector of means.
#' @param min lower limit of the distribution (must be finite and in the correct order).
#' @param max upper limit of the distribution (must be finite and in the correct order).
#' @param mu alternative parametrization via mean.
#' @param n the number of black balls in the urn.
#' @param ncp non-centrality parameter \eqn{\delta}.
#' @param prob probability of success in each trial.
#' @param rate an alternative way to specify the scale; must be positive.
#' @param sd vector of standard deviations.
#' @param size number of trials (binom) or target for number of successful trials (nbinom).
#' @param shape shape parameter, must be positive (gamma, weibull).
#' @param shape1 positive parameter of the Beta distribution.
#' @param shape2 positive parameter of the Beta distribution.
#'
#' @return a complex number in rectangular (cartesian) coordinates.
#'
#' @details
#' The characteristic function \eqn{\phi} of a random variable \eqn{X} is defined by
#' \deqn{\phi(t) = E e^{itX}}{phi(t) = E e^(itX)}
#' for all \eqn{-\infty < t < \infty}.
#'
#' Every random variable has a characteristic function, and every characteristic function
#' uniquely determines the distribution of its associated random variable. For
#' more details on characteristic functions and their properties, see Lukacs (1970).
#'
#' @section Beta distribution:
#' For the probability density function, see \code{\link{dbeta}}.
#' The characteristic function for central Beta is given by
#' \deqn{\phi(t) = _{1}F_{1}(\alpha; \alpha + \beta, it)}{\phi(t) = _{1}F_{1}(shape1; shape1 + shape2, it)}
#' where \eqn{F} is the confluent hypergeometric function calculated with
#' \code{{kummerM}} in the \code{fAsianOptions} package.
#'
#' As of the time of this writing, we must calculate the characteristic function
#' of the noncentral Beta with numerical integration according to the definition.
#'
#' @section Binomial distribution:
#' For the probability mass function, see \code{\link{dbinom}}.
#' The characteristic function is given by
#' \deqn{\phi(t) = [p e^{it} + (1-p)]^{n}}{\phi(t) = [pe^(it) + (1-p)]^n}
#'
#' @section Cauchy Distribution:
#' For the probability density function, see \code{\link{dcauchy}}.
#' The characteristic function is given by
#' \deqn{\phi(t) = e^(it\theta - \sigma|t|)}{\phi(t) = e^(i*t*theta - sigma*|t|)}
#'
#' @section Chi-square Distribution:
#' For the probability density function, see \code{\link{dchisq}}.
#' The characteristic function is given by
#' \deqn{\phi(t) = \frac{\exp(\frac{i\delta t}{1 - 2it})}{(1 - 2it)^{df/2}}}{\phi(t) = \frac{\exp(\frac{i\delta t}{1 - 2it})}{(1 - 2it)^{df/2}}}
#'
#' @section Exponential Distribution:
#' For the probability density function, see \code{\link{dexp}}.
#' This is the special case of gamma when \eqn{\alpha = 1}.
#'
#' @section F Distribution:
#' For the probability density function, see \code{\link{df}}.
#' For the central \eqn{F} we use the confluent hypergeometric function of the second kind,
#' also known as \code{{kummerU}}, from the
#' \code{fAsianOptions} package.
#'
#' For noncentral \eqn{F} we use the confluent hypergeometric function of the
#' first kind. See the vignette for details.
#'
#' @section Gamma Distribution:
#' For the probability density function, see \code{\link{dgamma}}.
#' The characteristic function is given by
#' \deqn{\phi(t) = (1 - \beta it)^{- \alpha}}{\phi(t) = (1 - beta*i*t)^(-alpha)}
#'
#' @section Geometric Distribution:
#' For the probability mass function, see \code{\link{dgeom}}.
#' This is the special case of negative binomial when \eqn{r = 1}.
#'
#' @section Hypergeometric Distribution:
#' For the probability mass function, see \code{\link{dhyper}}.
#' The formula for the characteristic function is based on the Gaussian
#' hypergeometric series, calculated with \code{\link[hypergeo]{hypergeo}} in
#' package \code{hypergeo}. It is too complicated to be included here; please see
#' the vignette.
#'
#' @section Logistic Distribution:
#' For the probability density function, see \code{\link{dlogis}}.
#' The characteristic function is given by
#' \deqn{\phi(t) = \pi t / \sinh(\pi t)}{\phi(t) = \pi t \cosech \pi t}
#'
#' @section Lognormal Distribution:
#' For the probability density function, see \code{\link{dlnorm}}.
#' This characteristic function is uniquely complicated and delicate, but
#' there is a recent numerical algorithm for computation due to Beaulieu
#' (2008). See the vignette and the References.
#'
#' @section Negative Binomial Distribution:
#' For the probability mass function, see \code{\link{dnbinom}}.
#' The characteristic function is given by
#' \deqn{\phi(t) = (p/(1-(1-p)e^{it}))^{r}}{\phi(t) = [p/(1-(1-p)e^(it))]^r}
#'
#' @section Normal Distribution:
#' For the probability density function, see \code{\link{dnorm}}.
#' The characteristic function is
#' \deqn{\phi(t) = e^{i\mu t + t^{2} \sigma^{2} /2}}{\phi(t) = e^(i*mu*t + t^2 * sigma^2 /2)}
#'
#' @section Poisson Distribution:
#' For the probability mass function, see \code{\link{dpois}}.
#' The characteristic function is
#' \deqn{\phi(t) = e^{\lambda (e^{it} - 1)}}{\phi(t) = e^(lambda*(e^it - 1))}
#'
#' @section Wilcoxon Sign Rank Distribution:
#' For the probability density function, see \code{\link{dsignrank}}.
#' The characteristic function is calculated according to the definition.
#'
#' @section Student's t Distribution:
#' For the probability density function, see \code{\link{dt}}.
#' See the vignette for a formula for the characteristic function for central t.
#' As of the time of this writing, we must calculate the characteristic function
#' of the noncentral t with numerical integration according to the definition.
#'
#' @section Continuous Uniform Distribution:
#' For the probability density function, see \code{\link{dunif}}.
#' The characteristic function is
#' \deqn{\phi(t) = \frac{e^{itb} - e^{ita}}{(b - a)it}}{\phi(t) = \frac{e^(itb) - e^(ita)}{(b - a)it}}
#'
#' @section Weibull Distribution:
#' For the probability density function, see \code{\link{dweibull}}.
#' We must at the time of this writing calculate the characteristic function
#' with numerical integration according to the definition.
#'
#' @section Wilcoxon Rank Sum Distribution:
#' For the probability density function, see \code{\link{dwilcox}}.
#' The characteristic function is calculated according to the definition.
#'
#' @references
#' Abramowitz, M. and Stegun, I. A. (1972) \emph{Handbook of Mathematical Functions.} New York: Dover.
#'
#' Beaulieu, N.C. (2008) Fast convenient numerical computation of lognormal characteristic functions, IEEE Transactions on Communications, Volume \bold{56}, Issue 3, 331--333.
#'
#' Hurst, S. (1995) The Characteristic Function of the Student-t Distribution, Financial Mathematics Research Report No. FMRR006-95, Statistics Research Report No. SRR044-95.
#'
#' Johnson, N. L., Kotz, S., and Kemp, A. W. (1992) \emph{Univariate Discrete Distributions}, Second Edition. New York: Wiley.
#'
#' Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) \emph{Continuous Univariate Distributions}, volume 1. New York: Wiley.
#'
#' Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) \emph{Continuous Univariate Distributions}, volume 2. New York: Wiley.
#'
#' Lukacs, E. (1970) \emph{Characteristic Functions}, Second Edition. London: Griffin.
#'
#' @importFrom stats dbeta integrate dt dwilcox dsignrank dweibull
#' @importFrom hypergeo hypergeo
#'
#' @examples
#' cfbeta(1, 2, 3)
#' cfbinom(1, 10, 0.5)
#' cfcauchy(1, 0, 1)
#' cfchisq(1, 2)
#' cfexp(1, 1)
#' cff(1, 2, 3, ncp=0)
#' cfgamma(1, 2, 1)
#' cfgeom(1, 0.5)
#' cfhyper(1, 2, 3, 4)
#' cflnorm(1, 0, 1)
#' cflogis(1, 0, 1)
#' cfnbinom(1, 10, 0.5)
#' cfnorm(1, 0, 1)
#' cfpois(1, 1)
#' cfsignrank(1, 10)
#' cft(1, 10)
#' cfunif(1, 0, 1)
#' cfweibull(1, 2, 1)
#' cfwilcox(1, 10, 10)
#' @export
cfbeta <- function(t, shape1, shape2, ncp = 0){
  if (shape1 <=0 || shape2 <=0)
    stop("shape1, shape2 must be positive")
  if (identical(all.equal(ncp, 0), TRUE)){
    # require(fAsianOptions)
    kummerM(1i*t, shape1, shape1 + shape2)
  } else {
    fr <- function(x) cos(t*x)*dbeta(x, shape1, shape2, ncp)
    fi <- function(x) sin(t*x)*dbeta(x, shape1, shape2, ncp)
    Rp <- integrate(fr, lower = 0, upper = 1)$value
    Ip <- integrate(fi, lower = 0, upper = 1)$value
    return( Rp + 1i*Ip )
  }
}

#' @export
cfbinom <- function(t, size, prob){
  if (size <= 0 )
    stop("size must be positive")
  if (prob < 0 || prob > 1)
    stop("prob must be in [0,1]")
  (prob*exp(1i*t) + (1 - prob))^size
}

#' @export
cfcauchy = function(t, location = 0, scale = 1){
  if (scale <= 0 )
    stop("scale must be positive")
  exp(1i*location*t - scale*abs(t))
}


#' @export
cfchisq <- function(t, df, ncp = 0){
  if (df < 0 || ncp < 0  )
    stop("df and ncp must be nonnegative")
  exp(1i*ncp*t/(1-2i*t))/(1 - 2i*t)^(df/2)
}


#' @export
cfexp <- function(t, rate = 1){
  cfgamma(t, shape = 1, scale = 1/rate)
}


#' @export
cff <- function(t, df1, df2, ncp, kmax = 10){
  if (df1 <= 0 || df2 <= 0  )
    stop("df1 and df2 must be positive")
  # require(fAsianOptions)
  if( identical(all.equal(ncp, 0), TRUE) ){
    gamma((df1+df2)/2) / gamma(df2/2) * kummerU(-1i*df2*t/df1, df1/2, 1 - df2/2)
  } else {
    exp(-ncp/2)*sum((ncp/2)^(0:kmax)/factorial(0:kmax)* kummerM(-1i*df2*t/df1, df1/2 + 0:kmax, -df2/2))
  }
}



#' @export
cfgamma <- function(t, shape, rate = 1, scale = 1/rate){
  if (rate <= 0  || scale <= 0)
    stop("rate must be positive")
  (1-scale*1i*t)^(-shape)
}



#' @export
cfgeom <- function(t, prob){
  cfnbinom(t, size = 1, prob = prob)
}



#' @export
cfhyper <- function(t, m, n, k){
  if (m < 0 || n < 0 || k < 0)
    stop("m, n, k must be positive")
  hypergeo(-k, -m, n - k + 1, exp(1i*t))/hypergeo(-k, -m, n - k + 1, 1)
}



#' @export
cflnorm <- function(t, meanlog = 0, sdlog = 1){
  if (sdlog <= 0)
    stop("sdlog must be positive")
  if (identical(all.equal(t, 0), TRUE)){
    return(1+0i)
  } else {
    t <- t * exp(meanlog)
    Rp1 <- integrate(function(y) exp(-log(y/t)^2/2/sdlog^2) * cos(y)/y, lower = 0, upper = t )$value
    Rp2 <- integrate(function(y) exp(-log(y*t)^2/2/sdlog^2) * cos(1/y)/y, lower = 0, upper = 1/t )$value
    Ip1 <- integrate(function(y) exp(-log(y/t)^2/2/sdlog^2) * sin(y)/y, lower = 0, upper = t )$value
    Ip2 <- integrate(function(y) exp(-log(y*t)^2/2/sdlog^2) * sin(1/y)/y, lower = 0, upper = 1/t )$value
    return((Rp1 + Rp2 + 1i*(Ip1 + Ip2))/(sqrt(2*pi) * sdlog))
  }
}



#' @export
cflogis <- function(t, location = 0, scale = 1){
  if (scale <= 0)
    stop("scale must be positive")
  ifelse( identical(all.equal(t, 0), TRUE),
          return(1),
          return(exp(1i*location)*pi*scale*t/sinh(pi*scale*t)))
}



#' @export
cfnbinom <- function(t, size, prob, mu){
  if (size <= 0 )
    stop("size must be positive")
  if (prob <= 0 || prob > 1)
    stop("prob must be in (0,1]")
  if (!missing(mu)) {
    if (!missing(prob))
      stop("'prob' and 'mu' both specified")
    prob <- size/(size+mu)
  }
  (prob/(1-(1-prob)*exp(1i*t)))^size
}



#' @export
cfnorm <- function(t, mean = 0, sd = 1){
  if (sd <= 0)
    stop("sd must be positive")
  exp(1i*mean - (sd*t)^2/2)
}


#' @export
cfpois <- function(t, lambda){
  if (lambda <= 0)
    stop("lambda must be positive")
  exp(lambda*(exp(1i*t) - 1))
}



#' @export
cfsignrank <- function(t, n){
  sum(exp(1i*t*0:((n+1)*n/2)) * dsignrank(0:((n+1)*n/2), n))
}



#' @export
cft <- function(t, df, ncp){
  if(missing(ncp)) ncp <- 0
  if (df <= 0)
    stop("df must be positive")
  if (identical(all.equal(ncp, 0), TRUE)){
    ifelse(identical(all.equal(t, 0), TRUE), 1+0i,
           as.complex(besselK(sqrt(df)*abs(t), df/2)*(sqrt(df)*abs(t))^(df/2)/( gamma(df/2) * 2^(df/2 - 1) ))
    )
  } else {
    fr <- function(x) cos(t*x)*dt(x, df, ncp)
    fi <- function(x) sin(t*x)*dt(x, df, ncp)
    Rp <- integrate(fr, lower = -Inf, upper = Inf)$value
    Ip <- integrate(fi, lower = -Inf, upper = Inf)$value
    return(Rp + 1i*Ip)
  }
}



#' @export
cfunif <- function(t, min = 0, max = 1){
  if (max < min)
    stop("min cannot be greater than max")
  ifelse( identical(all.equal(t, 0), TRUE),
          1+0i,
          (exp(1i*t*max) - exp(1i*t*min))/(1i*t*(max - min)))
}



#' @export
cfweibull <- function(t, shape, scale = 1){
  if (shape <= 0 || scale <= 0)
    stop("shape and scale must be positive")
  fr <- function(x) cos(t*x)*dweibull(x, shape, scale)
  fi <- function(x) sin(t*x)*dweibull(x, shape, scale)
  Rp <- integrate(fr, lower = 0, upper = Inf)$value
  Ip <- integrate(fi, lower = 0, upper = Inf)$value
  return( Rp + 1i*Ip )
}



#' @export
cfwilcox <- function(t, m, n){
  sum(exp(1i*t*0:(m*n)) * dwilcox(0:(m*n), m, n))
}




