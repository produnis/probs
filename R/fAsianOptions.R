####################################################
# In order to let roxygen2() create all man pages, #
# you must run the following command:              #
#       roxygen2::roxygenise()                     #
####################################################
#
## Functions from the old fAsianOptions package
#-----------------------------------------------

#' A function implemented by Diethelm Wuertz
#' @description Calculate the Confluent Hypergeometric Function of the First Kind for complex argument "x" and complex indexes "a" and "b"
#' @author Diethelm Wuertz
#' @param x complex function argument
#' @param a complex index
#' @param b complet index
#' @param lnchf default 0
#' @param ip  default 0
#' @return a complex value
#' @useDynLib probs, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @export
kummerM =
  function(x, a, b, lnchf = 0, ip = 0)
  {   #
    # You can also input real arguments:
    if (!is.complex(x)) x = complex(real = x, imaginary = 0*x)
    if (!is.complex(a)) a = complex(real = a, imaginary = 0)
    if (!is.complex(b)) b = complex(real = b, imaginary = 0)

    # Calculate KummerM:
    chm = rep(complex(real = 0, imaginary = 0), length = length(x))
    value = .Fortran("chfm",
                     as.double(Re(x)),
                     as.double(Im(x)),
                     as.double(Re(a)),
                     as.double(Im(a)),
                     as.double(Re(b)),
                     as.double(Im(b)),
                     as.double(Re(chm)),
                     as.double(Im(chm)),
                     as.integer(length(x)),
                     as.integer(lnchf),
                     as.integer(ip),
                     PACKAGE = "probs")
    result = complex(real = value[[7]], imaginary = value[[8]])

    # Return Value:
    return(result)
  }


# ------------------------------------------------------------------------------

#' A function implemented by Diethelm Wuertz
#' @description Calculate the Confluent Hypergeometric Function of the Second Kind for complex argument "x" and complex indexes "a" and "b"
#' @author Diethelm Wuertz
#' @param x complex function argument
#' @param a complex index
#' @param b complet index
#' @param ip  default 0
#' @return a complex value
#' @export
kummerU =
  function(x, a, b, ip = 0)
  {   # A function implemented by Diethelm Wuertz
    # Todo ...
    lnchf = 0

    # Test for complex arguments:
    if (!is.complex(x)) x = complex(real = x, imaginary = 0*x)
    if (!is.complex(a)) a = complex(real = a, imaginary = 0)
    if (!is.complex(b)) b = complex(real = b, imaginary = 0)

    # Calculate KummerU:
    # From KummerM:
    # Uses the formula ...
    #   pi/sin(pi*b) [ M(a,b,z) / (Gamma(1+a-b)*Gamma(b)) -
    #        x^(1-b) * M(1+a-b,2-b,z) / (Gamma(a)*Gamma(2-b)) ]
    ans = ( pi/sin(pi*b) ) * (
      kummerM(x, a = a, b = b, lnchf = lnchf, ip=ip) /
        ( cgamma(1+a-b)*cgamma(b) ) - (x^(1-b)) *
        kummerM(x, a = (1+a-b), b=2-b, lnchf = lnchf, ip = ip) /
        ( cgamma(a)*cgamma(2-b) ) )

    # Return Value:
    return(ans)
  }

#' A function implemented by Diethelm Wuertz
#' @description Computes the Gamma Function for complex argument "x"
#' @author Diethelm Wuertz
#' @param x a complex or real vector
#' @param log if TRUE the logarithm of the gamma is calculated otherwise if FALSE, the gamma function itself will be calculated.
#' @return a complex value
#' @source For the Fortran Routine: http://iris-lee3.ece.uiuc.edu/~jjin/routines/routines.html
#' @useDynLib probs, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @export
cgamma =
  function(x, log = FALSE)
  {   #
    # Test for complex arguments:
    if (!is.complex(x)) x = complex(real = x, imaginary = 0*x)

    # Calculate Gamma:
    KF = 1
    if (log) {
      KF = KF - 1
    }
    result = rep(NA, times = length(x))
    for ( i in 1:length(x) ) {
      value = .Fortran("cgama",
                       as.double(Re(x[i])),
                       as.double(Im(x[i])),
                       as.integer(KF),
                       as.double(0),
                       as.double(0),
                       PACKAGE = "probs")
      # PACKAGE = "fAsianOptions")
      result[i] = complex(real = value[[4]], imaginary = value[[5]])
    }

    # Return Value:
    result
  }

