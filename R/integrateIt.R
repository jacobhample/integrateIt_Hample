#' Estimates the integral of a mathematical function
#'
#' Estimates the integral of a mathematical funciton using either the trapezoidal
#' rule or Simpson's rule
#'
#' @param Rule Which rule, either 'Trap' or 'Simp', one would like to use to estimate the integral
#' @param x A vector of values
#' @param y A vector of evaluated values
#' @param a Lower bound of the integral
#' @param b Upper bound of the integral
#'
#' @return An object of either class `Trapezoid' or class 'Simpson' that contains
#' \itemize{
#' \item \code{x} A vector of values
#' \item \code{y} A vector of evaluated values
#' \item \code{a} Lower bound of the integral
#' \item \code{b} Upper bound of the integral
#' \item \code{estInt} Estimated integral
#' }
#'
#' @author Jacob H. Hample: \email{jacob.hample@@wustl.edu}
#' @examples
#'
#' integrateIt("Trap", 1:25, (1:25)^2, 4, 22)
#'
#' @rdname integrateIt
#' @export

setGeneric(name = "integrateIt",
           def = function(Rule, x, y, a, b)
           {standardGeneric("integrateIt")}
)

setMethod(f = "integrateIt",
          definition = function(Rule, x, y, a, b) {
            if (Rule == "Trap") {
              return(new("Trapezoid", x = x, y = y, a = a, b = b))
            }
            else if (Rule == "Simp") {
              return(new("Simpson", x = x, y = y, a = a, b = b))
            }
            else {
              stop("Please input either 'Trap' or 'Simp'")
            }
          }
)




