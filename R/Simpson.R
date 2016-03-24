#' A Simpson object
#'
#' Objects of class \code{Simpson} are integrals estimated using Simpson's rule
#'
#' #' An object of the class Simpson has the following slots:
#' \itemize{
#' \item \code{x} A vector of values
#' \item \code{y} A vector of evaluated values
#' \item \code{a} Lower bound of the integral
#' \item \code{b} Upper bound of the integral
#' \item \code{estInt} Estimated integral
#' }
#'
#' @author Jacob H. Hample: \email{jacob.hample@@wustl.edu}
#' @rdname Simpson
#' @export


setClass(Class = "Simpson",
         slots = c(
           x = "numeric",
           y = "numeric",
           a = "numeric",
           b = "numeric",
           estInt = "numeric"),
         prototype = prototype(
           x = numeric(),
           y = numeric(),
           a = numeric(),
           b = numeric(),
           estInt = numeric()
         )
)


#' @export
setMethod("initialize", "Simpson",
          function(.Object, x, y, a, b) {
            .Object@x <- x
            .Object@y <- y
            .Object@a <- a
            .Object@b <- b

            n <- length(.Object@x) - 1
            h <- (.Object@x[length(.Object@x)] - .Object@x[1]) / n
            X <- seq(.Object@a, .Object@b, by = h)
            y.indices <- which(round(.Object@x, 5) %in% round(X, 5))
            Y <- .Object@y[y.indices]
            n <- length(X) - 1
            if (n %% 2 == 1) {
              stop("Shame on you, you've entered a sequence of even length. Please enter a sequence of odd length to continue.")
            }
            else if (n == 2) {
              .Object@estInt <- Y[1] + 4 * Y[2] + Y[3]
            }
            else {
              .Object@estInt <- h/3 * (Y[1] + 4 * sum(Y[seq(2, n, by = 2)]) + 2 * sum(Y[seq(3, n-1, by = 2)]) + Y[n+1])
            }
            return(.Object@estInt)
          }
)

#' @export
# Print method
setMethod(f = "print",
          signature = "Simpson",
          definition = function(x) {
            print(x@estInt)
          }
)

# Validity test
setValidity("Simpson", function(object) {
  test1 <- object@x < object@y
  test2 <- object@a < object@b
  test3 <- length(object@a) %% 2 == 1
  
  if(!test1) {return("x needs to be less than y")}
  if(!test2) {return("a needs to be less than b")}
  if(!test3) {return("vector x needs to be of odd length")}
}
)




