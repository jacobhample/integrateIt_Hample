#' A Trapezoid object
#'
#' Objects of class \code{Trapezoid} are integrals estimated using the trapezoidal rule
#'
#' #' An object of the class Trapezoid has the following slots:
#' \itemize{
#' \item \code{x} A vector of values
#' \item \code{y} A vector of evaluated values
#' \item \code{a} Lower bound of the integral
#' \item \code{b} Upper bound of the integral
#' \item \code{estInt} Estimated integral
#' }
#'
#' @author Jacob H. Hample: \email{jacob.hample@@wustl.edu}
#' @rdname Trapezoid
#' @export


setClass(Class = "Trapezoid",
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
setMethod("initialize", "Trapezoid",
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
            .Object@estInt <- h/2 * (Y[1] + sum(2*Y[2:n]) + Y[n+1])
            return(.Object@estInt)
          }
)

#' @export
# Print method
setMethod(f = "print",
          signature = "Trapezoid",
          definition = function(x) {
            print(x@estInt)
          }
)

#' @export
# Plot method
setMethod(f = "plot",
          signature = "Trapezoid",
          definition = function(object = .Object, x, y) {
            n <- length(object@x) - 1
            h <- (object@x[length(object@x)] - object@x[1]) / n
            X <- seq(object@a, object@b, by = h)
            y.indices <- which(round(object@x, 5) %in% round(X, 5))
            Y <- object@y[y.indices]
            n <- length(X) - 1

            plot(NULL, xlim = c(min(X) - 1, max(X) + 1), ylim = c(min(Y) - 1, max(Y) + 1),
                 main = "Trapezoids", xlab = "X Values", ylab = "Y Values")
            sapply(1:X[n], function(i) segments(X[i], Y[i], X[i + 1], Y[i + 1]))
            segments(X[1], 0, X[n + 1], 0)
            sapply(1:X[n + 1], function(i) segments(X[i], 0, X[i], Y[i]))

          }
)

# Validity test
setValidity("Trapezoid", function(object) {
  test1 <- object@x < object@y
  test2 <- object@a < object@b
  
  if(!test1) {return("x needs to be less than y")}
  if(!test2) {return("a needs to be less than b")}
  }
)











