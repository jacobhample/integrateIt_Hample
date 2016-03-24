\name{integrateIt}
\alias{integrateIt}
\alias{integrateIt,ANY-method}
\title{Creates an object of class `Candidate'}
\usage{
integrateIt(Rule, x, y, a, b)
}
\arguments{
\item{Rule}{Which rule, either 'Trap' or 'Simp', one would like to use to estimate the integral}

\item{x}{A vector of values}

\item{y}{A vector of evaluated values}

\item{a}{Lower bound of the integral}

\item{b}{Upper bound of the integral}
\value{
An object of either class `Trapezoid' or class 'Simpson' that contains
\itemize{
\item \code{x} A vector of values
\item \code{y} A vector of evaluated values
\item \code{a} Lower bound of the integral
\item \code{b} Upper bound of the integral
\item \code{estInt} Estimated integral
}
}
\description{
Estimates the integral of a mathematical funciton using either the trapezoidal
rule or Simpson's rule
}
\examples{

integrateIt("Trap", 1:25, (1:25)^2, 4, 22)

}
\author{
Jacob H. Hample: \email{jacob.hample@wustl.edu}
}

