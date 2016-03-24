# Jacob Hample
# Professor Montgomery
# Applied Statistical Programming
# Midterm Exam

library(devtools)
library(roxygen2)

# devtools
current.code <- as.package("integrateIt")
load_all(current.code)
document(current.code)

# Creates vectors containing x & y values of a mathematical function
x <- seq(1, 25, length.out = 25)
y <- (x-10)^2 + 5


# S3 Trapezoidal Rule
trap <- function(x, y, a, b) {
  n <- length(x) - 1
  h <- (x[length(x)] - x[1]) / n
  X <- seq(a, b, by = h)
  y.indices <- which(round(x, 5) %in% round(X, 5))
  Y <- y[y.indices]
  n <- length(X) - 1
  estInt <- h/2 * (Y[1] + sum(2*Y[2:n]) + Y[n+1])
  return(estInt)
}

trap(x, y, 4, 22)


# Print method
print.trap <- function(x, y, a, b) {
  estInt <- trap(x, y, a, b)
  print(estInt)
}

print.trap(x, y, 4, 22)


# Plot method
plot.trap <- function(x, y, a, b) {
  n <- length(x) - 1
  h <- (x[length(x)] - x[1]) / n
  X <- seq(a, b, by = h)
  y.indices <- which(round(x, 5) %in% round(X, 5))
  Y <- y[y.indices]
  n <- length(X) - 1

  plot(NULL, xlim = c(min(X) - 1, max(X) + 1), ylim = c(min(Y) - 1, max(Y) + 1),
       main = "Trapezoids", xlab = "X Values", ylab = "Y Values")
  sapply(1:X[n], function(i) segments(X[i], Y[i], X[i + 1], Y[i + 1]))
  segments(X[1], 0, X[n + 1], 0)
  sapply(1:X[n + 1], function(i) segments(X[i], 0, X[i], Y[i]))
}

plot.trap(x, y, 4, 22)



# S3 Simpson's Rule
simp <- function(x, y, a, b) {
  n <- length(x) - 1
  h <- (x[length(x)] - x[1]) / n
  X <- seq(a, b, by = h)
  y.indices <- which(round(x, 5) %in% round(X, 5))
  Y <- y[y.indices]
  n <- length(X) - 1
  if (n %% 2 == 1) {
    stop("Shame on you, you've entered a sequence of even length. Please enter a sequence of odd length to continue.")
  }
  else if (n == 2) {
    estInt <- Y[1] + 4 * Y[2] + Y[3]
  }
  else {
    estInt <- h/3 * (Y[1] + 4 * sum(Y[seq(2, n, by = 2)]) + 2 * sum(Y[seq(3, n-1, by = 2)]) + Y[n+1])
  }
  return(estInt)
}

simp(x, y, 4, 22)


# Print method
print.simp <- function(x, y, a, b) {
  estInt <- simp(x, y, a, b)
  print(estInt)
}

print.simp(x, y, 4, 22)


# Plot method
plot.simp <- function(x, y, a, b) {
  n <- length(x) - 1
  h <- (x[length(x)] - x[1]) / n
  X <- seq(a, b, by = h)
  y.indices <- which(round(x, 5) %in% round(X, 5)) 
  Y <- y[y.indices]
  n <- length(X) - 1
  
  plot(NULL, xlim = c(min(X) - 1, max(X) + 1), ylim = c(min(Y) - 1, max(Y) + 1),
       main = "Simpson's Parabolas", xlab = "X Values", ylab = "Y Values")
  sapply(1:X[n - 1], function(i) segments(X[i], Y[i], X[i + 2], Y[i + 2]))
}

plot.simp(x, y, 4, 22)

