PROBABILITY <- 0.95
TOLERANCE <- 1e-8

high_density <- function(alpha, beta, ...) {
  compute <- function(p) qbeta(p, shape1 = alpha, shape2 = beta)
  high_density_kernel(compute, ...)
}

high_density <- Vectorize(high_density, vectorize.args = c('alpha', 'beta'), SIMPLIFY = FALSE)

high_density_kernel <- function(inverse, probability = PROBABILITY, tolerance = TOLERANCE) {
  compute <- function(p) inverse(p + probability) - inverse(p)
  p <- optimize(compute, c(0, 1 - probability), tol = tolerance)$minimum
  c(inverse(p), inverse(p + probability))
}
