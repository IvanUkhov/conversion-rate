high_density <- function(alpha, beta, ...) {
  high_density_kernel(qbeta, shape1 = alpha, shape2 = beta, ...)
}

high_density <- Vectorize(high_density, vectorize.args = c('alpha', 'beta'), SIMPLIFY = FALSE)

high_density_kernel <- function(icdf, probability = 0.95, tolerance = 1e-8, ...) {
  evaluate <- function(left, ...) {
    icdf(left + probability, ...) - icdf(left, ...)
  }
  left <- optimize(evaluate, c(0, 1 - probability), tol = tolerance, ...)$minimum
  c(icdf(left, ...), icdf(left + probability, ...))
}
