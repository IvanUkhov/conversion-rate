PROBABILITY <- 0.95
TOLERANCE <- 1e-8

effect_high_density <- function(alpha_a, beta_a,
                                alpha_b, beta_b,
                                probability = PROBABILITY,
                                tolerance = TOLERANCE, ...) {
  compute <- effect_inverse_function(alpha_a, beta_a, alpha_b, beta_b, tolerance = tolerance, ...)
  high_density_kernel(compute, probability = probability, tolerance = tolerance)
}

effect_high_density <- Vectorize(effect_high_density,
                                 vectorize.args = c('alpha_a', 'beta_a', 'alpha_b', 'beta_b'),
                                 SIMPLIFY = FALSE)

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
