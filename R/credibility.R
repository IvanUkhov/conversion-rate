high_density <- function(..., approximate = FALSE) {
  high_density_accurate(...)
}

high_density_accurate <- function(alpha, beta, probability = 0.95, tolerance = 1e-8) {
  inverse <- function(p) qbeta(p, shape1 = alpha, shape2 = beta)
  compute <- function(p) inverse(p + probability) - inverse(p)
  p <- optimize(compute, c(0, 1 - probability), tol = tolerance)$minimum
  c(inverse(p), inverse(p + probability))
}

high_density_accurate <- Vectorize(high_density_accurate,
                                   vectorize.args = c('alpha', 'beta'),
                                   SIMPLIFY = FALSE)

effect_high_density <- function(..., approximate = TRUE) {
  effect_high_density_approximate(...)
}

effect_high_density_approximate <- function(alpha_a, beta_a,
                                            alpha_b, beta_b,
                                            probability = 0.95,
                                            sample_size = 10000) {
  sample <- sort(rbeta(sample_size, alpha_b, beta_b) - rbeta(sample_size, alpha_a, beta_a))
  lower <- floor(probability * sample_size)
  index <- seq(1, sample_size - lower)
  width <- sample[index + lower] - sample[index]
  c(sample[which.min(width)], sample[which.min(width) + lower])
}

effect_high_density_approximate <- Vectorize(effect_high_density_approximate,
                                             vectorize.args = c('alpha_a', 'beta_a',
                                                                'alpha_b', 'beta_b'),
                                             SIMPLIFY = FALSE)
