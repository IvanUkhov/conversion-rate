effect_density_function <- function(alpha_a, beta_a,
                                    alpha_b, beta_b,
                                    convolution = c(0, 1),
                                    tolerance = 1e-8) {
  convolution <- c(max(convolution[1], 0), min(convolution[2], 1))
  evaluate <- function(y, x) dbeta(y, alpha_b, beta_b) *
                             dbeta(-(x - y), alpha_a, beta_a)
  function(x) integrate(evaluate,
                        lower = convolution[1],
                        upper = convolution[2],
                        x = x,
                        abs.tol = tolerance)$value
}
