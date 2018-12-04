effect_density_function <- function(alpha_a, beta_a,
                                    alpha_b, beta_b,
                                    convolution = c(0, 1),
                                    tolerance = 1e-8) {
  if (length(convolution) == 1) {
    mean <- alpha_b / (alpha_b + beta_b)
    variance <- alpha_b * beta_b / (alpha_b + beta_b)^2 / (alpha_b + beta_b + 1)
    convolution <- c(mean - convolution * sqrt(variance),
                     mean + convolution * sqrt(variance))
  }
  density_a <- function(x) dbeta(-x, alpha_a, beta_a)
  density_b <- function(x) dbeta(x, alpha_b, beta_b)
  evaluate <- function(y, x) density_b(y) * density_a(x - y)
  function(x) integrate(evaluate,
                        max(0, convolution[1]),
                        min(1, convolution[2]),
                        x,
                        abs.tol = tolerance)$value
}
