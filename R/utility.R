expected_gain <- function(...) {
  expected_gain_kernel(log_greater_probability, ...)
}

expected_gain_approximate <- function(...) {
  expected_gain_kernel(log_greater_probability_approximate, ...)
}

expected_loss <- function(...) {
  expected_loss_kernel(log_greater_probability, ...)
}

expected_loss_approximate <- function(...) {
  expected_loss_kernel(log_greater_probability_approximate, ...)
}

expected_gain_kernel <- function(log_greater_probability, alpha_a, beta_a, alpha_b, beta_b) {
  integral_1 <-
    lbeta(alpha_b + 1, beta_b) -
    lbeta(alpha_b, beta_b) +
    log_greater_probability(alpha_b + 1, beta_b, alpha_a, beta_a)
  integral_2 <-
    lbeta(alpha_a + 1, beta_a) -
    lbeta(alpha_a, beta_a) +
    log_greater_probability(alpha_b, beta_b, alpha_a + 1, beta_a)
  exp(integral_1) - exp(integral_2)
}

expected_loss_kernel <- function(log_greater_probability, alpha_a, beta_a, alpha_b, beta_b) {
  expected_gain_kernel(log_greater_probability, alpha_a, beta_a, alpha_b, beta_b) -
  exp(lbeta(alpha_b + 1, beta_b) - lbeta(alpha_b, beta_b)) +
  exp(lbeta(alpha_a + 1, beta_a) - lbeta(alpha_a, beta_a))
}
