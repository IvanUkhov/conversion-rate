expected_gain <- function(..., approximate = FALSE) {
  if (approximate) expected_gain_approximate(...) else expected_gain_accurate(...)
}

expected_loss <- function(..., approximate = FALSE) {
  if (approximate) expected_loss_approximate(...) else expected_loss_accurate(...)
}

expected_gain_accurate <- function(...) {
  expected_gain_kernel(log_greater_probability_accurate, ...)
}

expected_gain_approximate <- function(...) {
  expected_gain_kernel(log_greater_probability_approximate, ...)
}

expected_loss_accurate <- function(...) {
  expected_loss_kernel(log_greater_probability_accurate, ...)
}

expected_loss_approximate <- function(...) {
  expected_loss_kernel(log_greater_probability_approximate, ...)
}

expected_gain_kernel <- function(log_greater_probability, alpha_a, beta_a, alpha_b, beta_b) {
  integral_1 <-
    lbeta(alpha_b + 1, beta_b) -
    lbeta(alpha_b, beta_b) +
    log_greater_probability(alpha_a, beta_a, alpha_b + 1, beta_b)
  integral_2 <-
    lbeta(alpha_a + 1, beta_a) -
    lbeta(alpha_a, beta_a) +
    log_greater_probability(alpha_a + 1, beta_a, alpha_b, beta_b)
  exp(integral_1) - exp(integral_2)
}

expected_loss_kernel <- function(log_greater_probability, alpha_a, beta_a, alpha_b, beta_b) {
  expected_gain_kernel(log_greater_probability, alpha_a, beta_a, alpha_b, beta_b) -
  exp(lbeta(alpha_b + 1, beta_b) - lbeta(alpha_b, beta_b)) +
  exp(lbeta(alpha_a + 1, beta_a) - lbeta(alpha_a, beta_a))
}
