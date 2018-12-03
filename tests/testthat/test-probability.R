context('probability')

test <- function(log_compute, epsilon = 1e-10) {
  tibble(alpha_a = c(1, 1000, 1),
         beta_a = c(1, 1, 1000),
         alpha_b = c(1, 1, 1000),
         beta_b = c(1, 1000, 1),
         expected = c(0.5, 0, 1)) %>%
    mutate(computed = exp(log_compute(alpha_a, beta_a, alpha_b, beta_b))) %>%
    summarize(error = sum(abs(computed - expected))) %>%
    { expect_true(.$error < epsilon) }
}

test_that('the greater probability is correct', {
  test(log_greater_probability_accurate)
})

test_that('the approximate greater probability is correct', {
  test(log_greater_probability_approximate)
})
