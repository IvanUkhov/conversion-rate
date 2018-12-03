context('probability')

test <- function(compute) {
  tibble(alpha_a = c(1, 1000, 1),
         beta_a = c(1, 1, 1000),
         alpha_b = c(1, 1, 1000),
         beta_b = c(1, 1000, 1),
         expected = c(0.5, 1, 0)) %>%
    mutate(computed = exp(compute(alpha_a, beta_a, alpha_b, beta_b))) %>%
    summarize(error = sum(abs(computed - expected))) %>%
    { expect_true(.$error < 1e-10) }
}

test_that('the greater probability is correct', {
  test(greater_probability)
})

test_that('the approximate greater probability is correct', {
  test(greater_probability_approximate)
})
