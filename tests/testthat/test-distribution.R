context('distribution')

test_that('the effect density is correct', {
  compute <- effect_density_function(alpha_a = 1, beta_a = 1, alpha_b = 1, beta_b = 1)
  compute <- Vectorize(compute)
  tibble(effect = c(-2, -1, -0.5, 0, 0.5, 1, 2),
         expected = c(0, 0, 0.5, 1, 0.5, 0, 0)) %>%
    mutate(computed = compute(effect)) %>%
    summarize(error = max(abs(computed - expected))) %>%
    { expect_true(.$error < 1e-10) }
})

test_that('the effect distribution is correct', {
  compute <- effect_distribution_function(alpha_a = 1, beta_a = 1, alpha_b = 1, beta_b = 1)
  compute <- Vectorize(compute)
  tibble(effect = c(-1, -0.5, 0, 0.5, 1),
         expected = c(0, 0.125, 0.5, 0.875, 1)) %>%
    mutate(computed = compute(effect)) %>%
    summarize(error = max(abs(computed - expected))) %>%
    { expect_true(.$error < 1e-5) }
})
