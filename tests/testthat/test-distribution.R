context('distribution')

test_that('the effect density is correct', {
  compute <- effect_density_function(alpha_a = 1, beta_a = 1,
                                     alpha_b = 1, beta_b = 1)
  compute <- Vectorize(compute)
  tibble(effect = c(-2, -1, -0.5, 0, 0.5, 1, 2),
         expected = c(0, 0, 0.5, 1, 0.5, 0, 0)) %>%
    mutate(computed = compute(effect),
           error = abs(computed - expected)) %>%
    { expect_true(max(.$error) < 1e-10) }
})

test_that('the effect density is correct for narrow distributions', {
  set.seed(0)
  compute <- effect_density_function(alpha_a = 1200, beta_a = 64873,
                                     alpha_b = 1065, beta_b = 64829,
                                     convolution = 3)
  compute_empirical <- approxfun(density(rbeta(1000000, 1065, 64829) -
                                         rbeta(1000000, 1200, 64873)))
  compute <- Vectorize(compute)
  tibble(effect = c(-0.003, -0.002, -0.001)) %>%
    mutate(computed = compute(effect),
           expected = compute_empirical(effect),
           error = abs(computed - expected) / expected) %>%
    { expect_true(max(.$error) < 1e-2) }
})
