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
  alpha_a = 1200
  alpha_b = 1065
  beta_a = 64873
  beta_b = 64829
  mean_b <- alpha_b / (alpha_b + beta_b)
  variance_b <- alpha_b * beta_b / (alpha_b + beta_b)^2 / (alpha_b + beta_b + 1)
  convolution <- c(mean_b - 3 * sqrt(variance_b), mean_b + 3 * sqrt(variance_b))
  compute <- effect_density_function(alpha_a = alpha_a, beta_a = beta_a,
                                     alpha_b = alpha_b, beta_b = beta_b,
                                     convolution = convolution)
  compute_empirical <- approxfun(density(rbeta(1000000, alpha_b, beta_b) -
                                         rbeta(1000000, alpha_a, beta_a)))
  compute <- Vectorize(compute)
  tibble(effect = c(-0.003, -0.002, -0.001)) %>%
    mutate(computed = compute(effect),
           expected = compute_empirical(effect),
           error = abs(computed - expected) / expected) %>%
    { expect_true(max(.$error) < 1e-2) }
})
