context('credibility')

test_that('the approximate high-density interval is correct', {
  set.seed(0)
  interval <- effect_high_density_approximate(alpha_a = 1,
                                              beta_a = 1,
                                              alpha_b = 1,
                                              beta_b = 1,
                                              probability = 0.75) %>%
    simplify()
  expect_true(abs(interval[1] + 0.5) < 0.01)
  expect_true(abs(interval[2] - 0.5) < 0.01)
})
