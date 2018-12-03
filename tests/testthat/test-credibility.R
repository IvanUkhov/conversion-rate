context('credibility')

test_named <- function(compute) {
  interval <- simplify(compute(alpha_a = 1, beta_a = 1,
                               alpha_b = 1, beta_b = 1,
                               probability = 0.75))
  expect_true(abs(interval[1] + 0.5) < 0.01)
  expect_true(abs(interval[2] - 0.5) < 0.01)
}

test_plain <- function(compute) {
  interval <- simplify(compute(1, 1, 1, 1, 0.75))
  expect_true(abs(interval[1] + 0.5) < 0.01)
  expect_true(abs(interval[2] - 0.5) < 0.01)
}

test_that('the approximate high-density interval is correct', {
  set.seed(0)
  test_named(effect_high_density)
  set.seed(0)
  test_named(effect_high_density_approximate)
  set.seed(0)
  test_plain(effect_high_density)
  set.seed(0)
  test_plain(effect_high_density_approximate)
})
