context('simulate')

create_fixture_1 <- function(...) {
  tibble(replication = seq_len(20)) %>%
    simulate(day_num = 14,
             daily_num = 10000,
             rate_a = 0.01,
             effect_b = 0,
             alpha_prior_a = 100,
             alpha_prior_b = 100,
             beta_prior_a = 990,
             beta_prior_b = 990, ...)
}

create_fixture_2 <- function(...) {
  tibble(replication = seq_len(10)) %>%
    simulate(day_num = 7,
             daily_num = 10000,
             rate_a = 0.001,
             effect_b = -0.0001,
             alpha_prior_a = 10,
             alpha_prior_b = 10,
             beta_prior_a = 90,
             beta_prior_b = 90, ...)
}

expect_close <- function(one, another, epsilon = 1e-6) {
  expect_true(abs(one - another) < 1e-6)
}

test_that('the number of rows is correct', {
  set.seed(0)
  data <- tibble(replication = seq_len(42)) %>%
    simulate(day_num = 7,
             daily_num = 10,
             rate_a = 0.01,
             effect_b = 0)
  expect_equal(nrow(data), 42 * 7)
})

test_that('the expected gain is correct', {
  set.seed(0)
  data <- create_fixture_1(expected_gain = TRUE)
  expect_close(sum(data$expected_gain), 0.1365721)
})

test_that('the expected loss is correct', {
  set.seed(0)
  data <- create_fixture_1(expected_loss = TRUE)
  expect_close(sum(data$expected_loss), 0.1529044)
})

test_that('the greater probability is correct', {
  set.seed(0)
  data <- create_fixture_1(greater_probability = TRUE)
  expect_close(mean(data$greater_probability), 0.4832106)
})

test_that('the high-density interval is correct', {
  set.seed(0)
  data <- create_fixture_2(high_density = TRUE)
  expect_close(mean(map_dbl(data$high_density_a, ~ .[1])), 0.00109067)
  expect_close(mean(map_dbl(data$high_density_a, ~ .[2])), 0.002440082)
  expect_close(mean(map_dbl(data$high_density_b, ~ .[1])), 0.001013332)
  expect_close(mean(map_dbl(data$high_density_b, ~ .[2])), 0.002327531)
})

test_that('the approximate expected gain is correct', {
  set.seed(0)
  data <- create_fixture_2(expected_gain = list(approximate = TRUE))
  expect_close(sum(data$expected_gain), 0.01281684)
})

test_that('the approximate expected loss is correct', {
  set.seed(0)
  data <- create_fixture_2(expected_loss = list(approximate = TRUE))
  expect_close(sum(data$expected_loss), 0.01946671)
})

test_that('the approximate greater probability is correct', {
  set.seed(0)
  data <- create_fixture_2(greater_probability = list(approximate = TRUE))
  expect_close(mean(data$greater_probability), 0.4034808)
})
