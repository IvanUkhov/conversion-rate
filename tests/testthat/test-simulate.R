context('simulate')

create_fixture_1 <- function(...) {
  tibble(replication = seq_len(20)) %>%
    simulate(day_num = 14,
             daily_num = 10000,
             a_rate = 0.01,
             b_effect = 0,
             a_alpha_prior = 100,
             b_alpha_prior = 100,
             a_beta_prior = 990,
             b_beta_prior = 990, ...)
}

create_fixture_2 <- function(...) {
  tibble(replication = seq_len(10)) %>%
    simulate(day_num = 7,
             daily_num = 10000,
             a_rate = 0.001,
             b_effect = -0.0001,
             a_alpha_prior = 10,
             b_alpha_prior = 10,
             a_beta_prior = 90,
             b_beta_prior = 90, ...)
}

expect_close <- function(one, another, epsilon = 1e-6) {
  expect_true(abs(one - another) < 1e-6)
}

test_that('the number of rows is correct', {
  set.seed(0)
  data <- tibble(replication = seq_len(42)) %>%
    simulate(day_num = 7,
             daily_num = 10,
             a_rate = 0.01,
             b_effect = 0)
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

test_that('the approximate expected gain is correct', {
  set.seed(0)
  data <- create_fixture_2(expected_gain = TRUE, approximate = TRUE)
  expect_close(sum(data$expected_gain), 0.01281684)
})

test_that('the approximate expected loss is correct', {
  set.seed(0)
  data <- create_fixture_2(expected_loss = TRUE, approximate = TRUE)
  expect_close(sum(data$expected_loss), 0.01946671)
})

test_that('the approximate greater probability is correct', {
  set.seed(0)
  data <- create_fixture_2(greater_probability = TRUE, approximate = TRUE)
  expect_close(mean(data$greater_probability), 0.4034808)
})
