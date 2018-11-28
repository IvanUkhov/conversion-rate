context('simulate')

test_that('the number of rows is correct', {
  set.seed(0)
  data <- tibble(replication = seq_len(42)) %>%
    simulate(day_num = 7,
             daily_num = 10,
             a_rate = 0.01,
             b_effect = 0)
  expect_equal(nrow(data), 42 * 7)
})

test_that('the expected loss is correct', {
  set.seed(0)
  data <- tibble(replication = seq_len(20)) %>%
    simulate(day_num = 14,
             daily_num = 10000,
             a_rate = 0.01,
             b_effect = 0,
             a_alpha_prior = 100,
             b_alpha_prior = 100,
             a_beta_prior = 990,
             b_beta_prior = 990,
             expected_loss = TRUE)
  expect_true(abs(sum(data$expected_loss) - 0.1529044) < 1e-6)
})

test_that('the approximate expected loss is correct', {
  set.seed(0)
  data <- tibble(replication = seq_len(10)) %>%
    simulate(day_num = 7,
             daily_num = 10000,
             a_rate = 0.001,
             b_effect = -0.0001,
             a_alpha_prior = 10,
             b_alpha_prior = 10,
             a_beta_prior = 90,
             b_beta_prior = 90,
             expected_loss = TRUE,
             approximate = TRUE)
  expect_true(abs(sum(data$expected_loss) - 0.01946671) < 1e-6)
})
