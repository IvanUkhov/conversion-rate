simulate <- function(data,
                     day_num,
                     expected_gain = FALSE,
                     expected_loss = FALSE,
                     greater_probability = FALSE, ...) {
  data <- data %>%
    cbind(...) %>%
    crossing(day = seq_len(day_num)) %>%
    crossing(group = c('a', 'b')) %>%
    mutate(group = factor(group),
           total_num = rbinom(n(), daily_num, 0.5),
           success_num = rbinom(n(), total_num, a_rate + b_effect * (group == 'b'))) %>%
    group_by_at(vars(-day, -total_num, -success_num)) %>%
    arrange(day) %>%
    mutate(total_num = cumsum(total_num),
           success_num = cumsum(success_num)) %>%
    ungroup() %>%
    tidyr::gather(key, value, total_num, success_num) %>%
    tidyr::unite(key, group, key, sep = '_') %>%
    tidyr::spread(key, value)
  if (!isFALSE(expected_gain) || !isFALSE(expected_loss) || !isFALSE(greater_probability)) {
    data <- data %>%
      mutate_posterior()
  }
  if (!isFALSE(expected_gain)) {
    data <- data %>%
      mutate(expected_gain = do.call(compute_expected_gain,
                                     extract_posterior(data, expected_gain)))
  }
  if (!isFALSE(expected_loss)) {
    data <- data %>%
      mutate(expected_loss = do.call(compute_expected_loss,
                                     extract_posterior(data, expected_loss)))
  }
  if (!isFALSE(greater_probability)) {
    data <- data %>%
      mutate(greater_probability = do.call(compute_greater_probability,
                                           extract_posterior(data, greater_probability)),
             greater_probability = 1 - exp(greater_probability))
  }
  data
}

compute_expected_gain <- function(...) {
  compute_with_approximation(expected_gain, expected_gain_approximate, ...)
}

compute_expected_loss <- function(...) {
  compute_with_approximation(expected_loss, expected_loss_approximate, ...)
}

compute_greater_probability <- function(...) {
  compute_with_approximation(greater_probability, greater_probability_approximate, ...)
}

compute_with_approximation <- function(compute,
                                       compute_approximate,
                                       a_alpha,
                                       b_alpha,
                                       a_beta,
                                       b_beta,
                                       approximate = FALSE) {
  if (!approximate) {
    sapply(seq_along(a_alpha), function(i) {
      compute(a_alpha[i], a_beta[i], b_alpha[i], b_beta[i])
    })
  } else {
    compute_approximate(a_alpha, a_beta, b_alpha, b_beta)
  }
}

extract_posterior <- function(data, arguments) {
  arguments = if (isTRUE(arguments)) list() else arguments
  c(list(a_alpha=data$a_alpha_posterior,
         a_beta=data$a_beta_posterior,
         b_alpha=data$b_alpha_posterior,
         b_beta=data$b_beta_posterior),
    arguments)
}

mutate_posterior <- function(data) {
  data %>%
    mutate(a_alpha_posterior = a_alpha_prior + a_success_num,
           b_alpha_posterior = b_alpha_prior + b_success_num,
           a_beta_posterior = a_beta_prior + a_total_num - a_success_num,
           b_beta_posterior = b_beta_prior + b_total_num - b_success_num)
}
