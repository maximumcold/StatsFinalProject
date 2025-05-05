library(tidyverse)

pizza_data <- read.csv("data/pizza_requests.csv")

clean_data <- pizza_data %>%
  mutate(
    request_id                     = request_id,
    request_time                   = as.POSIXct(unix_timestamp_of_request_utc,
                                               origin = "1970-01-01", tz = "America/Chicago"),
    successes                      = ifelse(requester_received_pizza == "True", 1, 0),
    failures                       = ifelse(requester_received_pizza == "True", 0, 1),
    number_upvotes_at_retrieval    = as.numeric(number_of_upvotes_of_request_at_retrieval),
    number_downvotes_at_retrieval  = as.numeric(number_of_downvotes_of_request_at_retrieval),
    account_age_request            = as.numeric(requester_account_age_in_days_at_request),
    account_age_retrieval          = as.numeric(requester_account_age_in_days_at_retrieval)
  )


# clean_data %>%
#   mutate(request_date = as.Date(request_time)) %>%
#   group_by(request_date) %>%
#   summarise(
#     n_requests = n(),
#     success_rate = mean(received_pizza)
#   )

# REUSED VALUES
hours <- c(hour(clean_data$request_time))
totalsuccesses <- sum(clean_data$successes)
totalfailures <- sum(clean_data$failures)
h0successrate <- totalsuccesses / (totalfailures + totalsuccesses)
h0failurerate <- 1 - h0successrate

# ---------------------------------------- 1h bins ----------------------------------------
succ_1h <- c()
fail_1h <- c()
for (i in 0:23) {
  succ_1h <- c(succ_1h, sum(clean_data$successes[hours==i]))
  fail_1h <- c(fail_1h, sum(clean_data$failures[hours==i]))
}

totals_1h <- c()
for (i in 1:length(succ_1h)) {
  totals_1h <- c(totals_1h, succ_1h[i] + fail_1h[i])
}

csq_1h <- 0
for (i in 1:length(succ_1h)) {
  s_exp_1h <- totals_1h[i] * h0successrate
  f_exp_1h <- totals_1h[i] * h0failurerate
  csq_1h <- csq_1h + ((succ_1h[i] - s_exp_1h) ^ 2) / s_exp_1h
  csq_1h <- csq_1h + ((fail_1h[i] - f_exp_1h) ^ 2) / f_exp_1h
}

prob_timeofday_1h <- pchisq(csq_1h, 11, lower.tail = FALSE)
# -----------------------------------------------------------------------------------------
# ---------------------------------------- 2h bins ----------------------------------------
succ_2h <- c()
fail_2h <- c()
for (i in 0:11) {
  succ_2h <- c(succ_2h, sum(clean_data$successes[hours >= (2 * i) & hours < (2 * i) + 2]))
  fail_2h <- c(fail_2h, sum(clean_data$failures[hours >= (2 * i) & hours < (2 * i) + 2]))
}

totals_2h <- c()
for (i in 1:length(succ_2h)) {
  totals_2h <- c(totals_2h, succ_2h[i] + fail_2h[i])
}

csq_2h <- 0
for (i in 1:length(succ_2h)) {
  s_exp_2h <- totals_2h[i] * h0successrate
  f_exp_2h <- totals_2h[i] * h0failurerate
  csq_2h <- csq_2h + ((succ_2h[i] - s_exp_2h) ^ 2) / s_exp_2h
  csq_2h <- csq_2h + ((fail_2h[i] - f_exp_2h) ^ 2) / f_exp_2h
}

prob_timeofday_2h <- pchisq(csq_2h, 11, lower.tail = FALSE)
# -----------------------------------------------------------------------------------------
# ---------------------------------------- 4h bins ----------------------------------------

succ_4h <- c()
fail_4h <- c()
for (i in 0:5) {
  succ_4h <- c(succ_4h, sum(clean_data$successes[hours >= (4 * i) & hours < (4 * i) + 4]))
  fail_4h <- c(fail_4h, sum(clean_data$failures[hours >= (4 * i) & hours < (4 * i) + 4]))
}

totals_4h <- c()
for (i in 1:length(succ_4h)) {
  totals_4h <- c(totals_4h, succ_4h[i] + fail_4h[i])
}

csq_4h <- 0
for (i in 1:length(succ_4h)) {
  s_exp_4h <- totals_4h[i] * h0successrate
  f_exp_4h <- totals_4h[i] * h0failurerate
  csq_4h <- csq_4h + ((succ_4h[i] - s_exp_4h) ^ 2) / s_exp_4h
  csq_4h <- csq_4h + ((fail_4h[i] - f_exp_4h) ^ 2) / f_exp_4h
}

prob_timeofday_4h <- pchisq(csq_4h, 11, lower.tail = FALSE)
# -----------------------------------------------------------------------------------------

# 2x2 chi-square tests
results <- c()
for (i in 1:length(succ_4h)) {
  for (j in 1:length(succ_4h)) {
    if (i >= j) {
      next
    }
    this_ts <- 0
    thish0succ <- (succ_4h[i] + succ_4h[j]) / (succ_4h[i] + succ_4h[j] + fail_4h[i] + fail_4h[j])
    thish0fail <- 1 - thish0succ
    s_exp_i <- totals_4h[i] * thish0succ
    f_exp_i <- totals_4h[i] * thish0fail
    s_exp_j <- totals_4h[j] * thish0succ
    f_exp_j <- totals_4h[j] * thish0fail
    this_ts <- this_ts + ((succ_4h[i] - s_exp_i) ^ 2) / s_exp_i
    this_ts <- this_ts + ((succ_4h[j] - s_exp_j) ^ 2) / s_exp_j
    this_ts <- this_ts + ((fail_4h[i] - f_exp_i) ^ 2) / f_exp_i
    this_ts <- this_ts + ((fail_4h[j] - f_exp_j) ^ 2) / f_exp_j
    results <- c(results, c(i, j, pchisq(this_ts, 1, lower.tail=FALSE)))
  }
}


