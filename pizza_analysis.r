library(tidyverse)

pizza_data <- read.csv("data/pizza_requests.csv")

clean_data <- pizza_data %>%
  mutate(
    request_id                     = request_id,
    request_time                   = as.POSIXct(unix_timestamp_of_request_utc,
                                               origin = "1970-01-01", tz = "UTC"),
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


hour <- c(hour(clean_data$request_time))

totalpizza <- sum(clean_data$successes)
earlypizza <- sum(clean_data$successes[hour <= 4])
morningpizza <- sum(clean_data$successes[hour > 4 & hour <= 8])
breakfastpizza <- sum(clean_data$successes[hour > 8 & hour <= 12])
middaypizza <- sum(clean_data$successes[hour > 12 & hour <= 16])
dinnerpizza <- sum(clean_data$successes[hour > 16 & hour <= 20])
eveningpizza <- sum(clean_data$successes[hour > 20])
pizzasuccesses <- c(earlypizza, morningpizza, breakfastpizza, middaypizza, dinnerpizza, eveningpizza)

ftotalpizza <- sum(clean_data$failures)
fearlypizza <- sum(clean_data$failures[hour <= 4])
fmorningpizza <- sum(clean_data$failures[hour > 4 & hour <= 8])
fbreakfastpizza <- sum(clean_data$failures[hour > 8 & hour <= 12])
fmiddaypizza <- sum(clean_data$failures[hour > 12 & hour <= 16])
fdinnerpizza <- sum(clean_data$failures[hour > 16 & hour <= 20])
feveningpizza <- sum(clean_data$failures[hour > 20])
pizzafailures <- c(fearlypizza, fmorningpizza, fbreakfastpizza, fmiddaypizza, fdinnerpizza, feveningpizza)

for (i in 1:length(pizzasuccesses)) {
  pizzatotals <- c(pizzatotals, pizzasuccesses[i] + pizzafailures[i])
}

h0successrate <- totalpizza / ftotalpizza
h0failurerate <- 1 - h0successrate

csq <- 0
for (i in 1:length(pizzasuccesses)) {
  s_expected <- pizzatotals[i] * h0successrate
  f_expected <- pizzatotals[i] * h0failurerate
  csq <- csq + ((pizzasuccesses[i] - s_expected) ^ 2) / s_expected
  csq <- csq + ((pizzafailures[i] - f_expected) ^ 2) / f_expected
}

prob_timeofday <- pchisq(csq, 5, lower.tail = FALSE)


#   %>%
#   summarise(success_rate = mean(received_pizza))

posts <- pizza_data$requester_number_of_posts_at_request
mean_posts <- mean(posts)

postsgt <- pizza_data %>% filter(posts >= mean_posts)
postslt <- pizza_data %>% filter(posts < mean_posts)


