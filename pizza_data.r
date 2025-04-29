library(dplyr)
library(lubridate)
library(ggplot2) 

pizza_data <- read.csv("data/pizza_requests.csv")

pizza_data <- pizza_data %>%
  mutate(
    request_id                     = request_id,
    request_time                   = as.POSIXct(unix_timestamp_of_request_utc,
                                               origin = "1970-01-01", tz = "UTC"),
    received_pizza                 = ifelse(requester_received_pizza == "True", 1, 0),
    number_upvotes_at_retrieval    = as.numeric(number_of_upvotes_of_request_at_retrieval),
    number_downvotes_at_retrieval  = as.numeric(number_of_downvotes_of_request_at_retrieval),
    account_age_request            = as.numeric(requester_account_age_in_days_at_request),
    account_age_retrieval          = as.numeric(requester_account_age_in_days_at_retrieval)
  )


pizza_data %>%
  mutate(request_date = as.Date(request_time)) %>%
  group_by(request_date) %>%
  summarise(
    n_requests = n(),
    success_rate = mean(received_pizza)
  ) %>%

  ggplot(aes(x = floor_date(request_date,"week"), y = success_rate, color = success_rate)) +
    # geom_line() +
    geom_point(aes(size = n_requests), alpha = 0.5) +
    scale_color_gradient(low = "#C23B23", high = "#976ED7") +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
    scale_size_continuous(name = "Number of Requests") +
    scale_y_continuous(labels = scales::percent_format())    +
    labs(
      title = "Daily Success Rate of Pizza Requests",
      x     = "Date",
      y     = "Percent Received"
    ) +
    theme_minimal()
ggsave("pizza_requests_success_rate.jpeg", width = 8, height = 4)

pizza_data %>%
  mutate(
    hour    = hour(request_time),
    weekday = wday(request_time, label = TRUE, abbr = TRUE)
  ) %>%
  group_by(hour) %>%
  summarise(success_rate = mean(received_pizza)) %>%
  ggplot(aes(x = hour, y = success_rate, fill = factor(hour))) +
    geom_col() +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_fill_viridis_d(name = "Hour") +
    labs(
      title = "Success Rate by Hour of Day",
      x     = "Hour (0–23)",
      y     = "Percent Received"
    ) +
    theme_minimal()
ggsave("pizza_requests_hour.jpeg", width = 8, height = 4)

pizza_data %>%
  mutate(weekday = wday(request_time, label = TRUE)) %>%
  group_by(weekday) %>%
  summarise(success_rate = mean(received_pizza)) %>%
  ggplot(aes(weekday, success_rate, fill=factor(weekday))) +
    geom_col() +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_fill_brewer(palette = "Spectral", name = "Weekday") +
    labs(title = "Success Rate by Day of Week") +
    theme_minimal()
ggsave("pizza_requests_weekday.jpeg", width = 8, height = 4)