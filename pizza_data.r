library(dplyr)
library(lubridate)
library(ggmosaic)
library(ggplot2) 

unfiltered_pizza_data <- read.csv("data/pizza_requests.csv")

clean_data <- unfiltered_pizza_data %>%
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

pizza_data <- read.csv("data/pizza_requests.csv")

pizza_data <- pizza_data %>%
  mutate(
    request_id                     = request_id,
    request_time                   = as.POSIXct(unix_timestamp_of_request_utc,
                                               origin = "1970-01-01", tz = "America/Chicago"),
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

  ggplot(aes(x = floor_date(request_date, "month"), y = success_rate, color = success_rate)) +
    # geom_line() +
    geom_point(aes(size = n_requests), alpha = 0.5) +
    scale_color_gradient(low = "#C23B23", high = "#976ED7") +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
    scale_size_continuous(name = "Number of Requests") +
    scale_y_continuous(labels = scales::percent_format())    +
    labs(
      title = "Monthly Success Rate of Pizza Requests",
      x     = "Month",
      y     = "Percent Received"  
    ) +
    theme_minimal()
ggsave("pizza_requests_success_rate.jpeg", width = 8, height = 4)

pizza_data %>%
  mutate(
    hour = (hour(request_time) + 1) %% 24,  # Shift hours to start at 05:00
    hour = factor(hour, levels = c(5:23, 0:4), labels = c(5:23, 0:4))  # Reorder and relabel hours
  ) %>%
  group_by(hour) %>%
  summarise(success_rate = mean(received_pizza)) %>%
  ggplot(aes(x = hour, y = success_rate, fill = hour)) +
    geom_col() +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_fill_viridis_d(name = "Hour") +
    labs(
      title = "Success Rate by Hour of Day (5:00–4:00)",
      x     = "Hour (5–4)",
      y     = "Percent Received"
    ) +
    theme_minimal()
ggsave("pizza_requests_hour_cols.jpeg", width = 8, height = 4)

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


# num_requests_by_hour <- clean_data %>%
#   mutate(hour = (hour(request_time) + 1) %% 24) %>%
#   group_by(hour) %>%
#   summarise(n_requests = n())

# num_requests_by_hour <- num_requests_by_hour %>%
#   mutate(hour = factor(hour, levels = c(5:23, 0:4), labels = c(5:23, 0:4)))

# num_requests_by_hour %>%
#   ggplot(aes(x = hour, y = n_requests)) +
#     geom_col() +
#     scale_y_continuous(labels = scales::comma_format()) +
#     labs(
#       title = "Number of Requests by Hour of Day",
#       x     = "Hour (5–4)",
#       y     = "Number of Requests"
#     ) +
#     theme_minimal()
# ggsave("pizza_requests_hour.jpeg", width = 8, height = 4)
pizza_data %>%
  mutate(
    hour_group = (floor((hour(request_time) - 5) %% 24 / 2) * 2 + 5) %% 24,
    hour_group = factor(hour_group, levels = c(5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 1, 3), 
                        labels = c("5–6", "7–8", "9–10", "11–12", "13–14", "15–16", "17–18", 
                                   "19–20", "21–22", "23–0", "1–2", "3–4"))
  ) %>%
  group_by(hour_group) %>%
  summarise(success_rate = mean(received_pizza, na.rm = TRUE)) %>%
  ggplot(aes(x = hour_group, y = success_rate, fill = hour_group)) +
    geom_col() +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_fill_brewer(palette = "Paired", name = "Hour Group") +
    labs(
      title = "Success Rate by 2-Hour Time Blocks (5:00–4:00)",
      x     = "Time of Day",
      y     = "Percent Received"
    ) +
    theme_minimal()
ggsave("pizza_requests_hour_hist_2hour.jpeg", width = 8, height = 4)

num_requests_by_hour <- clean_data %>%
  mutate(hour = (hour(request_time) + 1) %% 24) %>%
  group_by(hour) %>%
  summarise(n_requests = n())

num_requests_by_hour <- num_requests_by_hour %>%
  mutate(hour = factor(hour, levels = c(5:23, 0:4), labels = c(5:23, 0:4)))

num_requests_by_hour %>%
  ggplot(aes(x = hour, y = n_requests, group = 1, color = hour)) +
    geom_line(linewidth = 1) +
    scale_y_continuous(labels = scales::comma_format()) +
    scale_color_viridis_d(name = "Hour", option = "C") +
    labs(
      title = "Number of Requests by Hour of Day",
      x     = "Hour (5–4)",
      y     = "Number of Requests"
    ) +
    theme_minimal()
ggsave("pizza_requests_hour_line.jpeg", width = 8, height = 4)

num_requests_by_hour %>%
  ggplot(aes(x = hour, y = n_requests, fill = hour)) +
    geom_col() +
    scale_y_continuous(labels = scales::comma_format()) +
    scale_fill_viridis_d(name = "Hour", option = "C") +
    labs(
      title = "Number of Requests by Hour of Day",
      x     = "Hour (5–4)",
      y     = "Number of Requests"
    ) +
    theme_minimal()
ggsave("pizza_requests_hour.jpeg", width = 8, height = 4)


# Step 1: Preprocess data to get both success rate and request counts by hour
pizza_hour_summary <- pizza_data %>%
  mutate(
    hour = (hour(request_time) + 1) %% 24,
    hour = factor(hour, levels = c(5:23, 0:4), labels = c(5:23, 0:4))
  ) %>%
  group_by(hour) %>%
  summarise(
    success_rate = mean(received_pizza, na.rm = TRUE),
    n_requests = n()
  )

# Step 2: Plot
ggplot(pizza_hour_summary, aes(x = hour)) +
  geom_col(aes(y = success_rate, fill = hour), width = 0.9) +
  geom_line(aes(y = n_requests / max(n_requests)), group = 1, color = "#957DAD", linewidth = 1.2, linetype = "dashed") +
  
  scale_y_continuous(
    name = "Percent Received",
    labels = scales::percent_format(),
    sec.axis = sec_axis(~ . * max(pizza_hour_summary$n_requests), name = "Number of Requests", labels = scales::comma_format())
  ) +
  scale_fill_viridis_d(name = "Hour", option = 'A') +
  labs(
    title = "Success Rate and Number of Requests by Hour (5:00–4:00)",
    x     = "Hour of Day",
    y     = "Percent Received"
  ) +
  theme_minimal() +
  theme(
    axis.title.y.right = element_text(color = "black"),
    axis.text.y.right  = element_text(color = "black"),
    axis.title.y.left  = element_text(color = "black"),
    axis.text.y.left   = element_text(color = "black"),
    legend.position     = "none"
  )

# Save the combined plot
ggsave("pizza_requests_hour_combined.jpeg", width = 8, height = 4)

did_use_please_pizza <- read.csv("data/please_pizza.csv") %>%
  mutate(
    used_please = "Used 'Please'",
    outcome = ifelse(requester_received_pizza == "True", "Received Pizza", "Did Not Receive Pizza")
  )

did_not_use_please_pizza <- read.csv("data/no_please_pizza.csv") %>%
  mutate(
    used_please = "Did Not Use 'Please'",
    outcome = ifelse(requester_received_pizza == "True", "Received Pizza", "Did Not Receive Pizza")
  )

# Combine datasets
combined_pizza <- bind_rows(did_use_please_pizza, did_not_use_please_pizza)

# Count outcomes by group
please_pizza_summary <- combined_pizza %>%
  count(used_please, outcome)

# Set custom color palette
outcome_colors <- c("Received Pizza" = "#4CAF50",   # Green
                    "Did Not Receive Pizza" = "#F44336")  # Red

# Plot
please_pizza_summary %>%
  ggplot(aes(x = used_please, y = n, fill = outcome)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = outcome_colors) +
  labs(
    title = "Pizza Request Outcomes by 'Please' Usage",
    x     = "'Please' Usage",
    y     = "Number of Requests"
  ) +
  theme_minimal() +
  theme(legend.position = "top")
ggsave("pizza_requests_please_usage.jpeg", width = 8, height = 4)

please_pizza_summary %>%
  ggplot() +
  geom_mosaic(aes(weight = n, x = product(used_please), fill = outcome)) +
  scale_fill_manual(values = outcome_colors) +
  labs(
    title = "Pizza Request Outcomes by 'Please' Usage",
    x     = "'Please' Usage",
    y     = "Proportion of Requests"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

ggsave("pizza_requests_please_usage_mosaic.jpeg", width = 8, height = 4)
