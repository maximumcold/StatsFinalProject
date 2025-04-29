pizza_data <- read.csv("data/pizza_requests.csv")

pizza_data <- pizza_data %>%
    mutate(
        year = Year