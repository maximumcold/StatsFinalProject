library(ggplot2)
library(tidyverse)


job_data <- read.csv("data/FilteredJobOpportunities2019-2023.csv")

job_salary_dates <- job_data %>%
    separate(Salary.Range, into = c('min_sal', 'max_sal'), sep = ' - ') %>% # nolint
    mutate(
        min_sal = parse_number(min_sal),      # drops £ and commas
        max_sal = parse_number(max_sal),
        avg_sal = (min_sal + max_sal) / 2,
        date_post  = mdy(Date.Posted),
        year_posted = year(mdy(Date.Posted))  # Extract year from date_post
    )

date_color <- job_salary_dates %>%
    distinct(year_posted) %>%
    mutate(color = scales::brewer_pal(palette = "Pastel1")(n())) %>%
    deframe()

job_salary_dates <- job_salary_dates %>%
    mutate(date_color = date_color[as.character(year_posted)])

ggplot(job_salary_dates, aes(x = date_post, y = avg_sal, color = date_color)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::comma_format(prefix = "£")) +
  labs(
    x     = "Date Posted",
    y     = "Average Salary (£)",
    title = "Salary Trend Over Time"
  ) +
  theme_minimal()
