library(ggplot2)
library(tidyverse)
library(priceR)
library(ggpmisc)

year_colors <- c(
    # "2019" = "#FF9999",
    "2020" = "#66B3FF",
    "2021" = "#99FF99",
    "2022" = "#FFCC99",
    "2023" = "#FFD700"
)


salary_data <- read.csv("data/USDSalariesDS.csv", stringsAsFactors = FALSE)

salary_data <- salary_data %>%
    mutate(
        year = factor(work_year),
        salary = salary_in_usd,
    )
    
ggplot(salary_data, aes(x = year, y = salary, color = as.factor(year))) +
    geom_jitter(width = 0.2, size = 1.5, alpha = 0.6) + 
    stat_smooth(method = "lm", se = FALSE, color = "steelblue") +
    stat_poly_eq(
        aes(label = paste(after_stat(eq.label), after_stat(rr.label), sep = "~~~")),
        formula = y ~ x, parse = TRUE, label.x = "left"
    ) +
    scale_y_continuous(labels = scales::comma_format(prefix = "$")) +
    labs(x = "Year", y = "Salary (USD)", title = "Salary Distribution by Year") +
    theme_minimal() +
    theme(legend.position = "none")
    
ggsave("yearly_salary_ds.jpeg", width = 8, height = 6)

library(ggridges)

ggplot(salary_data, aes(x=salary, y=as.factor(year), fill=as.factor(year))) +
    geom_density_ridges(alpha=0.7) +
    scale_x_continuous(labels=scales::comma_format(prefix="$")) +
    labs(x="Salary", y="Year", title="Salary Distribution by Year") +
    theme_minimal() +
    theme(legend.position="none")

ggsave("yearly_salary_ds_ridges.jpeg", width = 8, height = 6)

job_data <- read.csv("data/FilteredJobOpportunities2019-2023.csv", stringsAsFactors = FALSE)

job_salary_dates <- job_data %>%
    separate(Salary.Range, into = c('min_sal', 'max_sal'), sep = ' - ') %>% # nolint
    mutate(
        min_sal = parse_number(min_sal) * 1.34,      # drops £ and commas
        max_sal = parse_number(max_sal) * 1.34,
        avg_sal = (min_sal + max_sal) / 2,
        date_post  = mdy(Date.Posted),
        year_posted = year(mdy(Date.Posted)),  # Extract year from date_post
        experience_level = case_when(
            Experience.Level == "Entry-Level" ~ "Entry Level",
            Experience.Level == "Mid-Level" ~ "Mid Level",
            Experience.Level == "Senior" ~ "Senior Level",
            TRUE ~ NA_character_
        )
    )

job_salary_dates <- job_salary_dates %>%
    mutate(year_posted = as.factor(year_posted)) %>%
    arrange(year_posted, date_post)



ggplot(job_salary_dates, aes(x = date_post, y = avg_sal, group = year_posted, color = as.factor(year_posted))) +

    geom_line(linewidth = 1) +
    geom_point(size = 2) +

    geom_smooth(method = "lm", se = FALSE, fill = "lightblue", color = "black", group = 1) +

    scale_color_manual(name = "Year", values = year_colors) +

    scale_y_continuous(labels = scales::comma_format(prefix = "$")) +
    scale_x_date(date_breaks = "3 months", date_labels = "%b %Y", expand = expansion(add = c(0, 0))) +

    labs(
        x     = "Date Posted",
        y     = "Average Salary ($)",
        title = "Salary Trend Over Time"
    ) +

    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("salary_trend.jpeg", width = 8, height = 6)


unemployment <- read.csv("data/2021-2022unemployment.csv")

unemployment_data <- unemployment %>%
    mutate(
        year = Year,
        employed = Employed,
        unemployed = Unemployed,
        rates = Rates
    )

ggplot(unemployment_data, aes(x = year, y = rates)) +
    geom_line(aes(y = rates), color = "red",  linewidth = 1) +
    geom_point(aes(y = rates), color = "red") +
    labs(
        x     = "Year",
        y     = "Rates",
        title = "Unemployement Rate Over Time"
    ) +
    theme_minimal()
ggsave("unemployment_rate.jpeg", width = 8, height = 6)

salary_2021_2025 <- read.csv("data/Salary_2021-2025.csv")

salary_2021_2025 <- salary_2021_2025 %>%
    mutate(
        year = Year,
        sal = Salary
    )

ggplot(salary_2021_2025, aes(x = year, y = sal)) +
    geom_line(aes(y = sal), color = "blue",  linewidth = 1) +
    geom_point(aes(y = sal), color = "blue") +
    labs(
        x     = "Year",
        y     = "Salary",
        title = "Salary Over Time"
    ) +
    theme_minimal()
ggsave("salary_over_time.jpeg", width = 8, height = 6)