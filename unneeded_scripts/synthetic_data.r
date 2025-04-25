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
