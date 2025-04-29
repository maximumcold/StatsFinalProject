library(ggplot2)
library(tidyverse)
library(priceR)
library(ggpmisc)
par(bg = 'black')
# -------------------------------------------------------------------------------
# Colors for the years
# -------------------------------------------------------------------------------

year_colors <- c(
    "2010" = "#FF9999",
    "2011" = "#FFCC99",
    "2012" = "#FFFF99",
    "2013" = "#CCFF99",
    "2014" = "#99FFCC",
    "2015" = "#99CCFF",
    "2016" = "#CC99FF",

    "2020" = "#66B3FF",
    "2021" = "#99FF99",
    "2022" = "#FFCC99",
    "2023" = "#FFD700"
)
# -------------------------------------------------------------------------------
# Degrees Awarded Over Time
# -------------------------------------------------------------------------------

enrollment_data <- read.csv("data/degrees_awarded_by_year.csv", stringsAsFactors = FALSE)

enrollment_long <- enrollment_data %>%
  pivot_longer(
    cols = -`Field.of.study`,
    names_to = "Year",
    values_to = "Students"
  )

enrollment_data <- read.csv("data/degrees_awarded_by_year.csv", stringsAsFactors = FALSE)

enrollment_long <- enrollment_data %>%
  pivot_longer(
    cols = -`Field.of.study`,      
    names_to = "Year", 
    values_to = "Degrees_Awarded"   
  )
enrollment_long <- enrollment_long %>%
  mutate(Year = str_replace(Year, "-", "/")) 
  enrollment_long <- enrollment_long %>%
  mutate(
    Year = str_replace(Year, "^X", ""),
    Year = str_replace(Year, "\\.", "-"),
    Year = factor(Year, levels = unique(Year))
  )

ggplot(enrollment_long, aes(x = Year, y = Degrees_Awarded, group = `Field.of.study`, color = Year)) +
  geom_line(linewidth = 1) +
    geom_point() +
    labs(
        title = "Degrees Awarded Over Time",
        x = "Academic Year",
        y = "Number of Degrees Awarded",
        color = "Field of Study"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("degrees_awarded_over_time.jpeg", width = 8, height = 6)

# -------------------------------------------------------------------------------
# Unemployment Rate Over Time
# -------------------------------------------------------------------------------
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

# -------------------------------------------------------------------------------
# Salary Over Time
# -------------------------------------------------------------------------------

salary_2010_2025 <- read.csv("data/salary_2010-2025.csv")

salary_2010_2025 <- salary_2010_2025 %>%
    mutate(
        year = Year,
        sal = Salary
    )

ggplot(salary_2010_2025, aes(x = year, y = sal)) +
    geom_line(aes(y = sal), color = "blue",  linewidth = 1) +
    geom_point(aes(y = sal), color = "blue") +
    scale_y_continuous(labels = scales::comma_format(prefix = "$")) +
    labs(
        x     = "Year",
        y     = "Salary",
        title = "Salary Over Time"
    ) +
    theme_minimal()
ggsave("salary_over_time.jpeg", width = 8, height = 6)

# -------------------------------------------------------------------------------
# Workforce Over Time
# -------------------------------------------------------------------------------

workforce_data <- read.csv("data/Employment_Over_Time.csv")

workforce_data <- workforce_data %>%
    mutate(
        year = Year,
        gender = Gender,
        total_population = Total.Population
    )

workforce_data <- workforce_data %>%
    group_by(year) %>%
    summarise(total_population = sum(total_population, na.rm = TRUE))

work_force_colors <- c(
    "2014" = "#FFB3BA",
    "2015" = "#FFDFBA",
    "2016" = "#FFFFBA",
    "2017" = "#BAFFC9",
    "2018" = "#BAE1FF",
    "2019" = "#D5BAFF",
    "2020" = "#FFB3E6",
    "2021" = "#B3E6FF",
    "2022" = "#E6FFB3"
)

ggplot(workforce_data, aes(x = year, y = total_population, color = year, group = 1)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_y_continuous(labels = scales::comma_format()) +
    labs(
        x     = "Year",
        y     = "Total Population",
        title = "Workforce Over Time",
        color = "Year"
    ) +
    theme_minimal()
ggsave("workforce_over_time.jpeg", width = 8, height = 6)

# -------------------------------------------------------------------------------
# Layoffs Over Time
# -------------------------------------------------------------------------------

layoffs_data <- read_csv("data/FilteredLayoffData.csv",
  col_types = cols(
    Company   = col_character(),
    `Laid Off` = col_double(),
    Date      = col_character()
  )
) %>%
  mutate(
    date    = mdy(Date),
    layoffs = `Laid Off`
  )

monthly_totals <- layoffs_data %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(total_layoffs = sum(layoffs, na.rm = TRUE)) %>%
  ungroup()

ggplot(monthly_totals, aes(x = month, y = total_layoffs)) +
  geom_line(color = "purple", linewidth = 1) +
  geom_point(color = "purple", size = 2) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y",
    expand = expansion(add = c(0, 0))
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x     = "Month",
    y     = "Total Layoffs",
    title = "Monthly Layoffs Trend"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
  ggsave("layoffs_over_time.jpeg", width = 8, height = 6)