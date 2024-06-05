library(ggplot2)

# Read data
data <- read.csv("master.csv")

# Select only data for year 2002 and age group 55-74 years
selected_data <- subset(data, year == 2002 & age == "55-74 years")

selected_data |>
  ggplot(aes(x = sex, y = suicides.100k.pop)) +
  geom_boxplot() +
  labs(
    title = "Comparison of Suicides per 100k Population between Genders (2002, Age 55-74)",
    x = "Sex",
    y = "Suicides per 100k Population",
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5)
  )

