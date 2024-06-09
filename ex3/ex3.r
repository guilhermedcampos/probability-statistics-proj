library(readxl)
library(ggplot2)

# Reads the data, then filters it and creates an additional column for date.
renewables_data <- read_excel("electricity.xlsx") |>
  subset(select = c("COUNTRY", "YEAR", "MONTH", "PRODUCT", "share")) |>
  transform(
    share = as.numeric(share),
    Date = as.Date(paste(YEAR, MONTH, "01", sep = "-"))
  ) |>
  subset(COUNTRY %in% c("IEA Total", "Hungary", "Iceland") & YEAR >= 2015 & PRODUCT == "Renewables")

# Create plot
renewables_data |>
  ggplot(aes(x = Date, y = share * 100, color = COUNTRY)) +
  geom_line(aes(group = COUNTRY)) +  # Separate lines by country
  geom_point() +  # Add points for each month
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_date(limits = as.Date(c("2015-01-01", "2022-12-01")), 
               date_breaks = "1 year", date_labels = "%Y") +  # Show every single year 
  labs(title = "Evolution of the Monthly Production of Renewable Energy",
       x = "Year",
       y = "Share of Renewable energy (%)",
       color = "Country") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center title
  )

