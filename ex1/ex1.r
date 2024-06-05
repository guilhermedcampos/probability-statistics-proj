library(ggplot2)

# Read data
data <- read.csv("Paises_PIB_ICH.csv")

# Filter data to only "Asia" and "Africa"
filtered_data <- subset(data, Continent %in% c("Asia", "Africa"))

# Highlight the following countries
highlight_countries <- c("United Arab Emirates", "Nepal", "Comoros", "Namibia")

# Filter the data 
highlighted_data <- subset(filtered_data, Country %in% highlight_countries)

# Create the graph
filtered_data |>
  ggplot() + 
  geom_point(aes(x = GDP, y = HCI, color = Continent)) +
  geom_point(data = highlighted_data,
             aes(x = GDP, y = HCI), size = 2, shape = 21, fill = "black") + 
  geom_text(data = highlighted_data,
            aes(x = GDP, y = HCI, label = Country, color = Continent), 
            vjust = -1, check_overlap = TRUE, show.legend = FALSE) +
  scale_x_log10() +
  labs(
    title = "Human Capital Index vs GDP per Capita",
    x = "GDP per Capita (log scale)",
    y = "Human Capital Index (HCI)",
    color = "Continent"
  ) +
  theme_minimal() + 
  theme(
    legend.title = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0)),
    plot.title = element_text(hjust = 0.5)  # Center title
  )

