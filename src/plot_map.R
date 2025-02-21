# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(rnaturalearth)
library(readr)
library(viridis)
library(scales)

#setwd("C:/Users/harsh/Downloads")

# Load processed IAMC dataset
data <- read_csv("outputs/historical_iso.csv") #modified to outputs/ for github

# Convert wide format to long format (Year as a column)
data_long <- data %>%
  pivot_longer(cols = starts_with("19") | starts_with("20"), 
               names_to = "Year", values_to = "Value") %>%
  mutate(Year = as.integer(Year))  # Convert Year to integer

# Print diagnostic information
print(paste("Available units:", paste(unique(variable_data$unit), collapse = ", ")))
print(paste("Available years:", paste(unique(variable_data$Year), collapse = ", ")))
print(paste("Number of countries with data:", length(unique(variable_data$region))))

# Variable and unit selection (change as per requirement)
selected_variable <- "Capacity|Electricity|Wind"
selected_unit <- "GW"  # Unit for wind capacity
selected_year <- 2020  # Year selection

# Optional settings
normalize_by_population <- FALSE  # Set to FALSE for total values
normalize_by_gdp <- FALSE  # Set to FALSE if not normalizing by GDP

# Filter dataset for selected variable and unit
variable_data <- data_long %>% 
  filter(variable == selected_variable, unit == selected_unit)

# Load world map data
world <- ne_countries(scale = "medium", returnclass = "sf")
world <- st_transform(world, crs = 4326)

# Convert region names to uppercase in both data frames
variable_data <- variable_data %>%
  mutate(region = toupper(region))
world <- world %>%
  mutate(iso_a3 = toupper(iso_a3))

# Merge with spatial data
map_data <- world %>%
  left_join(variable_data, by = c("iso_a3" = "region"))

# Filter for selected year
map_data_filtered <- map_data %>% 
  filter(Year == selected_year)

# Create the plot
ggplot(map_data_filtered) +
  geom_sf(aes(fill = Value), color = "white") +
  scale_fill_viridis_c(
    option = "plasma", 
    na.value = "grey50", 
    direction = -1,
    labels = scales::number,
    name = "GW"  # Add unit to legend
  ) +
  theme_minimal() +
  labs(
    title = paste("Wind Power Capacity by Country", "(", selected_year, ")", sep=" "),
    subtitle = "Unit: Gigawatts (GW)"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "right"
  )
