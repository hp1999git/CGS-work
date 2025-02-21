# Load required libraries
library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(readr)

# Load processed IAMC dataset
data <- read_csv("outputs/historical_iso.csv")  # Adjust path if needed

# Select wind capacity and population data (modify based on actual dataset columns)
wind_capacity <- data %>% filter(Variable == "Wind Capacity")
population <- data %>% filter(Variable == "Population")

# Merge datasets
data_merged <- wind_capacity %>%
  left_join(population, by = c("ISO" = "ISO")) %>%
  mutate(PerCapita_Wind = Value.x / Value.y)  # Calculate wind capacity per capita

# Load world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Merge with spatial data
map_data <- world %>%
  left_join(data_merged, by = c("iso_a3" = "ISO"))

# Plot the map
ggplot(map_data) +
  geom_sf(aes(fill = PerCapita_Wind), color = "white") +
  scale_fill_viridis_c(name = "Wind Capacity (kW/person)", na.value = "grey50") +
  theme_minimal() +
  labs(title = "Global Wind Capacity per Capita", subtitle = "Data from IAMC format")
