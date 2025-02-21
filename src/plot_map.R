# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(rnaturalearth)
library(readr)

# Load processed IAMC dataset
data <- read_csv("outputs/historical_iso.csv")  

# Convert wide format to long format (Year as a column)
data_long <- data %>%
  pivot_longer(cols = starts_with("19") | starts_with("20"), 
               names_to = "Year", values_to = "Value") %>%
  mutate(Year = as.integer(Year))  # Convert Year to integer

# **USER INPUTS: SELECT VARIABLE TO PLOT**
# Uncomment the variable you want to plot, only ONE should be active

selected_variable <- "Wind Capacity"  # Default selection
# selected_variable <- "CO2 Emissions"
# selected_variable <- "Total Energy Use"
# selected_variable <- "Electricity Generation"
# selected_variable <- "Solar Capacity"
# selected_variable <- "Fossil Fuel Use"
# selected_variable <- "Hydro Capacity"
# selected_variable <- "Nuclear Capacity"
# selected_variable <- "GDP"
# selected_variable <- "Temperature Anomaly"

# **USER INPUT: PER CAPITA OR TOTAL VALUE**
normalize_by_population <- TRUE  # Set to FALSE if you want total values

# **USER INPUT: CHOOSE YEAR**
selected_year <- 2020  # Change year as needed

# Filter dataset for selected variable
variable_data <- data_long %>% filter(variable == selected_variable)

# If per capita normalization is enabled, fetch population data
if (normalize_by_population) {
  population <- data_long %>% filter(variable == "Population")
  
  # Merge datasets on region and year
  variable_data <- variable_data %>%
    left_join(population, by = c("region", "Year")) %>%
    mutate(Value = Value.x / Value.y) %>%  # Compute per-capita value
    select(region, Year, Value)  # Keep only relevant columns
}

# Define unit dynamically based on user choice
unit_label <- ifelse(normalize_by_population, "per capita", unique(variable_data$unit))

# Load world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Merge with spatial data (Ensure matching country codes)
map_data <- world %>%
  left_join(variable_data, by = c("iso_a3" = "region"))

# Filter for selected year
map_data_filtered <- map_data %>% filter(Year == selected_year)

# Plot the map
ggplot(map_data_filtered) +
  geom_sf(aes(fill = Value), color = "white") +
  scale_fill_viridis_c(name = paste(selected_variable, "(", unit_label, ")", sep=" "), na.value = "grey50") +
  theme_minimal() +
  labs(title = paste(selected_variable, "(", selected_year, ")", sep=" "),
       subtitle = paste("Unit:", unit_label, "- Data from IAMC format"))
