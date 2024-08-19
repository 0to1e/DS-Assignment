library(tidyverse)
library(ggradar)
library(scales)

"
################################
#                              #
#  Graphs of Paid House Data   #
#                              #
################################
"

# Load the cleaned house price dataset
house_dataset <- read_csv("Cleaned_datasets/House_Pricing_Data/clean_house_pricing_data.csv")

# Filter the dataset for transactions that occurred in 2023
house_data_2023 <- subset(house_dataset, format(as.Date(Date), "%Y") == "2023")

# Generate a boxplot comparing house prices in Bristol and Cornwall for 2023
ggplot(house_data_2023, aes(x = County, y = log(Price), fill = County)) +
  geom_boxplot() +
  labs(
    title = "Boxplot of House Prices in 2023 for CITY OF BRISTOL and CORNWALL",
    x = "County",
    y = "House Price"
  )


# Save the boxplot as a PNG file
ggsave(paste0(getwd(), "/Graphs/House_Pricing/BoxP_BL_CL_2023.png"))


####################################################################################################################################################

# Calculate the total house price for each town within each county in 2023
town_price_summary <- house_data_2023 %>%
  group_by(County, Town.City) %>%
  summarise(Total_Price = sum(Price))

# Create a stacked bar graph showing the total house prices by town within each county
ggplot(town_price_summary, aes(x = County, y = Total_Price, fill = Town.City)) +
  geom_bar(stat = "identity") + # Stack bars based on the sum
  labs(
    title = "Total House Prices by Town within County (2023)",
    x = "County",
    y = "Total Price"
  ) +
  theme_classic() + # Optional: adjust plot aesthetics
  guides(fill = guide_legend(title = "Town")) # Label the legend as "Town"

# Save the stacked bar graph as a PNG file
ggsave(paste0(getwd(), "/Graphs/House_Pricing/BarC_Stack_Town_BL_CL_2023.png"))

# TODO:###################################################################################################################################################

# Calculate the average house price for each town across all years
avg_prices <- house_dataset %>%
  group_by(Town.City) %>%
  summarize(avg_price = mean(Price, na.rm = TRUE)) %>%
  arrange(desc(avg_price))

# Create a line graph showing the average house price by town
ggplot(avg_prices, aes(x = reorder(Town.City, -avg_price), y = avg_price)) +
  geom_line(group = 1) +
  geom_point() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(
    title = "Average House Prices by Town",
    x = "Town/City",
    y = "Average Price"
  )

# Save the line graph as a PNG file
ggsave(paste0(getwd(), "/Graphs/House_Pricing/Line_Avg_Price_Town.png"))






"
################################
#                              #
#  Graphs of Broadband Data    #
#                              #
################################
"

# Load the cleaned broadband performance dataset
broadband_performance_dataset <- read_csv("Cleaned_datasets/Broadband/clean_broadband_performance.csv")

# Generate a boxplot of average download speeds by county
ggplot(broadband_performance_dataset, aes(x = county, y = `Average download speed (Mbit/s)`, fill = county)) +
  geom_boxplot() + # Create a boxplot to show distribution of average download speeds
  labs(
    title = "Average Download Speed by County", # Title of the plot
    x = "County", # Label for the x-axis
    y = "Average Download Speed (Mbit/s)" # Label for the y-axis
  )

# Save the boxplot as a PNG file in the specified directory
ggsave(paste0(getwd(), "/Graphs/Broadband/average_down_speed_boxplots.png"))

####################################################################################################################################################

# Filter the dataset to include only data for Bristol
bristol_data <- broadband_performance_dataset %>%
  filter(County == "Bristol")

# Filter the dataset to include only data for Cornwall
cornwall_data <- broadband_performance_dataset %>%
  filter(County == "Cornwall")

# Reshape the Bristol data into a long format for easier plotting
long_bristol <- tidyr::gather(bristol_data, key = "speed_type", value = "speed", `Maximum download speed (Mbit/s)`, `Average download speed (Mbit/s)`)

# Reshape the Cornwall data into a long format for easier plotting
long_cornwall <- tidyr::gather(cornwall_data, key = "speed_type", value = "speed", `Maximum download speed (Mbit/s)`, `Average download speed (Mbit/s)`)

# Create a side-by-side bar chart for Bristol showing maximum vs average download speeds by city
ggplot(long_bristol, aes(x = City, y = speed, fill = speed_type)) +
  geom_bar(stat = "identity", position = "dodge") + # Use bars to show the values and dodge to place bars side by side
  labs(
    title = "Bristol: Maximum vs Average Download Speed by City", # Title of the plot
    x = "City", # Label for the x-axis
    y = "Download Speed (Mbit/s)" # Label for the y-axis
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability

# Save the Bristol bar chart as a PNG file in the specified directory
ggsave(paste0(getwd(), "/Graphs/Broadband/bristol_down_median_bar_chart.png"))

####################################################################################################################################################

# Create a side-by-side bar chart for Cornwall showing maximum vs average download speeds by city
ggplot(long_cornwall, aes(x = City, y = speed, fill = speed_type)) +
  geom_bar(stat = "identity", position = "dodge") + # Use bars to show the values and dodge to place bars side by side
  labs(
    title = "Cornwall: Maximum vs Average Download Speed by City", # Title of the plot
    x = "City", # Label for the x-axis
    y = "Download Speed (Mbit/s)" # Label for the y-axis
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability

# Save the Cornwall bar chart as a PNG file in the specified directory
ggsave(paste0(getwd(), "/Graphs/Broadband/cornwall_down_median_bar_chart.png"))







"
################################
#                              #
#  Graphs of Crime Data        #
#                              #
################################
"

# Load population dataset to aceess data on population around Bristol and Cornall
population_2011 <- read_csv("Cleaned_datasets/Population_clean.csv") %>%
  # Convert the data frame to a named vector
  deframe()

# Calculate the total population for Bristol and Cornwall combined
total_population <- sum(population_2011[c("Bristol", "Cornwall")], na.rm = TRUE)

# Add the combined total to the named vector with the name "Total"
population_2011 <- c(population_2011, Total = total_population)

# Remove any NA entries from the named vector
population_2011 <- population_2011[!is.na(names(population_2011))]

# Estimate the population for 2023 using a growth factor
population_2023 <- floor((1.00561255390388033 * population_2011))

####################################################################################################################################################

# Load crime data
crime_dataset <- read_csv("Cleaned_datasets/Crime/Crime_Data_Combined.csv")

# Filter crime data for drug offences between 2020 and 2023
drug_offences_2020_2023 <- crime_dataset %>%
  filter(Crime == "Drugs", year(date) >= 2020 & year(date) <= 2023)

# Calculate the drug offence rate per 10,000 population
drug_offences_rate_2020_2023 <- drug_offences_2020_2023 %>%
  # Extract month and year from the date for grouping
  mutate(
    month = floor_date(ymd(date), "month"),
    year = year(date)
  ) %>%
  # Group by year and county, then count the total number of offences
  group_by(year, County) %>%
  summarize(
    total_offences = n(),
    .groups = "drop"
  ) %>%
  # Join with population data to get population numbers for each county
  left_join(
    tibble(
      County = names(population_2023),
      population = as.numeric(population_2023)
    ),
    by = "County"
  ) %>%
  # Calculate the offence rate per 10,000 population
  mutate(offence_rate = (total_offences / population) * 10000)

# Create a line graph to visualize drug offence rates by year
ggplot(drug_offences_rate_2020_2023, aes(x = year, y = offence_rate, color = County, group = County)) +
  geom_line() + # Add lines to connect points
  geom_point(size = 3) + # Add points to the line graph
  labs(
    title = "Yearly Drug Offence Rate per 10000 Population (2020-2023)", # Title of the plot
    x = "Year", # Label for the x-axis
    y = "Offence Rate per 10000 Population", # Label for the y-axis
    color = "Region" # Label for the color legend
  ) +
  theme_minimal() + # Use a minimal theme for the plot
  theme(
    legend.position = "bottom" # Position the legend at the bottom
  ) +
  scale_x_continuous(breaks = 2020:2023) # Set x-axis breaks to display years 2020 to 2023

# Save the line graph as a PNG file in the specified directory
ggsave(paste0(getwd(), "/Graphs/Crime/LineG_Drug_10000_2023.png"))

####################################################################################################################################################

# Filter drug offences data for the year 2023
drug_offences_2023 <- subset(drug_offences_2020_2023, format(as.Date(date), "%Y") == "2023")

# Calculate drug offence rates for 2023
drug_offences_rate_2023 <- drug_offences_2023 %>%
  filter(Crime == "Drugs") %>% # Ensure the crime type is "Drugs"
  mutate(month = floor_date(ymd(date), "month")) %>% # Extract month from the date
  group_by(month, County) %>% # Group by month and county
  summarize(
    total_offences = n(), # Count total drug offences
    offence_rate = (total_offences / population_2023[County]) * 10000 # Calculate offence rate per 10,000 population
  )

# Create a boxplot to visualize drug offence rates in 2023
ggplot(drug_offences_rate_2023, aes(x = County, y = offence_rate, fill = County)) +
  geom_boxplot() + # Draw boxplots
  labs(
    title = "Drug Offence Rate (2023)", # Title of the plot
    x = "Region", # Label for the x-axis
    y = "Offence Rate " # Label for the y-axis
  )

# Save the boxplot as a PNG file
ggsave(paste0(getwd(), "/Graphs/Crime/BoxP_Drug_BL_CL_2023.png"))

####################################################################################################################################################

# Filter robbery offences data for October 2022
robbery_offences_oct_2023 <- crime_dataset %>%
  filter(
    Crime == "Robbery", # Filter for robbery offences
    year(date) == 2022, # Only data from the year 2022
    month(date) == 10 # Only data from October
  )

# Calculate robbery rates for October 2022
robbery_rates <- robbery_offences_oct_2023 %>%
  group_by(County) %>% # Group by county
  summarize(
    total_offences = n(), # Count total robbery offences
    .groups = "drop" # Drop the grouping after summarizing
  ) %>%
  # Join with population data for each county
  left_join(
    tibble(
      County = names(population_2023),
      population = as.numeric(population_2023)
    ),
    by = "County"
  ) %>%
  mutate(
    offence_rate = (total_offences / population) * 10000, # Calculate offence rate per 10,000 population
    percentage = total_offences / sum(total_offences) * 100
  ) # Calculate percentage of total offences


# Create a pie chart to visualize the distribution of robbery offences in October 2022
ggplot(robbery_rates, aes(x = "", y = percentage, fill = County)) +
  geom_bar(stat = "identity", width = 1) + # Use bars to represent proportions
  coord_polar("y", start = 0) + # Convert bar chart to a pie chart
  geom_text(aes(label = sprintf("%.1f%%\n(%d)", percentage, total_offences)), # Add labels with percentage and count
    position = position_stack(vjust = 0.5)
  ) +
  labs(
    title = "Robbery Offences Distribution (October 2022)", # Title of the plot
    subtitle = paste("Total offences:", sum(robbery_rates$total_offences)), # Subtitle with total offences
    fill = "Region" # Label for the fill legend
  ) +
  theme_void() + # Use a void theme to remove background and axes
  theme(legend.position = "bottom") # Position the legend at the bottom

# Save the pie chart as a PNG file
ggsave(paste0(getwd(), "/Graphs/Crime/Pi_Robbery_oct_2022.png"))








"
@##############################
#                             #
#  Graph of School Data       #
#                             #
###############################
"

# Read the cleaned school dataset and filter out data for Wiltshire
school_dataset <- read_csv("Cleaned_datasets/Schools/Schools_Clean.csv") %>%
  filter(County != "Wiltshire")

# Subset the dataset to include only records for the year 2023
school_data_2023 <- subset(school_dataset, Year == 2023)

# Create a boxplot of ATT8SCR (Attainment 8 Score) by County for the year 2023
ggplot(school_data_2023, aes(x = County, y = ATT8SCR, fill = County)) +
  geom_boxplot() + # Use a boxplot to visualize the distribution of ATT8SCR by County
  labs(
    title = "Boxplot of Attainment 8 Score by County",
    x = "County",
    y = "ATT8SCR"
  ) + # Label the plot
  theme(axis.text.x = element_text(hjust = 1)) # Adjust text alignment for x-axis labels

# Save the boxplot to a file
ggsave(paste0(getwd(), "/Graphs/Schools/BoxP_avg_att8_BL_CL_2023.png"))

# Filter data for Cornwall
cornwall_data <- school_dataset %>%
  filter(County == "Cornwall")

# Filter data for Bristol
bristol_data <- school_dataset %>%
  filter(County == "Bristol")

# Create a line graph of average Attainment 8 Score by City in Cornwall over the years
ggplot(cornwall_data, aes(x = Year, y = ATT8SCR, color = City)) +
  geom_line() + # Use a line graph to show the trend of ATT8SCR by City
  labs(
    title = "Average Attainment 8 Score by City in Cornwall",
    x = "Year",
    y = "Average Attainment 8 Score"
  ) # Label the plot

# Save the boxplot to a file
ggsave(paste0(getwd(), "/Graphs/Schools/line_avg_att8_CL.png"))

# Create a line graph of average Attainment 8 Score by City in Bristol over the years
ggplot(bristol_data, aes(x = Year, y = ATT8SCR, color = City)) +
  geom_line() + # Use a line graph to show the trend of ATT8SCR by City
  labs(
    title = "Average Attainment 8 Score by City in Bristol",
    x = "Year",
    y = "Average Attainment 8 Score"
  ) # Label the plot

# Save the boxplot to a file
ggsave(paste0(getwd(), "/Graphs/Schools/line_avg_att8_BL.png"))