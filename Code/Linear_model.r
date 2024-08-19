library(tidyverse)

school_dataset <- read_csv("Cleaned_datasets/Schools/school_cleaned_data.csv")
house_dataset <- read_csv("Cleaned_datasets/House_Pricing_Data/clean_house_pricing_data.csv")


population_2011 <- read_csv("Cleaned_datasets/Population_clean.csv") %>%
  deframe()

total_population <- sum(population_2011[c("Bristol", "Cornwall")], na.rm = TRUE)
# Add the total to the list with the name "Total"
population_2011 <- c(population_2011, Total = total_population)
# Remove the NA entry if it exists
population_2011 <- population_2011[!is.na(names(population_2011))]


population_2023 <- floor((1.00561255390388033 * population_2011))

merged_dataset <- merge(
  school_dataset,
  house_dataset,
  by = "PostalCode", # Use the updated column name for matching
  all.x = TRUE, # Include all rows from the school_dataset even if there is no matching data in house_dataset
  all.y = FALSE # Exclude rows from house_dataset if there is no matching row in school_dataset
) %>%
  filter(!is.na(ATT8SCR), !is.na(Price))

model <- lm(ATT8SCR ~ Price, data = merged_dataset)

# Check the model summary
summary(model)

ggplot(merged_dataset, aes(x = Price, y = ATT8SCR)) +
  geom_point(color = "blue", alpha = 0.5) + # Scatter plot of data points
  geom_smooth(method = "lm", color = "red", se = FALSE) + # Line of best fit
  labs(
    title = "Line of best fit between School ATT8 Scores and House Prices",
    x = "House Price",
    y = "ATT8 Score"
  ) +
  theme_minimal()

ggsave(paste0(getwd(), "/Graphs/Linear_Models/Line of best fit between School ATT8 Scores and House Prices.png"))

####################################################################################################################################################

school_dataset <- read_csv("Cleaned_datasets/Schools/school_cleaned_data.csv")
broadband_performance_dataset <- read_csv("Cleaned_datasets/Broadband/clean_broadband_performance.csv")

merged_dataset <- merge(
  broadband_performance_dataset,
  school_dataset,
  by = "County", # Use the updated column name for matching
  all.x = TRUE, # Include all rows from the school_dataset even if there is no matching data in house_dataset
  all.y = FALSE # Exclude rows from house_dataset if there is no matching row in school_dataset
) %>%
  filter(!is.na(`Average download speed (Mbit/s)`), !is.na(ATT8SCR))

# Fit a linear model
model <- lm(ATT8SCR ~ `Average download speed (Mbit/s)`, data = merged_dataset)

# Check the model summary
summary(model)

# Create a scatter plot with the line of best fit
ggplot(merged_dataset, aes(x = `Average download speed (Mbit/s)`, y = ATT8SCR)) +
  geom_point(color = "blue", alpha = 0.5) + # Scatter plot of data points
  geom_smooth(method = "lm", color = "red", se = FALSE) + # Line of best fit
  labs(
    title = "Line of best fit between Average Download Speed and Attainment 8 Score",
    x = "Average Download Speed (Mbit/s)",
    y = "Attainment 8 Score"
  ) +
  theme_minimal()

ggsave(paste0(getwd(), "/Graphs/Linear_Models/Line of best fit between Average Download Speed and Attainment 8 Score.png"))
####################################################################################################################################################

broadband_dataset <- read_csv("Cleaned_datasets/Broadband/clean_broadband_performance.csv")

drug_offences_2020_2023 <- read_csv("Cleaned_datasets/Crime/Crime_Data_Combined.csv") %>%
  filter(Crime == "Drugs", year(date) >= 2020 & year(date) <= 2023)

population_dataset <- read_csv("Obtained_Data/Population2011_1656567141570.csv") %>%
  mutate(Postcode = substr(Postcode, start = 1, stop = 2))

postcode_dataset <- read_csv("Cleaned_datasets/Postcode_clean.csv") %>%
  select(pcd7, County) %>%
  mutate(pcd7 = substr(pcd7, start = 1, stop = 2)) %>%
  distinct()

population_2011 <- read_csv("Cleaned_datasets/Population_clean.csv") %>%
  deframe()

total_population <- sum(population_2011[c("Bristol", "Cornwall")], na.rm = TRUE)
# Add the total to the list with the name "Total"
population_2011 <- c(population_2011, Total = total_population)
# Remove the NA entry if it exists
population_2011 <- population_2011[!is.na(names(population_2011))]


population_2023 <- floor((1.00561255390388033 * population_2011))

drug_offences_rate_2020_2023 <- drug_offences_2020_2023 %>%
  # Extract month and year from the date for grouping
  mutate(
    month = floor_date(ymd(date), "month"),
    year = year(date)
  ) %>%
  # Group by year and county, then count the total number of offences
  group_by(year, City, County) %>%
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
  mutate(offence_rate = (total_offences / population) * 10000) %>%
  rename("Street" = "City")


# Merge datasets based on common columns, such as post codes or county
# Here, we'll merge on street as an example
merged_dataset <- merge(
  broadband_dataset,
  drug_offences_rate_2020_2023,
  by = "Street",
  all.x = TRUE,
  all.y = FALSE
) %>%
  na.omit()

# Fit a linear model
model <- lm(offence_rate ~ `Average download speed (Mbit/s)`, data = merged_dataset)

# Check the model summary
summary(model)


# Create a scatter plot with the line of best fit
ggplot(merged_dataset, aes(x = `Average download speed (Mbit/s)`, y = offence_rate)) +
  geom_point(color = "blue", alpha = 0.5) + # Scatter plot of data points
  geom_smooth(method = "lm", color = "red", se = FALSE) + # Line of best fit
  labs(
    title = "Line of best fit between Average Download Speed and Drug Offence Rates per 10000 people",
    x = "Average Download Speed (Mbit/s)",
    y = "Drug Offence Rate per 1000 Population"
  ) +
  theme_minimal()


ggsave(paste0(getwd(), "/Graphs/Linear_Models/Line of best fit between Average Download Speed and Drug Offence Rates per 10000 people.png"))

####################################################################################################################################################

house_dataset_2023 <- read_csv("Cleaned_datasets/House_Pricing_Data/clean_house_pricing_data.csv") %>%
  filter(format(as.Date(Date), "%Y") == "2023")

drug_offences_2023 <- read_csv("Cleaned_datasets/Crime/Crime_Data_Combined.csv") %>%
  filter(Crime == "Drugs", year(date) == 2023)

population_2011 <- read_csv("Cleaned_datasets/Population_clean.csv") %>%
  deframe()

total_population <- sum(population_2011[c("Bristol", "Cornwall")], na.rm = TRUE)
# Add the total to the list with the name "Total"
population_2011 <- c(population_2011, Total = total_population)
# Remove the NA entry if it exists
population_2011 <- population_2011[!is.na(names(population_2011))]


population_2023 <- floor((1.00561255390388033 * population_2011))

drug_offences_rate_2023 <- drug_offences_2023 %>%
  # Extract month and year from the date for grouping
  mutate(
    month = floor_date(ymd(date), "month"),
    year = year(date)
  ) %>%
  # Group by year and county, then count the total number of offences
  group_by(year, City, County) %>%
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
  mutate(offence_rate = (total_offences / population)) %>%
  rename("Street" = "City")

merged_data <- left_join(house_dataset_2023, drug_offences_rate_2023, by = c("Street", "County")) %>%
  na.omit()



model <- lm(Price ~ offence_rate, data = merged_data)

# Summary of the model to view details
summary(model)

ggplot(merged_data, aes(x = offence_rate, y = Price)) +
  geom_point(color = "blue") + # Scatter plot of the data
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  scale_y_continuous(trans = "log10") + # Plot the data points# Line of best fit
  labs(
    title = "Line of best fit between House Price and Drug Offence Rate (2023)",
    x = "Offence Rate",
    y = "House Price"
  ) +
  theme_minimal()

ggsave(paste0(getwd(), "/Graphs/Linear_Models/Line of best fit between House Price and Drug Offence Rate (2023).png"))

####################################################################################################################################################

house_dataset <- read_csv("Cleaned_datasets/House_Pricing_Data/clean_house_pricing_data.csv")
broadband_performance_dataset <- read_csv("Cleaned_datasets/Broadband/clean_broadband_performance.csv")

merged_data <- left_join(house_dataset, broadband_performance_dataset, by = "PostalCode") %>%
  na.omit()


model <- lm(Price ~ `Average download speed (Mbit/s)`, data = merged_data)

# Summary of the model to view details
summary(model)

# Plotting the data and the line of best fit
ggplot(merged_data, aes(x = `Average download speed (Mbit/s)`, y = Price)) +
  geom_point(color = "blue") + # Scatter plot of the data
  scale_y_continuous(trans = "log10") + # Plot the data points
  geom_smooth(method = "lm", color = "red", se = FALSE) + # Line of best fit
  labs(
    title = "Line of best fit between House Price and Average Download Speed",
    x = "Average Download Speed (Mbit/s)",
    y = "House Price"
  ) +
  theme_minimal()

ggsave(paste0(getwd(), "/Graphs/Linear_Models/Line of best fit between House Price and Average Download Speed.png"))
