library(tidyverse)
library(openxlsx)

# ################################
# #                              #
# #  Ranking of Schools          #
# #                              #
# ################################

school_dataset <- read_csv("Cleaned_datasets/Schools/school_cleaned_data.csv")

# Rank and select top 10 entries for each county
top_schools_by_county <- school_dataset %>%
  group_by(County) %>%
  arrange(desc(ATT8SCR)) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 10) %>%
  ungroup()

# Print the result
print(top_schools_by_county)

# ################################
# #                              #
# #  Ranking of House Pricing    #
# #                              #
# ################################
house_pricing_dataset <- read_csv("Cleaned_datasets/House_Pricing_Data/clean_house_pricing_data.csv")
house_rankings <- house_pricing_dataset %>%
  group_by(County, Town.City) %>%
  # Calculate percentile values for price distribution
  summarize(
    p25 = quantile(Price, 0.25, na.rm = TRUE),
    p50 = quantile(Price, 0.50, na.rm = TRUE),
    p75 = quantile(Price, 0.75, na.rm = TRUE),
    skewness = (mean(Price, na.rm = TRUE) - median(Price, na.rm = TRUE)) / sd(Price, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Calculate a score based on your formula
  mutate(score = p25 * 0.25 + p50 * 0.35 + p75 * 0.4 - skewness * 1000) %>%
  # Rank cities within each county based on score
  group_by(County) %>%
  arrange(desc(score)) %>%
  mutate(rank = row_number()) %>%
  # Filter for top 10 cities in each county
  filter(rank <= 10) %>%
  ungroup()

# Print the result
print(house_rankings)




# ################################
# #                              #
# #  Ranking of Broadband        #
# #                              #
# ################################



broadband_coverage_dataset <- read_csv("Cleaned_datasets/Broadband/clean_broadband_coverage.csv")
broadband_performance_dataset <- read_csv("Cleaned_datasets/Broadband/clean_broadband_performance.csv")



# Normalize the data
normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# Merge the datasets by PostalCode
combined_dataset <- broadband_coverage_dataset %>%
  inner_join(broadband_performance_dataset, by = c("PostalCode", "oa11cd", "Street", "City", "County"))


combined_dataset <- combined_dataset %>%
  mutate(
    SFBB_norm = normalize(`SFBB availability (% premises)`),
    UFBB_norm = normalize(`UFBB availability (% premises)`),
    FTTP_norm = normalize(`FTTP availability (% premises)`),
    MaxDownload_norm = normalize(`Maximum download speed (Mbit/s)`),
    AvgDownload_norm = normalize(`Average download speed (Mbit/s)`),
    Weighted_Score = SFBB_norm * 0.2 + UFBB_norm * 0.25 + FTTP_norm * 0.25 + 
                     MaxDownload_norm * 0.15 + AvgDownload_norm * 0.15
  )

# Aggregate at the City level
broadband_rankings <- combined_dataset %>%
  group_by(City, County) %>%
  summarize(Aggregated_Score = mean(Weighted_Score, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(County) %>%
  arrange(desc(Aggregated_Score)) %>%
  mutate(Rank = dense_rank(desc(Aggregated_Score))) %>%
  filter(Rank <= 10) %>%
  arrange(County, Rank)


# ################################
# #                              #
# #  Ranking of Crime            #
# #                              #
# ################################

crime_dataset <- read_csv("Cleaned_datasets/Crime/Crime_Data_Combined.csv")

crime_totals <- crime_dataset %>%
  group_by(County, City, Crime) %>%
  summarize(total_offences = n(), .groups = 'drop') %>%
  # Pivot the data to have total offences of each crime type as columns
  pivot_wider(names_from = Crime, values_from = total_offences, values_fill = 0) %>%
  # Calculate the total offences across all crime types for each city
  rowwise() %>%
  mutate(total_crime_offences = sum(c_across(where(is.numeric)))) %>%
  ungroup() %>%
  # Rank cities within each county based on total crime offences
  group_by(County) %>%
  arrange(total_crime_offences) %>%
  mutate(rank = row_number()) %>%
  # Select top 10 cities with the fewest total crime offences for each county
  filter(rank <= 10) %>%
  ungroup()

# Print the result
print(crime_totals)

crime_rank <- crime_totals %>%
  select(County, City, rank)


# ################################
# #                              #
# #  Final Ranking               #
# #                              #
# ################################


# Step 1: Combine all rankings into a single dataframe with full outer join to include all cities
combined_rankings <- full_join(
  broadband_rankings %>% select(County, City, broadband_rank = Rank),
  crime_rank %>% select(County, City, crime_rank = rank),
  by = c("County", "City")
) %>%
  full_join(house_rankings %>% select(County, City = Town.City, house_rank = rank), by = c("County", "City")) %>%
  full_join(top_schools_by_county %>% select(County, City, school_rank = rank), by = c("County", "City"))

# Step 2: Replace NA values with a high number to ensure missing cities are ranked lower
combined_rankings <- combined_rankings %>%
  mutate(
    broadband_rank = ifelse(is.na(broadband_rank), max(combined_rankings$broadband_rank, na.rm = TRUE) + 1, broadband_rank),
    crime_rank = ifelse(is.na(crime_rank), max(combined_rankings$crime_rank, na.rm = TRUE) + 1, crime_rank),
    house_rank = ifelse(is.na(house_rank), max(combined_rankings$house_rank, na.rm = TRUE) + 1, house_rank),
    school_rank = ifelse(is.na(school_rank), max(combined_rankings$school_rank, na.rm = TRUE) + 1, school_rank)
  )

# Step 3: Calculate the average rank for each city
overall_rankings <- combined_rankings %>%
  group_by(County, City) %>%
  summarize(
    avg_broadband_rank = mean(broadband_rank, na.rm = TRUE),
    avg_crime_rank = mean(crime_rank, na.rm = TRUE),
    avg_house_rank = mean(house_rank, na.rm = TRUE),
    avg_school_rank = mean(school_rank, na.rm = TRUE),
    overall_rank = mean(c(avg_broadband_rank, avg_crime_rank, avg_house_rank, avg_school_rank), na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(County, overall_rank)

# Step 4: Get top 3 entries from each county
top_3_by_county <- overall_rankings %>%
  group_by(County) %>%
  slice_head(n = 3) %>%
  arrange(County, overall_rank)

# Print the result into excel file
write.xlsx(top_3_by_county, file = paste0(getwd(), "/Report/Final_Ranking.xlsx"))