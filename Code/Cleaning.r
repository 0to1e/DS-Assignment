library(tidyverse)

# ################################
# #                              #
# #  Cleaning of Paid House Data #
# #                              #
# ################################

# Define the column names for the house price data
column_names <- c("Transaction_ID", "Price", "Date_of_Transfer", "Postal Code", "Property_Type", "Old/New", "Duration", "PAON", "SAON", "Street", "Locality", "Town/City", "District", "County", "PPD_Category_Type", "Record_Status")


# Read CSV files for postcodes dataset containing geo-location information about UK
postcode_data <- read_csv("Cleaned_datasets/Postcode_clean.csv") %>%
  select(pcd7, Street)

# Read CSV files for house price data from 2020 to 2023, with no headers and assigned column names
data0 <- read.csv("Obtained_Data/House_Pricing/pp-2020.csv", header = FALSE, col.names = column_names)
data1 <- read.csv("Obtained_Data/House_Pricing/pp-2021.csv", header = FALSE, col.names = column_names)
data2 <- read.csv("Obtained_Data/House_Pricing/pp-2022.csv", header = FALSE, col.names = column_names)
data3 <- read.csv("Obtained_Data/House_Pricing/pp-2023.csv", header = FALSE, col.names = column_names)

# Combine the datasets from each year into one data frame
combined_data <- bind_rows(data0, data1, data2, data3)

# Clean the combined house price data
cleaned_house_data <- combined_data %>%
  # Select only the relevant columns
  select(Price, Date_of_Transfer, Postal.Code, Town.City, County) %>%
  mutate(County = ifelse(County == "CITY OF BRISTOL", "Bristol", County)) %>%
  mutate(County = ifelse(County == "CORNWALL", "Cornwall", County)) %>%
  rename("PostalCode" = "Postal.Code", "Date" = "Date_of_Transfer") %>%
  mutate(PostalCode = gsub(" ", "", PostalCode)) %>%
  # Filter for Bristol and Cornwall
  filter((County %in% c("Bristol", "Cornwall"))) %>%
  # Convert Date_of_Transfer to Date type
  mutate(Date = as.Date(Date))

#join post_code datase with combined house dataset to add street location data
final_house_data <- merge(
  cleaned_house_data,
  postcode_data,
  by.x = "PostalCode",
  by.y = "pcd7",
  all.x = TRUE,  # Keeps all rows from cleaned_house_data
  all.y = FALSE  # Excludes rows from postcode_data that do not match
) %>%
  na.omit()


# Save the cleaned house price data to a CSV file
write_csv(final_house_data, file = paste0(getwd(), "/Cleaned_datasets/House_Pricing_Data/clean_house_pricing_data.csv"))





# ################################
# #                              #
# #  Cleaning of Broadband Data  #
# #                              #
# ################################

# Load and clean the broadband coverage data
broadband_coverage <- read_csv("Obtained_Data/broadband/201809_fixed_pc_coverage_r01.csv") %>%
  # Select only relevant columns related to broadband coverage
  select(postcode, `SFBB availability (% premises)`, `UFBB availability (% premises)`, `FTTP availability (% premises)`) %>%
  # Remove spaces from postcodes to ensure consistency
  mutate(postcode = gsub(" ", "", postcode)) %>%
  # Rename the postcode column to match other datasets
  rename("PostalCode" = postcode)

# Load and clean the broadband performance data
broadband_perfomance <- read_csv("Obtained_Data/broadband/201805_fixed_pc_performance_r03.csv") %>%
  # Select only relevant columns related to broadband performance
  select(postcode, `Maximum download speed (Mbit/s)`, `Average download speed (Mbit/s)`) %>%
  # Rename the postcode column to match other datasets
  rename("PostalCode" = postcode)

# Load postcode data to merge with street and County locations based on the postcode data available
postcode_clean <- read_csv("Cleaned_datasets/Postcode_clean.csv")

# Step 3: Join the broadband coverage data with postcode data to add city and county information
clean_coverage <- broadband_coverage %>%
  inner_join(postcode_clean, by = c("PostalCode" = "pcd7")) %>%
  # Handle any unmatched postcodes by assigning "NA" to City and County
  mutate(City = ifelse(is.na(City), "NA", City), County = ifelse(is.na(County), "NA", County))

# Join the broadband performance data with postcode data to add city and county information
clean_performance <- broadband_perfomance %>%
  inner_join(postcode_clean, by = c("PostalCode" = "pcd7")) %>%
  mutate(City = ifelse(is.na(City), "NA", City), County = ifelse(is.na(County), "NA", County)) %>%
  # Handle missing values in the Maximum download speed column by imputing the median
  mutate(`Maximum download speed (Mbit/s)` = ifelse(is.na(`Maximum download speed (Mbit/s)`), median(`Maximum download speed (Mbit/s)`, na.rm = TRUE), `Maximum download speed (Mbit/s)`)) %>%
  # Handle missing values in the Average download speed column by imputing the mean
  mutate(`Average download speed (Mbit/s)` = ifelse(is.na(`Average download speed (Mbit/s)`), mean(`Average download speed (Mbit/s)`, na.rm = TRUE), `Average download speed (Mbit/s)`))

# Save the cleaned broadband coverage data to a CSV file
write_csv(clean_coverage, file = paste0(getwd(), "/Cleaned_datasets/Broadband/clean_broadband_coverage.csv"))

# Save the cleaned broadband performance data to a CSV file
write_csv(clean_performance, file = paste0(getwd(), "/Cleaned_datasets/Broadband/clean_broadband_performance.csv"))







# ################################
# #                              #
# #  Cleaning of Crime Data      #
# #                              #
# ################################

# Define the directory containing the crime data files
crime_data_dir <- "Obtained_Data/Crime"
# List all CSV files in the directory recursively
all_files <- list.files(crime_data_dir, recursive = TRUE, full.names = TRUE, pattern = "\\.csv$")

# Function to read and combine multiple CSV files into one data frame
combine_csv_files <- function(files) {
  map_df(files, read_csv, .id = "file_id")
}

# Combine all crime data files into one dataset
combined_data <- combine_csv_files(all_files) %>%
  # Select relevant columns for crime analysis
  select(`Crime ID`, `Month`, `Falls within`, `LSOA code`, `LSOA name`, `Crime type`)

# Clean the crime data
clean_data <- combined_data %>%
  # Replace Crime ID with a unique identifier
  mutate(`Crime ID` = row_number(), Month = ymd(paste0(Month, "-01"))) %>%
  # Filter data to include only crimes in Bristol or Cornwall
  filter(str_detect(`LSOA name`, "Bristol|Cornwall")) %>%
  # Rename the Month column to date and assign County based on LSOA name
  rename(date = Month) %>%
  mutate(
    `Falls within` = ifelse(
      startsWith(`LSOA name`, "Bristol"),
      "Bristol",
      "Cornwall"
    )
  ) %>%
  rename("County" = "Falls within", lsoa_code = `LSOA code`, City = `LSOA name`, Crime = `Crime type`)

# Save the cleaned crime data to a CSV file
write_csv(clean_data, file = paste0(getwd(), "/Cleaned_datasets/Crime/Crime_Data_Combined.csv"))







# ################################
# #                              #
# #  Cleaning of School Data     #
# #                              #
# ################################

# Load postcode dataset to enable joining tables and fetching street location information
postcode_dataset <- read_csv("Cleaned_datasets/Postcode_clean.csv")

# Load the 2021-2022 Bristol Key Stage 4 final data
bristol_key_stage_4_final_2021_2022 <- read_csv("Obtained_Data/schools/Bristol/2021-2022/801_ks4final.csv") %>%
  # Select relevant columns
  select(SCHNAME, PCODE, TOWN, ATT8SCR) %>%
  # Add a Year column indicating the year of the data
  mutate(Year = 2022)

# Load the 2022-2023 Bristol Key Stage 4 final data
bristol_key_stage_4_final_2022_2023 <- read_csv("Obtained_Data/schools/Bristol/2022-2023/801_ks4final.csv") %>%
  # Select relevant columns
  select(SCHNAME, PCODE, TOWN, ATT8SCR) %>%
  # Add a Year column indicating the year of the data
  mutate(Year = 2023)

# Load the 2021-2022 Cornwall Key Stage 4 final data
cornwall_key_stage_4_final_2021_2022 <- read_csv("Obtained_Data/schools/Cornwall/2021-2022/908_ks4final.csv") %>%
  # Select relevant columns
  select(SCHNAME, PCODE, TOWN, ATT8SCR) %>%
  # Add a Year column indicating the year of the data
  mutate(Year = 2022)

# Load the 2022-2023 Cornwall Key Stage 4 final data
cornwall_key_stage_4_final_2022_2023 <- read_csv("Obtained_Data/schools/Cornwall/2022-2023/908_ks4final.csv") %>%
  # Select relevant columns
  select(SCHNAME, PCODE, TOWN, ATT8SCR) %>%
  # Add a Year column indicating the year of the data
  mutate(Year = 2023)

# Combine all the school data into one dataset
combined_ks4_data <- bind_rows(
  bristol_key_stage_4_final_2021_2022,
  bristol_key_stage_4_final_2022_2023,
  cornwall_key_stage_4_final_2021_2022,
  cornwall_key_stage_4_final_2022_2023
)

# Filter out rows with missing school names (NA values)
clean_school_data <- combined_ks4_data %>%
  filter(!is.na(SCHNAME)) %>% # Filter for rows where SCHNAME is not NA
  # Convert ATT8SCR column to numeric (assuming it's a character vector)
  mutate(ATT8SCR = as.numeric(ATT8SCR)) %>% # Convert ATT8SCR to numeric
  # Impute missing values in ATT8SCR with the mean (excluding NAs)
  mutate(ATT8SCR = ifelse(is.na(ATT8SCR), mean(ATT8SCR, na.rm = TRUE), ATT8SCR)) %>% # Replace NA with mean
  # Remove spaces from PCODE column
  mutate(PCODE = gsub(" ", "", PCODE)) %>% # Remove spaces in PCODE using gsub
  # Inner join with postcode_clean data based on PCODE and pcd7
  inner_join(postcode_dataset, by = c("PCODE" = "pcd7")) %>% # Join with postcode data
  # Rename PCODE column to PostalCode
  rename("PostalCode" = "PCODE") %>% # Rename PCODE to PostalCode
  # Impute missing City and County with "NA"
  mutate(City = ifelse(is.na(City), "NA", City), County = ifelse(is.na(County), "NA", County)) %>% # Replace NA with "NA"
  # Select all columns except TOWN
  select(-TOWN)

# Save the cleaned school data to a CSV file
write_csv(clean_school_data, file = paste0(getwd(), "/Cleaned_datasets/Schools/school_cleaned_data.csv"))

"
@##############################
#                             #
#  Cleaning of Post Code      #
#                             #
###############################
"


# Read the CSV file containing postcode to LSOA mapping and select relevant columns
postcode_clean <- read_csv("Obtained_Data/Postcode to LSOA.csv") %>%
  # Select only the necessary columns from the dataset
  select(pcd7, oa11cd, lsoa11nm, msoa11nm, ladnm) %>%
  # Rename the columns for clarity
  rename(County = ladnm, City = msoa11nm, Street = lsoa11nm) %>%
  # Remove spaces from the 'pcd7' column values for consistency
  mutate(pcd7 = gsub(" ", "", pcd7)) %>%
  # Standardize the 'County' column to ensure uniform naming
  mutate(County = ifelse(County == "Bristol, City of", "Bristol", County)) %>%
  # Filter the data to include only rows where the 'County' is either 'Bristol' or 'Cornwall'
  filter((County %in% c("Bristol", "Cornwall")))

# Write the cleaned data to a new CSV file in the specified directory
write_csv(postcode_clean, file = paste0(getwd(), "/Cleaned_datasets/Postcode_clean.csv"))




"
@##############################
#                             #
#  Cleaning of Population     #
#                             #
###############################
"
# Read the population data from the CSV file
population_dataset <- read_csv("Obtained_Data/Population2011_1656567141570.csv") %>%
  # Extract the first two characters from the 'Postcode' column to match the format in the postcode dataset
  mutate(Postcode = substr(Postcode, start = 1, stop = 2))

# Read the cleaned postcode data and select relevant columns
postcode_dataset <- read_csv("Cleaned_datasets/Postcode_clean.csv") %>%
  # Select only the columns 'pcd7' (postcode) and 'County'
  select(pcd7, County) %>%
  # Extract the first two characters from the 'pcd7' column to match the format in the population dataset
  mutate(pcd7 = substr(pcd7, start = 1, stop = 2)) %>%
  # Remove duplicate rows based on 'pcd7' and 'County'
  distinct()

# Merge the population data with the postcode data and calculate the total population by county
population_2011 <- population_dataset %>%
  # Perform a left join to combine the population data with postcode data based on matching 'Postcode' and 'pcd7'
  left_join(postcode_dataset, by = c("Postcode" = "pcd7")) %>%
  # Group the data by 'County'
  group_by(County) %>%
  # Summarize the data by calculating the total population for each county
  summarize(total_population = sum(Population, na.rm = TRUE))

# Write the cleaned and summarized population data to a new CSV file
write_csv(population_2011, file = paste0(getwd(), "/Cleaned_datasets/Population_clean.csv"))
