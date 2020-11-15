# Data cleaning
#
## @knitr load_data 
library(tidyverse)
library(here)

# Load and clean the data similarly for each dataset:
# 1. Load data from CSV files
# 2. Select only the columns country code, country name, year and value
# 3. Rename columns with more easy to use and descriptive names
# 4. Remove footnotes at bottom of dataframe, which were present in the CSV
#    (denoted by NAs in year column)

waste <- read_csv(here::here("data/waste_UNdata_world_1990-2016.csv")) %>%  # 1.
  select(c("Country or Area Code", "Country or Area", "Year", "Value")) %>% # 2.
  rename("code"="Country or Area Code") %>%                                 # 3.
  rename("name"="Country or Area") %>%
  rename("year" = "Year") %>%
  rename("waste"="Value") %>%
  filter(!is.na(year))                                                      # 4.

gdp_pc <- read_csv(here::here("data/gdp_UNdata_world_1989-2018.csv")) %>%
  select(c("Country or Area Code", "Country or Area", "Year", "Value")) %>%
  rename("code"="Country or Area Code") %>%
  rename("name"="Country or Area") %>%
  rename("year" = "Year") %>%
  rename("gdp_pc"="Value") %>%
  filter(!is.na(year))

pop <- read_csv(here::here("data/pop_UNdata_world_1990-2020.csv")) %>%
  # Use only population values for both sexes and the country's total area
  filter(Sex=="Both Sexes" & Area=="Total") %>%
  select(c("Country or Area Code", "Country or Area", "Year", "Value")) %>%
  rename("code"="Country or Area Code") %>%
  rename("name"="Country or Area") %>%
  rename("year" = "Year") %>%
  rename("pop"="Value") %>%
  filter(!is.na(year))

