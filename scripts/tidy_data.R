# Data cleaning
# 
library(tidyverse)
library(here)

gdp <- read_csv(here::here("data/gdp_UNdata_Export_20201112_074503531.csv"))
waste <- read_csv(here::here("data/wastepercapita_UNdata_Export_20201112_074648716.csv"))

pop <- read_csv(here::here("data/pop_UNdata_Export_20201112_075133467.csv"))
psw <- read_csv(here::here("data/popservedwaste_UNdata_Export_20201112_074710796.csv"))

# Filter pop to contain total national population of both sexes
pop_tot <- filter(pop, Sex=="Both Sexes" & Area=="Total") %>%
  + select(c("Country or Area", "Year", "Value"))

# Convert psw year from chr to num, and adjust Value from percentage to proportion
psw <- mutate(psw, Year=as.numeric(Year)) %>%
  mutate(Value=Value/100)

# Wrangle into one df
data <- select(waste, c("Country or Area", "Year", "Value")) %>%
  rename("waste"="Value") %>%
  filter(!is.na(Year)) %>%
  left_join(select(gdp, -Item)) %>%
  rename("gdp_pc"="Value") %>%
  left_join(pop_tot) %>%
  rename("pop"="Value")  %>%
  left_join(select(psw, -`Value Footnotes`, -Unit)) %>%
  rename("psw"="Value") %>%
  rename("country"="Country or Area") %>%
  rename("year" = "Year")

# Waste in 1000s tonnes -> tonnes per capita
data <- mutate(data, waste_pc=waste*1000/pop)

# Adjust waste_pc to only reflect population actually served by waste collection
data <- mutate(data, waste_pc_adj=waste*1000/(pop*psw))

# Split data into two; 
# - one with waste_pc as the response variable
# - the other with waste_pc_adj as the response variable

d_waste_pc <- data %>%
  select(-waste, -waste_pc_adj, -psw)

d_waste_adj <- data %>%
  select(-waste, -waste_pc)
