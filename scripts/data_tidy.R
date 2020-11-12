# Data cleaning
# 
library(tidyverse)
library(here)

gdp <- read_csv(here::here("data/gdp_UNdata_Export_20201112_074503531.csv"))
waste <- read_csv(here::here("data/wastepercapita_UNdata_Export_20201112_074648716.csv"))

pop <- read_csv(here::here("data/pop_UNdata_Export_20201112_075133467.csv"))
psw <- read_csv(here::here("data/popservedwaste_UNdata_Export_20201112_074710796.csv"))

# Filter pop to contain total national population of both sexes
pop_tot = filter(pop, Sex=="Both Sexes" & Area=="Total") %>%
  + select(c("Country or Area", "Year", "Value"))

# Wrangle into one df
data <- select(waste, c("Country or Area", "Year", "Value")) %>%
  rename("waste"="Value") %>%
  filter(!is.na(Year)) %>%
  left_join(select(gdp, -Item)) %>%
  rename("gdp_pc"="Value") %>%
  left_join(pop_tot) %>%
  rename("pop"="Value")

# Waste in 1000s tonnes -> tonnes per capita
data <- mutate(data, waste=waste*1000/pop) %>%
  rename("waste_pc"="waste")

