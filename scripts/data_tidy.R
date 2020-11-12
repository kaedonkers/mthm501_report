# Data cleaning
# 
library(tidyverse)
library(here)

gdp <- read_csv(here::here("data/gdp_UNdata_Export_20201112_074503531.csv"))
waste <- read_csv(here::here("data/wastepercapita_UNdata_Export_20201112_074648716.csv"))

pop <- read_csv(here::here("data/pop_UNdata_Export_20201112_075133467.csv"))
psw <- read_csv(here::here("data/popservedwaste_UNdata_Export_20201112_074710796.csv"))
