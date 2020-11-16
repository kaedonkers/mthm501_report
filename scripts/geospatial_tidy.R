# Matchup data and geometry data

library(ggplot2)
library(dplyr)
library(rnaturalearth)

world <- ne_countries(scale = "large", returnclass = "sf")
length(intersect(data$country, world$admin))
dadmin <- setdiff(data$country, world$admin)

length(intersect(data$country, world$name))
dname <- setdiff(data$country, world$name)

length(intersect(data$country, world$name_id))
dnamid = setdiff(data$country, world$name_id)
length(intersect(data$country, world$name_sort))
dnamesort = setdiff(data$country, world$name_sort)


# Get geometries of countries
world_s <- ne_countries(scale = "small", returnclass = "sf") %>%
  filter(name != "Antarctica") %>%
  select(c("name", "continent", "region_un")) %>%
  rename("country"="name")

length(intersect(data$country, world_s$country))
setdiff(data$country, world_s$country)
# [1] "Andorra"                                             
# [2] "Anguilla"                                            
# [3] "Antigua and Barbuda"                                 
# [4] "Bermuda"                                             
# [5] "Bolivia (Plurinational State of)"                    
# [6] "Bosnia and Herzegovina"                              
# [7] "British Virgin Islands"                              
# [8] "Cabo Verde"                                          
# [9] "China, Hong Kong Special Administrative Region"      
# [10] "China, Macao Special Administrative Region"          
# [11] "Czechia"                                             
# [12] "Dominica"                                            
# [13] "French Guiana"                                       
# [14] "French Polynesia"                                    
# [15] "Guadeloupe"                                          
# [16] "Liechtenstein"                                       
# [17] "Maldives"                                            
# [18] "Malta"                                               
# [19] "Marshall Islands"                                    
# [20] "Martinique"                                          
# [21] "Mauritius"                                           
# [22] "Monaco"                                              
# [23] "Republic of Korea"                                   
# [24] "Republic of Moldova"                                 
# [25] "Réunion"                                             
# [26] "Saint Lucia"                                         
# [27] "Saint Vincent and the Grenadines"                    
# [28] "Samoa"                                               
# [29] "Singapore"                                           
# [30] "State of Palestine"                                  
# [31] "Sudan (Former)"                                      
# [32] "The former Yugoslav Republic of Macedonia"           
# [33] "United Kingdom of Great Britain and Northern Ireland"
# [34] "United Republic of Tanzania"                         
# [35] "United States of America" 

world_m <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(name != "Antarctica") %>%
  select(c("name", "continent", "region_un")) %>%
  rename("country"="name")

length(intersect(data$country, world_m$country))
setdiff(data$country, world_m$country)
# [1] "Antigua and Barbuda"                                 
# [2] "Bolivia (Plurinational State of)"                    
# [3] "Bosnia and Herzegovina"                              
# [4] "British Virgin Islands"                              
# [5] "Cabo Verde"                                          
# [6] "China, Hong Kong Special Administrative Region"      
# [7] "China, Macao Special Administrative Region"          
# [8] "Czechia"                                             
# [9] "French Guiana"                                       
# [10] "French Polynesia"                                    
# [11] "Guadeloupe"                                          
# [12] "Marshall Islands"                                    
# [13] "Martinique"                                          
# [14] "Republic of Korea"                                   
# [15] "Republic of Moldova"                                 
# [16] "Réunion"                                             
# [17] "Saint Vincent and the Grenadines"                    
# [18] "State of Palestine"                                  
# [19] "Sudan (Former)"                                      
# [20] "The former Yugoslav Republic of Macedonia"           
# [21] "United Kingdom of Great Britain and Northern Ireland"
# [22] "United Republic of Tanzania"                         
# [23] "United States of America"   

world_l <- ne_countries(scale = "large", returnclass = "sf") %>%
  filter(name != "Antarctica") %>%
  select(c("name", "continent", "region_un")) %>%
  rename("country"="name")

length(intersect(data$country, world_l$country))
setdiff(data$country, world_l$country)
# [1] "Antigua and Barbuda"                                 
# [2] "Bolivia (Plurinational State of)"                    
# [3] "Bosnia and Herzegovina"                              
# [4] "British Virgin Islands"                              
# [5] "China, Hong Kong Special Administrative Region"      
# [6] "China, Macao Special Administrative Region"          
# [7] "French Guiana"                                       
# [8] "French Polynesia"                                    
# [9] "Guadeloupe"                                          
# [10] "Marshall Islands"                                    
# [11] "Martinique"                                          
# [12] "Republic of Korea"                                   
# [13] "Republic of Moldova"                                 
# [14] "Réunion"                                             
# [15] "Saint Vincent and the Grenadines"                    
# [16] "State of Palestine"                                  
# [17] "Sudan (Former)"                                      
# [18] "The former Yugoslav Republic of Macedonia"           
# [19] "United Kingdom of Great Britain and Northern Ireland"
# [20] "United Republic of Tanzania"


data %>%
  filter(country>="R") %>%
  unique(.[country])


# ---
# Use `countrycode` package to guess country code names

library(countrycode)

data_codes <- guess_field(unique(data$country))

world <- ne_countries(scale = "large", returnclass = "sf") %>%
  filter(name != "Antarctica")

world %>%
  st_set_geometry(NULL) %>%
  rapply(function(x)length(unique(x))) %>%
  sort()

codes = world$un_a3 %>%
  countrycode::countrycode("un", "un.name.en")

length(intersect(data$country, codes))
setdiff(data$country, codes)


uncodes = world %>%
  st_set_geometry(NULL) %>%
  select(name,un_a3) %>%
  mutate(un_a3=as.numeric(un_a3))
View(uncodes)
