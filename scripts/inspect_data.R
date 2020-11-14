# Inspect tidied data
# 

library(ggplot2)

# Summarise the now tidied dataset
summary(data)





# Find max outliers
data %>%
  slice_max(waste_pc, n=20)

# Find max outlier countries
data %>% 
  group_by(country) %>% 
  summarise(max=max(waste_pc)) %>% 
  slice_max(max, n=5)
# country               max
# <chr>               <dbl>
# 1 Botswana            61.9 
# 2 Kuwait               4.31
# 3 Singapore            2.04
# 4 Qatar                1.72
# 5 Antigua and Barbuda  1.54

# Find min outliers
data %>%
  slice_min(waste_pc, n=20)

# Find min outlier countries
data %>%
  group_by(country) %>%
  summarise(min=min(waste_pc)) %>% 
  slice_min(min, n=20)
# country                              min
# <chr>                              <dbl>
#  1 United Republic of Tanzania      0.00244
#  2 Uganda                           0.00812
#  3 Indonesia                        0.0138 
#  4 Ukraine                          0.0151 
#  5 Nepal                            0.0177 
#  6 Bangladesh                       0.0193 
#  7 Madagascar                       0.0200 
#  8 Cambodia                         0.0252 
#  9 Samoa                            0.0262 
# 10 Maldives                         0.0267 


data %>%
  group_by(country) %>%
  summarise(min=min(waste_pc_adj)) %>% 
  slice_min(min, n=20)
#    country                             min
#    <chr>                             <dbl>
#  1 Samoa                            0.0270
#  2 Armenia                          0.0843
#  3 Madagascar                       0.128 
#  4 Philippines                      0.140 
#  5 Algeria                          0.145 
#  6 Zambia                           0.164 
#  7 Honduras                         0.181 
#  8 Bhutan                           0.215 
#  9 Cabo Verde                       0.218 
# 10 Ecuador                          0.233
