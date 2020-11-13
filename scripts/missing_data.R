# Multiple strategies to dealing with missing data
# 
# General strategy is to deal with missings, 
# then fit a linear model to determine the effects of different missing value methods


# 1. Remove NA entries
d <- d_waste_pc %>%
  filter(!is.na(waste_pc) & !is.na(gdp_pc))

lm(log(waste_pc) ~ log(gdp_pc) + pop, data = d) %>%
  summary()


da <- d_waste_adj %>%
  filter(!is.na(waste_pc_adj) & !is.na(gdp_pc) & !is.na(psw))

lm(log(waste_pc_adj) ~ log(gdp_pc) + pop, data = da) %>%
  summary()



# ---
# 2. Remove NA and outlier countries

# Plot log-log to highlight outliers

da %>% 
  group_by(country) %>% 
  summarise(max=max(waste_pc_adj)) %>% 
  slice_max(max, n=5)
# Botswana is the maximum outlier

da %>% 
  group_by(country) %>% 
  summarise(min=min(waste_pc_adj)) %>% 
  slice_min(min, n=5)
# Zimbabwe and Samoe appear to be the lower outliers





# ---
# 3. Use UN imputation method







# ---
# 4. Multiple imputation (country clustered)
# - Might need removal of low entry countries with NAs?
