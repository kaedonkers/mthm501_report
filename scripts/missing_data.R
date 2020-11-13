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
# https://uneplive.unep.org/media/docs/graphs/aggregation_methods.pdf
# 1. Gaps <= 10 years = exponential interpolation
# 2. Ends = 3 year extrapolation of last value
# 3. Gaps > 10 years = 3 year extrapolation each end







# ---
# 4. Multiple imputation (country clustered)
# - Might need removal of low entry countries with NAs?
library(Amelia)

data <- select(waste, c("Country or Area", "Year", "Value")) %>%
  rename("waste"="Value") %>%
  filter(!is.na(Year)) %>%
  left_join(select(gdp, -Item)) %>%
  rename("gdp_pc"="Value") %>%
  left_join(pop_tot) %>%
  rename("pop"="Value") %>%
  rename("country"="Country or Area") %>%
  rename("year" = "Year")

set.seed(467)
imp_amelia <- amelia(x = as.data.frame(data), #dataframe to overcome tibble error
                     m = 5, # repeat 5 times
                     idvars="country", #not imputed
                     logs = c("gdp_pc", "waste", "pop"), #all vars need log transform
                     p2s = 1) #Some textual output

# TODO: How to stop producing negative imputations?
# Read docs -> https://cran.r-project.org/web/packages/Amelia/vignettes/amelia.pdf
# SOLUTION: Log pop also -> otherwise it thinks the population is normally distributed around low values, and bleeds over into negatives

# Plot missings
missmap(imp_amelia)

# Calculate 
mutate.amelia.out <- lapply(imp_amelia$imputations, function(i) mutate(i, waste_pc=waste*1000/pop))

# Fit linear model to all the imputations
lm.amelia.out <- lapply(mutate.amelia.out, 
                        function(i) lm(log(waste_pc) ~ log(gdp_pc) + log(pop), data = i))

# Pull out coefficients and standard errors to get an MI average
coefs.amelia <- do.call(rbind, lapply(lm.amelia.out, function(i) coef(summary(i))[,1]))
ses.amelia <- do.call(rbind, lapply(lm.amelia.out, function(i) coef(summary(i))[,2]))

mi.meld(coefs.amelia, ses.amelia)
