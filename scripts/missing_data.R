# Multiple strategies to dealing with missing data
# 
# General strategy is to deal with missing values, 
# then fit a linear model to determine the effects of different missing value methods

library(tidyverse)

## @knitr combine_data 
data <- waste %>%
  left_join(gdp_pc) %>%
  left_join(pop) 

## @knitr remove_nas
 
# 1. Remove NA entries

# Remove NA values from gdp_pc and pop columns then calculate waste per capita
data.rmnas <- data %>%
  filter(!is.na(gdp_pc) & !is.na(pop)) %>% 
  mutate(waste_pc=waste*1000/pop) 

# Fit a linear model of waste per capita vs GDP per capita and total population
mod.rmnas = lm(log(waste_pc) ~ log(gdp_pc) * log(pop), data = data.rmnas)

# Print a summary of the model parameters
summary(mod.rmnas)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -4.4657 -0.2641 -0.0299  0.2355  5.1317 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           5.934065   0.802349   7.396 2.01e-13 ***
#   log(gdp_pc)          -0.543428   0.080042  -6.789 1.46e-11 ***
#   log(pop)             -0.686317   0.050511 -13.587  < 2e-16 ***
#   log(gdp_pc):log(pop)  0.060929   0.005061  12.038  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.6176 on 2135 degrees of freedom
# Multiple R-squared:  0.5234,	Adjusted R-squared:  0.5227 
# F-statistic: 781.5 on 3 and 2135 DF,  p-value: < 2.2e-16


## @knitr multiple_imputation

# 2. Multiple imputation
library(Amelia)

set.seed(467)
data.mi <- amelia(x = as.data.frame(data),   # Convert to dataframe to overcome tibble error
                  m = 10,                    # Repeat 10 times
                  idvars = c("code", "name"),# Variables not imputed
                  logs = c("gdp_pc", "waste", "pop"), # Log transform all vars
                  p2s = 1)                   # Text based output

# Calculate waste per capita for all imputations
data.mi.mutate <- lapply(data.mi$imputations, 
                         function(i) mutate(i, waste_pc=waste*1000/pop))

# Fit linear model to all the imputations
mod.mi <- lapply(data.mi.mutate, 
                 function(i) lm(log(waste_pc) ~ log(gdp_pc) * log(pop), data = i))

# Pull out coefficients and standard errors to get an MI average
mod.mi.coefs <- do.call(rbind, lapply(lm.amelia.out, function(i) coef(summary(i))[,1]))
mod.mi.ses <- do.call(rbind, lapply(lm.amelia.out, function(i) coef(summary(i))[,2]))

mi.meld(coefs.amelia, ses.amelia)

## @knitr missing_map
# Plot missing value map
missmap(imp_amelia)
