# Inspect tidied data
# 




# Summarise the now tidied dataset
summary(data)

## @knitr tabcount
# Create a dataframe with a count of each 


## @knitr plotsdataprep
# Prepare data for plotting

library(ggplot2)
library(patchwork)
library(rnaturalearth)
library(latex2exp)

# Get geometries for plotting countries
world <- ne_countries(scale = "small", returnclass = "sf") %>%
  filter(name != "Antarctica") %>%
  select(c("name", "continent", "region_un", "un_a3")) %>%
  rename("code"="un_a3") %>%
  mutate(code = as.numeric(code))

# Calculate centroid of each country
world <- cbind(world, st_coordinates(st_centroid(world$geometry, of_largest_polygon = TRUE)))

# Get only the most recent year of data for each country in each dataset
waste_latest = waste %>%
  group_by(name) %>%
  arrange(desc(year)) %>%
  filter(row_number()==1)

gdp_latest = gdp_pc %>%
  group_by(name) %>%
  arrange(desc(year)) %>%
  filter(row_number()==1)

pop_latest = pop %>%
  group_by(name) %>%
  arrange(desc(year)) %>%
  filter(row_number()==1)

# Merge geometries with the latest data
world_waste <- world %>%
  select(-name) %>%
  right_join(waste_latest, by=c("code" = "code"))

world_gdp <- world %>%
  select(-name) %>%
  right_join(gdp_latest, by=c("code" = "code"))

world_pop <- world %>%
  select(-name) %>%
  right_join(pop_latest, by=c("code" = "code"))



# Create a patchwork plot for GDP dataset, containing
# - Histogram
# - Log-histogram
# - Map

## @knitr wasteplot
# darkslategray
p1 = qplot(waste$waste, 
           geom="histogram",
           fill=I("darkslategray3")) +
  labs(x="Municipal waste / 1000s tonnes", y="Count", title="Histograms")

p2 = qplot(log10(waste$waste), 
           geom="histogram",
           fill=I("darkslategray3")) +
  labs(x=TeX("log$_{10}$(Municipal waste / 1000s tonnes)"), y="Count", title="", parse=TRUE)

p3 = ggplot(data = world) +
  geom_sf() +
  geom_sf(data=world_waste, aes(fill=waste), show.legend = TRUE) +
  scale_fill_gradient(low="darkslategray4", high="darkslategray1", trans="log10",
                      name="Municipal waste / 1000s tonnes") +
  labs(x="Longitude", y="Latitude", title="Geospatial: Most recent year of data")

# Use patchwork to arrange plots
(p1+p2)/p3 + plot_layout(heights=c(1,1))


## @knitr gdpplot
# darkorange
p1 = qplot(gdp_pc$gdp_pc, 
           geom="histogram",
           fill=I("darkorange2")) +
  labs(x="GDP per capita / $USD", y="Count", title="Histograms")

p2 = qplot(log10(gdp_pc$gdp_pc), 
           geom="histogram",
           fill=I("darkorange2")) +
  labs(x=TeX("log$_{10}$(GDP per capita / $USD)"), y="Count", title="", parse=TRUE)

p3 = ggplot(data = world) +
  geom_sf() +
  geom_sf(data=world_gdp, aes(fill=gdp_pc), show.legend = TRUE) +
  scale_fill_gradient(low="darkorange4", high="darkorange1", trans="log10",
                      name="GDP per capita / $USD") +
  labs(x="Longitude", y="Latitude", title="Geospatial: Most recent year of data")

# Use patchwork to arrange plots
(p1+p2)/p3 + plot_layout(heights=c(1,1))


## @knitr popplot
# firebrick
p1 = qplot(pop$pop, 
           geom="histogram",
           fill=I("firebrick2")) +
  labs(x="Population", y="Count", title="Histograms")

p2 = qplot(log10(gdp_pc$gdp_pc), 
           geom="histogram",
           fill=I("firebrick2")) +
  labs(x=TeX("log$_{10}$(Population)"), y="Count", title="", parse=TRUE)

p3 = ggplot(data = world) +
  geom_sf() +
  geom_sf(data=world_pop, aes(fill=pop), show.legend = TRUE) +
  scale_fill_gradient(low="firebrick4", high="firebrick1", trans="log10",
                      name="Population") +
  labs(x="Longitude", y="Latitude", title="Geospatial: Most recent year of data")

# Use patchwork to arrange plots
(p1+p2)/p3 + plot_layout(heights=c(1,1))
