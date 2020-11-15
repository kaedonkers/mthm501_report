# Inspect tidied data
# 

library(ggplot2)
library(patchwork)
library(rnaturalearth)
library(latex2exp)


# Summarise the now tidied dataset
summary(data)

## @knitr tabcount
# Create a dataframe with a count of each 


## @knitr distgeoplot
# Create a patchwork plot containing
# - Histogram
# - Log-histogram
# - Map
# Of each dataset

# Geometries for plotting countries
world <- ne_countries(scale = "small", returnclass = "sf") %>%
  filter(name != "Antarctica") %>%
  select(c("name", "continent", "region_un", "un_a3")) %>%
  rename("code"="un_a3") %>%
  mutate(code = as.numeric(code))

# Centroid of each country
world <- cbind(world, st_coordinates(st_centroid(world$geometry, of_largest_polygon = TRUE)))

waste_latest = waste %>%
  group_by(name) %>%
  arrange(desc(year)) %>%
  filter(row_number()==1)

world_waste <- world %>%
  select(-name) %>%
  right_join(waste_latest, by=c("code" = "code"))

p1 = qplot(waste$waste, 
           geom="histogram",
           fill=I("firebrick2")) +
  labs(x="Municipal waste / 1000s tonnes", y="Count", title="Histograms",
       subtitle("kjsojnaf"))

p2 = qplot(log10(waste$waste), 
           geom="histogram",
           fill=I("firebrick2")) +
  labs(x=TeX("log[10](Municipal waste / 1000s tonnes)"), y="Count", title="", parse=TRUE)



p3 = ggplot(data = world) +
  geom_sf() +
  geom_sf(data=world_waste, aes(fill=waste), show.legend = TRUE) +
  scale_fill_gradient(low="firebrick4", high="firebrick1", trans="log10") +
  labs(x="Longitude", y="Latitude", title="Geospatial: Most recent year of data")

(p1+p2)/p3 + plot_layout(heights=c(1,1))

waste_latest = waste %>%
  group_by(name) %>%
  arrange(desc(year)) %>%
  filter(row_number()==1)

# dodgerblue1
gdp_latest = gdp %>%
  group_by(name) %>%
  arrange(desc(year)) %>%
  filter(row_number()==1)

# darkorange1
pop_latest = pop %>%
  group_by(name) %>%
  arrange(desc(year)) %>%
  filter(row_number()==1)
