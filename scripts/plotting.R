# Geospatial plotting
# 

library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(viridis)

# Geometries for plotting countries
world <- ne_countries(scale = "small", returnclass = "sf") %>%
  filter(name != "Antarctica") %>%
  select(c("name", "continent", "region_un", "un_a3"))

# Centroid of each country
world <- cbind(world, st_coordinates(st_centroid(world$geometry, of_largest_polygon = TRUE)))

# Join waste and world data
world_waste <- world %>%
  right_join(data, "country")


ggplot(data = world) +
  geom_sf(show.legend = FALSE) +
  geom_sf(data=world_waste, aes(fill=continent), show.legend = FALSE) +
  geom_point(data=world_waste, aes(x=X, y=Y, size=gdp_pc, color=waste, alpha=0.2)) +
  xlab("Longitude") + ylab("Latitude") +
  labs(title="World Map")
# geom_point(data=world_points, aes(x=X, y=Y, size=pop, color=gdp_pc))


# TODO: Latitude axis values 



