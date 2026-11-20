# remember to open the .Rproj file first, so that here() finds the data

packages <- c("tidyverse", "here", "rnaturalearth", "rnaturalearthdata", "sf", "ggeffects", "patchwork", "sdmTMB")

# install a package if missing, then load it
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

lapply(packages, install_if_missing)

# 01 Load data ----

data <- read_csv(here("data", "model_input", "data.csv"))
data$log_density <- log(data$kg_km2 + 1)

grid <- read_csv(here("data", "model_input", "grid.csv"))

# coastline for plotting
map_data <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf", continent = "europe")

north_sea <- suppressWarnings(suppressMessages(st_crop(
  map_data, c(xmin = -7, ymin = 49, xmax = 13, ymax = 63)
)))

# 02 Choose your species ----

species <- unique(data$species)
species

my_species <- "" # choose your species

my_data <- data |> filter(species == my_species)

# map the raw presence-absence data
ggplot(my_data, aes(longitude, latitude, color = as.factor(present))) +
  geom_point(size = 0.5) +
  geom_sf(data = north_sea, inherit.aes = FALSE) +
  xlim(-6, 12) +
  ylim(50, 62) +
  labs(x = "Longitude", y = "Latitude") +
  scale_color_manual(
    name = my_species,
    values = c("0" = "red", "1" = "blue"),
    labels = c("0" = "Absent", "1" = "Present")
  ) +
  theme_light()

# map the biomass (log density) data
ggplot(my_data, aes(longitude, latitude, color = log_density)) +
  geom_point(size = 0.5) +
  geom_sf(data = north_sea, inherit.aes = FALSE) +
  xlim(-6, 12) +
  ylim(50, 62) +
  labs(x = "Longitude", y = "Latitude") +
  scale_color_viridis_c(paste0(my_species, " log(density)")) +
  theme_light()

# 03 Part 1: simple models ----

# presence-absence or biomass? your choice!

# my_sdm_biomass <- lm(log_density ~ 1 + bottom_temp, data = my_data)
# my_sdm_presence <- glm(present ~ 1 + bottom_temp, data = my_data, family = binomial(link = "logit"))

# ideas to try:
# - a thermal niche: present ~ poly(bottom_temp, 2)
# - add depth: present ~ bottom_temp + log(depth)
#   (a few grid cells have depth 0: predict on grid |> filter(depth > 0))

# useful functions:
# summary() for coefficient estimates (what is the interpretation of the slope?)
# partial_effect <- predict_response(my_sdm, terms = "bottom_temp")
# plot(partial_effect, ci_style = "dash")

# predict on current and future conditions (type = "response" for presence-absence):
# grid$now <- predict(my_sdm, grid |> mutate(bottom_temp = bottom_temp_now), type = "response")
# grid$future <- predict(my_sdm, grid |> mutate(bottom_temp = bottom_temp_2100_ssp585), type = "response")
# remember you also have bottom_temp_2100_ssp119

# 04 Part 2: spatial models with sdmTMB ----

# coordinates in km (UTM zone 31N) for the random field
# my_data <- my_data |> add_utm_columns(c("longitude", "latitude"), utm_crs = 32631)
# grid <- grid |> add_utm_columns(c("longitude", "latitude"), utm_crs = 32631)

# mesh: cutoff = 50 km keeps fitting fast (a few seconds); smaller cutoff = finer but slower
# mesh <- make_mesh(my_data, c("X", "Y"), cutoff = 50)
# plot(mesh)

# spatial model (presence-absence); for biomass try family = tweedie() with kg_km2 as response
# m_spatial <- sdmTMB(present ~ 1 + bottom_temp, data = my_data, mesh = mesh,
#   family = binomial(), spatial = "on")
# m_spatial
# sanity(m_spatial)

# spatiotemporal model (takes a bit longer)
# m_st <- sdmTMB(present ~ 1 + bottom_temp, data = my_data, mesh = mesh,
#   family = binomial(), spatial = "on", time = "year", spatiotemporal = "iid")
# m_st
# sanity(m_st)

# compare models
# AIC(m_spatial, m_st)

# predict and map the spatial field (predictions are on the link scale: use plogis() for probability)
# pred <- predict(m_spatial, newdata = grid |> mutate(bottom_temp = bottom_temp_now))
# ggplot(pred, aes(longitude, latitude, fill = omega_s)) +
#   geom_raster() +
#   geom_sf(data = north_sea, inherit.aes = FALSE) +
#   xlim(-6, 12) +
#   ylim(50, 62) +
#   scale_fill_gradient2() +
#   theme_light()

# 05 Questions to answer for your species ----

# 1. Does your species prefer warmer or colder water? Is the effect linear or hump-shaped?
# 2. How does the temperature effect (estimate and standard error) change when you add a spatial field?
# 3. Add log(depth) to the spatial model. Does the spatial SD shrink? Why?
# 4. Is your species' distribution mostly persistent (large spatial SD) or variable between years (large spatiotemporal SD)?
# 5. Where does your species gain or lose habitat by 2100 under SSP1-1.9 and SSP5-8.5?
#    How much of that is extrapolation beyond the temperatures in your data?
# 6. One sentence for a manager: what would you tell them, and with which caveat?
