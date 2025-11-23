## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 4
)

library(ggplot2)
library(ggmapcn)

## ----basic_map, fig.alt="A standard world map using WGS84 projection with default styling."----
ggplot() +
  geom_world() +
  theme_void()

## ----explicit_crs, fig.alt="A world map explicitly set to EPSG:4326 projection."----
ggplot() +
  geom_world(crs = 4326) +
  coord_sf(crs = 4326) +
  theme_void()

## ----hide_ocean, fig.alt="A world map with the blue ocean layer removed, showing grey countries on a white background."----
ggplot() +
  geom_world(
    show_ocean   = FALSE,
    country_fill = "grey90"
  ) +
  theme_minimal()

## ----hide_admin, fig.alt="A world map showing continental landmasses without internal country borders."----
ggplot() +
  geom_world(
    show_admin_boundaries = FALSE,
    country_fill          = "white"
  ) +
  theme_minimal()

## ----minimal_map, fig.alt="A minimalist world map showing only land shapes with no ocean or borders."----
ggplot() +
  geom_world(
    show_ocean            = FALSE,
    show_admin_boundaries = FALSE
  ) +
  theme_minimal()

## ----robinson, fig.alt="A world map using the Robinson projection."-----------
crs_robin <- "+proj=robin +datum=WGS84"

ggplot() +
  geom_world(crs = crs_robin) +
  coord_sf(crs = crs_robin) +
  theme_void()

## ----robin_pacific, fig.alt="A Robinson projection world map centered on the Pacific Ocean (150 degrees East)."----
crs_robin_150 <- "+proj=robin +lon_0=150 +datum=WGS84"

ggplot() +
  geom_world(crs = crs_robin_150) +
  coord_sf(crs = crs_robin_150) +
  theme_void()

## ----wgs84_pacific, fig.alt="A rectangular projection world map centered on 150 degrees East."----
crs_wgs84_150 <- "+proj=longlat +datum=WGS84 +lon_0=150"

ggplot() +
  geom_world(crs = crs_wgs84_150) +
  coord_sf(crs = crs_wgs84_150) +
  theme_void()

## ----axis_labels, fig.alt="A world map with clear longitude and latitude axis labels and gridlines drawn on top of the land layer."----
ggplot() +
  geom_world() +
  coord_sf(
    crs    = 4326,
    expand = FALSE,
    datum  = sf::st_crs(4326)
  ) +
  theme_minimal() +
  theme(panel.ontop = TRUE)

## ----graticule_global, fig.alt="A world map with custom graticule lines labeled every 60 degrees longitude and 30 degrees latitude."----
ggplot() +
  geom_world() +
  annotation_graticule(
    lon_step     = 60,
    lat_step     = 30,
    label_offset = 5
  ) +
  coord_sf(
    crs    = 4326,
    expand = FALSE,
    datum  = sf::st_crs(4326)
  ) +
  theme_void() +
  theme(panel.ontop = TRUE)

## ----graticule_robin, fig.alt="A Robinson projection map with curved graticule lines."----
crs_robin <- "+proj=robin +datum=WGS84"

ggplot() +
  geom_world(crs = crs_robin) +
  annotation_graticule(
    crs          = crs_robin,
    lon_step     = 30,
    lat_step     = 15,
    label_offset = 3e5
  ) +
  coord_sf(crs = crs_robin) +
  theme_void()

## ----region_cn, fig.alt="A regional map of China and surrounding areas with clean axis labels and specific graticule limits."----
cn_xlim <- c(70, 140)
cn_ylim <- c(0, 60)

ggplot() +
  geom_world() +
  annotation_graticule(
    xlim         = cn_xlim,
    ylim         = cn_ylim,
    crs          = 4326,
    lon_step     = 10,
    lat_step     = 10,
    label_color  = NA,
    label_offset = 1,
    label_size   = 3.5
  ) +
  coord_sf(
    xlim   = cn_xlim,
    ylim   = cn_ylim,
    expand = FALSE
  ) +
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw()

## ----highlight_cn, fig.alt="A world map with China highlighted in red."-------
ggplot() +
  geom_world(
    country_fill = "white",
    show_frame   = TRUE
  ) +
  geom_world(
    filter_attribute = "SOC",
    filter           = "CHN",
    country_fill     = "red"
  ) +
  theme_void()

## ----highlight_multi, fig.alt="A world map highlighting China, Japan, and South Korea in orange."----
focus <- c("CHN", "JPN", "KOR")

ggplot() +
  geom_world(
    country_fill = "grey95",
    show_frame   = TRUE
  ) +
  geom_world(
    filter_attribute = "SOC",
    filter           = focus,
    country_fill     = "#f57f17"
  ) +
  theme_void()

## ----custom_data_gdp, fig.alt="A world map with countries colored by GDP using a continuous color scale."----
# 1. Ensure data availability and GET FILE PATHS
map_files <- check_geodata(c("world_countries.rda", "world_coastlines.rda"))

# 2. Load the world countries data (object name: 'countries')
load(map_files[1])

# 3. Create custom data: Real 2023 Population Estimates (Top 25+ major nations)
# Unit: Millions
custom_data <- data.frame(
  iso_code = c("CHN", "IND", "USA", "IDN", "PAK", "NGA", "BRA", "BGD", 
               "RUS", "MEX", "JPN", "ETH", "PHL", "EGY", "VNM", "COD", 
               "TUR", "IRN", "DEU", "THA", "GBR", "FRA", "ITA", "ZAF", 
               "KOR", "ESP", "COL", "CAN", "AUS", "SAU"),
  pop_mil  = c(1425.7, 1428.6, 339.9, 277.5, 240.5, 223.8, 216.4, 172.9, 
               144.4, 128.5, 123.3, 126.5, 117.3, 112.7, 98.9, 102.3, 
               85.8, 89.2, 83.2, 71.8, 67.7, 64.7, 58.9, 60.4, 
               51.7, 47.5, 52.1, 38.8, 26.6, 36.9)
)

# 4. Merge custom data with the 'countries' object
# Note: Use 'all.x = TRUE' to preserve the map geometry for all countries
merged_data <- merge(
  countries, 
  custom_data, 
  by.x  = "SOC", 
  by.y  = "iso_code", 
  all.x = TRUE
)

# 5. Plot with layering strategy
ggplot() +
  # Layer 1: Data Fill (No borders, just color)
  geom_sf(
    data  = merged_data, 
    aes(fill = pop_mil), 
    color = "transparent"
  ) +
  # Layer 2: World Boundaries (Transparent fill, standard borders)
  geom_world(
    country_fill = NA, 
    show_ocean   = FALSE
  ) +
  # Styling
  scale_fill_viridis_c(
    option    = "plasma", 
    na.value  = "grey95", 
    direction = -1,      # Reverse color scale so dark = high population
    name      = "Population (Millions)"
  ) +
  theme_void() +
  theme(legend.position = "bottom")

