## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  fig.width  = 6,
  fig.height = 4
)

## ----setup, fig.alt="A base map of North Carolina with grey fill and minimal theme."----
library(ggplot2)
library(sf)
library(ggmapcn)

# Load example data
nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

# Create a base map object
base_geo <- ggplot() +
  geom_sf(data = nc, fill = "grey90", color = "grey40") +
  theme_minimal()

base_geo

## ----proj_scale, fig.alt="Two maps of North Carolina. The first shows a scale bar in the bottom left. The second shows a wider scale bar in the top right."----
base_proj <- base_geo +
  coord_sf(crs = 32617) +
  theme(axis.title = element_blank())

# Default scale bar at bottom-left
base_proj +
  annotation_scalebar(location = "bl")

# Top-right scale bar with custom width hint
base_proj +
  annotation_scalebar(
    location   = "tr",
    width_hint = 0.5
  )

## ----styles_1, fig.alt="A map showing the 'segment' style scale bar."---------
# 1. Segment style
p_segment <- base_proj +
  annotation_scalebar(
    location = "bl",
    style    = "segment",
    label_show = "all"
  )
p_segment

## ----styles_2, fig.alt="A map showing the 'ticks' style scale bar."-----------
# 2. Ticks style
p_ticks <- base_proj +
  annotation_scalebar(
    location = "br",
    style    = "ticks"
  )
p_ticks

## ----styles_3, fig.alt="A map showing the 'bar' style scale bar with black and white blocks."----
# 3. Bar style with custom colors
p_bar <- base_proj +
  annotation_scalebar(
    location = "tl",
    style    = "bar",
    bar_cols = c("black", "white")
  )
p_bar

## ----styles_custom, fig.alt="A map with a red scale bar fixed to 100km length."----
base_proj +
  annotation_scalebar(
    location     = "bl",
    fixed_width  = 100000,   # 100 km in meters
    display_unit = "km",
    line_col     = "red"
  )

## ----geo_mode, fig.alt="Two maps comparing scale bars. One shows distance in kilometers, the other in degrees."----
# Approximate meters
base_geo +
  coord_sf(crs = 4326) +
  annotation_scalebar(
    location        = "bl",
    geographic_mode = "approx_m"
  )

# Degrees
base_geo +
  coord_sf(crs = 4326) +
  annotation_scalebar(
    location        = "bl",
    geographic_mode = "degrees"
  )

## ----compass_basic, fig.alt="Two maps showing basic north arrows. One is standard size, the other is larger with padding."----
# Default classic arrow
base_geo +
  annotation_compass(
    location = "tl",
    style    = north_arrow_classic()
  )

# Custom size and padding
base_geo +
  annotation_compass(
    location = "tl",
    height   = grid::unit(0.8, "cm"),
    width    = grid::unit(0.8, "cm"),
    pad_x    = grid::unit(0.3, "cm"),
    pad_y    = grid::unit(0.3, "cm")
  )

## ----true_north, fig.alt="Two maps demonstrating north arrows. The first shows a rotated compass rose pointing to True North. The second shows a manually rotated Grid North arrow."----
# Define a Lambert Conformal Conic projection
base_lcc <- base_geo +
  coord_sf(crs = "+proj=lcc +lon_0=-100 +lat_1=33 +lat_2=45") +
  theme(axis.title = element_blank())

# True North (automatically rotated)
base_lcc +
  annotation_compass(
    which_north = "true",
    location    = "br",
    style       = compass_rose_simple()
  )

# Grid North with manual rotation
base_lcc +
  annotation_compass(
    which_north = "grid",
    rotation    = 30,
    location    = "tr",
    style       = north_arrow_solid()
  )

## ----compass_styles, fig.alt="Three maps showing different compass styles: Classic, Rose, and Sinan (Si Nan)."----
p_classic <- base_proj +
  annotation_compass(
    location = "tl",
    style    = north_arrow_classic()
  )

p_rose <- base_proj +
  annotation_compass(
    location = "br",
    style    = compass_rose_classic()
  )

p_sinan <- base_proj +
  annotation_compass(
    location = "tl",
    style    = compass_sinan()
  )

p_classic
p_rose
p_sinan

## ----combine, fig.alt="A final map combining a ticks-style scale bar at the bottom left and a compass rose at the top left."----
base_proj +
  annotation_scalebar(
    location   = "bl",
    style      = "ticks",
    width_hint = 0.3
  ) +
  annotation_compass(
    location = "tl",
    style    = compass_rose_circle()
  )

