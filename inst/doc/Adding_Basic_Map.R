## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ggmapcn)

## ----example1, fig.alt='Basic Map'--------------------------------------------
ggplot() +
  geom_mapcn() +
  theme_minimal()

## ----example2, fig.alt='Map of China'-----------------------------------------
ggplot() +
  geom_buffer_cn(mainland_dist = 40000) +
  geom_buffer_cn(mainland_dist = 20000, fill = "#BBB3D8") +
  geom_mapcn(fill = "white") +
  geom_boundary_cn() +
  theme_bw()

## ----example3, fig.alt='Map of world'-----------------------------------------
# Define projections
china_proj <- "+proj=aeqd +lat_0=35 +lon_0=105 +ellps=WGS84 +units=m +no_defs"

# Combine world map as a background and China map as overlay
ggplot() +
  # World map as background
  geom_world(fill = "gray90", color = "gray70", linewidth = 0.2) +
  coord_proj(
    crs = "+proj=merc",
    xlim = c(-180, 180),
    ylim = c(-90, 90)
  ) +
  # Overlay China map
  geom_mapcn(
    fill = "lightblue",
    color = "black",
    linewidth = 0.5
  ) +
  geom_boundary_cn(color = "red", linewidth = 0.6) +
  theme_minimal()

## ----example4, fig.alt='Map of China'-----------------------------------------
# Define neighboring countries
china_neighbors <- c("CHN", "AFG", "BTN", "MMR", "LAO", "NPL", "PRK", "KOR",
                     "KAZ", "KGZ", "MNG", "IND", "BGD", "TJK", "PAK", "LKA", "VNM")

# Plot world map with filtered countries
ggplot() +
  geom_world(fill = "gray90", color = "gray70", linewidth = 0.2) +
  geom_world(
    filter = china_neighbors,
    filter_attribute = "SOC",
    fill = "lightblue",
    color = "black",
    linewidth = 0.5
  ) +
  geom_world(
    filter = "CHN",
    filter_attribute = "SOC",
    fill = "red",
    color = "black",
    linewidth = 0.8
  ) +
  coord_proj(
    crs = "+proj=merc",
    xlim = c(60, 140),
    ylim = c(-10, 60)
  ) +
  theme_minimal()

