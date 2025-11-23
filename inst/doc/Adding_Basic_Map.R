## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ggmapcn)
# Define Azimuthal Equidistant projection centered on China
china_proj <- "+proj=aeqd +lat_0=35 +lon_0=105 +ellps=WGS84 +units=m +no_defs"

## ----example1, fig.alt='Basic Map'--------------------------------------------
ggplot() +
  geom_mapcn() +
  geom_boundary_cn() +
  theme_bw()

## ----example2, fig.alt='Map of China'-----------------------------------------
ggplot() +
  geom_buffer_cn(mainland_dist = 40000) +
  geom_buffer_cn(mainland_dist = 20000, fill = "#BBB3D8") +
  geom_mapcn(fill = "white") +
  geom_boundary_cn() +
  theme_bw()

## ----geom_loc, fig.alt='Sample site'------------------------------------------
# Create a ggplot with spatial points colored by 'Category'
set.seed(123)
data_sim <- data.frame(
   Longitude = runif(100, 80, 120),
   Latitude = runif(100, 28, 40),
   Category = sample(c("Type A", "Type B", "Type C"), 100, replace = TRUE)
   )
ggplot() +
   geom_boundary_cn() +
   geom_loc(
     data = data_sim, lon = "Longitude", lat = "Latitude",
     mapping = aes(color = Category), size = 1, alpha = 0.7
   ) +
   theme_bw()

## ----check-geodata, eval=FALSE------------------------------------------------
#  # This function checks if the required data files are available
#  # It may take some time, especially if your network connection is slow.
#    check_geodata(files = c("vege_1km_projected.tif"), quiet = FALSE)

## ----vegetion map, fig.alt='Vegetation Map of China', eval=FALSE--------------
#  # Add vegetation raster of China to a ggplot
#  ggplot() +
#    basemap_vege() +
#    guides(fill = guide_none()) +
#    theme_bw()

## ----dem map, fig.alt='Elevation Map of China'--------------------------------
# Apply Azimuthal Equidistant projection centered on China
ggplot() +
  basemap_dem(crs = china_proj, within_china = TRUE) +
  geom_boundary_cn(crs = china_proj) +
  tidyterra::scale_fill_hypso_c(
    palette = "dem_print",
    breaks = c(0, 2000, 4000, 6000),
    limits = c(0, 7000)
  ) +
  labs(fill = "Elevation (m)") +
  theme_minimal() +
  theme(legend.position = "bottom")

## ----clip region, fig.alt='clip region'---------------------------------------
ggplot() +
  geom_mapcn(fill = "white") +
  geom_boundary_cn() +
  coord_proj(
    crs = china_proj,
    xlim = c(60, 140),
    ylim = c(10, 50)
  ) +
  theme_bw()


## ----Nanhai, fig.alt='Naihai'-------------------------------------------------
ggplot() +
  geom_mapcn(fill = "white") +
  geom_boundary_cn() +
  theme_bw() +
  coord_proj(
    crs = china_proj,
    expand = FALSE,
    xlim = c(105, 126),
    ylim = c(2, 23)
  )

