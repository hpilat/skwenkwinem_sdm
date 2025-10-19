# Author: Jason Pither, Hannah Pilat
# Date: April 12th, 2024
# Updated by Jason Pither May 21, 2025

# This is script 03/10

# This script aims to create a geospatial vector dataset that provides the 
# boundary for the continental divide of North America, buffered by 200km to 
# its east, then bounded by the Arctic and Pacific oceans, and the Mexico border

# Load libraries
library(sf)
library(dplyr)
library(here)

# Import data
basin.na <- st_read(here::here("data", "raw", "north_america_hydro", "hybas_na_lev02_v1c.shp"))
basin.ar <- st_read(here::here("data", "raw", "arctic_hydro", "hybas_ar_lev02_v1c.shp"))

# Ensure HYBAS_ID is treated as character
basin.ar$HYBAS_ID <- as.character(basin.ar$HYBAS_ID)
basin.na$HYBAS_ID <- as.character(basin.na$HYBAS_ID)

# Simplify geometries
basin.ar <- st_make_valid(basin.ar)
arctic.simple <- st_simplify(basin.ar, dTolerance = 100)

basin.na <- st_make_valid(basin.na)
na.simple <- st_simplify(basin.na, dTolerance = 100)

# Filter to focal basins
arctic.filtered <- arctic.simple |>
  dplyr::filter(HYBAS_ID == "8020000010")

na.filtered <- na.simple |>
  dplyr::filter(HYBAS_ID == "7020000010" | HYBAS_ID == "7020014250")

# Merge and dissolve polygons
western.divide <- rbind(arctic.filtered, na.filtered)
western.divide <- western.divide |>
  st_buffer(10) |>
  st_union()

# Export simplified and dissolved western divide
write_sf(western.divide, here::here("data", "extents", "continental_divide.shp"))

# Buffer fails with 200km — will try alternative approach below

# Isolate eastern adjacent basins
arctic.inner.filtered <- arctic.simple |>
  dplyr::filter(HYBAS_ID == "8020008900")

na.inner.filtered <- na.simple |>
  dplyr::filter(HYBAS_ID == "7020021430" | HYBAS_ID == "7020046750" | HYBAS_ID == "7020047840")

western.inner.divide <- rbind(arctic.inner.filtered, na.inner.filtered) |>
  st_buffer(10) |>
  st_union()

# Intersect to get border
western.border <- st_intersection(st_make_valid(western.divide), st_make_valid(western.inner.divide))

# Buffer border 300 km eastward
western.border.buffer <- western.border |>
  st_make_valid() |>
  st_buffer(300000)

# Merge original western divide with buffered border
western.final.boundary <- st_union(
  st_make_valid(st_sf(western.divide)),
  st_make_valid(st_sf(western.border.buffer))
)

# Export final boundary
st_write(
  western.final.boundary,
  dsn = here::here("data", "extents", "continental_divide_buffer_boundary.shp"),
  append = FALSE
)
