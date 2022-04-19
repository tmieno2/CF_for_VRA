#/*----------------------------------*/
#' ## Preparation
#/*----------------------------------*/
library(here)
library(tmap)
library(sp)
library(sf)
library(agricolae)
library(lwgeom)
library(measurements)
library(raster)
library(data.table)
library(tidyverse)
library(gstat)

# === Load Functions === #
source(here("GitControlled/Codes/0_1_functions_gen_analysis_data.R"))

# === Load Field data === #
field <- readRDS(here("Shared/Data/for_Simulations/analysis_field.rds"))

# /*=================================================*/
#' # Unconditional Gaussian geostatistical Simulation
# /*=================================================*/

# === Set up === #

# --- Geographical locations of Cells --- #
xy <- dplyr::select(field, unique_cell_id) %>%
  cbind(., st_coordinates(st_centroid(.))) %>%
  st_drop_geometry() %>%
  data.table()

# --- Number of iterations --- #
b = 1000

# --- Range (m) --- #
Range = 400

# --- yield error size  --- #
Error = 0.015 


# === Generate coefficients === #
coef_data <- 
  gen_coefs_par(
    B = b, 
    geo_xy = xy,
    sp_range = Range,
    psill_merror = Error
    )

saveRDS(coef_data, here("Shared/Data/for_Simulations", paste0('coefficients_sprange_',Range,'.rds')))


