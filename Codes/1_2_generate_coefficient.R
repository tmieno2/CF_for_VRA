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

# --- Range (m) --- #
Range <- 400

#/*--------------------------------------------------------*/
#' ## (1) Generate raw coefficients (Medium m_error: psill = 0.015)
#/*--------------------------------------------------------*/

# --- Number of iterations --- #
b <- 1000

# --- start simulation --- #
coef_data <- 
  gen_coefs_par(
    B = b, 
    geo_xy = xy,
    sp_range = Range,
    psill_merror = 0.015
    )

saveRDS(coef_data, here("Shared/Data/for_Simulations", paste0('coefficients_sprange_',Range,'.rds')))




#/*--------------------------------------------------------*/
#' ## (2) Generate raw coefficients (Small m_error: psill = 0.0075)
#/*--------------------------------------------------------*/
# --- set seed --- #
set.seed(39476)

# --- Number of iterations --- #
b <- 100

# --- start simulation --- #
coef_data_low <- 
  gen_coefs_par(
    B = b, 
    geo_xy = xy,
    sp_range = Range,
    psill_merror = 0.0075
    )


saveRDS(coef_data_low, here("Shared/Data/for_Simulations", paste0('coefficients_sprange_',Range,'_low_error.rds')))





#/*--------------------------------------------------------*/
#' ## (3) Generate raw coefficients (Large m_error: psill = 0.0225)
#/*--------------------------------------------------------*/
# --- set seed --- #
set.seed(57864)

# --- Number of iterations --- #
b <- 100

# --- start simulation --- #
coef_data_high <- 
  gen_coefs_par(
    B = b, 
    geo_xy = xy,
    sp_range = Range,
    psill_merror = 0.0225
    )

saveRDS(coef_data_high, here("Shared/Data/for_Simulations", paste0('coefficients_sprange_',Range,'_high_error.rds')))


