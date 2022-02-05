# /*=================================================*/
#' # Preparation
# /*=================================================*/

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
library(parallel)
library(mapedit)


#--- source functions ---#
source(here("Codes", "0_0_functions.R"))

#--- github ---#
# source("https://raw.githubusercontent.com/brittanikedge/DIFM/main/Functions.R")

# ==========================================================================
#  1. Field Data
# ==========================================================================

ffy <- "Wendte_LaueLib80_2020"

#--- field boundary ---#
field <- ffy %>%
  file.path(here("Data"), ., "Raw/boundary.shp") %>%
  st_read() %>%
  st_make_valid() %>%
  st_transform_utm() %>%
  st_buffer(60) %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_as_sf()

tm_shape(field) +
  tm_borders()

#--- bbox ---#
wf_bbox <- st_bbox(field)

#--- create ab-line ---#
ab_line <- rbind(
  c(wf_bbox["xmin"], wf_bbox["ymin"]),
  c(wf_bbox["xmin"], wf_bbox["ymax"])
) %>%
  st_linestring()

# /*=================================================*/
#' # Create polygons (plots, harvester, applicator, planter)
# /*=================================================*/
# /*----------------------------------*/
#' ## Set parameters
# /*----------------------------------*/
starting_point <- c(wf_bbox["xmin"] - 100, wf_bbox["ymin"] - 100)

# use 60 ft design with 30 ft yield monitor
cell_height <- conv_unit(10, "ft", "m")
polygon_width <- conv_unit(60, "ft", "m")

# /*----------------------------------*/
#' ## Create polygons
# /*----------------------------------*/
# group: strip id
# basic_plot_id: ids for the small polygons
# plot_id: plot id

all_grids <- make_trial_grids(
  field,
  ab_line,
  starting_point,
  polygon_width,
  cell_height
)

field_dt <- all_grids %>%
  data.table() %>%
  .[, dummy := 1] %>%
  .[, cell_id := cumsum(dummy), by = strip_id] %>%
  .[, num_obs := .N, by = .(strip_id, plot_id)] %>%
  #--- get rid of short plots ---#
  .[num_obs == median(num_obs), ] %>%
  .[, subplot_id := ceiling(cell_id / 6), by = strip_id] %>% 
  .[, unique_cell_id := paste0(cell_id, "_", strip_id)]

# ggplot(st_as_sf(field_dt)) +
#   geom_sf(aes(fill = factor(plot_id)))

# ggplot(filter(st_as_sf(field_dt), strip_id %in% c(1))) +
#   geom_sf(aes(fill = factor(subplot_id)))

num_levels <- 6

field_dt <- gen_cluster(field_dt, num_levels, dim = 1)
field_dt <- gen_cluster(field_dt, num_levels, dim = 2)

# ggplot(st_as_sf(field_dt) %>% filter(strip_id == 1)) +
#   geom_sf(aes(fill = factor(cluster_id_1)), size = 0)

# saveRDS(st_as_sf(field_dt), here("Data", "Simulations", "field.rds"))



# ==========================================================================
# 2. Modify Field data
#   (1) Expnad the field 
#   (2) Divide each subplot into 6 cells
#   (3) Create "padding" indicator
# ==========================================================================

# /*==================================*/
#' (1) Expnad the field 
# /*==================================*/

field <- readRDS(here("Data", "Simulations", "field.rds"))

# ggplot(field)+geom_sf()

# ggplot(field)+geom_sf(color=NA)+
#   geom_sf(data=field%>%filter(strip_id==1), fill="red", color=NA, alpha=0.8)+
#   geom_sf(data=field%>%filter(plot_id==1), fill="blue", color=NA, alpha=0.8)+
#   theme_void()

field_bbox <- st_bbox(field)


strip_bbox <- field%>%
  filter(strip_id==1)%>%
  st_bbox(strip)

plot_bbox <- field%>%
  filter(plot_id==1)%>%
  st_bbox()


# vertical line
strip_width <- strip_bbox["xmax"] - strip_bbox["xmin"]

# horizontal line
plot_length <- plot_bbox["ymax"]-plot_bbox["ymin"]


#-- current bounding box --#
bbox_source <- st_bbox(field) 

#-- expand the bounding box --#
bbox_source[1] <- field_bbox["xmin"] - strip_width #xmin
bbox_source[3] <- field_bbox["xmax"] + strip_width #xmax
bbox_source[2] <- field_bbox["ymin"] - plot_length #ymin
bbox_source[4] <- field_bbox["ymax"] + plot_length #ymax


field_new <- st_as_sfc(bbox_source)%>%st_as_sf()

#--- bbox ---#
wf_bbox_new <- st_bbox(field_new)

#--- create ab-line ---#
ab_line_new <- rbind(
  c(wf_bbox_new["xmin"], wf_bbox_new["ymin"]),
  c(wf_bbox_new["xmin"], wf_bbox_new["ymax"])
) %>%
  st_linestring()


# /*-----------------------------------------------------*/
#' # Create polygons (plots, harvester, applicator, planter)
# /*-----------------------------------------------------*/
# /*----------------------------------*/
#' ## Set parameters
# /*----------------------------------b */
starting_point_new <- c(wf_bbox_new["xmin"], wf_bbox_new["ymin"])

# use 60 ft design with 30 ft yield monitor
cell_height <- conv_unit(10, "ft", "m")
polygon_width <- conv_unit(60, "ft", "m")


# /*----------------------------------*/
#' ## Create polygons
# /*----------------------------------*/
# group: strip id
# basic_plot_id: ids for the small polygons
# plot_id: plot id

all_grids_new <- make_trial_grids(
  field_new,
  ab_line_new,
  starting_point_new,
  polygon_width,
  cell_height
)

ggplot()+
  geom_sf(data=field, fill="green", alpha=0.6)+
  geom_sf(data=all_grids_new, alpha=0)+
  theme_void()


##== create cell_id and subplot_id ==##
field_new_dt <- all_grids_new %>%
  data.table() %>%
  .[, dummy := 1] %>%
  .[, cell_id := cumsum(dummy), by = strip_id] %>%
  .[, num_obs := .N, by = .(strip_id, plot_id)] %>%
  #--- get rid of short plots ---#
  .[num_obs == median(num_obs), ] %>%
  .[, subplot_id := ceiling(cell_id / 6), by = strip_id] %>% 
  .[, unique_cell_id := paste0(cell_id, "_",strip_id)]


base_field <- st_as_sf(field_new_dt)%>%
  filter(!(strip_id==max(strip_id)))


#== Check ==#
# ggplot()+
#   geom_sf(data=field, fill="green", alpha=0.8)+
#   geom_sf(data=field_new_sf, alpha=0)+
#   geom_sf(data=field_new_sf%>%filter(plot_id==max(plot_id)), fill="blue", alpha=0.4)+
#   geom_sf(data=field_new_sf%>%filter(plot_id==min(plot_id)), fill="blue", alpha=0.4)+
#   theme_void()

# saveRDS(field_new_sf, here("Data", "CNN_Simulations", "base_field.rds"))


# /*========================================*/
#' (2) Divide each subplot into 6 cells
# /*========================================*/

base_field <- readRDS(here("Data", "CNN_Simulations", "base_field.rds"))

####==== make grids within a subplot for base_field ====####
make_grid_within_cell <- function(cell){
  # cell="2_1"
  temp_cell <- base_field%>%filter(unique_cell_id==cell)
  res <- st_make_grid(st_boundary(temp_cell),  n = c(6, 1))%>%
    st_as_sf()%>%
    mutate(unique_cell_id=cell, cell_in_cell.id=seq(from=1,to=6))

  return(res)
} 


cell_list <- unique(base_field$unique_cell_id)

field_grid_within_cell <- lapply(1:length(cell_list),
  function(x) make_grid_within_cell(cell_list[[x]]))%>%
  mapedit:::combine_list_of_sf()

##-- combine to the original field data --##
final_field <- left_join(
  field_grid_within_cell,
  data.table(base_field)%>%.[,geometry:=NULL], by="unique_cell_id")%>%
  cbind(., st_coordinates(st_centroid(.)))%>%
  mutate(unique_subplot_id=unique_cell_id,
    unique_cell_id=paste0(unique_subplot_id,"_",cell_in_cell.id))

#== Check ==#
# ggplot()+geom_sf(data=final_field)+
#   geom_sf(data=field_base, fill="green", alpha=0.8)+
#   geom_sf(data=final_field%>%filter(plot_id==min(plot_id)), fill="blue", alpha=0.4)+
#   geom_sf(data=final_field%>%filter(plot_id==max(plot_id)), fill="blue", alpha=0.4)


# /*========================================*/
#' (3) Create "padding" indicator
# /*========================================*/

final_field_point <- st_centroid(final_field)

field_base_boundary <- st_union(field_base)

padding_dt <- final_field_point%>%
  select(unique_cell_id)%>%
  mutate(padding = ifelse(st_within(final_field_point, field_base_boundary, sparse=FALSE), 1, 0))%>%
  st_drop_geometry()%>%
  data.table()

final_field_res <- left_join(final_field, padding_dt, by="unique_cell_id")

ggplot(final_field_res)+
  geom_sf(aes(fill=factor(padding)))


# saveRDS(final_field_res, here("Data", "CNN_Simulations", "field_padding.rds"))






















