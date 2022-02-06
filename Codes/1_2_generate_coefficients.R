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
library(gstat)



#--- source functions ---#
source(here("Codes", "functions.R"))

# /*----------------------------------*/
#' ## Field data
# /*----------------------------------*/

field_padding <- readRDS(here("Data", "CNN_Simulations", "field_padding.rds"))


# /*=================================================*/
#' # Generate coefficients
# /*=================================================*/
xy <- dplyr::select(field_padding, unique_cell_id) %>%
  cbind(., st_coordinates(st_centroid(.))) %>%
  st_drop_geometry() %>%
  data.table()

gen_coefs <- function(mean, psill, range, coef_name, nsim, xy) {
  
  g_N <- gstat(
    formula = z ~ 1,
    locations = ~ X + Y,
    dummy = T,
    beta = mean,
    model = vgm(
      psill = psill,
      range = range,
      nugget = 0,
      model = "Sph" # changed from "Exp", "Sph"

    ),
    nmax = 50 # number of nearest observations
  )

  b_sim <- predict(g_N, newdata = xy, nsim = nsim) %>%
    data.table()%>%
    melt(id.vars = c("X", "Y")) %>%
    # data.table() %>%
    setnames(c("variable", "value"), c("sim", coef_name)) %>%
    .[, sim := as.numeric(gsub("sim", "", sim))] %>%
    xy[., on = c("X", "Y")] %>%
    .[, c("unique_cell_id", "sim", coef_name), with = FALSE]

  return(b_sim)
}


sp_range <- 400
B <- 1000

#--- ymax ---#
ymax <- gen_coefs(
  mean = 12000,
  psill = 2000000,
  range = sp_range,
  coef_name = "ymax",
  nsim = B,
  xy = xy
)

#--- alpha ---#
alpha <- gen_coefs(
  mean = -0.5,
  psill = 0.02,
  range = sp_range,
  coef_name = "alpha",
  nsim = B,
  xy = xy
)



#--- beta ---#
beta <- gen_coefs(
  mean = 0,
  psill = 1,
  range = sp_range,
  coef_name = "beta_raw",
  nsim = B,
  xy = xy
) %>%
  .[, sd_beta := sd(beta_raw), by = sim] %>%
  .[, mean_beta := mean(beta_raw), by = sim] %>%
  .[, beta := pnorm(beta_raw, mean = mean_beta, sd = sd_beta)] %>%
  .[, beta := (beta * 1.8 - 2.8) * 0.01]



#--- m_error ---#
#=== error psill ===#
#' roughly,
#' 0.002 means 500 sd,
#' 0.015 means 1300 sd,
#' 0.028 means 2000 sd
m_error <- gen_coefs(
  mean = 0,
  psill = 0.015,
  range = sp_range,
  coef_name = "m_error_uncorrelated",
  nsim = B,
  xy = xy
)

split_ratio <- gen_coefs(
  mean = 0.5,
  psill = 0.005,
  range = sp_range,
  coef_name = "split_ratio",
  nsim = B,
  xy = xy
)

#-- This error term is used to correlate m_error with beta --#
mu_1 <- gen_coefs(
  mean = 0,
  psill = 1,
  range = sp_range,
  coef_name = "mu_1",
  nsim = B,
  xy = xy
)

#-- This error term is used for theta_1 --#
mu_2 <- gen_coefs(
  mean = 0,
  psill = 1,
  range = sp_range,
  coef_name = "mu_2",
  nsim = B,
  xy = xy
)

#-- This error term is used for theta_2 --#
mu_3 <- gen_coefs(
  mean = 0,
  psill = 1,
  range = sp_range,
  coef_name = "mu_3",
  nsim = B,
  xy = xy
)


  

# summary(split_ratio[,split_ratio])
# hist(split_ratio[,split_ratio])


## Put these coef data together ##
coef_data <- ymax[alpha, on = c("sim", "unique_cell_id")] %>%
  beta[., on = c("sim", "unique_cell_id")] %>%
  m_error[., on = c("sim", "unique_cell_id")] %>%
  split_ratio[., on =c("sim", "unique_cell_id")]%>%
  mu_1[., on =c("sim", "unique_cell_id")]%>%
  mu_2[., on =c("sim", "unique_cell_id")]%>%
  mu_3[., on =c("sim", "unique_cell_id")]%>%
  .[,`:=`(
    alpha1 = alpha*split_ratio,
    alpha2 = alpha*(1-split_ratio),
    beta1  = beta*split_ratio,
    beta2  = beta*(1-split_ratio),
    ymax1  = ymax*split_ratio,
    ymax2  = ymax*(1-split_ratio),
    beta_raw = NULL,
    sd_beta = NULL,
    mean_beta = NULL
  )]


# ggplot(left_join(field%>%select(unique_cell_id), coef_data[sim==2], by='unique_cell_id'))+
#   geom_sf(aes(fill=alpha1),lwd = 0)

# ggplot(left_join(field%>%select(unique_cell_id), coef_data[sim==2], by='unique_cell_id'))+
#   geom_sf(aes(fill=alpha2),lwd = 0)

# ggplot(left_join(field%>%select(unique_cell_id), coef_data[sim==2], by='unique_cell_id'))+
#   geom_sf(aes(fill=alpha),lwd = 0)



# /*=================================================*/
#' # Generate errors (make m_error correlated with beta)
# /*=================================================*/

##== This is the original code ==##
# coef_data[, beta_norm := (beta - mean(beta))/sd(beta)] %>% 
#   .[, m_error_raw := 0.6 * beta_norm + sqrt(1 - 0.6 ^ 2) * rnorm(nrow(.))] %>% 
#   .[, m_error := m_error_raw * sd(m_error_uncorrelated)] %>% 
#   ##== create irrelevant coefficints but are correlated with beta==##
#   .[, theta_1 := 0.6 * beta_norm + sqrt(1 - 0.6 ^ 2) * rnorm(nrow(.))] %>% 
#   .[, theta_2 := 0.8 * beta_norm + sqrt(1 - 0.6 ^ 2) * rnorm(nrow(.))] %>%
#   .[, `:=`(
#     beta_norm = NULL,
#     m_error_raw = NULL,
#     m_error_uncorrelated = NULL
#   )]


##== This is the modified code ==##
coef_data[, beta_norm := (beta - mean(beta))/sd(beta)] %>% 
  .[, mu1_norm := (mu_1 - mean(mu_1))/sd(mu_1)]%>%
  .[, mu2_norm := (mu_2 - mean(mu_2))/sd(mu_2)]%>%
  .[, mu3_norm := (mu_3 - mean(mu_3))/sd(mu_3)]%>%
  .[, m_error_raw := 0.6 * beta_norm + sqrt(1 - 0.6 ^ 2) * mu1_norm] %>% 
  .[, m_error := m_error_raw * sd(m_error_uncorrelated)] %>% 
  ##== create irrelevant coefficints but are correlated with beta==##
  .[, theta_1 := 0.6 * beta_norm + sqrt(1 - 0.6 ^ 2) * mu2_norm] %>% 
  .[, theta_2 := 0.8 * beta_norm + sqrt(1 - 0.6 ^ 2) * mu3_norm] %>%
  .[, `:=`(
    beta_norm = NULL,
    m_error_raw = NULL,
    mu_1 = NULL,
    mu_2 = NULL,
    mu_3 = NULL,
    mu1_norm = NULL, 
    mu2_norm = NULL, 
    mu3_norm = NULL,
    split_ratio = NULL
    # m_error_uncorrelated = NULL
  )]


coef_data[,.(beta, theta_1, theta_2, m_error)]%>%cor()


saveRDS(coef_data, here("Data", "CNN_Simulations", paste0('coefficients_sprange_',sp_range,'.rds')))










