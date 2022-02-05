library(here)
library(mgcv)
library(tmap)
library(ggplot2)
library(grf)
library(sp)
library(sf)
library(agricolae)
library(lwgeom)
library(measurements)
library(raster)
library(data.table)
library(tidyverse)
library(future.apply)
library(parallel)


#--- source functions ---#
source(here("Codes", "functions.R"))

# /*----------------------------------*/
#' ## Datasets
# /*----------------------------------*/
field_cnn <- readRDS(here("Data", "CNN_Simulations", "cnn_field_padding.rds"))

# ggplot(st_as_sf(field_dt)) +
#   geom_sf(aes(fill = cluster_id_1), size = 0) +
#   geom_sf_text(aes(label = cluster_id_1))
sp_range=400

cnn_coef_data <- readRDS(here("Data", "CNN_Simulations", paste0('cnn_coefficients_sprange_',sp_range,'.rds')))


# === prices ===#
pCorn <- price_table[2, pCorn]
pN <- price_table[2, pN]




# For example
x=2
field=field_cnn
coef_data_m=cnn_coef_data[sim == x, ]
# coef_data_t=coef_data[sim == ifelse(x + 1 >= max(sim), 1, x + 1), ]

# /*~~~~~~~~~~~~~~~~~~~~~~*/
#' ### Data preparation
# /*~~~~~~~~~~~~~~~~~~~~~~*/
# === merge field data with the coefs data ===#
data <- coef_data_m[data.table(field), on = "unique_cell_id"] %>%
  .[, opt_N := (log(pN) - log(-pCorn * ymax * beta) - alpha) / beta]


# test_data <- coef_data_t[data.table(field), on = "unique_cell_id"] %>%
# .[, opt_N := (log(pN) - log(-pCorn * ymax * beta) - alpha) / beta]


# === check the spatial distribution of optimal N ===#
# ggplot(st_as_sf(data)) +
#   geom_sf(aes(fill = opt_N),size = 0)+
#   scale_fill_viridis_c()

# /*~~~~~~~~~~~~~~~~~~~~~~*/
#' ### Assign N (5 N treatment rates)
# /*~~~~~~~~~~~~~~~~~~~~~~*/
# === define experimental N rate ===#
all_opt_N <- data[, opt_N]

# --- for training ---#
# hist(all_opt_N)
N_levels <- seq(
  quantile(all_opt_N, prob = 0.05) - 20,
  quantile(all_opt_N, prob = 0.95) + 20,
  length = 5 
  ) %>%
round()


# data <- assign_rates(st_as_sf(data), N_levels) %>% # N_levels are assigned as "rate" variable
#   data.table()%>%
#   .[,aa_n:=rate+rnorm(nrow(.),sd=5)]

data <- assign_rates(st_as_sf(data), N_levels) %>% # N_levels are assigned as "rate" variable
  data.table()%>%
  .[,aa_n:=rate+app_error_fn(.)]%>%
  .[!(subplot_id %in% c(1:3, 54:56)),] ### <- remoce 3 rows from top and bottom


# test_data <- assign_rates(st_as_sf(test_data), N_levels) %>%
#   data.table()%>%
#   .[,aa_n:=rate*(1+rnorm(nrow(.))/5)]%>%
#   .[!(subplot_id %in% c(1:3, 54:56)),] ### <- remoce 3 rows from top and bottom

# /*~~~~~~~~~~~~~~~~~~~~~~*/
#' ### Generate yield
# /*~~~~~~~~~~~~~~~~~~~~~~*/
data %<>% .[, det_yield := gen_yield_MB(ymax, alpha, beta, rate)] %>%
  .[, det_yield_aa_n := gen_yield_MB(ymax, alpha, beta, aa_n)] %>%
  # === error ===#
  .[, yield := det_yield * (1 + m_error)] %>%
  .[, yield_aa_n := det_yield_aa_n * (1 + m_error)] %>%
  # === keep the relevant vars ===#
  .[, .(
    unique_cell_id, opt_N, rate, aa_n, yield, yield_aa_n, m_error, alpha, beta, ymax, alpha1, alpha2, beta1, beta2, ymax1, ymax2,
    theta_1, theta_2, subplot_id, strip_id, padding, X, Y
  )]


saveRDS(data, here("Results", "cnn_raw_data2.rds"))

# test_data %<>% .[, det_yield := gen_yield_MB(ymax, alpha, beta, as_app_N)] %>% #(5/25)changed from rate to as_app_N
#   # .[,det_yield:=b0+b1*N+b2*N^2] %>%
#   # === error ===#
#   .[, yield := det_yield * (1 + m_error)] %>%
#   # === keep the relevant vars ===#
#   .[, .(
#     unique_cell_id, opt_N, rate, as_app_N, yield, m_error, alpha, beta, ymax, alpha1, alpha2, beta1, beta2, ymax1, ymax2,
#     theta_1, theta_2, subplot_id, strip_id, cluster_id_1, cluster_id_2, X, Y
#       )] %>%
#   .[sample(1:nrow(.), 1000), ]

  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### Aggregate data by analysis unit (subplot_id-strip_id)
  # /*~~~~~~~~~~~~~~~~~~~~~~*/

  # === by analysis unit ===#
  reg_data <- data[, .(
    yield = mean(yield),
    yield_aa_n = mean(yield_aa_n),
    rate = mean(rate),
    aa_n = mean(aa_n), #<- (5/25) newly added
    alpha = mean(alpha),
    beta = mean(beta),
    ymax = mean(ymax),
    alpha1 = mean(alpha1),
    alpha2 = mean(alpha2),
    beta1 = mean(beta1),
    beta2 = mean(beta2),
    ymax1 = mean(ymax1),
    ymax2 = mean(ymax2),
    theta_1 = mean(theta_1),
    theta_2 = mean(theta_2),
    padding = mean(padding),
    # cluster_id_1 = mean(cluster_id_1),
    # cluster_id_2 = mean(cluster_id_2),
    X = mean(X),
    Y = mean(Y)
  ), by = .(subplot_id, strip_id)]



saveRDS(reg_data, here("Results", "cnn_reg_data2.rds"))




# ==========================================================================
# Vosialization
# ==========================================================================
cnn_raw_data <- readRDS(here("Results", "cnn_raw_data1.rds"))
cnn_reg_data <- readRDS(here("Results", "cnn_reg_data1.rds"))


field_sf <- left_join(cnn_raw_data, field_cnn%>%select(unique_cell_id), by="unique_cell_id")%>%
  st_as_sf()




subplot_map <- field_sf%>%
    group_by(subplot_id, strip_id)%>%
    summarise()



ggplot(field_sf)+geom_sf(aes(fill=factor(padding)), size = 0)+ 
  geom_sf(
        data=subplot_map,
        color='blue',
        size=0.4,
        fill=NA)



theme <- theme(
      plot.title = element_text(hjust = 0.5),
      panel.background = element_rect(fill = "white"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.title = element_blank()
        )



####==== Experimental design ====####
field_Ndesign <- ggplot(field_sf) +
    geom_sf(aes(fill = factor(rate)), size = 0,
      inherit.aes = FALSE) +
    scale_fill_viridis_d() +
    theme

real_data <- field_sf%>%
  filter(padding==1)%>%
  st_union()


ggplot()+
    geom_sf(data=field_sf,aes(fill = factor(rate)), size = 0,
      inherit.aes = FALSE) +
    scale_fill_viridis_d() +
    geom_sf(
        data=real_data,
        color='red',
        size=0.8,
        fill=NA)+
    theme

####==== yield map ====####
field_yield <- ggplot(field_sf) +
    geom_sf(aes(fill = yield), size = 0) +
    scale_fill_viridis_c()+
    # ggtitle('yield')+
    theme

####==== opt_N map ====####
opt_N_dist <- ggplot(field_sf)+
  geom_density(aes(x=opt_N))


field_optN <- ggplot(field_sf) +
    geom_sf(aes(fill = opt_N), size = 0) +
    scale_fill_viridis_c()+
    theme

####==== ymax map ====####
ymax_dist <- ggplot(field_sf)+
  geom_density(aes(x=ymax))


field_ymax <- ggplot(field_sf) +
    geom_sf(aes(fill = ymax), size = 0) +
    scale_fill_viridis_c()+
    ggtitle('(a) ymax')+
    theme

####==== alpha map ====####
alpha_dist <- ggplot(field_sf)+
  geom_density(aes(x=alpha))


field_alpha <- ggplot(field_sf) +
    geom_sf(aes(fill = alpha), size = 0) +
    scale_fill_viridis_c()+
    ggtitle('(b) alpha')+
    theme

####==== beta map ====####
beta_dist <- ggplot(field_sf)+
  geom_density(aes(x=beta))


field_beta <- ggplot(field_sf) +
    geom_sf(aes(fill = beta), size = 0) +
    scale_fill_viridis_c()+
    ggtitle('(c) beta')+
    theme


####==== m_error map ====#### (this should be m_error*det_yield, not just m_error)
m_error_sf <-  left_join(field_cnn, coef_data_m, by="unique_cell_id")

field_m_error <- ggplot(m_error_sf) +
    geom_sf(aes(fill = m_error), size = 0) +
    scale_fill_viridis_c()+
    ggtitle('(d) u')+
    theme






cnn_raw_training <- readRDS(here("Data", "CNN_Simulations", "reg_raw_data.rds"))
cnn_raw_testing <- readRDS(here("Data", "CNN_Simulations", "reg_raw_data.rds"))

sim1 <- cnn_raw_training[sim==1,]


field_sim1 <- left_join(select(field_cnn, unique_cell_id), sim1, by="unique_cell_id")%>%
  na.omit()


ggplot(field_sim1)+geom_sf(aes(fill=factor(padding)), color=NA)

61200/36
1700*36












