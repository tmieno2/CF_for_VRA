# /*=================================================*/
#' # Preparation
# /*=================================================*/
library(here)
library(mgcv)
library(ggplot2)
library(grf)
library(sf)
library(measurements)
library(data.table)
library(tidyverse)
library(future.apply)
library(parallel)

#--- source functions ---#
source(here("GitControlled", "Codes_MACF", "0_0_functions.R"))

# /*----------------------------------*/
#' ## Datasets
# /*----------------------------------*/
# field <- readRDS(here("Shared", "Data", "for_Simulations", "field_padding.rds"))

sp_range=400

# coef_data <- readRDS(here("Shared", "Data", "for_Simulations", paste0('coefficients_sprange_',sp_range,'.rds')))

# === prices ===#
pCorn <- price_table[2, pCorn]
pN <- price_table[2, pN]



# /*=================================================*/
#' # Simulation
# /*=================================================*/
reg_data_all <- readRDS(here("Shared", "Data", "for_Simulations", "reg_data.rds"))
test_data_all <- readRDS(here("Shared", "Data", "for_Simulations", "test_agg_data.rds"))

sim_results <- lapply(
	  1:100, function(x) {
	    sim_par(
	      i = x,
	      reg_data = reg_data_all[sim==x&padding==1,],
	      test_agg_data = test_data_all[sim==x&padding==1,],
	      N_levels = reg_data_all[sim==x,]$rate%>%unique()%>%sort()
	    )
	  }
	) %>%
	rbindlist()%>%
	rowwise() %>%
	mutate(var_ls_name = paste0(var_ls, collapse = "_")) %>%
	data.table()%>%
	.[, var_case := factor(var_ls_name)] %>%
	.[, var_case := case_when(
    	var_case == "alpha_beta_ymax" ~ "aby",
    	var_case == "alpha_beta_ymax_theta_1_theta_2" ~ "abytt",
    	var_case == "alpha1_alpha2_beta1_beta2_ymax1_ymax2" ~ "aabbyy",
    	var_case == "alpha1_alpha2_beta1_beta2_ymax1_ymax2_theta_1_theta_2" ~ "aabbyytt"
  	)]%>%
  	.[, .(sim,  model, var_case, unique_subplot_id, yield, opt_N, pred_yield, opt_N_hat)]

saveRDS(sim_results, here("Shared", "Results_MACF", "SimRes_MACF_BRF_RF_100.rds"))



