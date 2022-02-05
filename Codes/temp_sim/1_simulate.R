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
source(here("GitControlled", "Codes", "temp_sim", "0_functions.R"))

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
#' # Generate dataset 
# /*=================================================*/

##== Generate Cell-Level Field Data ==##
raw_data <- lapply(
	1:1000, function(x) {
		prepare_raw_data(
			i = x,
			field = field,
			coef_data_m = coef_data[sim == x, ],
			coef_data_t = coef_data[sim == ifelse(x + 1 >= max(sim), 1, x + 1), ],
			app_error="no"
		)
	}
)

# saveRDS(raw_data, here("Shared", "Data", "for_Simulations", "raw_reg_test_data.rds"))
reg_raw_data <- sapply(raw_data,"[",1)%>%rbindlist()
test_raw_data <- sapply(raw_data,"[",1)%>%rbindlist()

# saveRDS(reg_raw_data, here("Shared", "Data", "for_Simulations", "reg_raw_data.rds"))
# saveRDS(test_raw_data, here("Shared", "Data", "for_Simulations", "test_raw_data.rds"))

##== Aggregate Cell-Level Field Data to Subplot-Level Field Data ==##
sim_data <- lapply(
	1:1000, function(x) {
		print(paste0("working on ", x, " th iteration."))
		prepare_data_for_sim(
			reg_raw_data = reg_raw_data[sim==x,],
			test_raw_data = test_raw_data[sim==x,]
		)
	}
)

reg_data <- sapply(sim_data,"[",1)%>%rbindlist()
test_agg_data = sapply(sim_data,"[",2)%>%rbindlist()
test_cell_data = sapply(sim_data,"[",3)%>%rbindlist()

# saveRDS(reg_data, here("Shared", "Data", "for_Simulations", "reg_data.rds"))
# saveRDS(test_agg_data, here("Shared", "Data", "for_Simulations", "test_agg_data.rds"))
# saveRDS(test_cell_data, here("Shared", "Data", "for_Simulations", "test_cell_data.rds"))




# /*=================================================*/
#' # Simulation
# /*=================================================*/
reg_data_all <- readRDS(here("Shared", "Data", "for_Simulations", "reg_data.rds"))

test_agg_data_all <- readRDS(here("Shared", "Data", "for_Simulations", "test_agg_data.rds"))


sim_results <- lapply(
	  1:100, function(x) {
	    sim_par(
	      i = x,
	      reg_data = reg_data_all[sim==x&padding==1,],
	      test_agg_data = test_agg_data_all[sim==x&padding==1,],
	      N_levels = reg_data_all[sim==x,]$rate%>%unique()%>%sort()
	    )
	  }
	) %>%
	rbindlist() %>%
	rowwise() %>%
	mutate(var_ls_name = paste0(var_ls, collapse = "_")) %>%
	data.table()%>%
	.[, var_case := factor(var_ls_name)] %>%
	# .[, var_case := case_when(
 #    	var_case == "alpha_beta_ymax" ~ "aby",
 #    	var_case == "alpha_beta_ymax_theta_1_theta_2" ~ "abytt",
 #    	var_case == "alpha1_alpha2_beta1_beta2_ymax1_ymax2" ~ "aabbyy",
 #    	var_case == "alpha1_alpha2_beta1_beta2_ymax1_ymax2_theta_1_theta_2" ~ "aabbyytt"
 #  	)]%>%
  	.[, .(sim,  model, var_case, unique_subplot_id, yield, opt_N, pred_yield, opt_N_hat, opt_N_hat_g)]

# saveRDS(sim_results, here("Shared", "Results", paste0("SimRes_tuning.rds")))
saveRDS(sim_results, here("Shared", "Results", "SimRes_XY_smooth.rds"))



# ==========================================================================
# Evaluation
# ==========================================================================
simRes_XY <- readRDS(here("Shared", "Results", "SimRes_XY.rds"))%>%
    .[,.(sim, model, unique_subplot_id, opt_N, opt_N_hat)] %>%
    setnames("model", "Method")


pi_loss_dt_XY <- readRDS(here("Shared", "Data", "for_Simulations", "test_agg_data.rds")) %>%
    .[padding==1,.(sim, unique_cell_id, alpha, beta, ymax)]%>%
    setnames('unique_cell_id', "unique_subplot_id") %>%
    .[simRes_XY, on=c("sim", "unique_subplot_id")] %>%
    .[,`:=`(
        max_pi = pCorn*gen_yield_MB(ymax, alpha, beta, opt_N) - pN*opt_N,
        pi =  pCorn*gen_yield_MB(ymax, alpha, beta, opt_N_hat) - pN*opt_N_hat)] %>%
    .[, pi_loss := max_pi - pi]

rmse_general <-function(preds,actual){ 
  sqrt(mean((actual - preds)^2))
}

rmse_pi_loss_bySim_XY <- pi_loss_dt_XY %>%
	.[,.(
	rmse_optN = rmse_general(opt_N_hat, opt_N),
    mean_pi_loss = mean(pi_loss)
    ), by= .(sim, Method)]

# /*---------------------------------*/
#' ## Summarize by  model
# /*---------------------------------*/
rmse_pi_loss_bySim_XY %>%
    .[,.(
    	mean_rmse_optN = mean(rmse_optN),
    	mean_pi_loss = mean(mean_pi_loss)
    	), by=.(Method)]




