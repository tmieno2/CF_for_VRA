
# /*=================================================*/
#' # Preparation
# /*=================================================*/

library(here)
library(tmap)
library(ggplot2)
library(ggthemes)
library(grf)
library(sf)
library(raster)
library(data.table)
library(tidyverse)
library(parallel)
`
# =========================
# Preparation
# =========================
#--- source functions ---#
source(here("Gitcontrolled", "Codes", "0_0_functions.R"))
# source(here("Gitcontrolled", "Codes", "functions_stepwise_vs_base.R"))

# field <- readRDS(here("Shared", "Data", "for_Simulations", "field_padding.rds")) %>%
#   cbind(., st_coordinates(st_centroid(.)))

# # === prices ===#
pCorn <- price_table[2, pCorn]
pN <- price_table[2, pN]

# sp_range=400
# coef_data <- readRDS(here("Shared", "Data", "Simulations", paste0("coefficients_sprange_", sp_range, ".rds")))

# ==========================================================================
# Function 
# ==========================================================================

tre_eval_function <- function(reg_data, test_data, var_ls){
	# x = 1
	# reg_data = reg_data_all[sim==x&padding==1,]
	# test_data = test_subplot_data_all[sim==x&padding==1,]
	# var_ls = c("alpha","beta","ymax")
	# print(paste0("working on ", x, " th iteration."))

	N_levels <- reg_data[,rate]%>%unique()%>%sort()

	##== CF-stepwiese ==##
	CF_stepwise_te_dt <- CF_analysis(
		reg_data=reg_data, 
		var_ls=var_ls, 
		cl_var = NA) %>%
  CF_stepwise_calculate_te(
    test_data = test_data, 
    cf_results = ., 
    var_ls = var_ls, 
    rates_ls = N_levels)%>%
  	.[,Method := "CF_stepwise"] %>%
  	setnames("tau_stepwise", "tau_hat")


  ####==== CF-base =====#####
	CF_base_te_dt <- CF_analysis_base(
    reg_data=reg_data, 
    var_ls=var_ls, 
    cl_var = NA
    )%>%
  CF_base_calculate_te(
    test_data = test_data, 
    cf_results = ., 
    var_ls = var_ls, 
    rates_ls = N_levels)%>%
  	.[,Method := "CF_base"] %>%
  	setnames("tau_base", "tau_hat")

  ####==== TURE TE ====####
  true_te_dt <- test_data %>%
		.[,.(sim, unique_cell_id, alpha, beta, ymax)]%>%
		.[rep(1:nrow(.), each = length(N_levels)), ] %>%
    .[, rate := rep(N_levels, nrow(.) / length(N_levels))] %>%
    .[, det_yield := gen_yield_MB(ymax, alpha, beta, rate)] %>%
		.[, yield_base := .SD[rate==min(rate), det_yield], by = .(unique_cell_id)] %>%
    .[, true_tau_base := det_yield - yield_base] %>%
    .[, true_tau_stepwise := det_yield - shift(det_yield), by = unique_cell_id] %>%
    .[, .(sim, unique_cell_id, rate, true_tau_base)]

  te_dt_allML <- rbind(CF_stepwise_te_dt, CF_base_te_dt)

	te_comp_dt <- left_join(te_dt_allML, true_te_dt, by=c("unique_cell_id", "rate"))%>%
		.[rate != N_levels[1],]%>%
		.[, Treatment := case_when(
			rate == N_levels[2] ~ "N1-N2",
			rate == N_levels[3] ~ "N1-N3",
			rate == N_levels[4] ~ "N1-N4",
			rate == N_levels[5] ~ "N1-N5"
		)]%>%
	.[, Method := factor(Method, levels = c("CF_stepwise", "CF_base"))] %>%
	setnames("unique_cell_id", "unique_subplot_id")

	return(te_comp_dt)
}

x= 1

temp_te_dt <- tre_eval_function(
	reg_data = reg_data_all[sim==x&padding==1,],
	test_data = test_agg_data_all[sim==x&padding==1,],
	var_ls = c("alpha","beta","ymax")
	)


sim_par_te <- function(i, reg_data, test_data) {
  print(paste0("working on ", i, " th iteration."))
  # for example
  # x=1 
  # i=x
  # reg_data = reg_data_all[sim==x&padding==1,]
  # test_data = test_data_all[sim==x&padding==1,]
  # N_levels = reg_data[,rate]%>%unique()%>%sort()

  # /*----------------------------------*/
  #' ## run ML analyses
  # /*----------------------------------*/

  ## == all the combinations of variables ==##
  var_ls_variations <- list(
    c("alpha", "beta", "ymax"),
    # c("alpha", "beta", "ymax", "theta_1", "theta_2"),
    # c("alpha1", "alpha2", "beta1", "beta2", "ymax1", "ymax2"),
    c("alpha1", "alpha2", "beta1", "beta2", "ymax1", "ymax2", "theta_1", "theta_2")
  )

  #--- all the cases to consider ---#
  case_data <- tibble(
    var_ls = var_ls_variations
  ) 

  results_data <- case_data %>%
    mutate(
      te_data = mclapply(
        seq_len(nrow(.)),
        function(x) {
          ### === apply various methods to get optimal N ===###
          tre_eval_function(
            reg_data = reg_data,
          	test_data = test_data,
          	var_ls = .$var_ls[[x]]
          )
        },
        mc.cores = detectCores() - 2
      )
    )%>%
    unnest(., cols= "te_data")%>%
    data.table()

    return(results_data)
}

  	

# ==========================================================================
# Treatment effect simulation: Stepwise vs Base 
# ==========================================================================

reg_data_all <- readRDS(here("Shared", "Data", "for_Simulations", "reg_data.rds"))
test_data_all <- readRDS(here("Shared", "Data", "for_Simulations", "test_agg_data.rds"))

# x=1
# reg_data <- reg_data_all[sim==1&padding==1,]
# test_data <- test_subplot_data_all[sim==1&padding==1,]
# var_ls = c("alpha","beta","ymax")

tre_sim_results <- lapply(
	1:1000, function(x){
		sim_par_te(
			i = x,
			reg_data = reg_data_all[sim==x&padding==1,],
			test_data = test_data_all[sim==x&padding==1,]
			)
		}
	)%>%
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
  	)] %>%
  .[,!c("var_ls_name", "var_ls")] %>%
  dcast(
			unique_subplot_id + rate + sim + true_tau_base + Treatment + var_case ~ Method, 
			value.var = "tau_hat")

# saveRDS(tre_sim_results, here("Shared/Data/for_writing/dt_CF_TEcomparison_RMSE.rds"))
# rmse_general <-function(preds,actual){ 
#   sqrt(mean((actual - preds)^2))
# }

tre_sim_results <- readRDS(here("Shared/Data/for_writing/dt_CF_TEcomparison_RMSE.rds"))

eval <- tre_sim_results %>%
	.[,.(
		te_RMSE_stepwise = sqrt(mean((true_tau_base - CF_stepwise)^2)),
		te_RMSE_base = sqrt(mean((true_tau_base - CF_base)^2))
		), by = .(sim, Treatment, var_case)]

summary <- eval %>%
	.[,.(
		te_RMSE_stepwise = mean(te_RMSE_stepwise),
		te_RMSE_base = mean(te_RMSE_base)
		), by = .(Treatment, var_case)]%>%
	.[order(var_case)]







