# /*=================================================*/
#' # Preparation
# /*=================================================*/
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
source(here("Codes", "cnn_honesty_yield_pred_function.R"))

# /*----------------------------------*/
#' ## Datasets
# /*----------------------------------*/
field_cnn <- readRDS(here("Data", "CNN_Simulations", "cnn_field_padding.rds"))

# ggplot(field_cnn) +
#   geom_sf()

# ggplot(field_cnn) +
#   geom_sf(aes(fill = factor(padding)), size = 0)

sp_range=400

# coef_data <- readRDS(here("Data", "CNN_Simulations", paste0('cnn_coefficients_sprange_',sp_range,'.rds')))

# === prices ===#
pCorn <- price_table[2, pCorn]
pN <- price_table[2, pN]



# /*=================================================*/
#' # Simulation
# /*=================================================*/
train_data <- readRDS(here("Data", "CNN_Simulations", "reg_data.rds"))
test_data <- readRDS(here("Data", "CNN_Simulations", "test_agg_data.rds"))


sim_results <- lapply(
	  1:100, function(x) {
	    sim_par(
	      i = x,
	      reg_data = train_data[sim==x&padding==1,],
	      test_agg_data = test_data[sim==x&padding==1,]
	    )
	  }
	) %>%
	rbindlist()%>%
	rowwise() %>%
	mutate(var_ls_name = paste0(var_ls, collapse = "_")) %>%
	data.table()%>%
	.[, var_case := factor(var_ls_name)] %>%
	.[, .(sim, honesty,  model, var_case, r2_y)] %>%
	.[, var_case := case_when(
    	var_case == "alpha_beta_ymax" ~ "aby",
    	var_case == "alpha_beta_ymax_theta_1_theta_2" ~ "abytt",
    	var_case == "alpha1_alpha2_beta1_beta2_ymax1_ymax2" ~ "aabbyy",
    	var_case == "alpha1_alpha2_beta1_beta2_ymax1_ymax2_theta_1_theta_2" ~ "aabbyytt"
  	)]


saveRDS(sim_results, here("CNN_Results", paste0("SimRes_sp_",sp_range,"_y_honesty_RF_BRF.rds")))

# sim_results <- readRDS(here("CNN_Results", paste0("SimRes_sp_",sp_range,"_y_honesty_RF_BRF.rds")))


res_total <- copy(sim_results)%>%
  .[,.(r2_y = mean(r2_y)), by = .(model, var_case, honesty)]


sim_results[var_case=="aby",]


ggplot(sim_results%>%
	.[, var_case:=factor(var_case, levels = c("aby", "abytt", "aabbyy", "aabbyytt"))])+
	geom_density(aes(x=r2_y, fill=model), alpha=0.5)+
	# facet_wrap(honesty~var_case, ncol = 2)
	# facet_grid(var_case~honesty)
	facet_grid(honesty~var_case)


ggplot(data=sim_results[var_case=="aby"&honesty==FALSE&model=="BRF",])+
	geom_density(aes(x=r2_y), alpha=0.5)
	# facet_wrap(honesty~var_case, ncol = 2)
	# facet_grid(var_case~honesty)
	facet_grid(honesty~var_case)



thesis_opt_N_nohonest <- readRDS(here("tuning_exp", "results", "optN_no_honest.rds"))
thesis_opt_N_honest <- readRDS(here("Results", paste0("SimRes_sp_",400,".rds")))%>%
	.[, model := case_when(
    model == "CF" ~ "CF_stepwise",
    model == "CF_base" ~ "CF_base",
    model == "RF" ~ "RF",
    model == "BRF" ~ "BRF"
  )]%>%
    .[,cluster_var:=NULL]%>%
	.[, var_case:=factor(var_case, levels = c("aby", "abytt", "aabbyy", "aabbyytt"))]%>%
    .[, model:=factor(model, levels = c("RF", "BRF", "CF_stepwise", "CF_base"))]


honest <- thesis_opt_N_honest%>%
	.[,.(r2_N_honest = mean(r2)), by = .(model, var_case)]

nohonest <- thesis_opt_N_nohonest%>%
	 .[,.(r2_N_nohonest = mean(r2)), by = .(model, var_case)]


honest[nohonest, on=c("model", "var_case")]



opt_N_honest <- readRDS(here("CNN_Results", "SimRes_sp_400.rds"))


thesis_res_optN_nohonest <- copy(thesis_opt_N_nohonest)%>%
	# .[model%in%c("BRF", "RF")]%>%
  	.[,.(r2 = mean(r2)), by = .(model, var_case)]








res_honest <- copy(opt_N_honest)%>%
  	.[model%in%c("BRF", "RF")]%>%
  	.[,.(r2 = mean(r2_agg), r2_y = mean(r2_y_agg)), by = .(model, var_case)]



