# /*===== Run on RGUI =====*/

#/*----------------------------------*/
#' ## Preparation
#/*----------------------------------*/
library(here)
library(grf)
library(data.table)
library(tidyverse)
library(future.apply)
library(parallel)


#--- source functions ---#
setwd("~/Dropbox/ResearchProject/ML_VRA")
source("./GitControlled/Codes/0_3_functions_main_sim.R")

# /*----------------------------------*/
#' ## Datasets
# /*----------------------------------*/
# === prices ===#
pCorn <- price_table[2, pCorn]
pN <- price_table[2, pN]

reg_data_all <- readRDS("./Shared/Data/for_Simulations/reg_data.rds")
test_data_all <- readRDS("./Shared/Data/for_Simulations/test_data.rds")


# x=1
# train_dt <- reg_data_all[sim==x&padding==1,]
# test_dt <- test_data_all[sim==x&padding==1,]
# N_levels <- reg_data_all[sim==x,]$rate%>%unique()%>%sort()



# /*=================================================*/
#' # Simulation 
# /*=================================================*/

# === set up for parallel computations === #
plan(multicore, workers = availableCores()-2)
options(future.globals.maxSize= 850*1024^2)
set.seed(1378)

# --- Number of iterations --- #
B=1000

# === run simulation === #
sim_results <- lapply(
	  1:B, function(x) {
	    sim_par(
	      i = x,
	      reg_data = reg_data_all[sim==x&padding==1,],
	      test_data = test_data_all[sim==x&padding==1,],
	      N_levels = reg_data_all[sim==x,]$rate%>%unique()%>%sort()
	    )
	  }
	) %>%
	rbindlist()%>%
	rowwise() %>%
	mutate(var_ls_name = paste0(var_ls, collapse = "_")) %>%
	data.table()%>%
	.[, Method := factor(Method, levels = c("RF", "BRF", "CF_base"))] %>%
	.[, Model := factor(var_ls_name)] %>%
	.[, Model := case_when(
    	Model == "alpha_beta_ymax" ~ "aby",
    	Model == "alpha_beta_ymax_theta_1_theta_2" ~ "abytt",
    	Model == "alpha1_alpha2_beta1_beta2_ymax1_ymax2" ~ "aabbyy",
    	Model == "alpha1_alpha2_beta1_beta2_ymax1_ymax2_theta_1_theta_2" ~ "aabbyytt"
  	)] %>%
  	.[,!c("var_ls", "var_ls_name")]

# 3:45 - 

### === this includes the results on training data sets as well === ###
saveRDS(sim_results, "./Shared/Results_journal/SimRes_all.rds")
