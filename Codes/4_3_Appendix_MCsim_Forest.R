# /*===== Run on R GUI =====*/

#/*----------------------------------*/
#' ## Preparation
#/*----------------------------------*/
library(grf)
library(data.table)
library(tidyverse)
library(future.apply)

# === Source Functions === #
setwd("~/Dropbox/ResearchProject/CF_for_VRA")
source("./GitControlled/Codes/0_2_functions_main_sim.R")

# /*----------------------------------*/
#' ## Datasets
# /*----------------------------------*/
# === prices ===#
pCorn <- price_table[2, pCorn]
pN <- price_table[2, pN]


# /*=================================================*/
#' # Simulation by scenarios
# /*=================================================*/

# === set up for parallel computations === #
plan(multicore, workers = availableCores()-2)
options(future.globals.maxSize= 850*1024^2)

# --- modeling scenario --- #
var_ls_variations <- list(
    c("alpha", "beta", "ymax"),
    c("alpha", "beta", "ymax", "theta_1", "theta_2"),
    c("alpha1", "alpha2", "beta1", "beta2", "ymax1", "ymax2"),
    c("alpha1", "alpha2", "beta1", "beta2", "ymax1", "ymax2", "theta_1", "theta_2")
  )

# --- Number of iterations --- #
B=100

#/*----------------------------------------------------*/
#' ## (1) Simulations for small degree of yield error
#/*----------------------------------------------------*/
# --- Data --- #
reg_data_low <- readRDS("./Shared/Data/for_Simulations/reg_data_low_error.rds")
test_data_low <- readRDS("./Shared/Data/for_Simulations/test_data_low_error.rds")

# === start simulations === #
for (var in var_ls_variations){
	# var = c("alpha", "beta", "ymax")
	set.seed(1378)
	# --- for each modeling scenario run: --- #
	sim_results <- lapply(1:B, 
		function(x){
			sim_par(
				i = x,
				var_ls = var,
				reg_data = reg_data_low[sim==x&padding==1,],
	      		test_data = test_data_low[sim==x&padding==1,],
	      		N_levels = reg_data_low[sim==x,]$rate%>%unique()%>%sort()
	      	)
		}
	) %>%
	rbindlist()

	saveRDS(sim_results, paste0("./Shared/Results/Forest_rawRes_low/forest_low_SimRes_", paste0(var, collapse = "_"), ".rds"))
}


#/*----------------------------------------------------*/
#' ## (2) Simulations for high degree of yield error
#/*----------------------------------------------------*/
# --- Data --- #
reg_data_high <- readRDS("./Shared/Data/for_Simulations/reg_data_high_error.rds")
test_data_high <- readRDS("./Shared/Data/for_Simulations/test_data_high_error.rds")

# === start simulations === #
for (var in var_ls_variations){
	# var = c("alpha", "beta", "ymax")
	set.seed(1378)
	# --- for each modeling scenario run: --- #
	sim_results <- lapply(1:B, 
		function(x){
			sim_par(
				i = x,
				var_ls = var,
				reg_data = reg_data_high[sim==x&padding==1,],
	      		test_data = test_data_high[sim==x&padding==1,],
	      		N_levels = reg_data_high[sim==x,]$rate%>%unique()%>%sort()
	      	)
		}
	) %>%
	rbindlist()

	saveRDS(sim_results, paste0("./Shared/Results/Forest_rawRes_high/forest_high_SimRes_", paste0(var, collapse = "_"), ".rds"))
}


