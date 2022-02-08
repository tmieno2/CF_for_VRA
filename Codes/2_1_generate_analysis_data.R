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
source(here("GitControlled/Codes/0_1_functions_gen_analysis_data.R"))

# /*----------------------------------*/
#' ## Datasets
# /*----------------------------------*/
coef_data <- readRDS(here("Shared/Data/for_Simulations/coefficients_sprange_400.rds")))
field <- readRDS(here("/Shared/Data/for_Simulations/field_padding.rds"))

# === prices ===#
pCorn <- price_table[2, pCorn]
pN <- price_table[2, pN]



# /*=================================================*/
#' # Generate raw dataset (cell-level)
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

# saveRDS(raw_data, here("Shared/Data/for_Simulations/raw_reg_test_data.rds"))
reg_raw_data <- sapply(raw_data,"[",1)%>%rbindlist()
test_raw_data <- sapply(raw_data,"[",1)%>%rbindlist()

# saveRDS(reg_raw_data, here("Shared/Data/for_Simulations/reg_raw_data.rds"))
# saveRDS(test_raw_data, here("Shared/Data/for_Simulations/test_raw_data.rds"))

# /*=================================================*/
#' # Generate analysis dataset (subplot-level)
# /*=================================================*/
reg_raw_data <- readRDS(here("Shared/Data/for_Simulations/reg_raw_data.rds"))
test_raw_data <- readRDS(here("Shared/Data/for_Simulations/test_raw_data.rds"))

##== Aggregate Cell-Level Field Data to Subplot-Level Field Data ==##
sim_data <- lapply(
	1:1000, function(x) {
		# print(paste0("working on ", x, " th iteration."))
		prepare_data_for_sim(
			reg_raw_data = reg_raw_data[sim==x,],
			test_raw_data = test_raw_data[sim==x,]
		)
	}
)

reg_data <- sapply(sim_data,"[",1)%>%rbindlist()
test_data <- sapply(sim_data,"[",2)%>%rbindlist()

saveRDS(reg_data, here("Shared/Data/for_Simulations/reg_data.rds"))
saveRDS(test_data, here("Shared/Data/for_Simulations/test_data.rds"))




