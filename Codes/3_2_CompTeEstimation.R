# /*=================================================*/
#' # Preparation
# /*=================================================*/
library(here)
library(ggplot2)
library(grf)
library(sf)
library(here)
library(data.table)
library(tidyverse)
library(ggthemes)


#/*----------------------------------*/
#' ## Preparation
#/*----------------------------------*/

#--- source functions ---#
source(here("GitControlled/Codes/0_3_functions_main_sim.R"))


#--- load the data ---#  
reg_data_all <- readRDS(here("Shared", "Data", "for_Simulations", "reg_data.rds"))
test_data_all <- readRDS(here("Shared", "Data", "for_Simulations", "test_agg_data.rds"))


#--- pick a single simulation round ---#
x = 1
reg_data_sample <- reg_data_all[sim==x & padding==1,]
test_data_sample <- test_data_all[sim==x & padding==1,]
N_levels <- reg_data_sample$rate%>%unique()%>%sort()


#--- all the cases to be considered ---#
te_var_ls_variations <- list(
    c("alpha1", "alpha2", "beta1", "beta2", "ymax1", "ymax2", "theta_1", "theta_2")
    )

te_case_data <- expand.grid(
    var_ls = te_var_ls_variations,
    Method = c("CF_base", "BRF", "RF")    
  ) %>%
  tibble()


# /*================================================================*/
#' # Treatment Effect Calculation (CF-base vs RF vs BRF)
# /*================================================================*/

# === set up for parallel computations === #
plan(multicore, workers = availableCores()-2)
options(future.globals.maxSize= 850*1024^2)
set.seed(1378)

te_dt_allML <- te_case_data %>%
	mutate(
		te_data = future_lapply(
			seq_len(nrow(.)),
			function(x) {
				 get_te_dt(
            test_data = test_data_sample,
            var_ls = .$var_ls[[x]],
            rates_ls = N_levels,
            Method = .$Method[[x]]
          )
        },
      future.seed = TRUE
    )
  )%>%
  unnest(., cols= "te_data")%>%
  data.table()%>%
  setnames("unique_cell_id", "unique_subplot_id") %>%
  .[, Method := factor(Method, levels = c("RF", "BRF", "CF_base"))] %>%
  .[,!c("var_ls")]



# /*================================================================*/
#' # Treatment Effect Comparison (CF-base vs RF vs BRF)
# /*================================================================*/
true_te_dt <- 
	test_data_sample %>%
	.[,.(sim, unique_cell_id, alpha, beta, ymax)]%>%
	.[rep(1:nrow(.), each = length(N_levels)), ] %>%
  .[, rate := rep(N_levels, nrow(.) / length(N_levels))] %>%
  .[, det_yield := gen_yield_MB(ymax, alpha, beta, rate)] %>%
	.[, yield_base := .SD[rate==min(rate), det_yield], by = .(unique_cell_id)] %>%
  .[, true_te_base := det_yield - yield_base] %>%
  .[, .(sim, unique_cell_id, rate, true_te_base)] %>%
  setnames("unique_cell_id", "unique_subplot_id")



te_comp_dt <- left_join(te_dt_allML, true_te_dt, by=c("unique_subplot_id", "rate"))%>%
	.[rate != N_levels[1],]%>%
	.[, Treatment := case_when(
		rate == N_levels[2] ~ "N1-N2",
		rate == N_levels[3] ~ "N1-N3",
		rate == N_levels[4] ~ "N1-N4",
		rate == N_levels[5] ~ "N1-N5"
	)]
	


saveRDS(te_comp_dt, here("Shared/Results_journal/dt_TEcomparison.rds"))



# /*================================================================*/
#' # Visualization
# /*================================================================*/

ggplot(te_comp_dt)+
	geom_point(aes(x=true_te_base, y=te_base), size=0.5)+
	facet_grid(Treatment ~ Method) +
	geom_abline(slope=1, intercept=0, color="red") +
	labs(y = "Estimated Treatment Effect")+
    labs(x = "True Treatment Effect")+
    theme_few()










