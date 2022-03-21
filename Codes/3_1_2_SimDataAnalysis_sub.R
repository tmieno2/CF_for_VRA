# ==========================================================================
# Objective:
# For simulation results derived from low and high error datasets, 
#' + Calculate profit-deficits and RMSE of EONR 
#' + Calculate RMSE of yield predictions for RF, BRF and CNN
# ==========================================================================

#/*----------------------------------*/
#' ## Preparation 
#/*----------------------------------*/
library(here)
library(data.table)
library(RColorBrewer)
library(patchwork)
library(ggplot2)
library(ggthemes)
library(viridis)
library(hrbrthemes)
library(tidyverse)
library(ggpubr)
library(here)

# === Source Functions === #
source(here("GitControlled/Codes/0_1_functions_gen_analysis_data.R"))
source(here("GitControlled/Codes/0_2_functions_main_sim.R"))

# === Prices ===#
pCorn <- price_table[2, pCorn]
pN <- price_table[2, pN]


# === Function for RMSE Calculation === #
rmse_general <-function(preds,actual){ 
  sqrt(mean((actual - preds)^2))
}



# ==========================================================================
# Forest simulation regression results
# ==========================================================================


# /*===========================================*/
#'=  (1) Low error datasets =
# /*===========================================*/

# === Load Training and Testing Data Sets for Evaluation === #
reg_data_low <- readRDS(here("Shared/Data/for_Simulations/reg_data_low_error.rds"))
test_data_low <- readRDS(here("Shared/Data/for_Simulations/test_data_low_error.rds"))

source_dt_low <- 
    bind_rows(reg_data_low, test_data_low, .id = "type") %>%
    .[padding==1, .(sim, type, unique_subplot_id, alpha, beta, ymax, rate, yield, opt_N)] %>%
    .[,type := case_when(type == "1" ~ "train", type == "2" ~ "test")]


#/*--------------------------------*/
#' ## Load and organize the Forest results
#/*--------------------------------*/

# === Load Forest Results === #
ls_res_forest_low <- 
    list.files(
        path = here("Shared/Results/Forest_rawRes_low"), 
        full.names = TRUE
    ) %>%
    gtools::mixedsort(decreasing = TRUE)


forest_low_simRes_all <- 
    lapply(ls_res_forest_low, readRDS) %>%
    rbindlist(., idcol = "Model") %>%
    .[,Model := case_when(
        Model == 1 ~ "aby",
        Model == 2 ~ "abytt",
        Model == 3 ~ "aabbyy",
        Model == 4 ~ "aabbyytt"
        )] %>%
    .[, Model := factor(Model, levels = c("aby", "abytt", "aabbyy", "aabbyytt"))]%>%
    .[, Method := factor(Method, levels = c("RF", "BRF", "CF_base"))] %>%
    .[, .(type, sim, Model, Method, unique_subplot_id, pred_yield, opt_N_hat, yield, opt_N)]


#/*--------------------------------*/
#' ## RMSE of EONRs and Pi-loss calculation
#/*--------------------------------*/
forest_optN_piLoss_low <- 
    source_dt_low[, !c("rate", "yield", "opt_N")] %>%
    forest_low_simRes_all[, on=c("sim", "type", "unique_subplot_id")] %>%
    .[,`:=`(
        max_pi = pCorn*gen_yield_MB(ymax, alpha, beta, opt_N) - pN*opt_N,
        pi =  pCorn*gen_yield_MB(ymax, alpha, beta, opt_N_hat) - pN*opt_N_hat)] %>%
    .[, pi_loss := max_pi - pi]

# === Summarize by Simulation Round === #
forest_summary_bySim_low <- 
    forest_optN_piLoss_low %>%
    .[,.(
        rmse_optN = rmse_general(opt_N_hat, opt_N),
        pi_loss = mean(pi_loss),
        rmse_y = rmse_general(pred_yield, yield)
    ), by= .(sim, type, Method, Model)]

# saveRDS(forest_summary_bySim_low, here("Shared/Results/for_writing/forest_summry_bySim_low.rds"))

# === Summarize by Method and Model === #
forest_summary_bySim_low %>%
    .[, .(
        mean_rmse_optN = mean(rmse_optN),
        mean_pi_loss = mean(pi_loss),
        mean_rmse_y = mean(rmse_y)
        ), by=.(type, Method, Model)] %>%
    .[order(type, Method)]

forest_summary_bySim_low %>%
  .[type == "test" & Method %in% c("RF", "BRF")] %>%
  dcast(sim + Model ~ Method, value.var = c("pi_loss", "rmse_y"))






# /*===========================================*/
#'=  (2) High error dataset =
# /*===========================================*/

# === Load Training and Testing Data Sets for Evaluation === #
reg_data_high <- readRDS(here("Shared/Data/for_Simulations/reg_data_high_error.rds"))
test_data_high <- readRDS(here("Shared/Data/for_Simulations/test_data_high_error.rds"))

source_dt_high <- 
    bind_rows(reg_data_high, test_data_high, .id = "type") %>%
    .[padding==1, .(sim, type, unique_subplot_id, alpha, beta, ymax, rate, yield, opt_N)] %>%
    .[,type := case_when(type == "1" ~ "train", type == "2" ~ "test")]

#/*--------------------------------*/
#' ## Load and organize the Forest results
#/*--------------------------------*/

# === Load Forest Results === #
ls_res_forest_high <- 
    list.files(
        path = here("Shared/Results/Forest_rawRes_high"), 
        full.names = TRUE
    ) %>%
    gtools::mixedsort(decreasing = TRUE)


forest_high_simRes_all <- 
    lapply(ls_res_forest_high, readRDS) %>%
    rbindlist(., idcol = "Model") %>%
    .[,Model := case_when(
        Model == 1 ~ "aby",
        Model == 2 ~ "abytt",
        Model == 3 ~ "aabbyy",
        Model == 4 ~ "aabbyytt"
        )] %>%
    .[, Model := factor(Model, levels = c("aby", "abytt", "aabbyy", "aabbyytt"))]%>%
    .[, Method := factor(Method, levels = c("RF", "BRF", "CF_base"))] %>%
    .[, .(type, sim, Model, Method, unique_subplot_id, pred_yield, opt_N_hat, yield, opt_N)]


#/*--------------------------------*/
#' ## RMSE of EONRs and Pi-loss calculation
#/*--------------------------------*/
forest_optN_piLoss_high <- 
    source_dt_high[, !c("rate", "yield", "opt_N")] %>%
    forest_high_simRes_all[, on=c("sim", "type", "unique_subplot_id")] %>%
    .[,`:=`(
        max_pi = pCorn*gen_yield_MB(ymax, alpha, beta, opt_N) - pN*opt_N,
        pi =  pCorn*gen_yield_MB(ymax, alpha, beta, opt_N_hat) - pN*opt_N_hat)] %>%
    .[, pi_loss := max_pi - pi]

# === Summarize by Simulation Round === #
forest_summary_bySim_high <- 
    forest_optN_piLoss_high %>%
    .[,.(
        rmse_optN = rmse_general(opt_N_hat, opt_N),
        pi_loss = mean(pi_loss),
        rmse_y = rmse_general(pred_yield, yield)
    ), by= .(sim, type, Method, Model)]

saveRDS(forest_summary_bySim_high, here("Shared/Results/for_writing/forest_summry_bySim_high.rds"))

# === Summarize by Method and Model === #
forest_summary_bySim_high %>%
    .[, .(
        mean_rmse_optN = mean(rmse_optN),
        mean_pi_loss = mean(pi_loss),
        mean_rmse_y = mean(rmse_y)
        ), by=.(type, Method, Model)] %>%
    .[order(type, Method)]






















