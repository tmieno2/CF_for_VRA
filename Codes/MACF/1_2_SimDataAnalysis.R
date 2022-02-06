# ==========================================================================
# Preparation
# ==========================================================================
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
library(gridExtra)
library(measurements)

source(here("GitControlled", "Codes_MACF", "0_0_functions.R"))


# ==========================================================================
# Optimal N: RF, BRF, CF-stepwise, CF-base
# ==========================================================================

####==== source data ====#####
# simRes_honest_source <- readRDS(here("Shared", "Results", "SimRes_withHonesty_source.rds"))%>%
#     setnames(c("model","var_case"), c("Method", "Model")) %>%
#     .[, Model := factor(Model, levels = c("aby", "abytt", "aabbyy", "aabbyytt"))]%>%
#     .[, Method:=factor(Method, levels = c("RF", "BRF", "CF_stepwise", "CF_base"))]

simRes_honest_source <- readRDS(here("Shared", "Results_MACF", "SimRes_MACF_BRF_RF_100.rds"))%>%
    setnames(c("model","var_case"), c("Method", "Model")) %>%
    .[, Model := factor(Model, levels = c("aby", "abytt", "aabbyy", "aabbyytt"))]%>%
    .[, Method:=factor(Method, levels = c("RF", "BRF", "MACF"))]

pCorn <- price_table[2, pCorn]
pN <- price_table[2, pN]


rmse_general <-function(preds,actual){ 
  sqrt(mean((actual - preds)^2))
}


# ===================================
# RMSE and profit loss calculation
# ===================================
# (1) calculate profit loss relative to the maximum profit for each simulation
# (2) get the summary of profit loss for all the methods.


pi_loss_dt <- readRDS(here("Shared", "Data", "for_Simulations", "test_agg_data.rds")) %>%
    .[padding==1,.(sim, unique_cell_id, alpha, beta, ymax)]%>%
    setnames('unique_cell_id', "unique_subplot_id") %>%
    .[simRes_honest_source, on=c("sim", "unique_subplot_id")] %>%
    .[,`:=`(
        max_pi = pCorn*gen_yield_MB(ymax, alpha, beta, opt_N) - pN*opt_N,
        pi =  pCorn*gen_yield_MB(ymax, alpha, beta, opt_N_hat) - pN*opt_N_hat)] %>%
    .[, pi_loss := max_pi - pi]

# /*---------------------------------*/
#' ## (1) Summarize by simulation round
# /*---------------------------------*/
rmse_pi_loss_bySim <- pi_loss_dt[,.(
    rmse_optN = rmse_general(opt_N_hat, opt_N),
    pi_loss = mean(pi_loss)
    # as.list(summary(pi_loss))
    ), by= .(sim, Method, Model)]


#/*----------------------------------*/
#' ## Visualization
#/*----------------------------------*/
###=== Density plots: mean profit loss by simulations ===###
# pi_loss_bySim[Method %in% c("CF_stepwise", "CF_base")] %>%
rmse_pi_loss_bySim %>%
    ggplot() + 
    geom_density(aes(x=pi_loss, fill=Method))+
    scale_fill_viridis(discrete = TRUE, alpha=0.5) +
    theme_few()+
    facet_wrap(~Model, ncol=1)


# /*---------------------------------*/
#' ## (2) Summarize by methods and models
# /*---------------------------------*/
rmse_pi_loss_bySim %>%
    .[, .(
        mean_rmse_optN = mean(rmse_optN),
        mean_rmse_optN = mean(pi_loss)
        ), by=.(Method, Model)]

