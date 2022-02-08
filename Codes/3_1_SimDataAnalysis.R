# ==========================================================================
# Objective:
#' + Calculate profit-deficits and RMSE of EONR 
#' + Calculate RMSE of yield predictions for RF, BRF and CNN
#'      + For CNN results, estimate  EONR in this codes
#' + Finally, put together those results into one data set
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

source(here("GitControlled/Codes/0_1_functions_gen_analysis_data.R"))
source(here("GitControlled/Codes/0_2_functions_main_sim.R"))

# === prices ===#
pCorn <- price_table[2, pCorn]
pN <- price_table[2, pN]


# === Load Training and Testing data sets for evaluation === #
reg_data_all <- readRDS("./Shared/Data/for_Simulations/reg_data.rds")
test_data_all <- readRDS("./Shared/Data/for_Simulations/test_data.rds")

source_dt <- 
    bind_rows(reg_data_all, test_data_all, .id = "type") %>%
    .[padding==1, .(sim, type, unique_subplot_id, alpha, beta, ymax)] %>%
    .[,type := case_when(type == "1" ~ "train", type == "2" ~ "test")]

subplots_infiled <- source_dt[,unique_subplot_id] %>% unique()

# ==========================================================================
# Results of RF, BRF and CF
# ==========================================================================

# === All Results === #
forest_simRes_all <- 
    readRDS(here("Shared/Results_journal/SimRes_all.rds")) %>%
    .[, Model := factor(Model, levels = c("aby", "abytt", "aabbyy", "aabbyytt"))]%>%
    .[, Method := factor(Method, levels = c("RF", "BRF", "CF_base"))] 

# === function for RMSE calculation === #
rmse_general <-function(preds,actual){ 
  sqrt(mean((actual - preds)^2))
}


# /*=================================================*/
#' # RMSE of EONRs and Profit Loss calculation
# /*=================================================*/
pi_loss_dt <- source_dt %>%
    forest_simRes_all[, on=c("sim", "type", "unique_subplot_id")] %>%
    .[,`:=`(
        max_pi = pCorn*gen_yield_MB(ymax, alpha, beta, opt_N) - pN*opt_N,
        pi =  pCorn*gen_yield_MB(ymax, alpha, beta, opt_N_hat) - pN*opt_N_hat)] %>%
    .[, pi_loss := max_pi - pi]


# /*---------------------------------*/
#' ## (1) Summarize by simulation round
# /*---------------------------------*/
rmse_pi_loss_bySim <- 
    pi_loss_dt %>%
    .[,.(
        rmse_optN = rmse_general(opt_N_hat, opt_N),
        pi_loss = mean(pi_loss),
        rmse_y = rmse_general(pred_yield, yield)
    ), by= .(sim, type, Method, Model)]


# /*---------------------------------*/
#' ## (2) Summarize by methods and models
# /*---------------------------------*/
rmse_pi_loss_bySim %>%
    .[, .(
        mean_rmse_optN = mean(rmse_optN),
        mean_pi_loss = mean(pi_loss),
        mean_rmse_y = mean(rmse_y)
        ), by=.(type, Method, Model)] %>%
    .[order(type)]





# ==========================================================================
# Results of CNN
# ==========================================================================

# /*=================================================*/
#' # Load and organize CNN results
# /*=================================================*/

# === Load CNN results === #
# --- CNN results on the evaluation data sets --- #
ls_res_cnn_onTrain <- 
    list.files(
        path = here("Shared/Results/CNN_rawRes_onTrain"), 
        pattern = "0_*.",
        full.names = TRUE
    )

res_cnn_onTrain <- 
    lapply(ls_res_cnn_onTrain, fread) %>%
    rbindlist(idcol = "Model") %>%
    .[,Model := case_when(
        Model == 1 ~ "aby",
        Model == 2 ~ "abytt",
        Model == 3 ~ "aabbyy",
        Model == 4 ~ "aabbyytt"
        )] %>%
    .[, type := "train"]


# --- CNN results on the evaluation data sets --- #
ls_res_cnn_onEval <- 
    list.files(
        path = here("Shared/Results/CNN_rawRes_onEval"),
        pattern="0_*.",
        full.names=TRUE
    )

res_cnn_onEval <- 
    lapply(ls_res_cnn_onEval, fread) %>%
    rbindlist(idcol = "Model") %>%
    .[,Model := case_when(
        Model == 1 ~ "aby",
        Model == 2 ~ "abytt",
        Model == 3 ~ "aabbyy",
        Model == 4 ~ "aabbyytt"
        )] %>%
    .[, type := "test"]


# === Combine to one data set=== #
cnn_simRes_all <- rbind(res_cnn_onTrain, res_cnn_onEval) %>%
  .[, c("subplot_id", "strip_id") := tstrsplit(id, "_", fixed=TRUE)] %>%
  .[,unique_subplot_id := paste0(strip_id,"_",subplot_id)]%>%
  .[unique_subplot_id %in% subplots_infiled,]%>%
  setnames("pred", "yield_hat")%>%
  .[,.(sim, unique_subplot_id, rate, yield_hat, Model)]%>%
  .[, Model:=factor(Model, levels = c("aby", "abytt", "aabbyy", "aabbyytt"))]%>%
  .[, Method:="CNN"]



# /*----------------------------------*/
#' ## Evaluate optN estimation
# /*----------------------------------*/
###=== get slope of the yield response function ===###
# slope_v1 <- copy(cnn_simRes_all)%>%
#   .[, .(slope = coef(lm(yield_hat~rate))["rate"]), by= .(id, sim, var_case)]

# all_var_case <- c("aby", "abytt", "aabbyy", "aabbyytt")

# cal_slope <- function(case){
#     # case="aby"
#     demo <- copy(cnn_simRes_all)%>%
#     .[var_case==case, ]%>%
#     .[, .(slope = coef(lm(yield_hat~rate))["rate"]), by= .(unique_subplot_id, sim, var_case)]%>%
#     .[order(sim)]

# }

# slope_all <- mclapply(all_var_case, cal_slope, mc.cores=detectCores()-2)%>%
#     rbindlist()

# saveRDS(slope_all, here("CNN_Results", "cnn_response_slope_all.rds"))å

slope_all <- readRDS(here("Shared", "Results", "cnn_response_slope_all.rds"))

pCorn <- price_table[2, pCorn]
pN <- price_table[2, pN]

pN_pC_ratio <- pN/pCorn


CNN_opt_N_eval <- copy(slope_all)%>%
    setnames(c("id", "var_case"), c("unique_subplot_id", "Model")) %>%
    cnn_simRes_all[.,on=c("sim", "unique_subplot_id", "Model")]%>%
    .[, opt_N_hat :=
        lapply(.SD, function(x) ifelse(slope<pN_pC_ratio, min(rate), max(rate)))
            , by=.(unique_subplot_id, sim, Model)]%>%
    .[rate==opt_N_hat,]%>%
    .[test_agg_dt[,.(sim, unique_subplot_id, alpha, beta, ymax, opt_N)], on=c("sim", "unique_subplot_id")]


CNN_pi_loss_dt <- CNN_opt_N_eval %>%
    .[,`:=`(
        max_pi = pCorn*gen_yield_MB(ymax, alpha, beta, opt_N) - pN*opt_N,
        pi =  pCorn*gen_yield_MB(ymax, alpha, beta, opt_N_hat) - pN*opt_N_hat)] %>%
    .[, pi_loss := max_pi - pi]



subplot_area <- conv_unit(60*60, "ft2", "hectare")

CNN_rmse_pi_loss_bySim <- CNN_pi_loss_dt[,c(
    rmse_optN = rmse_general(opt_N_hat, opt_N),
    as.list(summary(pi_loss)),
    sd_pi_loss = sd(pi_loss),
    cumulative_pi_loss = sum(pi_loss*subplot_area)
    ), by= .(sim, Method, Model)]



# /*----------------------------------*/
#' ## Evaluate yield prediction
# /*----------------------------------*/
CNN_rmse_y <- test_agg_dt[,.(unique_subplot_id, sim, yield, rate)]%>%
    cnn_simRes_all[., on = c("sim", "unique_subplot_id","rate")]%>%
    .[,.(
        # r2_y = summary(lm(yield ~ yield_hat))$r.squared,
        rmse_y = rmse_general(yield_hat, yield)
        ), by=.(sim, Model, Method)]


report_CNN <- CNN_rmse_y[CNN_rmse_pi_loss_bySim, on=c("sim", "Model", "Method")]

saveRDS(report_CNN, here("Shared", "Results", "SimRes_CNN_RMSE.rds"))


r2_CNN%>%
  .[,.(
    rmse_y = mean(rmse_y),
    rmse_optN =mean(rmse_optN)
    ), by = .(Model, Method)]



























