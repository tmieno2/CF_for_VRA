# ==========================================================================
# Prepare field data set for writing
# ==========================================================================
#' Objective:
#' + create field data sets by cells, subplots, and plots
library(here)
library(sf)
library(data.table)
library(ggplot2)
library(ggthemes)
library(viridis)
library(tidyverse)
library(ggpubr)


# === Load Functions === #
source(here("GitControlled/Codes/0_1_functions_gen_analysis_data.R"))

# === Prices === #
pCorn <- price_table[2, pCorn]
pN <- price_table[2, pN]

# /*=================================================*/
#' # Field Data Sets
# /*=================================================*/

#/*----------------------------------*/
#' ## (1) cell-level data set 
#/*----------------------------------*/
# + NOTE: I needed to do the following things, because the existing raw data does not have m_error
field <- readRDS(here("Shared/Data/for_Simulations/analysis_field.rds"))

coef_data <- readRDS(here("Shared/Data/for_Simulations/coefficients_sprange_400.rds"))

x=1
coef_data_m <- coef_data[sim == x, ]
coef_data_t <- coef_data[sim == ifelse(x + 1 >= max(sim), 1, x + 1), ]

sample_dt_cell <- 
  prepare_raw_data(
    x, field=field, coef_data_m=coef_data_m, coef_data_t=coef_data_t, app_error="no"
    )

sample_train_cell_dt <- sample_dt_cell$reg_raw_data
sample_test_cell_dt <- sample_dt_cell$test_raw_data

field_cell_sf <- 
  left_join(dplyr::select(field, unique_cell_id), sample_train_cell_dt, by="unique_cell_id")%>%
    na.omit() %>%
    mutate(plot_id = ceiling(subplot_id/4)) %>%
    filter(padding==1)
   
saveRDS(field_cell_sf, here("Shared/Results/for_writing/sample_field_cell_sf.rds"))


#/*----------------------------------*/
#' ## (2) subplot-level field without padding area 
#/*----------------------------------*/

field_subplot_sf <- 
  field_cell_sf %>%
  group_by(subplot_id, strip_id) %>%
  summarise(
    sim = mean(sim),
      yield = mean(yield),
      opt_N = mean(opt_N),
      rate = mean(rate),
      aa_n = mean(aa_n), 
      alpha = mean(alpha),
      beta = mean(beta),
      ymax = mean(ymax),
      alpha1 = mean(alpha1),
      alpha2 = mean(alpha2),
      beta1 = mean(beta1),
      beta2 = mean(beta2),
      ymax1 = mean(ymax1),
      ymax2 = mean(ymax2),
      theta_1 = mean(theta_1),
      theta_2 = mean(theta_2)
  ) %>%
  mutate(unique_subplot_id = paste0(strip_id,"_",subplot_id))%>%
  select(!c(strip_id, subplot_id))

saveRDS(field_subplot_sf, here("Shared/Results/for_writing/sample_field_subplot_sf.rds"))


field_subplot_test_dt <- 
  sample_test_cell_dt %>%
  .[padding==1,] %>%
  group_by(subplot_id, strip_id) %>%
  summarise(
    sim = mean(sim),
      yield = mean(yield),
      opt_N = mean(opt_N),
      rate = mean(rate),
      aa_n = mean(aa_n), 
      alpha = mean(alpha),
      beta = mean(beta),
      ymax = mean(ymax),
      m_error = mean(m_error),
      alpha1 = mean(alpha1),
      alpha2 = mean(alpha2),
      beta1 = mean(beta1),
      beta2 = mean(beta2),
      ymax1 = mean(ymax1),
      ymax2 = mean(ymax2),
      theta_1 = mean(theta_1),
      theta_2 = mean(theta_2)
  ) %>%
  mutate(unique_subplot_id = paste0(strip_id,"_",subplot_id))%>%
  dplyr::select(!c(strip_id, subplot_id))

saveRDS(field_subplot_test_dt, here("Shared/Results/for_writing/field_subplot_test_dt.rds"))

#/*----------------------------------*/
#' ## (3) plot-level dataset without padding area  
#/*----------------------------------*/
field_plot_sf <- field_cell_sf%>%
    group_by(plot_id, strip_id, rate)%>%
    summarise()

saveRDS(field_plot_sf, here("Shared/Results/for_writing/sample_field_plot_sf.rds"))



# /*===========================================*/
#'=  Prepare illustrative yield response curve estimated by RF =
# /*===========================================*/
library(grf)

# --- MB function --- #
gen_yield_MB <- function(ymax, alpha, beta, N) {
  yield <- ymax * (1 - exp(alpha + beta * N))
  return(yield)
}

# --- ML functions --- #
source(here("GitControlled/Codes/0_2_functions_main_sim.R"))

# --- Data --- #
train_dt <- 
  field_subplot_sf %>%
  data.table()

test_dt <- 
  field_subplot_test_dt %>%
  data.table()


# /*===== Train ML =====*/
temp_var_ls <- c("alpha", "beta", "ymax")

set.seed(1356)

RF <- 
  RF_run(
    reg_data = train_dt,
    var_ls = temp_var_ls
  )

BRF <- 
  BRF_run(
    reg_data = train_dt,
    var_ls = temp_var_ls
  )

# y_res_CNN <- 
#     list.files(
#         path = here("Shared/Results/CNN_rawRes_onEval"),
#         full.names=TRUE
#     ) %>%
#     lapply(., fread) %>%
#     rbindlist(.,idcol = "Model") %>%
#     .[sim==1&Model == 1, type := "test"]


N_rate <- test_dt[,aa_n] %>% unique() %>% sort()
tg_N_rate <- N_rate[c(2,4)]

# --- for true yield response function --- #
seq_N <- 
  seq(min(N_rate) -30, max(N_rate)+30, by=1)
# seq_N <- 
#   seq(min(N_rate), max(N_rate), by=1)

res_true <- 
  test_dt %>%
  .[aa_n %in% tg_N_rate] %>%
  .[rep(1:nrow(.), each = length(seq_N)), ] %>%
  .[, N := seq_N, by = unique_subplot_id] %>%
  .[,det_yield := gen_yield_MB(ymax, alpha, beta, N)] %>%
  .[,yield := det_yield*(1 + m_error)]

# --- for predicted yield at five experimental N rates --- #
res_pred <- 
  test_dt %>%
  .[aa_n %in% tg_N_rate] %>%
  .[rep(1:nrow(.), each = length(N_rate)),] %>%
  .[, N := N_rate, by = unique_subplot_id] %>%
  .[,pred_y_RF := predict(RF, newdata = .[, c("N", temp_var_ls), with = FALSE])] %>%
  .[,pred_y_BRF := predict(BRF, newdata = .[, c("N", temp_var_ls), with = FALSE])]

  
# === For visualization === #
set.seed(4098)
# set.seed(4957)
low <- 
  res_true[aa_n == tg_N_rate[1]] %>%
  get_rows_optN(data=., value = 0.1) %>%
  .[unique_subplot_id %in% sample(unique(unique_subplot_id), size = 1), unique_subplot_id] %>%
  unique()

high <- 
  res_true[aa_n == tg_N_rate[2]] %>%
  get_rows_optN(data=., value = 0.9) %>%
  .[unique_subplot_id %in% sample(unique(unique_subplot_id), size = 1), unique_subplot_id] %>%
  unique()

vis_res_true <- res_true[unique_subplot_id %in% c(low, high)]
vis_res_pred <- res_pred[unique_subplot_id %in% c(low, high)]

ggplot() +
  # --- true yield response curve --- #
  geom_line(
    data=vis_res_true, aes(x=N, y=yield, group=factor(unique_subplot_id))
  ) +
  # --- Predicted yield points --- #
  geom_point(
    data=vis_res_pred, aes(x=N, y=pred_y_BRF, group=factor(unique_subplot_id)), color="red"
  ) +
  geom_point(data = vis_res_true[aa_n==N], aes(x=aa_n, y= yield), color = 'blue') +
  geom_segment(data = vis_res_true[aa_n==N], aes(x = aa_n, xend=aa_n, y = yield, yend=-Inf), linetype = "dashed") +
  labs(y = "Yield (kg/ha)") +
  labs(x = "N (kg/ha)") +
  theme_dist +
  theme(legend.position = "none")




# /*===========================================*/
#'=  Preparation: True yield vs predicted yield =
# /*===========================================*/

#/*--------------------------------*/
#' ## Preparation
#/*--------------------------------*/
# === Prepare Actual Yield Data set === #
test_data_all <- 
  here("Shared/Data/for_Simulations/test_data.rds") %>%
  readRDS() %>%
  .[padding==1, .(sim, unique_subplot_id, alpha, beta, ymax, rate, yield)] %>%
  .[,det_yield := gen_yield_MB(ymax, alpha, beta, rate)]

# === Unique_subplot_id in a field === #
subplots_infiled <- test_data_all[,unique_subplot_id] %>% unique()


# === Load Forest Results === #
ls_res_forest <- 
    list.files(
        path = here("Shared/Results/Forest_rawRes"), 
        full.names = TRUE
    ) %>%
    gtools::mixedsort(decreasing = TRUE)

forest_simRes_test_raw <- 
    lapply(ls_res_forest, readRDS) %>%
    rbindlist(., idcol = "Model") %>%
    .[,Model := case_when(
        Model == 1 ~ "aby",
        Model == 2 ~ "abytt",
        Model == 3 ~ "aabbyy",
        Model == 4 ~ "aabbyytt"
        )] %>%
    .[, Model := factor(Model, levels = c("aby", "abytt", "aabbyy", "aabbyytt"))]%>%
    .[, Method := factor(Method, levels = c("RF", "BRF", "CF_base"))] %>%
    .[type == "test" & Method %in% c("RF", "BRF")]

forest_simRes_test <-
  test_data_all %>%
  forest_simRes_test_raw[, on = c("sim", "unique_subplot_id"), nomatch = NULL] %>%
  .[, .(sim, Model, Method, unique_subplot_id, pred_yield, yield, det_yield)]  



# === Load CNN Results === #
cnn_simRes_test_raw <- 
    list.files(
        path = here("Shared/Results/CNN_rawRes_onEval"),
        full.names=TRUE
    ) %>%
    lapply(., fread) %>%
    rbindlist(.,idcol = "Model") %>%
    .[,Model := case_when(
        Model == 1 ~ "aby",
        Model == 2 ~ "abytt",
        Model == 3 ~ "aabbyy",
        Model == 4 ~ "aabbyytt"
    )] %>%
    .[, Model:=factor(Model, levels = c("aby", "abytt", "aabbyy", "aabbyytt"))] %>%
    setnames("pred", "pred_yield") %>%
    # --- creation of unique_subplot_id--- #    
    .[,c("subplot_id", "strip_id") := tstrsplit(id, "_", fixed=TRUE)] %>%
    .[,unique_subplot_id := paste0(strip_id,"_",subplot_id)] %>%
    .[unique_subplot_id %in% subplots_infiled,] %>%
    .[,Method := "CNN"] %>%


# --- select rows matching with test data --- #
# For each subplot in cnn_simRes_test_raw data, five predicted yields per each N experiment rates in that round is contained.
# So, we want to select only rows where N rate matches with N rate in testing data.
cnn_simRes_test <-
  test_data_all %>%
  cnn_simRes_test_raw[, on = c("sim", "unique_subplot_id", "rate"), nomatch = NULL] %>%
  .[, .(sim, Model, Method, unique_subplot_id, pred_yield, yield, det_yield)]  


# === Combine Forest results and CNN results === #
simRes_test_y <- rbind(forest_simRes_test, cnn_simRes_test) %>%
  .[, rmse_y := rmse_general(pred_yield, yield), by= .(sim, Method, Model) ]

# saveRDS(simRes_test_y, here("Shared/Results/for_writing/simRes_test_y.rds"))

#/*--------------------------------*/
#' ## Visualization
#/*--------------------------------*/
x <- 100

sample_simRes_test_y <-
  simRes_test_y %>%
  .[sim==x]

saveRDS(sample_simRes_test_y, here("Shared/Results/for_writing/sample_simRes_test_y.rds"))

# rmse <- sample_simRes_test_y %>%
#   .[, .(
#     rmse_y_det = rmse_general(pred_yield, det_yield),
#     rmse_y = rmse_general(pred_yield, yield)
#     ), by= .(Method, Model)]

# # === Function for RMSE Calculation === #
# rmse_general <-function(preds,actual){ 
#   sqrt(mean((actual - preds)^2)) %>%
#   round(.,1) %>%
#   format(, nsmall = 1)

# }


# rmse <- sample_simRes_test_y %>%
#   .[, .(
#     rmse_y_det = paste0("RMSE = " ,rmse_general(pred_yield, det_yield)),
#     rmse_y = paste0("RMSE = ", rmse_general(pred_yield, yield))
#     ), by= .(Method, Model)]


# ggplot(sample_simRes_test_y) +
#   geom_point(aes(x=det_yield, y=pred_yield), size = 0.5) +
#   geom_abline(aes(intercept = 0, slope = 1), color = "red", show.legend = TRUE) +
#   geom_text(data=rmse, aes(x=-Inf,y=+Inf,label=rmse_y_det),
#               hjust = -0.1,  vjust = 3, size=3.5, family = "Times New Roman") +
#   facet_grid(Model ~ Method) +
#   guides(
#     fill = guide_legend(keywidth = 1, keyheight = 1),
#     linetype = guide_legend(keywidth = 3, keyheight = 1),
#     colour = guide_legend(keywidth = 3, keyheight = 1)
#   ) +
#   labs(y = "Predicted Yield (kg/ha)") +
#   labs(x = "True Yield without error (kg/ha)") +
#   theme_dist



