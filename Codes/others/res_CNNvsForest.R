# /*=================================================*/
#' # Preparation
# /*=================================================*/

library(here)
library(ggplot2)
library(grf)
library(sf)
library(data.table)
library(tidyverse)

#--- source functions ---#
source(here("Codes", "cnn_related_functions.R")) 

#--- field ---#
# field_cnn <- readRDS(here("Data", "CNN_Simulations", "cnn_field_padding.rds"))

# === prices ===#
pCorn <- price_table[2, pCorn]
pN <- price_table[2, pN]


#---training dataset ---#
# raw_data <- readRDS(here("Results","cnn_raw_data_1 (1).rds"))%>%
# 	.[,unique_cell_id := paste0(strip_id,"_",subplot_id)] %>% 
#   .[padding==1,]

reg_data <- readRDS(here("Results","cnn_reg_data_1 (1).rds"))%>%
  .[,unique_cell_id := paste0(strip_id,"_",subplot_id)]


reg_data_forest <- copy(reg_data) %>% 
  .[padding==1,]

# reg_data_forest$rate %>% unique() %>% sort()


#---training dataset ---#
test_raw_data <- readRDS(here("Results","cnn_raw_data_2 (1).rds"))


test_agg_data <- readRDS(here("Results","cnn_reg_data_2 (1).rds"))%>%
	.[,unique_cell_id := paste0(strip_id,"_",subplot_id)] %>%
  .[, `:=`(
    strip_id = NULL,
    subplot_id = NULL
  )]

test_agg_data$rate %>% unique()


# "cnn_reg_data_2 (1).rds" is the same as this one
test_agg_data_opt_N <- test_raw_data[, .(
    yield = mean(yield),
    opt_N = mean(opt_N),
    rate = mean(rate),
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
    theta_2 = mean(theta_2),
    padding = mean(padding),
    X = mean(X),
    Y = mean(Y)
  ), by = .(subplot_id, strip_id)]%>%
  .[,unique_cell_id:=paste0(strip_id,"_",subplot_id)]%>%
  .[, `:=`(
    strip_id = NULL,
    subplot_id = NULL
  )]


##-- this is the testing dataset for CF, RF, BRF --##
test_agg_data_forest <- copy(test_agg_data_opt_N) %>% 
  .[padding==1,]





# ==========================================================================
# 1. CNN : subplot basis yield prediction
# ==========================================================================


CNN_source <- read_csv(here("Results", "cnn.csv"))%>%
  data.table()%>%
  .[,unique_cell_id := paste0(strip_id,"_",subplot_id)]%>% 
  melt(id.vars = "unique_cell_id",
         measure.vars =names(.)[4:29])%>%
  .[,rate:= gsub("rate", "", variable)%>%as.numeric()]%>%
  setnames("value", "yield_hat") %>% 
  .[,variable:=NULL]


CNN_opt_N <- copy(CNN_source)  %>%
	.[, pi_hat := pCorn * yield_hat - pN * rate]%>%
	.[, .SD[pi_hat == max(pi_hat), ], by = unique_cell_id] %>%
    .[, .(unique_cell_id, rate)] %>%
    setnames("rate", "opt_N_hat")%>%
    .[, method := "CNN"]%>%
    .[test_agg_data_opt_N[,.(unique_cell_id, opt_N)], on="unique_cell_id"]



CNN_source$rate %>% unique() %>% sort()

test_agg_data$rate %>% unique() %>% sort()

CNN_source <- read_csv(here("Results", "cnn.csv"))%>%
  data.table()%>%
  .[,unique_cell_id := paste0(strip_id,"_",subplot_id)]%>% 
  melt(id.vars = "unique_cell_id",
       measure.vars =names(.)[4:29])%>%
  .[,rate:= gsub("rate", "", variable)%>%as.numeric()]%>%
  setnames("value", "yield_hat") %>% 
  .[,variable:=NULL]

CNN_yield_pred <- CNN_source[rate %in% c(91, 121, 151, 186, 216)] %>% 
  .[,rate := case_when(
    rate==91 ~ 87,
    rate==121 ~ 120,
    rate==151 ~ 153,
    rate==186 ~ 186,
    rate==216 ~ 219
  ) ] %>% 
  .[test_agg_data, on=c("unique_cell_id", "rate")] %>% 
  .[,summary(lm(yield ~ yield_hat))$r.squared]
  
CNN_yield_pred

ggplot(CNN_yield_pred)+
  geom_point(aes(x=yield, y=yield_hat))+
  geom_abline(slope = 1, intercept = 0, color="red")+
  coord_equal()






  



# unique_y_rate_91_216 <- read_csv(here("Results", "cnn.csv"))%>%
#   data.table()%>%
#   .[,unique_cell_id := paste0(strip_id,"_",subplot_id)]%>% 
#   .[,.(unique_cell_id, rate91, rate216)]
# 
# ggplot(unique_y_rate_91_216)+geom_point(aes(x=rate91, y=rate216))
# 
# subplot5_4 <- unique_y_rate_91_216[unique_cell_id=="5_4",]
# subplot30_54 <- unique_y_rate_91_216[unique_cell_id=="30_53",]
# 
# 
# 
# 
# sample_cell_id <- c("1_4", "5_4", "30_53")
# sample <- unique_y_rate_91_216[unique_cell_id%in% sample_cell_id]
# 
# ggplot(sample)+geom_point(aes(x=rate91, y=rate216, color=factor(unique_cell_id)))
# 
# 
# sample_cell_id <- sample(CNN_source$unique_cell_id%>%unique(),20,replace = FALSE)
# sample <- unique_y_rate_91_216[unique_cell_id%in% sample_cell_id]
# ggplot(sample)+geom_point(aes(x=rate91, y=rate216, color=factor(unique_cell_id)))
# 
# rate_91_216[unique_cell_id=="1_4", ]







CNN_opt_N[unique_cell_id=="1_4"]%>%
	ggplot(.)+
	geom_point(aes(x=rate, y=yield_hat))+
	ggtitle("subplot 1_4")

CNN_opt_N[unique_cell_id=="5_4"]%>%
	ggplot(.)+
	geom_point(aes(x=rate, y=yield_hat))+
	ggtitle("subplot 5_4")


CNN_opt_N[unique_cell_id=="30_53"]%>%
	ggplot(.)+
	geom_point(aes(x=rate, y=yield_hat))+
	ggtitle("subplot 30_53")





# ==========================================================================
# 2. CF, BRF and RF
# ==========================================================================
source(here("Codes", "functions.R"))


BRF_analysis_5 <- function(data_test, brf_results, var_ls, N_levels) {
  N_seq <- seq(min(N_levels), max(N_levels), by = 5)

  eval_data <- data_test[, c("unique_cell_id", var_ls), with = FALSE] %>%
    .[rep(1:nrow(.), each = length(N_seq)), ] %>%
    .[, rate := rep(N_seq, nrow(.) / length(N_seq))] %>%
    .[, yield_hat := predict(brf_results, newdata = .[, c("rate", var_ls), with = FALSE])] %>%
    .[, pi_hat := pCorn * yield_hat - pN * rate]

  return(eval_data)
}



RF_analysis_5 <- function(data_test, rf_results, var_ls, N_levels) {
  # N_levels is determined by training data
  N_seq <- seq(min(N_levels), max(N_levels), by = 5)

  eval_data <- data_test[, c("unique_cell_id", var_ls), with = FALSE] %>%
    .[rep(1:nrow(.), each = length(N_seq)), ] %>%
    .[, rate := rep(N_seq, nrow(.) / length(N_seq))] %>%
    .[, yield_hat := predict(rf_results, newdata = .[, c("rate", var_ls), with = FALSE])] %>%
    .[, pi_hat := pCorn * yield_hat - pN * rate]

  return(eval_data)
}

rates_ls <- reg_data[,rate]%>%unique()%>%sort()

var_ls <- c("alpha", "beta", "ymax")






#'-----------------------------
#' CF_stepwise
#'-----------------------------

CF_stepwise_opt_N <- CF_analysis(reg_data=reg_data_forest, var_ls=var_ls, cl_var=NA) %>%
      get_pi_dif(
        data = test_agg_data_forest,
        cf_results = .,
        var_ls = var_ls,
        rates_ls = rates_ls
      ) %>%
      .[,
        .SD[pi_change == max(pi_change), ], by = unique_cell_id
      ] %>%
      .[, .(unique_cell_id, N)] %>%
      setnames("N", "opt_N_hat") %>%
      .[, method := "CF_stepwise"] %>%
      .[test_agg_data_forest[,.(unique_cell_id, opt_N)], on="unique_cell_id"]

#'-----------------------------
#' CF_base
#'-----------------------------

CF_base_opt_N <- CF_analysis_base(reg_data=reg_data_forest, var_ls=var_ls, cl_var=NA) %>%
      get_pi_dif_base(
        data = test_agg_data_forest,
        cf_results = .,
        var_ls = var_ls,
        rates_ls = rates_ls
      ) %>%
      .[,
        .SD[pi_change == max(pi_change), ], by = unique_cell_id
      ] %>%
      .[, .(unique_cell_id, N)] %>%
      setnames("N", "opt_N_hat") %>%
      .[, method := "CF_base"] %>%
      .[test_agg_data_forest[,.(unique_cell_id, opt_N)], on="unique_cell_id"]




#'-----------------------------
#' BRF
#'-----------------------------
BRF <- BRF_run(reg_data = reg_data_forest, var_ls = var_ls, cl_id = NA)

#' yield prediction (test = test_agg_data)
#' 
BRF_yield <- test_agg_data_forest[, c("unique_cell_id", var_ls, "rate", "yield"), with = FALSE] %>%
.[, yield_hat := predict(BRF, newdata = .[, c("rate", var_ls), with = FALSE])]

summary(lm(yield ~ yield_hat, data=BRF_yield))$r.squared #! This is BRF result



#' opt N estimation
BRF_opt_N <-BRF_analysis_5(
        data_test = test_agg_data_forest,
        brf_results = BRF,
        var_ls = var_ls,
        N_levels = rates_ls 
      ) %>%
      .[, .SD[pi_hat == max(pi_hat), ], by = unique_cell_id] %>%
      .[, .(unique_cell_id, rate)] %>%
      setnames("rate", "opt_N_hat") %>%
      .[, method := "BRF"] %>%
      .[test_agg_data_forest[,.(unique_cell_id, opt_N)], on="unique_cell_id"]


summary(lm(opt_N ~ opt_N_hat, data=BRF_opt_N))$r.squared #! This is BRF result


#'-----------------------------
#' RF
#'-----------------------------
RF <- RF_run(reg_data = reg_data_forest, var_ls = var_ls, cl_id = NA)

#' yield prediction
RF_yield <- test_agg_data_forest[, c("unique_cell_id", var_ls, "rate", "yield"), with = FALSE] %>%
  .[, yield_hat := predict(RF, newdata = .[, c("rate", var_ls), with = FALSE])]

summary(lm(yield ~ yield_hat, data=RF_yield))$r.squared #! This is RF result



#' opt N estimation
RF_opt_N <-RF_analysis_5(
        data_test = test_agg_data_forest,
        rf_results = RF,
        var_ls = var_ls,
        N_levels = rates_ls 
      ) %>% 
    .[, .SD[pi_hat == max(pi_hat), ], by = unique_cell_id] %>%
    .[, .(unique_cell_id, rate)] %>%
    setnames("rate", "opt_N_hat") %>%
    .[, method := "RF"] %>%
    .[test_agg_data_forest[,.(unique_cell_id, opt_N)], on="unique_cell_id"]

summary(lm(opt_N ~ opt_N_hat, data=RF_opt_N))$r.squared #! This is RF result


RF_opt_N[unique_cell_id=="1_4"]%>%
	ggplot(.)+
	geom_point(aes(x=rate, y=yield_hat))



# ==========================================================================
# Total
# ==========================================================================
summary(lm(opt_N ~ opt_N_hat, data=CNN_opt_N))$r.squared
summary(lm(opt_N ~ opt_N_hat, data=RF_opt_N))$r.squared
summary(lm(opt_N ~ opt_N_hat, data=BRF_opt_N))$r.squared
summary(lm(opt_N ~ opt_N_hat, data=CF_stepwise_opt_N))$r.squared
summary(lm(opt_N ~ opt_N_hat, data=CF_base_opt_N))$r.squared


total <- rbind(
  CNN_opt_N,
  CF_stepwise_opt_N, 
  CF_base_opt_N, 
  BRF_opt_N, 
  RF_opt_N)%>%
	.[,summary(lm(opt_N ~ opt_N_hat))$r.squared, by=method]
      

























