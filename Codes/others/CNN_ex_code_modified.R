# ==========================================================================
#' This is just an example code  for demonstration purposes
# ==========================================================================


library(grf) # if you don't have one, "install.packages("grf")"
library(data.table) #if you don't have one, "install.packages("data.table")"
library(tidyverse) #if you don't have one, "install.packages("tidyverse")"
library(parallel) #if you don't have one, "install.packages("parallel")"



# ==========================================================================
# Load sample training and testing data
# ==========================================================================
#' Please note that the data(for training and testing) we are using for CF, RF and BRF are slightly different from the data for CNN.
#' The data for CNN is more finer scale(cell-level), so your data should have more number of rows in the data.

#' The shared data contains the data for just first three simulation rounds(sim=1,2,3)

# === load training and testing data === # 
# Please change the path to the folder where you downloaded and saved them
train_data_all <- readRDS("~/OneDrive\ -\ University\ of\ Nebraska-Lincoln/ML_VRA/Data/CNN_demo/demo_train_dt.rds")
test_data_all <- readRDS("~/OneDrive\ -\ University\ of\ Nebraska-Lincoln/ML_VRA/Data/CNN_demo/demo_test_dt.rds")



# === prices ===#
pCorn <- 0.138
pN <- 1.323


# /*====================================================================================*/
#' Simulation example
#' This is using two functions(sim_par() and BRF_analysis()) which are defined in the below
#' So, please load them first, then run the below code
#' 
#' The idea is that we just reapeat the same process(training the model and testing)
#' 
# /*====================================================================================*/
sim_results <- lapply(
    1:2, function(x) {
      sim_par(
        i = x,
        reg_data = train_data_all[sim==x & padding==1,], #<- padding==1 is just for our methods.
        test_data = test_data_all[sim==x & padding==1,]
      )
    }
  ) %>%
rbindlist()%>%
  rowwise() %>%
  mutate(var_ls_name = paste0(var_ls, collapse = "_")) %>%
  data.table()%>%
  .[, var_case := factor(var_ls_name)] %>%
  .[, .(sim, var_case, r2_opt_N, r2_y)] %>%
  .[, var_case := case_when(
      var_case == "alpha_beta_ymax" ~ "aby",
      var_case == "alpha_beta_ymax_theta_1_theta_2" ~ "abytt",
      var_case == "alpha1_alpha2_beta1_beta2_ymax1_ymax2" ~ "aabbyy",
      var_case == "alpha1_alpha2_beta1_beta2_ymax1_ymax2_theta_1_theta_2" ~ "aabbyytt"
    )]






# /*=====================================================*/
#' BRF analysis function for various model configurations
#' "BRF_analysis()" function is used in this function
# /*=====================================================*/
sim_par <- function(i, reg_data, test_data) {
  # for example (for 1st iteration)
  # i=1
  # reg_data = train_data_all[sim==1 & padding==1,]
  # test_data = test_data_all[sim==1 & padding==1,]

  print(paste0("working on ", i, " th iteration."))


  ## == all the combinations of variables ==##
  var_ls_variations <- tibble(
    var_ls = list(
    c("alpha", "beta", "ymax"),
    c("alpha", "beta", "ymax", "theta_1", "theta_2")
    # c("alpha1", "alpha2", "beta1", "beta2", "ymax1", "ymax2"),
    # c("alpha1", "alpha2", "beta1", "beta2", "ymax1", "ymax2", "theta_1", "theta_2")
    )
  )


  ## == BRF analysis ==##
  results_data <- var_ls_variations %>%
    mutate(
      opt_N_data = mclapply(
        seq_len(nrow(.)),
        function(x) {
          ### === run BRF (with BRF_analysis() function) ===###
          BRF_analysis(
            reg_data = reg_data,
            test_data = test_data,
            var_ls = .$var_ls[[x]]
          )
        },
        mc.cores = detectCores() - 2
      )
    )%>%
    rowwise()%>%
    mutate(
      r2_opt_N = summary(lm(opt_N ~ opt_N_hat, data = opt_N_data))$r.squared,
      r2_y = summary(lm(yield ~ pred_yield, data = opt_N_data))$r.squared
    ) %>%
    data.table() %>%
    .[,opt_N_data := NULL] %>%
    .[, sim := i]

  return(results_data)

}


##For example (just for 1st iteration)
# demo <- sim_par(i=1, 
#   reg_data = train_data_all[sim==1 & padding==1,],
#   test_data = test_data_all[sim==1 & padding==1,])



# /*===================================================================*/
#' Function for BRF modeling and prediction (the core function)
#' 
#' (Note: I think this part is especially similar to what you are doing)
# /*===================================================================*/

BRF_analysis <- function(reg_data, test_data, var_ls) {
  # For example,  ()
  # reg_data <- train_data
  # test_data <- test_data
  # var_ls <- c( "alpha", "beta", "ymax")

  # /*----------------------------------*/
  #' Training Boosted random forest
  # /*----------------------------------*/
  X <- reg_data[, c("aa_n",var_ls), with = FALSE] 
  Y <- reg_data[, yield]

  BRF_temp <- boosted_regression_forest(
      X = X,
      Y = Y,
      num.trees = 2000, 
      min.node.size = 10
    )


  # /*----------------------------------*/
  #' Yield prediction and EONR estimation
  # /*----------------------------------*/
  # Here, we specify the N range to be used for opt_N estimation.
  # -> This is based on the N rate used for that simulation round so it varies by simulation round
  N_levels <- test_data[,aa_n]%>%unique()   
  N_seq <- seq(min(N_levels), max(N_levels), by = 1)    
  

  eval_data <- test_data[, c("unique_cell_id", "aa_n", var_ls, "opt_N", "yield"), with = FALSE] %>% #<- this part is just substruct some necessary variables
    #==== yield prediction on test data using the BRF ====#
    .[, pred_yield := predict(BRF_temp, newdata = .[, c("aa_n", var_ls), with = FALSE])]%>%

    #====  EONR estimation with test data ====#
    # Here, we add a sequence of N rates(N_seq) for each unit. 
    .[rep(1:nrow(.), each = length(N_seq)), ] %>%
    .[, rate := rep(N_seq, nrow(.) / length(N_seq))] %>% # we name the newly added  N_seq coloumn as "rate"
    .[, yield_hat := predict(BRF_temp, newdata = .[, c("rate", var_ls), with = FALSE])] %>%
    #-- profit calculation --#
    .[, pi_hat := pCorn * yield_hat - pN * rate]%>%
    #-- find profit-maximizing N rate by each unit --#
    .[, .SD[pi_hat == max(pi_hat), ], by =  unique_cell_id] %>%
    .[, .(unique_cell_id, rate, pred_yield, opt_N, yield)] %>%
      setnames("rate", "opt_N_hat")

  return(eval_data)

}


eval_data$opt_N_hat%>%hist()







