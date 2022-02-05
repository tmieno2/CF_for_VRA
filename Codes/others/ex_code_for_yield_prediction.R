# ==========================================================================
#' Just for yield prediction with varioius N rates
# ==========================================================================


library(grf) # if you don't have one, "install.packages("grf")"
library(data.table) #if you don't have one, "install.packages("data.table")"
library(tidyverse) #if you don't have one, "install.packages("tidyverse")"
library(parallel) #if you don't have one, "install.packages("parallel")"



# === load training and testing data === # 
#' Please note that the data(for training and testing) we are using for CF, RF and BRF are slightly different from the data for CNN.
#' The data for CNN is more finer scale(cell-level), so your data should have more number of rows in the data.

#' The shared data contains the data for just first three simulation rounds(sim=1,2,3)

# Please change the path to the folder where you downloaded and saved them
train_data_all <- readRDS("~/OneDrive\ -\ University\ of\ Nebraska-Lincoln/ML_VRA/Data/CNN_demo/demo_train_dt.rds")
test_data_all <- readRDS("~/OneDrive\ -\ University\ of\ Nebraska-Lincoln/ML_VRA/Data/CNN_demo/demo_test_dt.rds")



#=== for example (the simplest modeling scenario)===#
var_case <- c("alpha", "beta", "ymax")



#=== demostration of yield prediction with BRF ===#
sim_res <- lapply(
	1:3, function(x){
	BRF_prediction(
		i = x,
		reg_data = train_data_all[sim==x & padding==1,], #<- padding==1 is just for our methods.
        test_data = test_data_all[sim==x & padding==1,], #<- padding==1 is just for our methods.
        var_ls = var_case
		)
	}
)%>%
rbindlist()





#=== this is the function of BRF for yield prediction ===#
BRF_prediction <- function(i, reg_data, test_data, var_ls) {
  print(paste0("working on ", i, " th iteration."))
  # For example,
  # i=1
  # reg_data = train_data_all[sim==1 & padding==1,]
  # test_data = test_data_all[sim==1 & padding==1,]
  # var_ls = c("alpha", "beta", "ymax")

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
  #' Yield prediction 
  # /*----------------------------------*/
  # Here, we specify the N range to be used yield prediction
  # -> This is based on the N rate used for that simulation round so it varies by simulation round
  N_levels <- test_data[,aa_n]%>%unique()   
  N_seq <- seq(min(N_levels), max(N_levels), by = 1)    

  #====  yield prediction ====#
  eval_data <- test_data[, c("unique_cell_id", "aa_n", var_ls), with = FALSE] %>% #<- this part is just substruct some necessary variables
    # Here, we add a sequence of N rates(N_seq) for each unit. 
    .[rep(1:nrow(.), each = length(N_seq)), ] %>% # we copy the data by each unit of observations
    .[, rate := rep(N_seq, nrow(.) / length(N_seq))] %>% # add a sequence of N rate used for prediction
    .[, yield_hat := predict(BRF_temp, newdata = .[, c("rate", var_ls), with = FALSE])] %>%
    .[, .(unique_cell_id, rate, yield_hat)] %>%
    .[, sim := i]

  return(eval_data)
}





