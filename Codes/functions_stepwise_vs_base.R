
# ==========================================================================
#' Gaol: I want to chech the performacne of treatement effect estimation 
#' between CF-stepwise and CF-base.
#' 
#'  
# ==========================================================================

prepare_data_step_base <- function(field, coef_data_m, coef_data_t) {
  # x=1
  # field=field; coef_data_m=coef_data[sim == x, ]; coef_data_t=coef_data[sim == ifelse(x + 1 >= max(sim), 1, x + 1), ]
  
  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### Data preparation
  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  # === merge field data with the coefs data ===#
  data <- coef_data_m[data.table(field), on = "unique_cell_id"] %>%
    .[, opt_N := (log(pN) - log(-pCorn * ymax * beta) - alpha) / beta]

  test_data <- coef_data_t[data.table(field), on = "unique_cell_id"] %>%
    .[, opt_N := (log(pN) - log(-pCorn * ymax * beta) - alpha) / beta]

  # ggplot(st_as_sf(data)) +
  #   geom_sf(aes(fill = cluster_id_1), size = 0) +
  #   scale_fill_viridis_c()

  # === check the spatial distribution of optimal N ===#
  # left_join(field,data[,.(unique_cell_id,opt_N)],by='unique_cell_id') %>%
  #   select(opt_N) %>%
  #   tm_shape() +
  #     tm_fill(col = "opt_N")

  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### Assign N (5 treatment levels)
  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  # === define experimental N rate ===#
  all_opt_N <- data[, opt_N]

  # --- for training ---#
  # hist(all_opt_N)
  N_levels <- seq(
    quantile(all_opt_N, prob = 0.05) - 20,
    quantile(all_opt_N, prob = 0.95) + 20,
    length = 5 
  ) %>%
    round()

  # data <- assign_rates(st_as_sf(data), N_levels, pattern = "sequential") %>%
  data <- assign_rates(st_as_sf(data), N_levels) %>% # N_levels are assigned as "rate" variable
    data.table()
    # .[, as_app_N:=rate*(1+rnorm(nrow(.))/5)] #<- (5/25) introduce application error 20%


  test_data <- assign_rates(st_as_sf(test_data), N_levels) %>%
    data.table()
    # .[, as_app_N:=rate*(1+rnorm(nrow(.))/5)] #<- (5/25) introduce application error 20%


  # ggplot(st_as_sf(data)) +
  #   geom_sf(aes(fill = factor(rate)), size = 0) +
  #   scale_fill_viridis_d()

  # ggplot(st_as_sf(data)) +
  #    geom_sf(aes(fill = factor(plot_id)))

  # ggplot(st_as_sf(data))+geom_sf(aes(fill=m_error),color = NA)+
  #   scale_fill_gradient(low = "yellow", high = "red")+
  #   ggtitle("m_error")

  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### Generate yield
  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  data %<>% .[, det_yield := gen_yield_MB(ymax, ate_hat_steplpha, beta, rate)] %>% #(5/25)changed from rate to as_app_N
    # .[,det_yield:=b0+b1*N+b2*N^2] %>%
    # === error ===#
    .[, yield := det_yield * (1 + m_error)] %>%
    # === keep the relevant vars ===#
    .[, .(  #(5/25)include  as_app_N
      unique_cell_id, opt_N, rate, yield, m_error, alpha, beta, ymax, alpha1, alpha2, beta1, beta2, ymax1, ymax2,
      theta_1, theta_2, subplot_id, strip_id, cluster_id_1, cluster_id_2, X, Y
    )]

  test_data %<>% .[, det_yield := gen_yield_MB(ymax, alpha, beta, rate)] %>% #(5/25)changed from rate to as_app_N
    # .[,det_yield:=b0+b1*N+b2*N^2] %>%
    # === error ===#
    .[, yield := det_yield * (1 + m_error)] %>%
    # === keep the relevant vars ===#
    .[, .(  #(5/25)include  as_app_N
      unique_cell_id, opt_N, rate, yield, m_error, alpha, beta, ymax, alpha1, alpha2, beta1, beta2, ymax1, ymax2,
      theta_1, theta_2, subplot_id, strip_id, cluster_id_1, cluster_id_2, X, Y
    )] 
    # %>%
    # .[sample(1:nrow(.), 1000), ]

  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### Aggregate data by analysis unit (subplot_id-strip_id)
  # /*~~~~~~~~~~~~~~~~~~~~~~*/

  # === by analysis unit ===#
  reg_data <- data[, .(
    yield = mean(yield),
    rate = mean(rate),
    # as_app_N = mean(as_app_N), #<- (5/25) newly added
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
    cluster_id_1 = mean(cluster_id_1),
    cluster_id_2 = mean(cluster_id_2)
  ), by = .(subplot_id, strip_id)]

  return(
    list(
      reg_data = reg_data,
      test_data = test_data,
      rates_ls = N_levels
    )
  )
}


# /*================================================================*/
#' # Run CF
# /*================================================================*/
# x=1
# data <- reg_data[sim==x&padding==1,]
# N_levels <- reg_data[sim==x,]$rate%>%unique()%>%sort()
# rates <- N_levels[1:2]
# var_ls <- c("ymax", "alpha", "beta")
# cl_id <- NA

## NOTE: CF_run() is the same for 1-2,2-3..TE estimation and 1-2, 1-3..TE estimation

CF_run <- function(data, rates, var_ls, cl_id = NA) {
  ## data; data for training the model
  ## rates: Two kinds of treatment levels
  ## var_ls: the names of covariates(predictor)


  # data=reg_data ;rates=rates_ls[c( 1, i+1 )];var_ls=var_ls;cl_id=NA
  # === treatment assignment ===#
  data_temp_dt <- data %>%
    .[rate %in% rates, ] %>%
    .[, trt := ifelse(rate == rates[1], 0, 1)]

  # === causal forest analysis ===#
  X <- data_temp_dt[, var_ls, with = FALSE]
  Y <- data_temp_dt[, yield]
  W <- data_temp_dt[, trt]

  if (!is.na(cl_id)) {
    cl <- data_temp_dt[[cl_id]]
  }

  # === preliminary runs ===#
  Y_forest <- regression_forest(X, Y)
  Y_hat <- predict(Y_forest)$predictions

  W_forest <- regression_forest(X, W)
  W_hat <- predict(W_forest)$predictions

  # #=== raw forest ===#
  # tau_forest_raw <- causal_forest(X, Y, W, Y.hat=Y_hat,W.hat=W_hat)
  # var_imp <- variable_importance(tau_forest_raw)
  # var_imp > mean(var_imp)

  # === causal forest analysis ===#
  if (!is.na(cl_id)) {
    #--- if cl_id is present ---#
    tau_forest_temp <- causal_forest(X, Y, W,
      Y.hat = Y_hat,
      W.hat = W_hat,
      clusters = cl,
      num.trees = 2000,
      honesty = FALSE,
      min.node.size = 10
      # sample.fraction = 0.4,
      # tune.parameters=''
      # tune.parameters='all'
      # tune.parameters=c("sample.fraction", "mtry", "honesty.fraction", "honesty.prune.leaves", "alpha", "imbalance.penalty")
    )
  } else {
    #--- if cl_id is NOT present ---#
    tau_forest_temp <- causal_forest(X, Y, W,
      Y.hat = Y_hat,
      W.hat = W_hat,
      num.trees = 2000,
      honesty = FALSE,
      min.node.size = 10
      # sample.fraction = 0.4,
      # min.node.size=10,
      # tune.parameters='all'
      # tune.parameters=c("sample.fraction", "mtry", "honesty.fraction", "honesty.prune.leaves", "alpha", "imbalance.penalty")
    )
  }

  return(tau_forest_temp)
}

# temp_cf <- CF_run(data=data, rates=rates, var_ls=var_ls, cl_id = NA)
# temp_cf$predictions
# temp <- get_opt_N(
#   reg_data,
#   var_ls = c("alpha", "beta", "ymax"),
#   cl_var = NA,
#   test_data = test_data,
#   rates_ls = N_levels,
#   model = "BRF"
# )


# /**==================================================**/
#' 1-2, 2-3, 3-4, 4-5, 5-6 CF treatment effect estimation ("stepwise")
# /**==================================================**/

CF_analysis <- function(reg_data, var_ls, cl_var = NA) {
  # reg_data=data; var_ls=c("alpha", "beta","ymax"); cl_var=NA
  rates_ls <- reg_data[, rate] %>%
    unique() %>%
    sort()
  exp_len <- length(rates_ls) - 1

  CF_all_run <- function(i) {
    tau_forest_temp <- CF_run(
      data = reg_data,
      rates = rates_ls[i:(i + 1)],
      var_ls = var_ls,
      cl_id = cl_var
    )

    return(tau_forest_temp)
  }

  all_results <- lapply(1:exp_len, CF_all_run)

  return(all_results)
}

# ex
# all_results <- CF_analysis(reg_data=reg_data, var_ls=var_ls, cl_var = NA)

get_pi_dif_ind <- function(data, cf_results, var_ls, rates_ls) {
  # data=test_data; cf_results=all_results; var_ls=c("alpha", "beta","ymax"); rates_ls=N_levels
  pi_dif_data <- lapply(
    1:(length(cf_results)),
    function(x) {
      get_changes_gradual_ind(
        N_index = x,
        data_base = data,
        var_ls = var_ls,
        rates_ls = rates_ls,
        cf_results = cf_results
      )
    }
  ) %>%
    rbindlist() %>%
    .[, pi_change := pCorn * yield_dif - pN * N_plus]

  return(pi_dif_data)
}

# temp_pi_diff <- get_pi_dif_ind(data=test_data, cf_results=all_results, var_ls=var_ls, rates_ls=rates_ls)

# temp_pi_diff[unique_cell_id=='250_20',]%>%.[pi_change>0, max(N)]


get_changes_gradual_ind <- function(N_index, data_base, var_ls, rates_ls, cf_results) {
	# N_index= 1; data_base= test_data; var_ls= var_ls; rates_ls= rates_ls; cf_results=all_results
  # /*----------------------------------*/
  #' ## Increase N
  # /*----------------------------------*/

  data_eval_base <- data_base %>%
      .[, var_ls, with = FALSE]

  tau_data <- predict(cf_results[[N_index]], newdata = data_eval_base, estimate.variance = FALSE)$predictions

  return_data <- data.table(
    unique_cell_id = data_base$unique_cell_id,
    yield_dif = tau_data, # yield diff shows the treatment effects
    N_plus = rates_ls[N_index+1] - rates_ls[N_index],
    N_index = paste0(N_index,'-',N_index+1),
    N = rates_ls[N_index+1]
  )
  return(return_data)
}



get_ture_pi_dif_ind <- function(data, cf_results, var_ls, rates_ls) {
  # data=test_data; var_ls=c("alpha", "beta","ymax"); rates_ls=N_levels
  pi_dif_data_true <- lapply(
    1:(length(cf_results)),
    function(x) {
      get_true_changes_gradual_ind(
        N_index = x,
        data_base = data,
        var_ls = var_ls,
        rates_ls = rates_ls
      )
    }
  ) %>%
    rbindlist() %>%
    .[, ture_pi_change := pCorn * ture_yield_dif - pN * N_plus]

  return(pi_dif_data_true)
}

# temp_pi_diff_true <- get_ture_pi_dif_ind(data=test_data, cf_results=all_results, var_ls=var_ls, rates_ls=rates_ls)

# temp_pi_diff_true[unique_cell_id=='250_20',]%>%.[ture_pi_change>0, max(N)]

# # 1-2
# plot(temp_pi_diff[N_index=='1-2',yield_dif], temp_pi_diff_true[N_index=='1-2',ture_yield_dif])


get_true_changes_gradual_ind <- function(N_index, data_base, var_ls, rates_ls) {
	# N_index= 1; data_base= test_data; var_ls= var_ls; rates_ls= rates_ls; cf_results=all_results
  # /*----------------------------------*/
  #' ## Increase N
  # /*----------------------------------*/
	#for each unique_cell_id, generate 5 yield with 5 N rates
	N_con <- rates_ls[N_index]
	N_tre <- rates_ls[N_index+1]

    ture_te_dt <- copy(data_base) %>%
      .[,.(unique_cell_id,alpha,beta,ymax)]%>%
      .[, ture_yield_diff:=gen_yield_MB(ymax, alpha, beta, N_tre)-gen_yield_MB(ymax, alpha, beta, N_con)]

    return_data <- data.table(
    	unique_cell_id = ture_te_dt[,unique_cell_id],
    	ture_yield_dif = ture_te_dt[,ture_yield_diff], # yield diff shows the treatment effects
    	N_plus = N_tre - N_con,
    	N_index = paste0(N_index,'-',N_index+1),
    	N = rates_ls[N_index+1]
  	)

  return(return_data)
}

# try <- get_true_changes_gradual_ind(N_index= 1, data_base= test_data, var_ls= var_ls, rates_ls= rates_ls)

# return_data[unique_cell_id=='250_20',]











# /**==================================================**/
#' 1-2, 1-3, 1-4, 1-5 CF treatment effect estimation ("base")
# /**==================================================**/
CF_analysis_base <- function(reg_data, var_ls, cl_var = NA) {
  rates_ls <- reg_data[, rate] %>%
    unique() %>%
    sort()
  exp_len <- length(rates_ls) - 1

  CF_all_run <- function(i) {
    tau_forest_temp <- CF_run(
      data = reg_data,
      rates = rates_ls[c(1, i + 1)], # <- this part is different from CF_analysis
      var_ls = var_ls,
      cl_id = cl_var
    )

    return(tau_forest_temp)
  }

  all_results <- lapply(1:exp_len, CF_all_run)

  return(all_results)
}

# CF_base <- CF_analysis_base(reg_data, var_ls, cl_var = NA)


get_pi_dif_base <- function(data, cf_results, var_ls, rates_ls) {
  # data=data_test; cf_results=cf_results_base ; var_ls=var_ls; rates_ls=N_levels
  pi_dif_data <- lapply(
    1:(length(cf_results)),
    function(x) {
      get_changes_gradual_base(
        N_index=x,
        data_base = data,
        var_ls = var_ls,
        rates_ls = rates_ls,
        cf_results = cf_results
      )
    }
  ) %>%
    rbindlist() %>%
    .[, pi_change := pCorn * yield_dif - pN * N_plus]

  return(pi_dif_data)
}

# pi_dif_data[unique_cell_id=='1_3']

get_changes_gradual_base <- function(N_index, data_base, var_ls, rates_ls, cf_results) {
  # data_base=data_test; var_ls=var_ls; rates_ls=N_levels; cf_results=cf_results_base
  # /*----------------------------------*/
  #' ## Increase N
  # /*----------------------------------*/
  # N_index=4
 
    data_eval_base <- data_base %>%
      .[, var_ls, with = FALSE]

    tau_data <- predict(cf_results[[N_index]], newdata = data_eval_base, estimate.variance = FALSE)$predictions

  return_data <- data.table(
    unique_cell_id = data_base$unique_cell_id,
    yield_dif = tau_data, # yield diff shows the treatment effects
    N_index= paste0(1,'-',N_index+1),
    N_plus = rates_ls[N_index+1] - rates_ls[1],
    N = rates_ls[N_index]
  )
  return(return_data)
}



get_ture_pi_dif_ind_base <- function(data, cf_results, var_ls, rates_ls) {
  # data=test_data; cf_results=CF_base; var_ls=c("alpha", "beta","ymax"); rates_ls=N_levels
  pi_dif_data_true <- lapply(
    1:(length(cf_results)),
    function(x) {
      get_true_changes_gradual_ind_base(
        N_index = x,
        data_base = data,
        var_ls = var_ls,
        rates_ls = rates_ls
      )
    }
  ) %>%
    rbindlist() %>%
    .[, ture_pi_change := pCorn * ture_yield_dif - pN * N_plus]

  return(pi_dif_data_true)
}

# temp_pi_diff_true_base <- get_ture_pi_dif_ind_base(data=test_data, cf_results=cf_results, var_ls=var_ls, rates_ls=rates_ls)

# temp_pi_diff_base <- get_pi_dif_base(data=test_data, cf_results=cf_results, var_ls=var_ls, rates_ls=rates_ls)

# 1-2
# plot(temp_pi_diff_base[N_index=='1-2',yield_dif], temp_pi_diff_true_base[N_index=='1-2',ture_yield_dif])


get_true_changes_gradual_ind_base <- function(N_index, data_base, var_ls, rates_ls) {
	# N_index= 1; data_base= test_data; var_ls= var_ls; rates_ls= rates_ls; cf_results=all_results
  # /*----------------------------------*/
  #' ## Increase N
  # /*----------------------------------*/
	#for each unique_cell_id, generate 5 yield with 5 N rates
	N_con <- rates_ls[1]
	N_tre <- rates_ls[N_index+1]

    ture_te_dt <- copy(data_base) %>%
      .[,.(unique_cell_id,alpha,beta,ymax)]%>%
      .[, ture_yield_diff:=gen_yield_MB(ymax, alpha, beta, N_tre)-gen_yield_MB(ymax, alpha, beta, N_con)]

    # plot(density(ture_te_dt[,ture_yield_diff]))

    return_data <- data.table(
    	unique_cell_id = ture_te_dt[,unique_cell_id],
    	ture_yield_dif = ture_te_dt[,ture_yield_diff], # yield diff shows the treatment effects
    	N_plus = N_tre - N_con,
    	N_index = paste0(1,'-',N_index+1),
    	N = rates_ls[N_index+1]
  	)

  return(return_data)
}

# try <- get_true_changes_gradual_ind(N_index= 1, data_base= test_data, var_ls= var_ls, rates_ls= rates_ls)

# return_data[unique_cell_id=='250_20',]




get_ture_pi_dif_ind <- function(data, cf_results, var_ls, rates_ls) {
  # data=test_data; var_ls=c("alpha", "beta","ymax"); rates_ls=N_levels
  pi_dif_data_true <- lapply(
    1:(length(cf_results)),
    function(x) {
      get_true_changes_gradual_ind(
        N_index = x,
        data_base = data,
        var_ls = var_ls,
        rates_ls = rates_ls
      )
    }
  ) %>%
    rbindlist() %>%
    .[, ture_pi_change := pCorn * ture_yield_dif - pN * N_plus]

  return(pi_dif_data_true)
}

# temp_pi_diff_true <- get_ture_pi_dif_ind(data=test_data, cf_results=all_results, var_ls=var_ls, rates_ls=rates_ls)

# temp_pi_diff_true[unique_cell_id=='250_20',]%>%.[ture_pi_change>0, max(N)]

# # 1-2
# plot(temp_pi_diff[N_index=='1-2',yield_dif], temp_pi_diff_true[N_index=='1-2',ture_yield_dif])


get_true_changes_gradual_ind <- function(N_index, data_base, var_ls, rates_ls) {
	# N_index= 1; data_base= test_data; var_ls= var_ls; rates_ls= rates_ls; cf_results=all_results
  # /*----------------------------------*/
  #' ## Increase N
  # /*----------------------------------*/
	#for each unique_cell_id, generate 5 yield with 5 N rates
	N_con <- rates_ls[N_index]
	N_tre <- rates_ls[N_index+1]

    ture_te_dt <- copy(data_base) %>%
      .[,.(unique_cell_id,alpha,beta,ymax)]%>%
      .[, ture_yield_diff:=gen_yield_MB(ymax, alpha, beta, N_tre)-gen_yield_MB(ymax, alpha, beta, N_con)]

    # plot(density(ture_te_dt[,ture_yield_diff]))
    
    return_data <- data.table(
    	unique_cell_id = ture_te_dt[,unique_cell_id],
    	ture_yield_dif = ture_te_dt[,ture_yield_diff], # yield diff shows the treatment effects
    	N_plus = N_tre - N_con,
    	N_index = paste0(N_index,'-',N_index+1),
    	N = rates_ls[N_index+1]
  	)

  return(return_data)
}






# ==========================================================================
# Treatment effect Evaluation
# ==========================================================================
estimate_tre <- function(x, reg_data_all, test_data_all){
  # x=3
  # field=field
  # coef_data_m=coef_data[sim == x, ]
  # coef_data_t=coef_data[sim == ifelse(x + 1 >= max(sim), 1, x + 1), ]
 
  # datasets <- prepare_data(field, coef_data_m, coef_data_t)

  reg_data <- reg_data_all[sim==x&padding==1,]
  test_data <- test_data_all[sim==x&padding==1,]
  N_levels <- reg_data$rate%>%unique()%>%sort()

  var_ls <- c("alpha","beta","ymax")

  ##== CF-stepwiese ==##
  CF_step <- CF_analysis(reg_data=reg_data, var_ls=var_ls, cl_var = NA)

  te_hat_step <- get_pi_dif_ind(data=test_data, cf_results=CF_step, var_ls=var_ls, rates_ls=N_levels)

  te_ture_step <- get_ture_pi_dif_ind(data=test_data, cf_results=CF_step, var_ls=var_ls, rates_ls=N_levels)

  te_step <- te_hat_step[,.(unique_cell_id,yield_dif,N_index,pi_change)]%>%
    te_ture_step[.,on=c("unique_cell_id", "N_index")]%>%
    .[,method:="CF-stepwise"]


  ##== CF-base ==##
  CF_base <- CF_analysis_base(reg_data=reg_data, var_ls=var_ls, cl_var = NA)

  te_hat_base <- get_pi_dif_base(data=test_data, cf_results=CF_base, var_ls=var_ls, rates_ls=N_levels)

  te_ture_base <- get_ture_pi_dif_ind_base(data=test_data, cf_results=CF_base, var_ls=var_ls, rates_ls=N_levels)

  te_base <- te_hat_base[,.(unique_cell_id,yield_dif,N_index,pi_change)]%>%
    te_ture_base[.,on=c("unique_cell_id", "N_index")]%>%
    .[,method:="CF-base"]

  eval <- rbind(te_step,te_base)

  step <- eval[method=="CF-stepwise"]%>%
  .[,.(N, ture_yield_dif,yield_dif)]%>%
  melt(id.vars= "N", measure.vars=c("ture_yield_dif","yield_dif"))

  base <- eval[method=="CF-base"]%>%
    .[,.(N, ture_yield_dif,yield_dif)]%>%
    melt(id.vars= "N", measure.vars=c("ture_yield_dif","yield_dif"))



eval2 <- eval[,.(N, N_index, method, ture_yield_dif,yield_dif)]%>%
  setnames(names(.),c("N", "N_index", "Method", "True Treatment Effects", "Estimated Treatment Effects"))%>%
  melt(id.vars= c("N", "N_index", "Method"), measure.vars=c("True Treatment Effects", "Estimated Treatment Effects"))%>%
  .[,N_index:=case_when(
    N_index == "1-2" ~ "N1-N2",
      N_index == "2-3" ~ "N2-N3",
      N_index == "3-4" ~ "N3-N4",
      N_index == "4-5" ~ "N4-N5",
      N_index == "1-3" ~ "N1-N3",
      N_index == "1-4" ~ "N1-N4",
      N_index == "1-5" ~ "N1-N5"
  )]

  r2 <- eval[,summary(lm(ture_yield_dif ~ yield_dif, data = .SD))$r.squared, by=.(method, N_index)]%>%
    setnames(names(.)[3], "R_squared")%>%
    .[,sim:=x]

  return(list(r2=r2, output=eval2))

}