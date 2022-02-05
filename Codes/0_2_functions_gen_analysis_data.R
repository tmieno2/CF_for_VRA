# /*=================================================*/
#' # Assign rates to the trial design data
# /*=================================================*/
# data_sf <- st_as_sf(data)
# rates_ls <- N_levels


assign_rates <- function(data_sf, rates_ls, pattern = "fixed-latin-square", merge = TRUE) {
  gen_sequence <- function(length) {
    if (length %% 2 == 0) {
      seq_r <- c(seq(1, length, by = 2), seq(length, 2, by = -2))
    } else {
      seq_r <- c(seq(1, length, by = 2), seq(length - 1, 2, by = -2))
    }
    return(seq_r)
  }

  gen_rd_seq <- function(seq_element, num) {
    for (i in 1:num) {
      if (i == 1) {
        seq_return <- seq_element
      } else {
        if (runif(1) < 0.5) {
          # seq_return <- c(seq_return, rev(seq_element))
          seq_return <- c(seq_return, seq_element)
        } else {
          seq_return <- c(seq_return, seq_element)
        }
      }
    }
    return(seq_return)
  }

  get_seq_for_strip <- function(pattern, rate_ranks_seq, num_seq, exclude_ls = NULL) {
    seq_possible <- gen_rd_seq(rate_ranks_seq, num_seq)

    position_ls <- 1:rates_len
    remaining_positions <- position_ls[!(position_ls %in% exclude_ls)]

    if (pattern == "block_randomized") {
      if (length(remaining_positions) == 1) {
        position <- remaining_positions
      } else {
        position <- sample(remaining_positions, 1)
      }
    } else if (pattern == "sequential") {
      if (all(exclude_ls != 0)) {
        previous_position <- exclude_ls[length(exclude_ls)]
      } else {
        previous_position <- 0
      }
      position <- previous_position + 1
    } else if (pattern == "fixed-latin-square") {
      if (all(exclude_ls != 0)) {
        previous_position <- exclude_ls[length(exclude_ls)]
        which_furthest <- which.max(abs(rate_ranks_seq[remaining_positions] - rate_ranks_seq[previous_position]))
        position <- remaining_positions[which_furthest]
      } else {
        position <- 1
      }
    }

    return(seq_possible[position:(position + max_plot_id - 1)])
  }

  # /*=================================================*/
  #' # Assign rates
  # /*=================================================*/

  rates_data <- data.table(
    rate = rates_ls,
    rate_rank = seq_len(length(rates_ls))
  )

  rates_len <- nrow(rates_data)

  #--- create a sequence of rate ranks ---#
  rate_ranks_seq <- gen_sequence(rates_len)

  data_dt <- data.table(data_sf)

  strip_ls <- data_dt[, strip_id] %>%
    unique() %>%
    .[order(.)]

  design_data_ls <- list()

  # i <- 12
  for (i in strip_ls) {
    # for (i in 1:10) {

    max_plot_id <- data_dt[strip_id == i, max(plot_id)]
    num_seq <- ceiling(max_plot_id / rates_len + 1)

    if ((i %% rates_len) == 1) {
      if (i == 1) {
        #--- the very first ---#
        rates_seq <- get_seq_for_strip(pattern, rate_ranks_seq, num_seq, 0)
        init_rate_memeory <- c(which(rates_seq[1] == rate_ranks_seq))
      } else {
        rates_seq <- get_seq_for_strip(
          pattern = pattern,
          rate_ranks_seq = rate_ranks_seq,
          num_seq = num_seq,
          #--- avoid having the same rate right next to it in the previous block ---#
          exclude_ls = init_rate_memeory[rates_len]
        )
        init_rate_memeory <- c(which(rates_seq[1] == rate_ranks_seq))
      }
    } else {
      rates_seq <- get_seq_for_strip(
        pattern = pattern,
        rate_ranks_seq = rate_ranks_seq,
        num_seq = num_seq,
        exclude_ls = init_rate_memeory
      )
      init_rate_memeory <- c(init_rate_memeory, which(rates_seq[1] == rate_ranks_seq))
    }

    design_data_ls[[i]] <- data.table(
      plot_id = seq_len(max_plot_id),
      strip_id = i,
      rate_rank = rates_seq
    )

    # print(rates_seq)
    # print(init_rate_memeory)
  }

  design_data <- rbindlist(design_data_ls)

  # design_data[, .N, by = rate_rank]

  if (merge == TRUE) {
    data <- left_join(data_sf, design_data, by = c("plot_id", "strip_id")) %>%
      left_join(., rates_data, by = "rate_rank")
    return(data)
  } else {
    design_data <- left_join(design_data, rates_data, by = "rate_rank")
    return(design_data)
  }
}


# /*----------------------------------*/
#' ## generate yield
# /*----------------------------------*/
gen_yield_MB <- function(ymax, alpha, beta, N) {
  yield <- ymax * (1 - exp(alpha + beta * N))
  return(yield)
}



# /*----------------------------------*/
#' ## generate application error (+-20)
# /*----------------------------------*/
app_error_fn <- function(data){
 # data = data

  x <- rnorm(nrow(data), sd=5)
  x <- x[x > -20 & x < 20]

  if (length(x)!=nrow(data)){
    repeat {
      lack_no <- nrow(data) - length(x)
      y <- rnorm(lack_no, sd=5)
      y <- y[y > -20 & y < 20]
      if (length(y) == lack_no){
      break
      }
    }
  z <- c(x,y)
  } else {
  z <- x
  }
  return(z)
}



# /*----------------------------------*/
#' ## Economic parameters
# /*----------------------------------*/
price_table <- data.table(
  # === $/kg corn ===#
  pCorn = round(c(3.5, 3.5, 5) * 200 / 5080, digits = 3),
  # === $/kg N ===#
  pN = round(c(0.4, 0.6, 0.4) / 0.453592, digits = 3)
)



# /*=================================================*/
#' # Generate Cell-Level Field Data
# /*=================================================*/

prepare_raw_data <- function(i, field, coef_data_m, coef_data_t, app_error="no") {
  print(paste0("working on ", i, " th iteration."))
  # i=1; x=1
  # field=field; coef_data_m=coef_data[sim == x, ]; coef_data_t=coef_data[sim == ifelse(x + 1 >= max(sim), 1, x + 1), ]
  # app_error="yes"

  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### Data preparation
  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  # === merge field data with the coefs data ===#
  data <- coef_data_m[data.table(field), on = "unique_cell_id"] %>%
    .[, opt_N := (log(pN) - log(-pCorn * ymax * beta) - alpha) / beta]
    

  test_data <- coef_data_t[data.table(field), on = "unique_cell_id"] %>%
    .[, opt_N := (log(pN) - log(-pCorn * ymax * beta) - alpha) / beta]

  # === check the spatial distribution of optimal N ===#
  # left_join(field,data[,.(unique_cell_id,opt_N)],by='unique_cell_id') %>%
  #   select(opt_N) %>%
  #   tm_shape() +
  #     tm_fill(col = "opt_N")

  # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
  #' treatment N rates 
  # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
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

  data <- assign_rates(st_as_sf(data), N_levels) %>% 
    data.table()%>%
    .[!(subplot_id %in% c(1:3, 54:56)),]

  test_data <- assign_rates(st_as_sf(test_data), N_levels) %>% 
    data.table()%>%
    .[!(subplot_id %in% c(1:3, 54:56)),]


  if(app_error=="no"){
  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  #' No application error
  # /*~~~~~~~~~~~~~~~~~~~~~~*/
    data[,aa_n := rate]
    test_data[,aa_n := rate]

  } else {
  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  #' Include application error
  # /*~~~~~~~~~~~~~~~~~~~~~~*/
    data <- data %>% .[,aa_n := rate + app_error_fn(.)]
    test_data <- test_data %>% .[,aa_n := rate + app_error_fn(.)]
  }

  # ggplot(data)+
  #   geom_histogram(aes(x=aa_n), bins=100)

  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### Generate yield
  # /*~~~~~~~~~~~~~~~~~~~~~~*/

  data %<>% .[, det_yield := gen_yield_MB(ymax, alpha, beta, aa_n)] %>%
      # === error ===#
    .[, yield := det_yield * (1 + m_error)] %>%
    .[, yield_error := det_yield*m_error] %>%
      # === keep the relevant vars ===#
    .[, .(
      yield, opt_N, rate, aa_n, alpha, beta, ymax, m_error, yield_error, alpha1, alpha2, beta1, beta2, ymax1, ymax2,
      theta_1, theta_2, subplot_id, strip_id, padding, X, Y, unique_cell_id
    )]%>%
    .[,sim := i]

  test_data %<>% .[, det_yield := gen_yield_MB(ymax, alpha, beta, aa_n)] %>%
      # === error ===#
    .[, yield := det_yield * (1 + m_error)] %>%
      # === keep the relevant vars ===#
    .[, .(
      yield, opt_N, rate, aa_n, alpha, beta, ymax, alpha1, alpha2, beta1, beta2, ymax1, ymax2,
      theta_1, theta_2, subplot_id, strip_id, padding, X, Y, unique_cell_id
    )]%>%
    .[,sim := i]


  return(
    list(
      reg_raw_data = data,
      test_raw_data = test_data
    )
  )
}


# /*=================================================*/
#' # Aggregate Cell-Level Field Data to Subplot-Level
# /*=================================================*/
prepare_data_for_sim <- function(reg_raw_data, test_raw_data){

  # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### Aggregate data by analysis unit (by subplot)
  # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

  # === by subplot ===#
  reg_data <- reg_raw_data[, .(
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

  test_agg_data <- test_raw_data[, .(
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

  return(
    list(
      reg_data = reg_data,
      test_agg_data = test_agg_data
    )
  )
}