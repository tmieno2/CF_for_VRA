# /*=================================================*/
#' # Make experiment grids (basic cell, plot, strip)
# /*=================================================*/
make_trial_grids <- function(field, ab_line, starting_point, plot_width, cell_height) {

  # /*=================================================*/
  #' # Define functions
  # /*=================================================*/
  # /*----------------------------------*/
  #' ## make polygons
  # /*----------------------------------*/
  make_polygon <- function(strt_point_new, multiplier, direction) {
    point_1 <- strt_point_new + cell_height * ab_xy_nml * (multiplier - 1)
    point_2 <- point_1 - plot_width * direction * ab_xy_nml_p90
    point_3 <- point_2 + ab_xy_nml * cell_height
    point_4 <- point_3 + plot_width * direction * ab_xy_nml_p90

    temp_polygon <- rbind(
      point_1,
      point_2,
      point_3,
      point_4,
      point_1
    ) %>%
      list() %>%
      st_polygon()

    return(temp_polygon)
  }

  # /*----------------------------------*/
  #' ## rotation matrix
  # /*----------------------------------*/
  rotate_mat_p90 <- matrix(
    c(
      cos(pi / 2),
      sin(pi / 2),
      -sin(pi / 2),
      cos(pi / 2)
    ),
    nrow = 2
  )

  # /*----------------------------------*/
  #' ## Make an extended line
  # /*----------------------------------*/
  # make line with starting point and a desired shift
  make_extended_line <- function(starting_point, step, multiplier, step_v) {
    extended_line <- rbind(
      starting_point + step * multiplier,
      starting_point + step_v * 1000 + step * multiplier
    ) %>%
      st_linestring() %>%
      list() %>%
      st_as_sfc() %>%
      st_set_crs(st_crs(field))

    return(extended_line)
  }

  # /*----------------------------------*/
  #' ## Check intersections
  # /*----------------------------------*/
  # check the intersection of the lines in the direction specified
  check_intersection <- function(field, step, direction, step_v) {
    is_int_ls <- rep(FALSE, 100)

    for (i in 1:100) {

      #--- shift 50 meter every time ---#
      line <- make_extended_line(
        starting_point,
        plot_width * step * direction,
        i,
        step_v
      )

      is_int_ls[i] <- st_intersects(field, line, sparse = FALSE)[1, 1]

      if (is_int_ls[i]) {
        break
      }
    }

    return(is_int_ls)
  }

  # /*----------------------------------*/
  #' ## vector of points of sf of points
  # /*----------------------------------*/
  vect_to_sf_point <- function(vec) {
    st_as_sfc(list(st_point(vec))) %>%
      st_set_crs(st_crs(field))
  }

  # /*----------------------------------*/
  #' ## Re-assign plot id based on observation numbers per plot
  # /*----------------------------------*/
  reassign_plot_id <- function(data, grp) {
    temp_data <- data[group == grp, ]

    if (max(temp_data$plot_id) == 1) {
      #--- if there is only one plot_id in the strip ---#
      return(temp_data[, .(id, plot_id, group, x)])
    }

    if (nrow(temp_data[too_short == TRUE, ]) == 0) {
      return(temp_data[, .(id, plot_id, group, x)])
    }

    num_obs_short <- temp_data[too_short == TRUE, obs_per_plot] %>%
      unique()

    short_plot_id <- temp_data[too_short == TRUE, plot_id] %>%
      unique()

    num_obs_short_1 <- temp_data[plot_id == (short_plot_id - 1), obs_per_plot] %>%
      unique()

    if (num_obs_short >= (2 * min_obs - mean_obs)) { # make the last two short

      first_obs_set <- ceiling((num_obs_short + mean_obs) / 2)

      temp_data[plot_id %in% c(short_plot_id, short_plot_id - 1), cum_num_reassign := cumsum(dummy)] %>%
        .[cum_num_reassign <= first_obs_set, plot_id := short_plot_id - 1]
    } else if ((max(temp_data$plot_id) >= 3) & num_obs_short >= (3 * min_obs - 2 * mean_obs)) {

      # make the last three short (there needs to be at least 3 plot ids)

      first_obs_set <- ceiling((num_obs_short + 2 * mean_obs) / 3)

      temp_data[plot_id %in% short_plot_id:(short_plot_id - 2), cum_num_reassign := cumsum(dummy)] %>%
        #--- third last i---#i
        .[plot_id %in% short_plot_id:(short_plot_id - 2), plot_id := short_plot_id] %>%
        .[cum_num_reassign <= first_obs_set, plot_id := short_plot_id - 2] %>%
        .[cum_num_reassign > first_obs_set & cum_num_reassign <= 2 * first_obs_set, plot_id := short_plot_id - 1]
    } else if (max(temp_data$plot_id) >= 3) {

      # make the 2nd and 3rd last longer (there needs to be at least 3 plot ids)

      first_obs_set <- ceiling((num_obs_short + 2 * mean_obs) / 2)

      temp_data[plot_id %in% short_plot_id:(short_plot_id - 2), cum_num_reassign := cumsum(dummy)] %>%
        .[plot_id %in% short_plot_id:(short_plot_id - 2), plot_id := short_plot_id - 1] %>%
        #--- third last ---#
        .[cum_num_reassign <= first_obs_set, plot_id := short_plot_id - 2]
    } else {

      # make the two into one (there needs to be at least 2 plot ids)
      temp_data[, plot_id := 1]
    }

    # temp_data[, .N, by = plot_id]

    return(temp_data[, .(id, plot_id, group, x)])
  }

  # /*=================================================*/
  #' # Main code
  # /*=================================================*/
  #--- get the vector (direction machines run)  ---#
  ab_xy <- st_geometry(ab_line)[[1]][2, ] - st_geometry(ab_line)[[1]][1, ]
  #--- distance of the vector ---#
  ab_length <- sqrt(sum(ab_xy^2))
  #--- normalize (distance == 1) ---#
  ab_xy_nml <- ab_xy / ab_length
  #--- create a vector that is perpendicular to ab_xy ---#
  ab_xy_nml_p90 <- ab_xy_nml %*% rotate_mat_p90

  # /*----------------------------------*/
  #' ## identify the number of subplots in a strip
  # /*----------------------------------*/
  f_bbox <- st_bbox(field)

  #--- maximum distance ---#
  max_dist <- sqrt(
    (f_bbox["xmax"] - f_bbox["xmin"])^2 +
      (f_bbox["ymax"] - f_bbox["ymin"])^2
  ) + 50

  max_dist_cover <- ceiling(max_dist / 10) * 10

  #--- number of subplots to create ---#
  num_subplots_in_a_strip <- ceiling(max_dist_cover / cell_height)

  # /*----------------------------------*/
  #' ## Detect which direction to go
  # /*----------------------------------*/
  is_int_p <- check_intersection(field, ab_xy_nml_p90, direction = 1, ab_xy_nml)

  if (any(is_int_p)) {
    direction <- 1
    #--- how many cells do you need to move to intersects with the field ---#
    how_many_in <- which(is_int_p)
  } else {
    direction <- -1
    #--- how many cells do you need to move to intersects with the field ---#
    how_many_in <- which(check_intersection(field, ab_xy_nml_p90, -1, ab_xy_nml))
  }

  #--- refresh the starting point ---#
  strt_point_new <- starting_point + how_many_in * plot_width * direction * ab_xy_nml_p90
  strt_point_new_sf <- vect_to_sf_point(strt_point_new)

  # /*----------------------------------*/
  #' ## Create strip of polygons strip by strip
  # /*----------------------------------*/
  is_intersecting <- TRUE

  exp_sf_ls <- list()
  group <- 1

  while (is_intersecting) {
    exp_sf_ls[[paste(group)]] <- lapply(
      1:num_subplots_in_a_strip,
      function(x) {
        make_polygon(
          strt_point_new + plot_width * direction * ab_xy_nml_p90 * (group - 1),
          x,
          direction
        )
      }
    ) %>%
      st_as_sfc() %>%
      st_set_crs(st_crs(field)) %>%
      st_as_sf() %>%
      mutate(
        group = group,
        id = 1:nrow(.)
      )

    is_intersecting <- st_intersects(exp_sf_ls[[paste(group)]], field, sparse = F)[, 1] %>% any()


    group <- group + 1
  }

  all_plygons <- do.call("rbind", exp_sf_ls) %>%
    .[field, ]

  # /*----------------------------------*/
  #' ## Reassign plot id
  # /*----------------------------------*/
  # group: strip id
  # id: subplot id
  # plot_id: plot id
  min_obs <- 20 # (200 feet)
  mean_obs <- 24 # (240 feet)
  max_obs <- 30 #  (300 feet)

  data <- all_plygons %>%
    data.table() %>%
    #--- observations per strip ---#
    .[, obs_per_strip := .N, by = .(group)] %>%
    #--- (initial) plot id ---#
    .[, dummy := 1] %>%
    .[, cum_num := cumsum(dummy), by = .(group)] %>%
    .[, plot_id := (cum_num - 1) %/% mean_obs + 1, by = .(group)] %>%
    #--- max number of plots per group ---#
    .[, max_plot_id := max(plot_id), by = .(group, plot_id)] %>%
    #--- number of subplots per plot ---#
    .[, obs_per_plot := .N, by = .(group, plot_id)] %>%
    .[, too_short := obs_per_plot <= min_obs]

  group_ls <- data$group %>% unique()

  final_ploygons <- lapply(group_ls, function(x) reassign_plot_id(data, x)) %>%
    rbindlist() %>%
    st_as_sf() %>%
    rename(geometry = x) %>%
    rename(strip_id = group) %>%
    mutate(cell_id := 1:nrow(.)) %>%
    dplyr::select(-id)

  return(final_ploygons)
}

# /*=================================================*/
#' # Assign rates to the trial design data
# /*=================================================*/
# data_sf <- st_as_sf(data)
# rates_ls <- N_levels

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


# /*=================================================*/
#' # Tilt the field
# /*=================================================*/

st_tilt <- function(data_sf, angle) {
  rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

  wf_bbox <- st_bbox(data_sf)
  data_geom <- st_geometry(data_sf)

  base_point <- c(wf_bbox["xmax"], wf_bbox["ymin"]) %>%
    st_point() %>%
    st_sfc()

  data_tilted <- ((data_geom - base_point) * rot(angle / 180 * pi) + base_point) %>%
    st_set_crs(st_crs(data_sf))

  data_sf$geometry <- data_tilted

  return(data_sf)
}

# /*=================================================*/
#' # Shift the field
# /*=================================================*/

st_shift <- function(data_sf, shift) {
  data_geom <- st_geometry(data_sf)

  shift_sfc <- st_point(shift) %>% st_sfc()

  data_shifted <- data_geom + shift_sfc %>%
    st_set_crs(st_crs(data_sf))

  data_sf$geometry <- data_shifted

  return(data_sf)
}

# /*=================================================*/
#' # Create cluster id
# /*=================================================*/

gen_cluster <- function(data, num_levels, dim) {
  data[, strip_group_id := ceiling(strip_id / num_levels / dim)] %>%
    .[, cluster_in_strip_id := ceiling(subplot_id / 6 / dim)] %>%
    .[, cluster_id := paste0(cluster_in_strip_id, strip_group_id)] %>%
    .[, `:=`(
      strip_group_id = NULL,
      cluster_in_strip_id = NULL
    )] %>%
    .[, cluster_id := as.numeric(as.factor(cluster_id))] %>%
    setnames("cluster_id", paste0("cluster_id_", dim))

  return(data)
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

  x <- rnorm(nrow(data), sd=5); x <- x[x > -20 & x < 20]

  if (length(x)!=nrow(data)){
    repeat {
      lack_no <- nrow(data) - length(x)
      y <- rnorm(lack_no, sd=5); y <- y[y > -20 & y < 20]

    if (length(y) == lack_no){
      break
      }
    } 
  }
  return(c(x,y))
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
#' # Run main simulations
# /*=================================================*/
# i <- 1
# pCorn <- price_table[2, pCorn]
# pN <- price_table[2, pN]

# field_cnn <- readRDS(here("Data", "CNN_Simulations", "cnn_field_padding.rds"))

# ggplot()+
#   geom_sf(data=field%>%filter(strip_id==1), aes(fill=factor(plot_id)))

# cnn_coef_data <- readRDS(here("Data", "CNN_Simulations", paste0('cnn_coefficients_sprange_',sp_range,'.rds')))

# prepare_data <- function(i, field, coef_data_m, coef_data_t, app_error="no") {
#   # i=1
#   # x=1
#   # field=field_cnn; coef_data_m=coef_data[sim == x, ]; coef_data_t=coef_data[sim == ifelse(x + 1 >= max(sim), 1, x + 1), ]
#   # app_error="no"

#   # /*~~~~~~~~~~~~~~~~~~~~~~*/
#   #' ### Data preparation
#   # /*~~~~~~~~~~~~~~~~~~~~~~*/
#   # === merge field data with the coefs data ===#
#   data <- coef_data_m[data.table(field), on = "unique_cell_id"] %>%
#     .[, opt_N := (log(pN) - log(-pCorn * ymax * beta) - alpha) / beta]
    

#   test_data <- coef_data_t[data.table(field), on = "unique_cell_id"] %>%
#     .[, opt_N := (log(pN) - log(-pCorn * ymax * beta) - alpha) / beta]
  
#   # ggplot(st_as_sf(data)) +
#   #   geom_sf(aes(fill = cluster_id_1), size = 0) +
#   #   scale_fill_viridis_c()

#   # === check the spatial distribution of optimal N ===#
#   # left_join(field,data[,.(unique_cell_id,opt_N)],by='unique_cell_id') %>%
#   #   select(opt_N) %>%
#   #   tm_shape() +
#   #     tm_fill(col = "opt_N")

#   # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
#   #' treatment N rates 
#   # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
#   # === define experimental N rate ===#
#   all_opt_N <- data[, opt_N]

#   # --- for training ---#
#   # hist(all_opt_N)
#   N_levels <- seq(
#     quantile(all_opt_N, prob = 0.05) - 20,
#     quantile(all_opt_N, prob = 0.95) + 20,
#     length = 5 
#   ) %>%
#     round()

#   data <- assign_rates(st_as_sf(data), N_levels) %>% # N_levels are assigned as "rate" variable
#     data.table()%>%
#     .[!(subplot_id %in% c(1:3, 54:56)),]

#   test_data <- assign_rates(st_as_sf(test_data), N_levels) %>% # N_levels are assigned as "rate" variable
#     data.table()%>%
#     .[!(subplot_id %in% c(1:3, 54:56)),]



#   if(app_error=="no"){
#   # /*~~~~~~~~~~~~~~~~~~~~~~*/
#   #' No application error
#   # /*~~~~~~~~~~~~~~~~~~~~~~*/
#     data[,aa_n := rate]
#     test_data[,aa_n := rate]

#   } else {
#   # /*~~~~~~~~~~~~~~~~~~~~~~*/
#   #' Include application error
#   # /*~~~~~~~~~~~~~~~~~~~~~~*/
#     data <- data %>% .[,aa_n := rate + app_error_fn(.)]
#     test_data <- test_data %>% .[,aa_n := rate + app_error_fn(.)]
#   }

#   # /*~~~~~~~~~~~~~~~~~~~~~~*/
#   #' ### Generate yield
#   # /*~~~~~~~~~~~~~~~~~~~~~~*/

#   data %<>% .[, det_yield := gen_yield_MB(ymax, alpha, beta, aa_n)] %>%
#       # === error ===#
#     .[, yield := det_yield * (1 + m_error)] %>%
#       # === keep the relevant vars ===#
#     .[, .(
#       yield, opt_N, rate, aa_n, alpha, beta, ymax, alpha1, alpha2, beta1, beta2, ymax1, ymax2,
#       theta_1, theta_2, subplot_id, strip_id, padding, X, Y, unique_cell_id
#     )]%>%
#     .[,sim := i]

#   test_data %<>% .[, det_yield := gen_yield_MB(ymax, alpha, beta, aa_n)] %>%
#       # === error ===#
#     .[, yield := det_yield * (1 + m_error)] %>%
#       # === keep the relevant vars ===#
#     .[, .(
#       yield, opt_N, rate, aa_n, alpha, beta, ymax, alpha1, alpha2, beta1, beta2, ymax1, ymax2,
#       theta_1, theta_2, subplot_id, strip_id, padding, X, Y, unique_cell_id
#     )]%>%
#     .[,sim := i]

#   test_cell_data <- copy(test_data)%>%
#     .[,unique_cell_id:=paste0(strip_id,"_",subplot_id)]%>%
#     .[padding==1,]%>%
#     .[sample(1:nrow(.), 1000), ]%>%
#     .[,`:=`(
#         strip_id = NULL,
#         subplot_id = NULL
#     )]

#   # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
#   #' ### Aggregate data by analysis unit (by subplot)
#   # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#   # === by subplot ===#
#   reg_data <- data[, .(
#     sim = mean(sim),
#     yield = mean(yield),
#     rate = mean(rate),
#     aa_n = mean(aa_n), 
#     alpha = mean(alpha),
#     beta = mean(beta),
#     ymax = mean(ymax),
#     alpha1 = mean(alpha1),
#     alpha2 = mean(alpha2),
#     beta1 = mean(beta1),
#     beta2 = mean(beta2),
#     ymax1 = mean(ymax1),
#     ymax2 = mean(ymax2),
#     theta_1 = mean(theta_1),
#     theta_2 = mean(theta_2),
#     padding = mean(padding),
#     # cluster_id_1 = mean(cluster_id_1),
#     # cluster_id_2 = mean(cluster_id_2),
#     X = mean(X),
#     Y = mean(Y)
#   ), by = .(subplot_id, strip_id)]%>%
#   .[,unique_cell_id:=paste0(strip_id,"_",subplot_id)]%>%
#   .[, `:=`(
#     strip_id = NULL,
#     subplot_id = NULL
#   )]  

#   test_agg_data <- test_data[, .(
#     sim = mean(sim),
#     yield = mean(yield),
#     opt_N = mean(opt_N),
#     rate = mean(rate),
#     aa_n = mean(aa_n), 
#     alpha = mean(alpha),
#     beta = mean(beta),
#     ymax = mean(ymax),
#     alpha1 = mean(alpha1),
#     alpha2 = mean(alpha2),
#     beta1 = mean(beta1),
#     beta2 = mean(beta2),
#     ymax1 = mean(ymax1),
#     ymax2 = mean(ymax2),
#     theta_1 = mean(theta_1),
#     theta_2 = mean(theta_2),
#     padding = mean(padding),
#     # cluster_id_1 = mean(cluster_id_1),
#     # cluster_id_2 = mean(cluster_id_2),
#     X = mean(X),
#     Y = mean(Y)
#   ), by = .(subplot_id, strip_id)]%>%
#   .[,unique_cell_id:=paste0(strip_id,"_",subplot_id)]%>%
#   .[, `:=`(
#     strip_id = NULL,
#     subplot_id = NULL
#   )]
    


#   # ggplot(st_as_sf(data)) +
#   #   geom_sf(aes(fill = factor(rate)), size = 0) +
#   #   scale_fill_viridis_d()

#   # ggplot(st_as_sf(data)) +
#   #    geom_sf(aes(fill = factor(plot_id)))

#   # ggplot(st_as_sf(data))+geom_sf(aes(fill=m_error),color = NA)+
#   #   scale_fill_gradient(low = "yellow", high = "red")+
#   #   ggtitle("m_error")

#   return(
#     list(
#       reg_raw_data = data,
#       reg_data = reg_data,
#       test_raw_data = test_data,
#       test_agg_data = test_agg_data,
#       test_cell_data = test_cell_data,
#       rates_ls = N_levels
#     )
#   )
# }

# For example
# x=1
# demo <- prepare_data(
#   field = field_cnn, 
#   coef_data_m = coef_data[sim == x, ],
#   coef_data_t = coef_data[sim == ifelse(x + 1 >= max(sim), 1, x + 1), ]
#   )



prepare_raw_data <- function(i, field, coef_data_m, coef_data_t, app_error="no") {
  print(paste0("working on ", i, " th iteration."))
  # i=1
  # x=1
  # field=field_cnn; coef_data_m=coef_data[sim == x, ]; coef_data_t=coef_data[sim == ifelse(x + 1 >= max(sim), 1, x + 1), ]
  # app_error="no"

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

  data <- assign_rates(st_as_sf(data), N_levels) %>% # N_levels are assigned as "rate" variable
    data.table()%>%
    .[!(subplot_id %in% c(1:3, 54:56)),]

  test_data <- assign_rates(st_as_sf(test_data), N_levels) %>% # N_levels are assigned as "rate" variable
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

# x=1
# demo <- prepare_source_data(
#       i = x, 
#       field = field_cnn,
#       coef_data_m = coef_data[sim == x, ],
#       coef_data_t = coef_data[sim == ifelse(x + 1 >= max(sim), 1, x + 1), ],
#       app_error="no")
# reg_raw_data <- demo$reg_raw_data
# test_raw_data <- demo$test_raw_data




# =======================================
# Recreate testing dataset 
# =======================================
recreate_raw_data <- function(i, field, coef_data_m, coef_data_t, app_error="no") {
  print(paste0("working on ", i, " th iteration."))
  # i=1
  # x=1
  # field=field_cnn; coef_data_m=coef_data[sim == x, ]; coef_data_t=coef_data[sim == ifelse(x + 1 >= max(sim), 1, x + 1), ]
  # app_error="no"

  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### Data preparation
  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  # === merge field data with the coefs data ===#
  data <- coef_data_m[data.table(field), on = "unique_cell_id"] %>%
    .[, opt_N := (log(pN) - log(-pCorn * ymax * beta) - alpha) / beta]

  test_m_error <- coef_data_t[,.(unique_cell_id,m_error)]
  
  # ggplot(st_as_sf(data)) +
  #   geom_sf(aes(fill = cluster_id_1), size = 0) +
  #   scale_fill_viridis_c()

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

  train_data <- assign_rates(st_as_sf(data), N_levels) %>% # N_levels are assigned as "rate" variable
    data.table()%>%
    .[!(subplot_id %in% c(1:3, 54:56)),]

  test_data <- assign_rates(st_as_sf(data), N_levels) %>% # N_levels are assigned as "rate" variable
    data.table()%>%
    .[,m_error:=NULL]%>%
    .[test_m_error, on="unique_cell_id"]%>%
    .[!(subplot_id %in% c(1:3, 54:56)),]


  if(app_error=="no"){
  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  #' No application error
  # /*~~~~~~~~~~~~~~~~~~~~~~*/
    train_data[,aa_n := rate]
    test_data[,aa_n := rate]

  } else {
  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  #' Include application error
  # /*~~~~~~~~~~~~~~~~~~~~~~*/
    train_data <- train_data %>% .[,aa_n := rate + app_error_fn(.)]
    test_data <- test_data %>% .[,aa_n := rate + app_error_fn(.)]
  }

  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### Generate yield
  # /*~~~~~~~~~~~~~~~~~~~~~~*/

  train_data %<>% .[, det_yield := gen_yield_MB(ymax, alpha, beta, aa_n)] %>%
      # === error ===#
    .[, yield := det_yield * (1 + m_error)] %>%
      # === keep the relevant vars ===#
    .[, .(
      yield, opt_N, rate, aa_n, alpha, beta, ymax, m_error, alpha1, alpha2, beta1, beta2, ymax1, ymax2,
      theta_1, theta_2, subplot_id, strip_id, padding, X, Y, unique_cell_id
    )]%>%
    .[,sim := i]

  test_data %<>% .[, det_yield := gen_yield_MB(ymax, alpha, beta, aa_n)] %>%
      # === error ===#
    .[, yield := det_yield * (1 + m_error)] %>%
      # === keep the relevant vars ===#
    .[, .(
      yield, opt_N, rate, aa_n, alpha, beta, ymax, m_error, alpha1, alpha2, beta1, beta2, ymax1, ymax2,
      theta_1, theta_2, subplot_id, strip_id, padding, X, Y, unique_cell_id
    )]%>%
    .[,sim := i]


  return(
    list(
      reg_raw_data = train_data,
      test_raw_data = test_data
    )
  )
}






prepare_data_for_sim <- function(reg_raw_data, test_raw_data){

  test_cell_data <- copy(test_raw_data)%>%
    .[padding==1,]%>%
    .[sample(1:nrow(.), 1000), ]%>%
    .[,`:=`(
        strip_id = NULL,
        subplot_id = NULL
    )]

  # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### Aggregate data by analysis unit (by subplot)
  # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

  # === by subplot ===#
  reg_data <- reg_raw_data[, .(
    sim = mean(sim),
    yield = mean(yield),
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
    # cluster_id_1 = mean(cluster_id_1),
    # cluster_id_2 = mean(cluster_id_2),
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
    # cluster_id_1 = mean(cluster_id_1),
    # cluster_id_2 = mean(cluster_id_2),
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
      test_agg_data = test_agg_data,
      test_cell_data = test_cell_data
    )
  )
}




prepare_testing_data_for_sim <- function(test_raw_data){

  # test_cell_data <- copy(test_raw_data)%>%
  #   .[padding==1,]%>%
  #   .[sample(1:nrow(.), 1000), ]%>%
  #   .[,`:=`(
  #       strip_id = NULL,
  #       subplot_id = NULL
  #   )]

  # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### Aggregate data by analysis unit (by subplot)
  # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

  # === by subplot ===#
  # reg_data <- reg_raw_data[, .(
  #   sim = mean(sim),
  #   yield = mean(yield),
  #   rate = mean(rate),
  #   aa_n = mean(aa_n), 
  #   alpha = mean(alpha),
  #   beta = mean(beta),
  #   ymax = mean(ymax),
  #   alpha1 = mean(alpha1),
  #   alpha2 = mean(alpha2),
  #   beta1 = mean(beta1),
  #   beta2 = mean(beta2),
  #   ymax1 = mean(ymax1),
  #   ymax2 = mean(ymax2),
  #   theta_1 = mean(theta_1),
  #   theta_2 = mean(theta_2),
  #   padding = mean(padding),
  #   # cluster_id_1 = mean(cluster_id_1),
  #   # cluster_id_2 = mean(cluster_id_2),
  #   X = mean(X),
  #   Y = mean(Y)
  # ), by = .(subplot_id, strip_id)]%>%
  # .[,unique_cell_id:=paste0(strip_id,"_",subplot_id)]%>%
  # .[, `:=`(
  #   strip_id = NULL,
  #   subplot_id = NULL
  # )]  

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
    # cluster_id_1 = mean(cluster_id_1),
    # cluster_id_2 = mean(cluster_id_2),
    X = mean(X),
    Y = mean(Y)
  ), by = .(subplot_id, strip_id)]%>%
  .[,unique_cell_id:=paste0(strip_id,"_",subplot_id)]%>%
  .[, `:=`(
    strip_id = NULL,
    subplot_id = NULL
  )]

  return(test_agg_data)

}

# sim_data <- prepare_data_for_sim(reg_raw_data, test_raw_data)

# reg_data <- sim_data$reg_data
# test_agg_data <- sim_data$test_agg_data
# test_cell_data <- sim_data$test_cell_data

# test_cell_agg <- rbind(test_agg_data, test_cell_data)
# names(test_agg_data)
# names(test_cell_data)


sim_par <- function(i, reg_data, test_agg_data, N_levels) {
  print(paste0("working on ", i, " th iteration."))
  # x=2
  # i=1; reg_data=reg_data[sim==x&padding==1,]; test_agg_data=test_agg_data[sim==x&padding==1,]; N_levels = reg_data[sim==x,]$rate%>%unique()%>%sort()
  # /*----------------------------------*/
  #' ## Prepare datasets
  # /*----------------------------------*/
  # i=1
  # field = field_cnn
  # coef_data_m = coef_data[sim == i, ]
  # coef_data_t = coef_data[sim == ifelse(i + 1 >= max(sim), 1, i + 1), ]
  # app_error="no"

  # datasets <- prepare_data(field, coef_data_m, coef_data_t, app_error)

  # reg_data <- datasets$reg_data[padding==1,]
  # test_agg_data = datasets$test_agg_data[padding==1,]
  # test_cell_data <- datasets$test_data%>%
  #   [padding==1,]%>%
  #   [sample(1:nrow(.), 1000), ]

  # N_levels <- datasets$rates_ls

  # /*----------------------------------*/
  #' ## run CF and BRF analyses
  # /*----------------------------------*/

  ## == all the combinations of variables ==##
  var_ls_variations <- list(
    c("alpha", "beta", "ymax"),
    c("alpha", "beta", "ymax", "theta_1", "theta_2"),
    c("alpha1", "alpha2", "beta1", "beta2", "ymax1", "ymax2"),
    c("alpha1", "alpha2", "beta1", "beta2", "ymax1", "ymax2", "theta_1", "theta_2")
  )

  #--- all the cases to consider ---#
  cf_case_data <- expand.grid(
    var_ls = var_ls_variations,
    cluster_var = NA, 
    # model = c("CF_stepwise", "CF_base")
    # model = c("BRF","RF")
    model = c("CF_stepwise", "CF_base", "BRF", "RF")      
  ) %>%
  tibble()


  results_data <- cf_case_data %>%
    mutate(
      opt_N_data = mclapply(
        seq_len(nrow(.)),
        function(x) {
          ### === apply various methods to get optimal N ===###
          get_opt_N(
            reg_data = reg_data,
            var_ls = .$var_ls[[x]],
            cl_var = .$cluster_var[[x]],
            test_data_subplot = test_agg_data,
            rates_ls = N_levels,
            model = .$model[[x]]
          )
        },
        mc.cores = detectCores() - 2
      )
    )%>%
    rowwise()%>%
    mutate(
      r2_agg = summary(lm(opt_N ~ opt_N_hat, data = opt_N_data))$r.squared,
      r2_y_agg = ifelse(model%in% c("CF_stepwise", "CF_base"), NA,
        summary(lm(yield ~ pred_yield, data = opt_N_data))$r.squared)
    ) %>%
    data.table() %>%
    .[,opt_N_data := NULL] %>%
    .[, sim := i]

  return(results_data)

     
}

# x=2
# demo <- sim_par(
#   i = x,
#   reg_data = reg_data[sim==x&padding==1,],
#   test_agg_data = test_agg_data[sim==x&padding==1,],
#   N_levels = reg_data[sim==x,]$rate%>%unique()%>%sort()
#   )

# temp <- get_opt_N(
#   reg_data=reg_data[sim==x&padding==1,],
#   var_ls=c("alpha","beta","ymax"),
#   cl_var=NA,
#   test_data_subplot=test_agg_data[sim==x&padding==1,],
#   rates_ls=reg_data[sim==x,]$rate%>%unique()%>%sort(),
#   model="CF_stepwise")

# temp[type=="cell"]%>%.[,opt_N_hat]


get_opt_N <- function(reg_data, var_ls, cl_var, test_data_subplot, rates_ls, model) {
  # reg_data=reg_data; var_ls=c("alpha","beta","ymax"); cl_var=NA; test_data_subplot=test_agg_data
  # test_data_cell=test_cell_data; rates_ls=N_levels

  if (model == "CF_stepwise") {
    ### === CF ===###
    opt_N_data <- CF_analysis(reg_data, var_ls, cl_var) %>%
      get_pi_dif(
        test_agg_data=test_data_subplot,
        cf_results=.,
        var_ls=var_ls,
        rates_ls=rates_ls
        )%>%
      .[,
        .SD[pi_change == max(pi_change), ], by = .(unique_cell_id,type)
  ] %>%
  .[, .(unique_cell_id, N, type)] %>%
  setnames("N", "opt_N_hat") %>%
  .[test_data_subplot, on = "unique_cell_id"]
    ### === CF_base ===###
  } else if (model == "CF_base") {
    opt_N_data <- CF_analysis_base(reg_data, var_ls, cl_var) %>%
      get_pi_dif_base(
        test_agg_data=test_data_subplot,
        cf_results=.,
        var_ls=var_ls,
        rates_ls=rates_ls
        )%>%
  .[,
    .SD[pi_change == max(pi_change), ], by = .(unique_cell_id,type)
  ] %>%
  .[, .(unique_cell_id, N, type)] %>%
  setnames("N", "opt_N_hat") %>%
  .[test_data_subplot, on = "unique_cell_id"]
    ### === BRF ===###
  } else if (model == "BRF") {
    opt_N_data <- BRF_run(
      reg_data = reg_data,
      var_ls = var_ls,
      cl_id = cl_var
    ) %>%
    BRF_analysis(
        test_agg_data = test_data_subplot,
        brf_results = .,
        var_ls = var_ls,
        N_levels = rates_ls
      )%>%
      .[, .SD[pi_hat == max(pi_hat), ], by = .(unique_cell_id, type)] %>%
      .[, .(unique_cell_id, rate, pred_yield, type)] %>%
      setnames("rate", "opt_N_hat") %>%
      .[test_data_subplot, on = "unique_cell_id"]
    ### === RF ===###
  } else if (model == "RF") {
    opt_N_data <- RF_run(
      reg_data = reg_data,
      var_ls = var_ls,
      cl_id = cl_var
    ) %>%
      RF_analysis(
        test_agg_data = test_data_subplot,
        rf_results = .,
        var_ls = var_ls,
        N_levels = rates_ls
      )%>%
      .[, .SD[pi_hat == max(pi_hat), ], by = .(unique_cell_id, type)] %>%
      .[, .(unique_cell_id, rate, pred_yield, type)] %>%
      setnames("rate", "opt_N_hat") %>%
      .[test_data_subplot, on = "unique_cell_id"]
    ### === Traditional ===###
  }
  return(opt_N_data)
}





# /*================================================================*/
#' # Run CF
# /*================================================================*/

# data <- reg_data
# rates <- N_levels[1:2]
# var_ls <- c("ymax", "alpha", "beta")
# cl_id <- "cluster_id_1"

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
      honesty = FALSE,
      clusters = cl,
      num.trees = 2000,
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
      honesty = FALSE,
      num.trees = 2000,
      min.node.size = 10
      # sample.fraction = 0.4,
      # min.node.size=10,
      # tune.parameters='all'
      # tune.parameters=c("sample.fraction", "mtry", "honesty.fraction", "honesty.prune.leaves", "alpha", "imbalance.penalty")
    )
  }

  return(tau_forest_temp)
}

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
  # reg_data=reg_data; var_ls=c("alpha", "beta","ymax"); cl_var=NA
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

get_changes_gradual <- function(N_index, data_base, var_ls, rates_ls, cf_results) {

  # /*----------------------------------*/
  #' ## Increase N
  # /*----------------------------------*/

  for (i in 1:N_index) {
    data_eval_base <- data_base %>%
      .[, var_ls, with = FALSE]

    if (i == 1) {
      tau_data <- rep(0, nrow(data_base))
    } else {
      tau_data <- tau_data + predict(cf_results[[i - 1]], newdata = data_eval_base, estimate.variance = FALSE)$predictions
    }
  } # end the loop over i

  return_data <- data.table(
    unique_cell_id = data_base$unique_cell_id,
    yield_dif = tau_data, # yield diff shows the treatment effects
    N_plus = rates_ls[N_index] - rates_ls[1],
    N = rates_ls[N_index]
  )
  return(return_data)
}



get_pi_dif <- function(test_agg_data, cf_results, var_ls, rates_ls) {
  # test_agg_data=test_agg_data; test_cell_data=test_cell_data; cf_results=cf_step
  # var_ls=var_ls; rates_ls=N_levels

  # /*-------------------------*/
  #' for aggregated testing data
  # /*-------------------------*/
  pi_dif_data_agg <- lapply(
    1:(length(cf_results) + 1),
    function(x) {
      get_changes_gradual(
        x,
        data_base = test_agg_data,
        var_ls = var_ls,
        rates_ls = rates_ls,
        cf_results = cf_results
      )
    }
  ) %>%
    rbindlist() %>%
    .[, pi_change := pCorn * yield_dif - pN * N_plus]%>%
    .[, type:="agg"]

  return(pi_dif_data_agg)
}



# var_ls <- c("alpha", "beta", "ymax")
# cf_step <- CF_analysis(reg_data=reg_data, var_ls=var_ls, cl_var=NA)

# test_cell_agg <- rbind(test_agg_data, test_cell_data)

# pi_dif_data <- get_pi_dif(test_agg_data=test_agg_data, test_cell_data=test_cell_data, cf_results=cf_step,
#   var_ls=var_ls, rates_ls=N_levels)%>%
#   .[,
#     .SD[pi_change == max(pi_change), ],
#         by = .(unique_cell_id,type)
#   ] %>%
#   .[, .(unique_cell_id, type, N)] %>%
#   setnames("N", "opt_N_hat") %>%
#   .[test_cell_agg, on = "unique_cell_id"]





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


get_changes_gradual_base <- function(N_index, data_base, var_ls, rates_ls, cf_results) {
  # data_base=data_test; var_ls=var_ls; rates_ls=N_levels; cf_results=cf_results_base
  # /*----------------------------------*/
  #' ## Increase N
  # /*----------------------------------*/
  # N_index=5
  for (i in 1:N_index) {
    data_eval_base <- data_base %>%
      .[, var_ls, with = FALSE]

    if (i == 1) {
      tau_data <- rep(0, nrow(data_base)) # <- Actually, this part is unnecessary, but just for the purpose of consistency
    } else {
      tau_data <- predict(cf_results[[i - 1]], newdata = data_eval_base, estimate.variance = FALSE)$predictions
    }
  }

  return_data <- data.table(
    unique_cell_id = data_base$unique_cell_id,
    yield_dif = tau_data, # yield diff shows the treatment effects
    N_plus = rates_ls[N_index] - rates_ls[1],
    N = rates_ls[N_index]
  )
  return(return_data)
}


# var_ls <- c("alpha", "beta", "ymax")
# cf_base <- CF_analysis_base(reg_data=reg_data, var_ls=var_ls, cl_var=NA)

get_pi_dif_base <- function(test_agg_data, test_cell_data, cf_results, var_ls, rates_ls) {
  # test_agg_data=test_agg_data; test_cell_data=test_cell_data; cf_results=cf_base
  # var_ls=var_ls; rates_ls=N_levels

  # /*-------------------------*/
  #' for aggregated testing data
  # /*-------------------------*/
  pi_dif_data_agg <- lapply(
    1:(length(cf_results) + 1),
    function(x) {
      get_changes_gradual_base(
        x,
        data_base = test_agg_data,
        var_ls = var_ls,
        rates_ls = rates_ls,
        cf_results = cf_results
      )
    }
  ) %>%
    rbindlist() %>%
    .[, pi_change := pCorn * yield_dif - pN * N_plus]%>%
    .[, type:="agg"]

  return(pi_dif_data_agg)
}



# test_cell_agg <- rbind(test_agg_data, test_cell_data)

# pi_dif_data_base <- get_pi_dif_base(test_agg_data=test_agg_data, test_cell_data=test_cell_data, cf_results=cf_base,
#   var_ls=var_ls, rates_ls=N_levels)%>%
#   .[,
#     .SD[pi_change == max(pi_change), ], by = .(unique_cell_id, type)
#   ] %>%
#   .[, .(unique_cell_id, type, N)] %>%
#   setnames("N", "opt_N_hat") %>%
#   .[test_cell_agg, on = "unique_cell_id"]




### For example ###

# ##1-2, 2-3,....
# var_ls <- c("alpha", "beta", "ymax")
# var_ls <- c("alpha", "beta","ymax","theta_1", "theta_2")

# cl_id = 'cluster_id_1'


# cf_results <- CF_analysis(reg_data, var_ls, cl_var = cl_id)
# pi_dif_data <- get_pi_dif(data_test, cf_results, var_ls, N_levels)

# # pi_dif_data[unique_cell_id=='1_1']


# # ##1-2, 1-3,....
# cf_results_base <- CF_analysis_base(reg_data, var_ls, cl_var = cl_id)
# pi_dif_data_base <- get_pi_dif_base(data=data_test, cf_results_base, var_ls, N_levels)

# pi_dif_data_base[unique_cell_id=='1_1']






# /*================================================================*/
#' # Run BRF
# /*================================================================*/

BRF_run <- function(reg_data, var_ls, cl_id = NA) {
  # var_ls <- c("alpha", "beta", "ymax")
  # reg_data = reg_data; var_ls = var_ls; cl_id = NA

  # === Boosted random forest analysis ===#
  X <- reg_data[, c("aa_n", var_ls), with = FALSE] 
  Y <- reg_data[, yield]

  if (!is.na(cl_id)) {
    cl <- reg_data[[cl_id]]
  }

  # === causal forest analysis ===#
  if (!is.na(cl_id)) {
    #--- if cl_id is present ---#
    BRF_temp <- boosted_regression_forest(
      X = X,
      Y = Y,
      honesty = FALSE,
      clusters = cl,
      num.trees = 2000, # changed from 2000
      min.node.size = 10
      # tune.parameters ="",
      # tune.parameters = c("sample.fraction", "mtry", "honesty.fraction", "honesty.prune.leaves", "alpha", "imbalance.penalty")
    )
  } else {
    #--- if cl_id is NOT present ---#
    BRF_temp <- boosted_regression_forest(
      X = X,
      Y = Y,
      honesty = FALSE,
      num.trees = 2000, # changed from 2000
      min.node.size = 10
      # tune.parameters ="",
      # tune.parameters = c("sample.fraction", "mtry", "honesty.fraction", "honesty.prune.leaves", "alpha", "imbalance.penalty")
    )
  }
  return(BRF_temp)
}

# var_ls <- c("alpha", "beta", "ymax")
# brf_temp <- BRF_run(reg_data=reg_data, var_ls=var_ls, cl_id = NA)



BRF_analysis <- function(test_agg_data, brf_results, var_ls, N_levels) {
# test_agg_data = test_agg_data; test_cell_data=test_cell_data; brf_results = brf_temp; var_ls = var_ls; N_levels = N_levels 

  N_seq <- seq(min(N_levels), max(N_levels), by = 1)
  
  # /*-------------------------*/
  #' for aggregated testing data
  # /*-------------------------*/
  eval_data_agg <- test_agg_data[, c("unique_cell_id", var_ls, "aa_n", "opt_N", "yield", "X", "Y"), with = FALSE] %>%
    ####==== for yield prediction ====####
    .[, pred_yield := predict(brf_results, newdata = .[, c("aa_n", var_ls), with = FALSE])]%>%
    ####==== for EONR estimation ====####
    .[rep(1:nrow(.), each = length(N_seq)), ] %>%
    .[, rate := rep(N_seq, nrow(.) / length(N_seq))] %>%
    .[, yield_hat := predict(brf_results, newdata = .[, c("rate", var_ls), with = FALSE])] %>%
    .[, pi_hat := pCorn * yield_hat - pN * rate]%>%
    .[,type:="agg"]

  return(eval_data_agg)
}


# Ex
# test_cell_agg <- rbind(test_agg_data, test_cell_data)

# opt_N_data <- BRF_analysis(
#         test_agg_data = test_agg_data,
#         test_cell_data = test_cell_data,
#         brf_results = brf_temp,
#         var_ls = var_ls,
#         N_levels = N_levels
#       )%>%
#       .[, .SD[pi_hat == max(pi_hat), ], by = .(unique_cell_id, type)] %>%
#       .[, .(unique_cell_id, rate, pred_yield)] %>%
#       setnames("rate", "opt_N_hat") %>%
#       .[test_cell_agg, on = "unique_cell_id"]


# /*================================================================*/
#' # Run RF
# /*================================================================*/
# temp_RF <- RF_run(reg_data=reg_data, var_ls=c("alpha","beta","ymax"), cl_id = NA)%>%
#   RF_analysis(data_test=test_data, brf_results=., var_ls=c("alpha","beta","ymax"), N_levels)

RF_run <- function(reg_data, var_ls, cl_id = NA) {

  # === Boosted random forest analysis ===#
  X <- reg_data[, c("aa_n", var_ls), with = FALSE] # (5/25)changed from "rate" to "as_app_N"
  Y <- reg_data[, yield]

  if (!is.na(cl_id)) {
    cl <- reg_data[[cl_id]]
  }

  # === causal forest analysis ===#
  if (!is.na(cl_id)) {
    #--- if cl_id is present ---#
    RF_temp <- regression_forest(
      X = X,
      Y = Y,
      honesty = FALSE,
      clusters = cl,
      num.trees = 2000, # changed from 2000
      min.node.size = 10
      # tune.parameters ="",
      # tune.parameters = c("sample.fraction", "mtry", "honesty.fraction", "honesty.prune.leaves", "alpha", "imbalance.penalty")
    )
  } else {
    #--- if cl_id is NOT present ---#
    RF_temp <- regression_forest(
      X = X,
      Y = Y,
      honesty = FALSE,
      num.trees = 2000, # changed from 2000
      min.node.size = 10
      # tune.parameters ="",
      # tune.parameters = c("sample.fraction", "mtry", "honesty.fraction", "honesty.prune.leaves", "alpha", "imbalance.penalty")
    )
  }
  return(RF_temp)
}

# var_ls <- c("alpha", "beta", "ymax")
# rf_temp <- RF_run(reg_data=reg_data, var_ls=var_ls, cl_id = NA)

RF_analysis <- function(test_agg_data, rf_results, var_ls, N_levels) {
  # test_agg_data = test_agg_data; test_cell_data=test_cell_data; rf_results = rf_temp; var_ls = var_ls; N_levels = N_levels 
  N_seq <- seq(min(N_levels), max(N_levels), by = 1)
  
  # /*-------------------------*/
  #' for aggregated testing data
  # /*-------------------------*/
  eval_data_agg <- test_agg_data[, c("unique_cell_id", var_ls, "aa_n", "opt_N", "yield", "X", "Y"), with = FALSE] %>%
    ####==== for yield prediction ====####
    .[, pred_yield := predict(rf_results, newdata = .[, c("aa_n", var_ls), with = FALSE])]%>%
    ####==== for EONR estimation ====####
    .[rep(1:nrow(.), each = length(N_seq)), ] %>%
    .[, rate := rep(N_seq, nrow(.) / length(N_seq))] %>%
    .[, yield_hat := predict(rf_results, newdata = .[, c("rate", var_ls), with = FALSE])] %>%
    .[, pi_hat := pCorn * yield_hat - pN * rate]%>%
    .[,type:="agg"]

  return(eval_data_agg)
}



# Ex
# test_cell_agg <- rbind(test_agg_data, test_cell_data)

# opt_N_data <- RF_analysis(
#         test_agg_data = test_agg_data,
#         test_cell_data = test_cell_data,
#         rf_results = rf_temp,
#         var_ls = var_ls,
#         N_levels = N_levels
#       )%>%
#       .[, .SD[pi_hat == max(pi_hat), ], by = .(unique_cell_id, type)] %>%
#       .[, .(unique_cell_id, rate, pred_yield)] %>%
#       setnames("rate", "opt_N_hat") %>%
#       .[test_cell_agg, on = "unique_cell_id"]



# /*================================================================*/
#' # Run Traditional approach
# /*================================================================*/
#' step1 regression using reg_data
#' step2 prediction(yield_hat) with test_data
#' step3 calculate pi
#' (step4) find opt_N

### ===  Model Estimation and prediction are completed only with this function ===###
Traditional_analysis <- function(reg_data, data_test, var_ls, N_levels) {
  # reg_data=reg_data; data_test=test_data; var_ls=var_ls

  trad_res <- lm(paste0(
    "yield ~ rate + ",
    paste0(var_ls, collapse = " + "),
    " + ",
    paste0(var_ls, "*rate", collapse = " + "),
    " + ",
    paste0(var_ls, "*I(rate^2)", collapse = " + ")
  ) %>%
    formula(), data = reg_data)

  N_seq <- seq(min(N_levels), max(N_levels), by = 1)

  eval_data <- data_test[, c("unique_cell_id", var_ls, "opt_N", "yield", "X", "Y"), with = FALSE] %>%
    .[rep(1:nrow(.), each = length(N_seq)), ] %>%
    .[, rate := rep(N_seq, nrow(.) / length(N_seq))] %>%
    .[, yield_hat := predict(trad_res, newdata = .)] %>%
    .[, pi_hat := pCorn * yield_hat - pN * rate]

  return(eval_data)

}






