library(sf)
library(data.table)
library(RColorBrewer)
library(patchwork)
library(grf)
library(magrittr)
library(ggplot2)
library(ggthemes)
library(viridis)
library(tidyverse)
library(ggpubr)
library(flextable)
library(officer)
library(modelsummary)
library(here)
library(gridExtra)
library(DiagrammeR)
library(causalTree)
library(rpart)
library(rattle)
library(sparkline)
library(htmltools)
library(htmlwidgets)
library(equatags)
library(equatiomatic)
# ===================================
# Load the data
# ===================================
source(here("Codes", "functions.R"))
source(here("Codes", "functions_stepwise_vs_base.R"))

pCorn <- price_table[2, pCorn]
pN <- price_table[2, pN]


# ===================================
# sample data creation 
# ===================================
field <- readRDS(here("Data", "Simulations", "field.rds")) %>%
  cbind(., st_coordinates(st_centroid(.)))

coef_data <- readRDS(here("Data", "Simulations", paste0("coefficients_sprange_600.rds")))

coef_data_m <- coef_data[sim == 1, ]

# data <- coef_data_m[data.table(field), on = "unique_cell_id"] %>%
#     .[, opt_N := (log(pN) - log(-pCorn * ymax * beta) - alpha) / beta]

# all_opt_N <- data[, opt_N]

# # --- for training ---#
# # hist(all_opt_N)
# N_levels <- seq(
#   quantile(all_opt_N, prob = 0.05) - 20,
#   quantile(all_opt_N, prob = 0.95) + 20,
#   length = 5
#       ) %>%
# round()

# # data <- assign_rates(st_as_sf(data), N_levels, pattern = "sequential") %>%
# data <- assign_rates(st_as_sf(data), N_levels) %>%
#   data.table()


# data %<>% .[, det_yield := gen_yield_MB(ymax, alpha, beta, rate)] %>%
#     # .[,det_yield:=b0+b1*N+b2*N^2] %>%
#     # === error ===#
#     .[, yield := det_yield * (1 + m_error)] %>%
#     # === keep the relevant vars ===#
#     .[, .(
#       unique_cell_id, opt_N, rate, m_error, det_yield, yield, alpha, beta, ymax, alpha1, alpha2, beta1, beta2, ymax1, ymax2,
#       theta_1, theta_2, subplot_id, strip_id, cluster_id_1, cluster_id_2, X, Y, geometry
#     )]

# yield_diff <- data[,yield_diff:=yield-det_yield]
# sd(yield_diff[,yield_diff])

# saveRDS(data, here("Data", "Simulations", "sample_train.dt.rds"))
# data <- readRDS(here("Data", "Simulations", "sample_train.dt.rds"))

# reg_data <- data[, .(
#     yield = mean(yield),
#     rate = mean(rate),
#     alpha = mean(alpha),
#     beta = mean(beta),
#     m_error = mean(m_error)
#     ymax = mean(ymax),
#     alpha1 = mean(alpha1),
#     alpha2 = mean(alpha2),
#     beta1 = mean(beta1),
#     beta2 = mean(beta2),
#     ymax1 = mean(ymax1),
#     ymax2 = mean(ymax2),
#     theta_1 = mean(theta_1),
#     theta_2 = mean(theta_2),
#     cluster_id_1 = mean(cluster_id_1),
#     cluster_id_2 = mean(cluster_id_2)
#   ), by = .(subplot_id, strip_id)]


# saveRDS(reg_data, here("Data", "Simulations", "sample_reg.dt.rds"))


# train_data_generation  <- function(i, field, coef_data_m) {
#   print(paste0("working on ", i, " th iteration."))
#   # i=1
#   # x=1
#   # field=field_cnn; coef_data_m=coef_data[sim == x, ]

#   data <- coef_data_m[data.table(field), on = "unique_cell_id"] %>%
#     .[, opt_N := (log(pN) - log(-pCorn * ymax * beta) - alpha) / beta]

#   all_opt_N <- data[, opt_N]

#   # --- for training ---#
#   N_levels <- seq(
#     quantile(all_opt_N, prob = 0.05) - 20,
#     quantile(all_opt_N, prob = 0.95) + 20,
#     length = 5 
#   ) %>%
#     round()

#   data <- assign_rates(st_as_sf(data), N_levels) %>% # N_levels are assigned as "rate" variable
#     data.table()

#   # /*~~~~~~~~~~~~~~~~~~~~~~*/
#   #' ### Generate yield
#   # /*~~~~~~~~~~~~~~~~~~~~~~*/

#   data %<>% .[, det_yield := gen_yield_MB(ymax, alpha, beta, rate)] %>%
#       # === error ===#
#     .[, yield := det_yield * (1 + m_error)] %>%
#       # === keep the relevant vars ===#
#     .[, .(
#       unique_cell_id, opt_N, rate, m_error, det_yield, yield, alpha, beta, ymax, alpha1, alpha2, beta1, beta2, ymax1, ymax2,
#       theta_1, theta_2, subplot_id, strip_id, cluster_id_1, cluster_id_2, X, Y, geometry
#     )] %>%
#     .[,yield_error := yield - det_yield]%>%
#     .[,sim := i]

#   return(data)
# }

# test <- train_data_generation(
#             i = 1,
#             field= field,
#             coef_data_m = coef_data[sim == 1, ]
#             )

# train_cell_dt_all <- mclapply(1:1000,
#     function(x){
#         train_data_generation(
#             i = x,
#             field= field,
#             coef_data_m = coef_data[sim == x, ]
#             )
#     }, mc.cores = detectCores() - 2
# ) %>%
# rbindlist()

# train_total <- rbindlist(train_cell_dt_all)

# saveRDS(train_total, here("Data", "Simulations", "train_dt_all"))



## the error ##
# train_total <- readRDS(here("Data", "Simulations", "train_dt_all"))

# train_total[,.(error_sd=sd(yield_error)), by=sim]%>%
#     .[,.(mean_sd_error=mean(error_sd))]

# train_total[,.(error_sd=sd(yield_error))]

# train_total[,.(mean_y=mean(yield_error)), by=sim]%>%
#     .[,.(mean(mean_y))]
    
# cor(train_total[sim==1,.(theta_1,theta_2, beta)])


# train_total[,.(mean_theta1=mean(theta_1),
#     mean_theta2=mean(theta_2)),by=sim]

# summary(train_total$opt_N)

# ggplot()+
#     geom_bar(data=train_total[sim==1,'opt_N'], aes(x=opt_N), stat="bin")

# ===================================
# Field map
# ===================================

# /*-------------------*/
#' ## Datasets
# /*-------------------*/
train_data <- readRDS(here("Data", "Simulations", "sample_train.dt.rds"))%>%
    .[,yield_error := yield - det_yield]%>%
    .[,plot_id := ceiling(subplot_id/4)]



reg_data <- readRDS(here("Data", "Simulations", "sample_reg.dt.rds"))


theme <- theme(
        plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill = "white"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.title = element_blank()
        )

####==== sample MB curve ====####
gen_yield_MB <- function(ymax, alpha, beta, N) {
  yield <- ymax * (1 - exp(alpha + beta * N))
  return(yield)
}

N_rates <- train_data[,rate]%>%unique()%>%sort()

N_seq <- 0:300

        # alpha        beta     ymax
# 1: -0.6605052 -0.01572583 10886.94

dt_sample_MB_curve <- train_data[1,.(alpha,beta,ymax)]%>%
    .[rep(1:nrow(.), each = length(N_seq)), ]%>%
    .[,N:= N_seq]%>%
    .[,Yield := gen_yield_MB(ymax, alpha, beta, N)]

sample_MB_curve <- ggplot()+
    geom_point(data=dt_sample_MB_curve, 
        aes(x=N, y=Yield))+
    theme_few()+
    xlab("N(kg/ha)") + ylab("Yield(kg/ha)")


sample_MB_curve_step <- ggplot()+
    geom_point(data=dt_sample_MB_curve, 
        aes(x=N, y=Yield))+
    theme_few()+
    xlab("N(kg/ha)") + ylab("Yield(kg/ha)")+
    geom_vline(xintercept=N_rates, linetype = "dashed", color="red")+
    geom_text(aes(x=N_rates[1], y=6000, label="N1"), size=5, parse=T)+
    geom_text(aes(x=N_rates[2], y=6000, label="N2"), size=5, parse=T)+
    geom_text(aes(x=N_rates[3], y=6000, label="N3"), size=5, parse=T)+
    geom_text(aes(x=N_rates[4], y=6000, label="N4"), size=5, parse=T)+
    geom_text(aes(x=N_rates[5], y=6000, label="N5"), size=5, parse=T)+
    geom_segment(aes(x = N_rates[1], y = dt_sample_MB_curve[N==N_rates[1],Yield],
        xend = N_rates[2], yend = dt_sample_MB_curve[N==N_rates[1],Yield]),color="blue",linetype = "dashed")+
    geom_segment(aes(x =  N_rates[2], y = dt_sample_MB_curve[N==N_rates[1],Yield], 
        xend =  N_rates[2], yend = dt_sample_MB_curve[N==N_rates[2],Yield]),
         arrow = arrow(length = unit(0.3, "cm")),color="blue")+
    geom_segment(aes(x = N_rates[2], y = dt_sample_MB_curve[N==N_rates[2],Yield],
        xend = N_rates[3], yend = dt_sample_MB_curve[N==N_rates[2],Yield]),color="blue",linetype = "dashed")+
    geom_segment(aes(x =  N_rates[3], y = dt_sample_MB_curve[N==N_rates[2],Yield], 
        xend =  N_rates[3], yend = dt_sample_MB_curve[N==N_rates[3],Yield]),
         arrow = arrow(length = unit(0.3, "cm")),color="blue")+
    geom_segment(aes(x = N_rates[3], y = dt_sample_MB_curve[N==N_rates[3],Yield],
        xend = N_rates[4], yend = dt_sample_MB_curve[N==N_rates[3],Yield]),color="blue",linetype = "dashed")+
    geom_segment(aes(x = N_rates[4], y = dt_sample_MB_curve[N==N_rates[3],Yield], 
        xend =  N_rates[4], yend = dt_sample_MB_curve[N==N_rates[4],Yield]),
         arrow = arrow(length = unit(0.3, "cm")),color="blue")+
    geom_segment(aes(x = N_rates[4], y = dt_sample_MB_curve[N==N_rates[4],Yield],
        xend = N_rates[5], yend = dt_sample_MB_curve[N==N_rates[4],Yield]),color="blue",linetype = "dashed")+
    geom_segment(aes(x =  N_rates[5], y = dt_sample_MB_curve[N==N_rates[4],Yield], 
        xend =  N_rates[5], yend = dt_sample_MB_curve[N==N_rates[5],Yield]),
         arrow = arrow(length = unit(0.3, "cm")),color="blue")


sample_MB_curve_base <- ggplot()+
    geom_point(data=dt_sample_MB_curve, 
        aes(x=N, y=Yield))+
    theme_few()+
    xlab("N(kg/ha)") + ylab("Yield(kg/ha)")+
    geom_vline(xintercept=N_rates, linetype = "dashed", color="red")+
    geom_text(aes(x=N_rates[1], y=6000, label="N1"), size=5, parse=T)+
    geom_text(aes(x=N_rates[2], y=6000, label="N2"), size=5, parse=T)+
    geom_text(aes(x=N_rates[3], y=6000, label="N3"), size=5, parse=T)+
    geom_text(aes(x=N_rates[4], y=6000, label="N4"), size=5, parse=T)+
    geom_text(aes(x=N_rates[5], y=6000, label="N5"), size=5, parse=T)+
    geom_segment(aes(x = N_rates[1], y = dt_sample_MB_curve[N==N_rates[1],Yield],
        xend = N_rates[5], yend = dt_sample_MB_curve[N==N_rates[1],Yield]),color="blue",linetype = "dashed")+
    geom_segment(aes(x =  N_rates[2], y = dt_sample_MB_curve[N==N_rates[1],Yield], 
        xend =  N_rates[2], yend = dt_sample_MB_curve[N==N_rates[2],Yield]),
         arrow = arrow(length = unit(0.3, "cm")),color="blue")+
    geom_segment(aes(x =  N_rates[3], y = dt_sample_MB_curve[N==N_rates[1],Yield], 
        xend =  N_rates[3], yend = dt_sample_MB_curve[N==N_rates[3],Yield]),
         arrow = arrow(length = unit(0.3, "cm")),color="blue")+
    geom_segment(aes(x = N_rates[4], y = dt_sample_MB_curve[N==N_rates[1],Yield], 
        xend =  N_rates[4], yend = dt_sample_MB_curve[N==N_rates[4],Yield]),
         arrow = arrow(length = unit(0.3, "cm")),color="blue")+
    geom_segment(aes(x =  N_rates[5], y = dt_sample_MB_curve[N==N_rates[1],Yield], 
        xend =  N_rates[5], yend = dt_sample_MB_curve[N==N_rates[5],Yield]),
         arrow = arrow(length = unit(0.3, "cm")),color="blue")



####==== field map ====#####
plot_map <- train_data%>%
    st_as_sf()%>%
    group_by(plot_id, strip_id, rate)%>%
    summarise()

# ggplot(plot_map)+geom_sf()

subplot_map <- train_data%>%
    st_as_sf()%>%
    group_by(subplot_id, strip_id)%>%
    summarise()

# ggplot(subplot_map)+geom_sf()


field_map <- ggplot()+
    geom_sf(data=st_as_sf(train_data),fill=NA, size=0.3)+
    geom_sf(
        data=plot_map[1,],
        fill='red',
        alpha=0.6
        )+
    geom_sf(
        data=subplot_map,
        color='blue',
        size=0.6,
        fill=NA)+
    theme

####==== example plot  ====#####

plot_field <- ggplot()+
    geom_sf(
        data=plot_map,
        fill=NA)+
     geom_sf(
        data=plot_map[1,],
        fill='red',
        alpha=0.6
        )+
    theme


plot_subplots <- ggplot()+
    geom_sf(
        data=plot_map[1,],
        fill='red',
        alpha=0.6
        )+
    geom_sf(
        data=subplot_map,
        color='blue',
        size=0.5,
        fill=NA
        )+
    theme



####==== example map of plot and subplots and cells ====#####
plot1 <- plot_map%>%filter(plot_id==1&strip_id==1)
subplots_plot1 <- subplot_map[plot1, , op = st_within]
cells_plot1 <- train_data%>%st_as_sf()%>%
    .[plot1, ,op=st_within]


ex_plot <- ggplot()+
    geom_sf(
        data=plot1,
        fill='red',
        alpha=0.6,
        # fill=NA
        )+
    theme


ex_plot_subplots <- ggplot()+
    geom_sf(
        data=plot1,
        fill='red',
        alpha=0.6,
        # fill=NA
        )+
     geom_sf(
        data=subplots_plot1,
        color='blue',
        size=2,
        fill=NA)+
    theme

ex_plot_subplots_cells <- ggplot()+
    geom_sf(
        data=plot1,
        fill='red',
        alpha=0.6,
        # fill=NA
        )+
    geom_sf(
        data=subplots_plot1,
        color='blue',
        size=2,
        fill=NA)+
    geom_sf(
        data=cells_plot1,
        size=0.8,
        fill=NA)+
    theme


####==== Experimental design ====####
field_Ndesign <- ggplot(st_as_sf(train_data)) +
    geom_sf(data=subplot_map, color='blue', size=0.8, fill=NA)+
    geom_sf(aes(fill = factor(rate)), size = 0,
        inherit.aes = FALSE, alpha=0.9) +
    scale_fill_viridis_d() +
    labs(fill = "N rates (kg/ha)")+
    theme(
        plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill = "white"),
        axis.ticks = element_blank(),
        axis.text = element_blank()
        # legend.title = element_blank()
        )


####==== yield map ====####
yield_dist <- ggplot(train_data)+
    geom_density(aes(x=yield))

##== cell level ==##
field_yield <- ggplot(st_as_sf(train_data)) +
    geom_sf(aes(fill = yield), size = 0) +
    scale_fill_viridis_c()+
    labs(fill = "Yield (kg/ha)")+
    theme(
        plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill = "white"),
        axis.ticks = element_blank(),
        axis.text = element_blank()
        # legend.title = element_blank()
        )

##== subplot yield level ==##
field_yield_subplot_sf <- left_join(subplot_map, reg_data, by=c("subplot_id", "strip_id"))

field_yield_subplot <- ggplot(field_yield_subplot_sf) +
    geom_sf(aes(fill = yield), size = 0) +
    scale_fill_viridis_c()+
    labs(fill = "Yield (kg/ha)")+
    theme(
        plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill = "white"),
        axis.ticks = element_blank(),
        axis.text = element_blank()
        # legend.title = element_blank()
        )


#> Loading required package: equatags

####==== opt_N map ====####
opt_N_dist <- ggplot(train_data)+
    geom_density(aes(x=opt_N))


field_optN <- ggplot(st_as_sf(train_data)) +
    geom_sf(aes(fill = opt_N), size = 0) +
    scale_fill_viridis_c()+
    labs(fill = "EONR (kg/ha)")+
    theme(
        plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill = "white"),
        axis.ticks = element_blank(),
        axis.text = element_blank()
        # legend.title = element_blank()
        )

####==== ymax map ====####
ymax_dist <- ggplot(train_data)+
    geom_density(aes(x=ymax))


field_ymax <- ggplot(st_as_sf(train_data)) +
    geom_sf(aes(fill = ymax), size = 0) +
    scale_fill_viridis_c()+
    ggtitle('(a) ymax')+
    theme

####==== alpha map ====####
alpha_dist <- ggplot(train_data)+
    geom_density(aes(x=alpha))


field_alpha <- ggplot(st_as_sf(train_data)) +
    geom_sf(aes(fill = alpha), size = 0) +
    scale_fill_viridis_c()+
    ggtitle('(b) alpha')+
    theme

####==== beta map ====####
beta_dist <- ggplot(train_data)+
    geom_density(aes(x=beta))


field_beta <- ggplot(st_as_sf(train_data)) +
    geom_sf(aes(fill = beta), size = 0) +
    scale_fill_viridis_c()+
    ggtitle('(c) beta')+
    theme


####==== m_error map ====#### (this should be m_error*det_yield, not just m_error)
# m_error_sf <-  left_join(field, coef_data_m, by="unique_cell_id")

field_m_error <- ggplot(st_as_sf(train_data)) +
    geom_sf(aes(fill = yield_error), size = 0) +
    scale_fill_viridis_c()+
    ggtitle('(d) u')+
    labs(fill = "Yield Error (kg/ha)")+
    theme(
        plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill = "white"),
        axis.ticks = element_blank(),
        axis.text = element_blank()
        # legend.title = element_blank()
        )


# field_alpha + field_beta + field_ymax 



####==== reg_data (aggregated by plot)====#### (<- this is not correct)
# reg_data.sf <- left_join(dplyr::select(field, subplot_id, strip_id),reg_data,
#    by = c("subplot_id", "strip_id"))


# reg.field_alpha <- ggplot(reg_data.sf) +
#     geom_sf(aes(fill = alpha), size = 0) +
#     scale_fill_viridis_c()

# reg.field_beta <- ggplot(reg_data.sf) +
#     geom_sf(aes(fill = beta), size = 0) +
#     scale_fill_viridis_c()


# reg.field_ymax <- ggplot(reg_data.sf) +
#     geom_sf(aes(fill = ymax), size = 0) +
#     scale_fill_viridis_c()

# ===================================
# Get tree 
# ===================================
# rates_ls <- reg_data[,rate]%>%unique()%>%sort()
# rates <- rates_ls[1:2]

# temp_reg_data <- copy(reg_data) %>%
#     .[rate %in% rates, ] %>%
#     .[, trt := ifelse(rate == rates[1], 0, 1)]



# sample_ct  <- causalTree(
#     yield ~ alpha + beta + ymax,
#     data=temp_reg_data,
#     treatment = temp_reg_data[, trt],
#     split.Rule = "CT",
#     cv.option = "CT",
#     split.Honest = T,
#     cv.Honest = T,
#     split.Bucket = F,
#     xval = 5,
#     cp = 0,
#     minsize = 20,
#     propensity = 0.5
#     )


# opcp <- sample_ct$cptable[,1][which.min(sample_ct$cptable[,4])]
# opfit <- prune(sample_ct, opcp)

# pdf(here("Writing","ctree_ex.pdf"))
# fancyRpartPlot(opfit,sub='')
# dev.off()


# ===================================
# Results 
# ===================================

##== distributions of the rusults of each model configuration ==##    
res_400 <- readRDS(here("Results", paste0("SimRes_sp_",400,".rds")))%>%
    .[, Method := case_when(
    model == "CF" ~ "CF_stepwise",
    model == "CF_base" ~ "CF_base",
    model == "RF" ~ "RF",
    model == "BRF" ~ "BRF"
  )]%>%
    .[,cluster_var:=NULL]%>%
    .[,model:=NULL]%>%
    .[, var_case:=factor(var_case, levels = c("aby", "abytt", "aabbyy", "aabbyytt"))]%>%
    .[, Method:=factor(Method, levels = c("RF", "BRF", "CF_stepwise", "CF_base"))]


# /*-------------------------------------------------------*/
#' ## Examination of te esimation (CF-stepwise vs CF-base)
# /*-------------------------------------------------------*/
te_res <- readRDS(here("Results", paste0 ("SimRes_optN_sp_",400,"_tre.rds")))%>%
    .[,.(method, N_index, R_squared)]%>%
    .[, N_index := case_when(
    N_index == "1-2" ~ "(N1,N2)",
    N_index == "2-3" ~ "(N2,N3)",
    N_index == "3-4" ~ "(N3,N4)",
    N_index == "4-5" ~ "(N4,N5)",
    N_index == "1-3" ~ "(N1,N3)",
    N_index == "1-4" ~ "(N1,N4)",
    N_index == "1-5" ~ "(N1,N5)"
  )]
    # setnames(names(.), c("Method", "N Rate Combinations", "R-squared"))

te_res_summary <- te_res[,.(R_squared=mean(R_squared)), by=c("method", "N_index")]%>%
    flextable(.)%>%
    set_header_labels(
    values = list(
        method="Method",
        N_index ="N Rates Combinations",
        R_squared = "R-squared"
    ))%>%
    align(align = "center", part = "header")%>%
    align_text_col(align = "center", header = TRUE, footer = TRUE)%>%
    autofit()



# /*----------------------------------*/
#' ## estimated opt N r^2 distribution
# /*----------------------------------*/
##-- Before GAM --##
dist_case <- copy(res_400)%>%
    .[, Method := case_when(
    Method == "CF_stepwise" ~ "CF-stepwise",
    Method == "CF_base" ~ "CF-base",
    Method == "RF" ~ "RF",
    Method == "BRF" ~ "BRF"
  )]%>%
    .[, Method:=factor(Method, levels = c("RF", "BRF", "CF-stepwise", "CF-base"))]%>%
    ggplot()+
    geom_density(aes(x=r2, fill=Method), alpha=0.7)+
    facet_wrap(~var_case, ncol = 1)+
    # labs(x = expression(R^2))+
    labs(x = "Mean R-squared")+
    theme_few()+
    theme(
        strip.text.x = element_text(size=12,face="bold"),
        legend.title = element_text(size=12,face="bold"),
        legend.text = element_text(size=12, face="bold"),
        legend.position = "bottom")


# aby <-  res_400[var_case=='aby']%>%
#   ggplot()+
#   geom_density(aes(x=r2, fill=Method), alpha=0.5)+
#   labs(x = "r squared")+
#   theme_few()+
#   ggtitle('aby')+
#   theme(
#       plot.title = element_text(hjust = 0.5),
#       legend.position = "bottom")




##-- After GAM --##
# dist_case_gam <- res_400%>%
#     ggplot()+
#     geom_density(aes(x=r2_g, fill=Method), alpha=0.7)+
#     facet_wrap(~var_case, ncol = 1)+
#     labs(x = "r squared")+
#     theme_few()+
#     theme(legend.position = "bottom")


# aby_gam <-  res_400[var_case=='aby']%>%
#   ggplot()+
#   geom_density(aes(x=r2_g, fill=Method), alpha=0.5)+
#   theme(legend.position="bottom")+
#   labs(x = "r squared")+
#   theme_few()




# res_200_summary <- readRDS(here("Results", paste0("sp_200_summary.rds")))%>%
#   setnames(names(.), c("r squared", "r squared(GAM)", "model", "cluster_var", "sinario"))%>%
#   .[model=="CF", model:=c("CF-stepwise", "CF-stepwise", "CF-stepwise", "CF-stepwise")]%>%
#   .[,model:=factor(model, levels= c("RF","BRF","CF-stepwise","CF-base"))]%>%
#   .[order(model),]%>%
#   setcolorder(., c("model", "sinario", "r squared", "r squared(GAM)"))%>%
#   .[,cluster_var:=NULL]


res_400_summary <- copy(res_400)%>%
    .[, .(r2 = mean(r2), r2_g = mean(r2_g), r2_Stdv =sd(r2),
    r2_g_Stdv =sd(r2_g)), by = .(Method, var_case)]%>%
    .[order(Method)]
    
    


# RF_CFbase <- res_400_summary[method=="CF-base",r2]-res_400_summary[method=="RF",r2]
# BRF_CFbase <- res_400_summary[method=="CF-base",r2]-res_400_summary[method=="BRF",r2]

# RF_CFstepwise <- res_400_summary[method=="CF-stepwise",r2]-res_400_summary[method=="RF",r2]
# BRF_CFstepwise <- res_400_summary[method=="CF-stepwise",r2]-res_400_summary[method=="BRF",r2]


##-- chech how much gam contributed to the precision accuracy? --##
# gam_improve <- res_400_summary%>%
#   .[,gam_imp := r2_g - r2]%>%
#   .[,.(gam_imp_avg = mean(gam_imp)), by=.(method)]
    


##-- create table to report including GAM results --##
# table_res400_summary_all <- res_400_summary%>%
#   .[,method:=factor(method, levels= c("RF","BRF","CF-stepwise","CF-base"))]%>%
#   .[order(method),]%>%
#   setnames(names(.), c("r squared", "r squared(GAM)", "model", "method"))%>%
#   setcolorder(., c("method", "model", "r squared", "r squared(GAM)"))%>%
#   flextable(.)%>% 
#       autofit()
#     # hline(i=seq(from=4, to=nrow(res_400)-1, by=4), border = fp_border(style = "dotted"))%>%
#     # add_header_lines(values = "Results of model evaluation(sp_range=400)")

###---- just for ML methods ----###
table_res400_summary_type1 <- copy(res_400_summary)%>%
    .[,.(Method, var_case, r2)]%>%
    dcast(var_case~Method, value.var = "r2")%>%
    setnames("var_case", "Model")%>%
    setnames(names(.), as.character(names(.)))%>%
    flextable(.)%>%
    set_header_labels(
    values = list(
        Model="",
        RF ="RF",
        BRF ="BRF",
        CF_stepwise = "CF-stepwise",
        CF_base = "CF-base"
    ))%>%
    add_header_row(
    values = c("Model","Mean R-squared"),
    colwidths = c(1,4)
    )%>%
    align(align = "center", part = "header")%>%
    # theme_vanilla()%>%
    autofit()


table_res400_summary <- copy(res_400_summary)%>%
    .[,.(Method, var_case, r2, r2_Stdv)]%>%
    setnames(names(.), c("Method", "Model", "R-squared", "Stdv."))%>%
    flextable()%>% 
    autofit()
    # hline(i=seq(from=4, to=nrow(res_400_summary)-1, by=4), border = fp_border(style = "dotted"))%>%
    # add_header_lines(values = "Results of model evaluatio (sp_range=600)")




# improve_change_400 <- copy(res_400)%>%
#   .[,diff_r2 := r2_g - r2]%>%
#   .[,avg_diff_r2 := mean(diff_r2),by=model]



###--- true treatment effect vs estimated treatment effect




# ==========================================================================
# Examination of Treatment Effects 
# ==========================================================================
# sim3 <- estimate_tre(3)
# saveRDS(sim3, here("Writing","Presentation","tre_demo.rds"))   
# r2 <- sim3$r2
sim3 <- readRDS(here("Writing","Presentation","tre_demo.rds"))
eval <- sim3$output


cf_step <- eval[Method=="CF-stepwise"]%>%
  ggplot()+
  geom_density(aes(x=value, fill=factor(variable)),alpha=0.8)+
  facet_wrap(~N_index, ncol = 2)+
  theme_few()+
  theme_few()+
      theme(
      legend.position="bottom",
      plot.title = element_text(hjust = 0.5),
      panel.background = element_rect(fill = "white"),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        strip.text.x = element_text(size=12,face="bold"),
        legend.text = element_text(size=12, face="bold"),
        )+
        labs(
        x = "Treatment Effects(kg/ha)",
        colour = "Cylinders",
        shape = "Transmission"
      )


cf_base <- eval[Method=="CF-base"]%>%
  ggplot()+
  geom_density(aes(x=value, fill=factor(variable)),alpha=0.8)+
  facet_wrap(~N_index, ncol = 2)+
  theme_few()+
  theme_few()+
      theme(
      legend.position="bottom",
      plot.title = element_text(hjust = 0.5),
      panel.background = element_rect(fill = "white"),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        strip.text.x = element_text(size=12,face="bold"),
        legend.text = element_text(size=12, face="bold"),
        )+
        labs(
        x = "Treatment effects(kg/ha)",
        colour = "Cylinders",
        shape = "Transmission"
      )































