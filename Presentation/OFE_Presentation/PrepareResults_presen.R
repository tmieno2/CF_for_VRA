## -----------------------------------------------------------------------------
library(knitr)
library(here)
knitr::opts_chunk$set(
	cache = FALSE,
	warning = FALSE,
	message = FALSE
	)

opts_knit$set(root.dir=here())


## ---- cache = FALSE-----------------------------------------------------------
# === packages ===#
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
library(webshot2) # remotes::install_github("rstudio/webshot2")
# library(sparkline)
# library(htmltools)
# library(htmlwidgets)
# library(equatags)
# library(equatiomatic)



## ----source, results = "hide", fig.show='hide', cache = FALSE-----------------
# source(here("Codes", "functions.R"))
source(here("Codes", "0_0_functions.R"))
source(here("Codes", "functions_stepwise_vs_base.R"))


## ----source-results, message=FALSE, warning=FALSE-----------------------------

# ===================================
# Forest resutls 
# ===================================

res_forest_all_honesty <- readRDS(here("Results", "CNN", "SimRes_sp_400.rds"))%>%
    setnames("model", "Method")%>%
    .[, var_case:=factor(var_case, levels = c("aby", "abytt", "aabbyy", "aabbyytt"))]%>%
    .[, Method:=factor(Method, levels = c("RF", "BRF", "CF_stepwise", "CF_base"))]%>%
    .[, Honesty := TRUE]


res_forest_all_nohonesty <- readRDS(here("Results", "CNN", "SimRes_sp_400_nohonesty.rds"))%>%
    setnames("model", "Method")%>%
    .[, var_case:=factor(var_case, levels = c("aby", "abytt", "aabbyy", "aabbyytt"))]%>%
    .[, Method:=factor(Method, levels = c("RF", "BRF", "CF_stepwise", "CF_base"))]%>%
    .[, Honesty := FALSE]


CNN_y_eval_v1 <- readRDS(here("Results", "CNN", "CNN_y_eval_v1.rds"))%>%
    .[,r2_y_cell:=NA]%>%
    .[,Honesty:=NA]%>%
    .[,.(Method, var_case, r2_y_cell, r2_y_agg, Honesty)]
    # .[,r2_y_agg:=round(r2_y_agg,digits=3)]


res_CNN_optN <- readRDS(here("Results", "CNN", "res_CNN_r2_optN.rds"))%>%
    .[,Method:="CNN"]%>%
    setnames("r2_N", "r2_agg")%>%
    .[,.(Method, var_case, r2_agg)]

# /*-------------------------------------------------------*/
#' ## Organize the data
# /*-------------------------------------------------------*/
res_forest_all <- rbind(res_forest_all_honesty, res_forest_all_nohonesty)
    # .[,`:=`(
    #     r2_cell=round(r2_cell,digits=3),
    #     r2_agg=round(r2_agg,digits=3),
    #     r2_y_cell=round(r2_y_cell,digits=3),
    #     r2_y_agg=round(r2_y_agg,digits=3)
    #     )]    

res_forest_yield <- res_forest_all[Method%in%c("RF","BRF"), .(Method, var_case, r2_y_cell, r2_y_agg, Honesty)]

res_forest_optN <- res_forest_all[, .(Method, var_case, r2_cell, r2_agg, Honesty)]

res_yield_all <- rbind(res_forest_yield, CNN_y_eval_v1)



## ---- dependson = "source-results"--------------------------------------------

res_optN_subplot <- copy(res_forest_optN)%>%
    .[Honesty==TRUE,.(Method, var_case, r2_agg)]%>%
    rbind(.,res_CNN_optN)%>%
    .[, Method:=factor(Method, levels = c("RF", "BRF", "CNN", "CF_stepwise", "CF_base"))]


##== Summary Table  ==##
table_optN_subplot_long <- copy(res_optN_subplot)%>%
    .[, .(r2_agg = mean(r2_agg), r2_Stdv = sd(r2_agg)), by=.(Method, var_case)]%>%
    setnames(names(.), c("Method", "Model", "R-squared", "Stdv."))%>%
    flextable()%>% 
    autofit()


table_optN_subplot_wide_prep <- copy(res_optN_subplot)%>%
    .[, .(r2_agg = mean(r2_agg)), by=.(Method, var_case)]%>%
    .[, r2_agg:= format(round(r2_agg, digits=3), nsmall = 3)]%>%
    .[,var_case:= case_when(
      var_case == "aby" ~ "Model 1",
      var_case == "abytt" ~ "Model 2",
      var_case == "aabbyy" ~ "Model 3",
      var_case == "aabbyytt" ~ "Model 4"
    )]

avg_optN <- copy(res_optN_subplot)%>%
    .[, .(Avg= mean(r2_agg)), by=Method]%>%
    .[, Avg:= format(round(Avg, 3), nsmall = 3)]%>%
    dcast(...~Method, value.var = "Avg")


table_optN_subplot_wide <- copy(table_optN_subplot_wide_prep)%>%
    dcast(var_case~Method, value.var = "r2_agg")%>%
    setnames("var_case", "Model")%>%
    mutate(
    across(
      everything(),
      as.character
        )
    ) %>% 
    add_row(Model = NA, RF = NA, BRF = NA, CF_stepwise = NA, CF_base = NA, .after = 4)%>%
    flextable(.)%>%
    add_body(
        Model="Avg.",
        RF = avg_optN[,RF],
        BRF = avg_optN[,BRF],
        CNN = avg_optN[,CNN],
        CF_stepwise = avg_optN[,CF_stepwise],
        CF_base = avg_optN[,CF_base], 
        top = FALSE
        )%>%
    theme_booktabs()%>%
    set_header_labels(values = list(
        Model = "Model",
        RF = "RF",
        BRF = "BRF",
        CNN = "CNN",
        CF_stepwise = "CF-stepwise",
        CF_base = "CF-base"
    ))%>%
    align(align = "center", part = "all")%>%
    align(j=1,  align = "left", part = "all")%>%
    # width(width = 2)%>%
    autofit()%>%
    set_caption("Table 1: Mean R-squared of EONR Estimates by ML Methods and Modeling Scenarios")%>%
    font(fontname="Helvetica", part = "all",
    # cs.family = fontname,
    # hansi.family = fontname,
    # eastasia.family = fontname
    )



## ---- dependson = "source-results"--------------------------------------------
####==== CNN vs RF vs BRF ====####
res_CNN_y <- readRDS(here("Results", "CNN", "CNN_y_eval_v1.rds"))%>%
    .[,.(sim, Method, var_case, r2_y_agg)]%>%
    setnames("r2_y_agg", "r2_y")
    # .[,r2_y_agg:=round(r2_y_agg,digits=3)]

res_CNN_optN <- readRDS(here("Results", "CNN", "res_CNN_r2_optN.rds"))%>%
    setnames("r2_N", "r2")

res_CNN <- res_CNN_y[res_CNN_optN, on=c("sim", "var_case")]%>%
    dcast(sim + var_case ~ Method, value.var = c("r2", "r2_y"))


res_CNN_RF_BRF <- readRDS(here("Results", "CNN", "SimRes_sp_400.rds"))%>%
    .[,!c("r2_cell", "r2_y_cell")]%>%
    .[model%in%c("RF","BRF"),] %>%
    .[, var_case:=factor(var_case, levels = c("aby", "abytt", "aabbyy", "aabbyytt"))]%>%
    setnames("model", "Method")%>%
    setnames(c("r2_agg", "r2_y_agg"), c("r2", "r2_y"))%>%
    dcast(sim + var_case ~ Method, value.var = c("r2", "r2_y"))%>%
    .[res_CNN, on=c("sim", "var_case")]%>%
    setnames("var_case", "Model")%>%
    .[,.(sim, Model, r2_RF, r2_BRF, r2_CNN, r2_y_RF, r2_y_BRF, r2_y_CNN)]%>%
    .[,which_optN := 
        ifelse(r2_BRF>r2_RF & r2_BRF>r2_CNN, "BRF",
            ifelse(r2_RF>r2_BRF & r2_RF>r2_CNN, "RF",
                ifelse(r2_CNN>r2_BRF & r2_CNN>r2_RF, "CNN", NA)
                )
            )
        ]%>%
    .[,which_y := 
        ifelse(r2_y_BRF>r2_y_RF & r2_y_BRF>r2_y_CNN, "BRF",
            ifelse(r2_y_RF>r2_y_BRF & r2_y_RF>r2_y_CNN, "RF",
                ifelse(r2_y_CNN>r2_y_BRF & r2_y_CNN>r2_y_RF, "CNN", NA)
                )
            )
        ]%>%
    .[,index_consist := ifelse(which_optN==which_y, 1, 0)]%>%
    .[,cons_y_optN := ifelse(index_consist==1, which_optN, NA)]%>%
    # RF vs BRF
    .[,r2_y_ratio_RF_BRF := r2_y_RF/r2_y_BRF]%>%
    .[,r2_optN_ratio_RF_BRF := r2_RF/r2_BRF]%>%
    # CNN vs RF
    .[,r2_y_ratio_CNN_RF := r2_y_CNN/r2_y_RF]%>%
    .[,r2_optN_ratio_CNN_RF := r2_CNN/r2_RF]%>%
    # CNN vs BRF 
    .[,r2_y_ratio_CNN_BRF := r2_y_CNN/r2_y_BRF]%>%
    .[,r2_optN_ratio_CNN_BRF := r2_CNN/r2_BRF]



# Summary Table 
sumry_res_CNN_RF_BRF <- res_CNN_RF_BRF%>%
    .[,.(
        count_BRF = nrow(.SD[cons_y_optN=="BRF"]),
        count_RF = nrow(.SD[cons_y_optN=="RF"]),
        count_CNN = nrow(.SD[cons_y_optN=="CNN"]),
        count_y_BRF = nrow(.SD[which_y=="BRF"]),
        count_y_RF = nrow(.SD[which_y=="RF"]),
        count_y_CNN = nrow(.SD[which_y=="CNN"]),
        Total= sum(index_consist)
        ), by=Model]%>%
    .[,BRF := paste0(count_BRF, " (",count_y_BRF,")")]%>%
    .[,RF := paste0(count_RF, " (",count_y_RF,")")]%>%
    .[,CNN := paste0(count_CNN, " (",count_y_CNN,")")]%>%
    .[,Model:= case_when(
      Model == "aby" ~ "Model 1",
      Model == "abytt" ~ "Model 2",
      Model == "aabbyy" ~ "Model 3",
      Model == "aabbyytt" ~ "Model 4"
    )]%>%
    .[,.(Model, RF, BRF, CNN, Total)]%>%
    flextable()%>%
    align(align = "center", part = "all")%>%
    align(j=1,  align = "left", part = "all")%>%
    set_caption("Table 2: Count table about the number of simulation rounds where an identical ML method showed the highest R-squared in both EONR estimation and yield prediction in that round")%>%
    add_footer_row(
    values = "The number in ( ) indicates the number of rounds with the highest R-squared in yield prediction.",
    colwidths = 5
    )%>%
    font(fontname="Helvetica", part = "all",
    # cs.family = fontname,
    # hansi.family = fontname,
    # eastasia.family = fontname
    )%>%
    autofit()



## -----------------------------------------------------------------------------
# ======================================================================
# Examination of te esimation (CF-stepwise vs CF-base)
# ======================================================================

te_res <- readRDS(here("Results", "res_eval_tre_effects.rds"))%>%
    .[,.(method, N_index, R_squared)]%>%
    .[, N_index := case_when(
    N_index == "1-2" ~ "(N1,N2)",
    N_index == "2-3" ~ "(N2,N3)",
    N_index == "3-4" ~ "(N3,N4)",
    N_index == "4-5" ~ "(N4,N5)",
    N_index == "1-3" ~ "(N1,N3)",
    N_index == "1-4" ~ "(N1,N4)",
    N_index == "1-5" ~ "(N1,N5)"
  )]%>%
    .[,sim:=rep(1:1000, each=8)]



# te_res <- readRDS(here("Results", "res_eval_tre_effects_no_honesty.rds"))%>%
#     .[,.(method, N_index, R_squared)]%>%
#     .[, N_index := case_when(
#     N_index == "1-2" ~ "(N1,N2)",
#     N_index == "2-3" ~ "(N2,N3)",
#     N_index == "3-4" ~ "(N3,N4)",
#     N_index == "4-5" ~ "(N4,N5)",
#     N_index == "1-3" ~ "(N1,N3)",
#     N_index == "1-4" ~ "(N1,N4)",
#     N_index == "1-5" ~ "(N1,N5)"
#   )]%>%
#     .[,sim:=rep(1:1000, each=8)]
# diff_2 <- function(x) c(0,diff(x))
# test <- te_res[method=="CF-stepwise", ]%>%
#     .[, diff := diff_2(R_squared), by="sim"]

# test[, .SD[diff<0,], by=sim]%>%.[,sim]%>%table()

# te_res[method=="CF-stepwise"&sim==8,]


##== Treatment Effects Summary Table ==##

te_res_summary <- te_res[,.(R_squared=mean(R_squared)), by=c("method", "N_index")]%>%
    .[, R_squared:= round(R_squared,3)]%>%
    flextable(.)%>%
    set_header_labels(
    values = list(
        method="Method",
        N_index ="N Rates Combinations",
        R_squared = "R-squared"
    ))%>%
    align(align = "center", part = "all")%>%
    align(j=1,  align = "left", part = "all")%>%
    # align(align = "center", part = "header")%>%
    # align_text_col(align = "center", header = TRUE, footer = TRUE)%>%
    autofit()


##== Graph ==##
#== tre calculation ==#

# reg_data_all <- readRDS(here("Data/CNN_Simulations/reg_data.rds"))
# test_agg_data_all <- readRDS(here("Data/CNN_Simulations/test_agg_data.rds"))

# # estimate_tre() is defined in "functions_stepwise_vs_base.R"
# sim_ex <- estimate_tre(x=32, reg_data_all=reg_data_all, test_data_all=test_agg_data_all)

# sim_ex$r2

# saveRDS(sim_ex, here("Data","CNN_writing","tre_demo.rds"))

sim_output <- readRDS(here("Data","CNN_writing","tre_demo.rds"))

# r2 <-sim_output$r2
eval <- sim_output$output

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

