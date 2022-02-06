---
title: "Machine Learning Methods for Site-specific Input Management"
author: Shunkei Kakimto
output: word_document
---


```r
library(knitr)
library(here)
```

```
## here() starts at /Users/shunkeikakimoto/OneDrive - University of Nebraska-Lincoln/ML_VRA
```

```r
knitr::opts_chunk$set(
	cache = FALSE,
	warning = FALSE,
	message = FALSE
	)

opts_knit$set(root.dir=here())
```



```r
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
```

# Load the data 


```r
# source(here("Codes", "functions.R"))
source(here("Codes", "0_0_functions.R"))
source(here("Codes", "functions_stepwise_vs_base.R"))
```

# Results

## Source Results and Preparation


```r
# ===================================
# Forest resutls 
# ===================================

res_forest_all_honesty <- readRDS(here("CNN_Results", "SimRes_sp_400.rds"))%>%
    setnames("model", "Method")%>%
    .[, var_case:=factor(var_case, levels = c("aby", "abytt", "aabbyy", "aabbyytt"))]%>%
    .[, Method:=factor(Method, levels = c("RF", "BRF", "CF_stepwise", "CF_base"))]%>%
    .[, Honesty := TRUE]
```

```
## Error in gzfile(file, "rb"): cannot open the connection
```

```r
res_forest_all_nohonesty <- readRDS(here("CNN_Results", "SimRes_sp_400_nohonesty.rds"))%>%
    setnames("model", "Method")%>%
    .[, var_case:=factor(var_case, levels = c("aby", "abytt", "aabbyy", "aabbyytt"))]%>%
    .[, Method:=factor(Method, levels = c("RF", "BRF", "CF_stepwise", "CF_base"))]%>%
    .[, Honesty := FALSE]
```

```
## Error in gzfile(file, "rb"): cannot open the connection
```

```r
CNN_y_eval_v1 <- readRDS(here("CNN_Results", "CNN_y_eval_v1.rds"))%>%
    .[,r2_y_cell:=NA]%>%
    .[,Honesty:=NA]%>%
    .[,.(Method, var_case, r2_y_cell, r2_y_agg, Honesty)]
```

```
## Error in gzfile(file, "rb"): cannot open the connection
```

```r
    # .[,r2_y_agg:=round(r2_y_agg,digits=3)]


res_CNN_optN <- readRDS(here("CNN_Results", "res_CNN_r2_optN.rds"))%>%
    .[,Method:="CNN"]%>%
    setnames("r2_N", "r2_agg")%>%
    .[,.(Method, var_case, r2_agg)]
```

```
## Error in gzfile(file, "rb"): cannot open the connection
```

```r
# /*-------------------------------------------------------*/
#' ## Organize the data
# /*-------------------------------------------------------*/
res_forest_all <- rbind(res_forest_all_honesty, res_forest_all_nohonesty)
```

```
## Error in rbind(res_forest_all_honesty, res_forest_all_nohonesty): object 'res_forest_all_honesty' not found
```

```r
    # .[,`:=`(
    #     r2_cell=round(r2_cell,digits=3),
    #     r2_agg=round(r2_agg,digits=3),
    #     r2_y_cell=round(r2_y_cell,digits=3),
    #     r2_y_agg=round(r2_y_agg,digits=3)
    #     )]    

res_forest_yield <- res_forest_all[Method%in%c("RF","BRF"), .(Method, var_case, r2_y_cell, r2_y_agg, Honesty)]
```

```
## Error in eval(expr, envir, enclos): object 'res_forest_all' not found
```

```r
res_forest_optN <- res_forest_all[, .(Method, var_case, r2_cell, r2_agg, Honesty)]
```

```
## Error in eval(expr, envir, enclos): object 'res_forest_all' not found
```

```r
res_yield_all <- rbind(res_forest_yield, CNN_y_eval_v1)
```

```
## Error in rbind(res_forest_yield, CNN_y_eval_v1): object 'res_forest_yield' not found
```



## EONR estimation and Yield Prediction


```r
###=== subplot-level ===###
res_y_subplot <- copy(res_yield_all)%>%
    .[Honesty%in%c(TRUE, NA),.(Method, var_case, r2_y_agg)]
```

```
## Error in copy(res_yield_all): object 'res_yield_all' not found
```

```r
res_optN_subplot <- copy(res_forest_optN)%>%
    .[Honesty==TRUE,.(Method, var_case, r2_agg)]%>%
    rbind(.,res_CNN_optN)%>%
    .[, Method:=factor(Method, levels = c("RF", "BRF", "CNN", "CF_stepwise", "CF_base"))]%>%
    .[,var_case:= case_when(
  var_case == "aby" ~ "Model 1",
  var_case == "abytt" ~ "Model 2",
  var_case == "aabbyy" ~ "Model 3",
  var_case == "aabbyytt" ~ "Model 4"
    )]
```

```
## Error in copy(res_forest_optN): object 'res_forest_optN' not found
```

```r
##== Summary Table  ==##
table_y_subplot_wide_prep <- copy(res_y_subplot)%>%
    .[,var_case:= case_when(
  var_case == "aby" ~ "Model 1",
  var_case == "abytt" ~ "Model 2",
  var_case == "aabbyy" ~ "Model 3",
  var_case == "aabbyytt" ~ "Model 4"
    )]%>%
    .[, .(r2_y_agg = mean(r2_y_agg)), by=.(Method, var_case)]%>%
    .[, r2_y_agg:= format(round(r2_y_agg,3), nsmall=3)]%>%
    dcast(var_case~Method, value.var = "r2_y_agg")
```

```
## Error in copy(res_y_subplot): object 'res_y_subplot' not found
```

```r
avg_y <- copy(res_y_subplot)%>%
    .[, .(Avg= mean(r2_y_agg)), by=Method]%>%
    .[, Avg:= format(round(Avg, 3), nsmall = 3)]
```

```
## Error in copy(res_y_subplot): object 'res_y_subplot' not found
```

```r
table_optN_subplot_wide_prep <- copy(res_optN_subplot)%>%
    .[, .(r2_agg = mean(r2_agg)), by=.(Method, var_case)]%>%
    .[, r2_agg:= format(round(r2_agg, digits=3), nsmall = 3)]
```

```
## Error in copy(res_optN_subplot): object 'res_optN_subplot' not found
```

```r
avg_optN <- copy(res_optN_subplot)%>%
    .[, .(Avg= mean(r2_agg)), by=Method]%>%
    .[, Avg:= format(round(Avg, 3), nsmall = 3)]%>%
    dcast(...~Method, value.var = "Avg")
```

```
## Error in copy(res_optN_subplot): object 'res_optN_subplot' not found
```

```r
table_optN_subplot_wide <- copy(table_optN_subplot_wide_prep)%>%
    dcast(var_case~Method, value.var = "r2_agg")%>%
    setnames("var_case", "Model")%>%
    mutate(
    across(
      everything(),
      as.character
        )
    ) %>% 
    .[,RF := paste0(RF, " (",table_y_subplot_wide_prep[,RF],")")]%>%
    .[,BRF := paste0(BRF, " (",table_y_subplot_wide_prep[,BRF],")")]%>%
    .[,CNN := paste0(CNN, " (",table_y_subplot_wide_prep[,CNN],")")]%>%
    .[,CF_stepwise := paste0(CF_stepwise, " ( - )")]%>%
    .[,CF_base := paste0(CF_base, " ( - )")]%>%
    add_row(Model = NA, RF = NA, BRF = NA, CF_stepwise = NA, CF_base = NA, .after = 4)%>%
    flextable(.)%>%
    add_body(
        Model="Avg.",
        RF = paste0(avg_optN[,RF]," (", avg_y[Method=="RF",Avg], ")"),
        BRF = paste0(avg_optN[,BRF]," (", avg_y[Method=="BRF",Avg], ")"),
        CNN = paste0(avg_optN[,CNN]," (", avg_y[Method=="CNN",Avg], ")"),
        CF_stepwise = paste0(avg_optN[,CF_stepwise]," ( - )"),
        CF_base = paste0(avg_optN[,CF_base]," ( - )"),
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
    add_footer_row(
    values = "The value in ( ) indicates yield prediction performances",
    colwidths = 6
    )%>%
    set_caption("Table1: Mean R-squared of EONR Estimates and Predicted Yield by ML Methods and Modeling Scenarios")%>%
    font(fontname="Helvetica", part = "all",
    # cs.family = fontname,
    # hansi.family = fontname,
    # eastasia.family = fontname
    )
```

```
## Error in copy(table_optN_subplot_wide_prep): object 'table_optN_subplot_wide_prep' not found
```

# 


```r
####==== CNN vs RF vs BRF ====####
res_CNN_y <- readRDS(here("CNN_Results", "CNN_y_eval_v1.rds"))%>%
    .[,.(sim, Method, var_case, r2_y_agg)]%>%
    setnames("r2_y_agg", "r2_y")
```

```
## Error in gzfile(file, "rb"): cannot open the connection
```

```r
    # .[,r2_y_agg:=round(r2_y_agg,digits=3)]

res_CNN_optN <- readRDS(here("CNN_Results", "res_CNN_r2_optN.rds"))%>%
    setnames("r2_N", "r2")
```

```
## Error in gzfile(file, "rb"): cannot open the connection
```

```r
res_CNN <- res_CNN_y[res_CNN_optN, on=c("sim", "var_case")]%>%
    dcast(sim + var_case ~ Method, value.var = c("r2", "r2_y"))%>%
    .[,var_case:= case_when(
  var_case == "aby" ~ "Model 1",
  var_case == "abytt" ~ "Model 2",
  var_case == "aabbyy" ~ "Model 3",
  var_case == "aabbyytt" ~ "Model 4"
    )]
```

```
## Error in is.data.table(data): object 'res_CNN_y' not found
```

```r
res_CNN_RF_BRF <- readRDS(here("CNN_Results", "SimRes_sp_400.rds"))%>%
    .[,!c("r2_cell", "r2_y_cell")]%>%
    .[model%in%c("RF","BRF"),] %>%
    .[, var_case:=factor(var_case, levels = c("aby", "abytt", "aabbyy", "aabbyytt"))]%>%
    setnames("model", "Method")%>%
    setnames(c("r2_agg", "r2_y_agg"), c("r2", "r2_y"))%>%
    .[,var_case:= case_when(
        var_case == "aby" ~ "Model 1",
        var_case == "abytt" ~ "Model 2",
        var_case == "aabbyy" ~ "Model 3",
        var_case == "aabbyytt" ~ "Model 4"
    )]%>%
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
```

```
## Error in gzfile(file, "rb"): cannot open the connection
```

```r
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
    .[,.(Model, RF, BRF, CNN, Total)]%>%
    flextable()%>%
    align(align = "center", part = "all")%>%
    align(j=1,  align = "left", part = "all")%>%
    autofit()
```

```
## Error in is.data.frame(data): object 'res_CNN_RF_BRF' not found
```

```r
# save_as_image(x = table_optN_subplot_wide, path = here("Writing/OFE Presentation/Res_y_cound.pdf"))
```



# Examination of Treatment Effects


```r
# ======================================================================
# Examination of te esimation (CF-stepwise vs CF-base)
# ======================================================================

te_res <- readRDS(here("CNN_Results", "res_eval_tre_effects.rds"))%>%
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
```

```
## Error in gzfile(file, "rb"): cannot open the connection
```

```r
# te_res <- readRDS(here("CNN_Results", "res_eval_tre_effects_no_honesty.rds"))%>%
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
```

```
## Error in is.data.frame(data): object 'te_res' not found
```

```r
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
```








