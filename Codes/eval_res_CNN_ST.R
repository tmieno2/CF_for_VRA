library(here)
library(ggplot2)
library(grf)
library(sf)
library(data.table)
library(tidyverse)
library(ggthemes)
library(gridExtra)
library(parallel)

#--- source functions ---#
source(here("Codes", "cnn_related_functions.R")) 

#--- field ---#
field_cnn <- readRDS(here("Data", "CNN_Simulations", "cnn_field_padding.rds"))

# === prices ===#
pCorn <- price_table[2, pCorn]
pN <- price_table[2, pN]


# /*=================================================*/
#' # Preparation: Load the Results 
# /*=================================================*/

#=== load the source data for training and testing ===#
# training <- readRDS(here("Data", "CNN_Simulations", "reg_raw_data.rds"))
# testing <- readRDS(here("Data", "CNN_Simulations", "test_raw_data.rds"))


# training_cell <- readRDS(here("Data", "CNN_Simulations", "reg_raw_data.rds"))

training_subplot <- readRDS(here("Data", "CNN_Simulations", "reg_data.rds"))%>%
	setnames("unique_cell_id", "id")%>%
	.[padding==1, ]


testing_subplot <- readRDS(here("Data", "CNN_Simulations", "test_agg_data.rds"))%>%
	setnames("unique_cell_id", "id")%>%
	.[padding==1, ]


nonpadding_id <- testing_subplot$id%>%unique()




##== for comparison: CF, BRF, RF resutls ==##
res_forest <- readRDS(here("CNN_Results", "SimRes_sp_400.rds"))



#=== load the CNN_ST results ===#
cnn_st_aby_source <- read_csv(here("CNN_Results", "alldata_model1_CNN_ST.csv"))%>%
	data.table()%>%
	setnames("pred", "yield_hat")%>%
	.[,.(id, rate, yield_hat, sim)]%>%
	.[, var_case:= "aby"]





#=== combine CNN resutls into one dataset ===#
res_CNN_ST <- copy(cnn_st_aby_source)%>%
	# rbind(aby_source_v1, abytt_source_v1, aabbyy_source_v1, aabbyytt_source_v1)%>%
  .[, c("subplot_id", "strip_id") := tstrsplit(id, "_", fixed=TRUE)]%>%
  .[,id := paste0(strip_id,"_",subplot_id)]%>%
  .[id %in% nonpadding_id,]%>%
  .[,.(sim, id, rate, yield_hat, var_case)]%>%
  # .[, var_case:=factor(var_case, levels = c("aby", "abytt", "aabbyy", "aabbyytt"))]%>%
  .[, model:="CNN_ST"]



# /*--------------------------------------*/
#' ## Visualize Yield Response Function
# /*--------------------------------------*/

ggplot(data=res_CNN_ST[sim==1 & var_case=="aby" & id%in%id[seq(from=1, length.out=100)]],
 		aes(x=rate, y=yield_hat, colour=factor(id)))+ 
  		geom_point(size=0.5)+
  		geom_smooth(method=lm,se=FALSE, size=0.5)+
  	 	theme(legend.title = element_blank(),
	 		legend.position = "none")



# /*=================================================*/
#' # Performance of Yield prediction
# /*=================================================*/

###=== R^2 calculation ===###
CNN_ST_y_eval <- testing_subplot[,.(id, sim, yield, rate)]%>%
	res_CNN_ST[., on=c("sim", "id","rate")]%>%
	.[,.(r2_y=summary(lm(yield ~ yield_hat))$r.squared), by=.(sim,model)]


# ggplot(CNN_y_eval[sim==1&var_case=="aby"])+
# 	geom_point(aes(x=yield_hat, y=yield))+
# 	geom_abline(slope=1, intercept=0, color="red")

CNN_ST_y_eval[,.(r2_y=mean(r2_y)), by=.(model,var_case)]


# res_CNN_ST[sim==1,id]%>%unique()%>%length()
# testing_subplot[sim==1,id]%>%unique()%>%length()

# res_CNN_ST[sim==1, rate]%>%unique()%>%sort()
# testing_subplot[sim==1, rate]%>%unique()%>%sort()



###=== density plot of r2 of yield prediction ===###
y_eval_all <- res_forest[model%in%c("BRF", "RF")&var_case%in%c("aby"),.(sim, model, r2_y_agg)]%>%
	setnames("r2_y_agg", "r2_y")%>%
	rbind(.,CNN_ST_y_eval)%>%
	.[order(model),]

y_eval_all[,.(r2_y=mean(r2_y)), by=.(model)]

ggplot(y_eval_all)+
	geom_density(aes(x=r2_y, fill=model), alpha=0.6)+
	# facet_wrap(~var_case, ncol = 1)+
	labs(title="Density plot of R^2 of yield predictions based on CNN, RF and BRF")

demo <- testing_subplot[,.(id, sim, yield, rate)]%>%
	res_CNN_ST[., on=c("sim", "id","rate")]

ggplot(demo[sim==1,])+
	geom_density(aes(x=yield_hat), fill="blue", alpha=0.6)+
	geom_density(aes(x=yield), fill="green", alpha=0.6)


demo_res1 <- demo[sim==1,]%>%
	.[,.(r2_y=summary(lm(yield ~ yield_hat))$r.squared)]

ggplot(demo)+
	geom_point(aes(x=yield, y=yield_hat))




test <-y_eval_all%>%
	dcast(sim~model, value.var="r2_y")%>%
	.[, .(RF_outperform= mean(RF>=CNN_ST))]






	

# /*----------------------------------*/
#' ## Performance of Opt N estimation
# /*----------------------------------*/



###=== get slope of the yield response function ===###

all_var_case <- c("aby", "abytt", "aabbyy", "aabbyytt")

cal_slope <- function(case){
	# case="aby"
	demo <- copy(res_CNN_ST)%>%
	.[var_case==case, ]%>%
	.[, .(slope = coef(lm(yield_hat~rate))["rate"]), by= .(id, sim,var_case)]%>%
	.[order(sim)]

}

slope_all <- mclapply(all_var_case[[1]], cal_slope, mc.cores=detectCores()-2)%>%
	rbindlist()

slope_all <- lapply(all_var_case[[1]], cal_slope)%>%
	rbindlist()


saveRDS(slope_all, here("CNN_Results", "cnn_st_response_slope_aby.rds"))




pN_pC_ratio <- pN/pCorn

CNN_opt_N_eval_v1 <- copy(slope_all)%>%
	res_CNN_ST[.,on=c("sim", "id", "var_case")]%>%
	.[, opt_N_hat:=lapply(.SD, function(x) ifelse(slope<pN_pC_ratio, min(rate), max(rate))), by=.(id, sim, var_case)]%>%
	.[rate==opt_N_hat,]%>%
	.[testing_subplot[,.(sim, id, opt_N)], on=c("sim", "id")]


any(CNN_opt_N_eval_v1$slope > pN_pC_ratio) #FALSE


res_CNN_optN <- CNN_opt_N_eval_v1%>%
	.[,.(r2_N=summary(lm(opt_N ~ opt_N_hat))$r.squared), by=.(sim, var_case),]


saveRDS(res_CNN_optN, here("CNN_Results", "res_CNN_r2_optN.rds"))



res_CNN_optN_summary <- res_CNN_optN%>%
	.[,.(r2_N=mean(r2_N)), by=var_case]









# aby_opt_N <- copy(aby)  %>%
# 	.[, pi_hat := pCorn * yield_hat - pN * rate]%>%
# 	.[, .SD[pi_hat == max(pi_hat), ], by = .(id, sim)] %>%
#     .[, .(sim, id, rate)] %>%
#     setnames("rate", "opt_N_hat")%>%
#     .[testing[,.(sim, id, opt_N)], on=c("sim", "id")]








