# /*=================================================*/
#' # Preparation
# /*=================================================*/

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
source(here("Gitcontrolled","Codes", "0_0_functions.R")) 

#--- field ---#
field_cnn <- readRDS(here("Shared", "Data", "for_Simulations", "field_padding.rds"))

# === prices ===#
pCorn <- price_table[2, pCorn]
pN <- price_table[2, pN]


#=== load the source data for training and testing ===#
training_agg <- readRDS(here("Shared", "Data", "for_Simulations", "reg_data.rds"))%>%
	setnames("unique_cell_id", "id")%>%
	.[padding==1, ]


testing_v1 <- readRDS(here("Shared", "Data", "for_Simulations", "test_agg_data.rds"))%>%
	setnames("unique_cell_id", "id")%>%
	.[padding==1, ]


nonpadding_id <- testing_v1$id%>%unique()




##== for comparison: CF, BRF, RF resutls ==##
# res_forest <- readRDS(here("Shared", "Results", "SimRes_sp_400.rds"))



#=== load the CNN results Ver1===#
aby_source_v1 <- read_csv(here("Shared", "Results", "alldata_model1.csv"))%>%
	data.table()%>%
	setnames("pred", "yield_hat")%>%
	.[,.(id, rate, yield_hat, sim)]%>%
	.[, var_case:= "aby"]

abytt_source_v1 <- read_csv(here("Shared", "Results", "alldata_model2.csv"))%>%
	data.table()%>%
	setnames("pred", "yield_hat")%>%
	.[,.(id, rate, yield_hat, sim)]%>%
	.[, var_case:= "abytt"]

aabbyy_source_v1 <- read_csv(here("Shared", "Results", "alldata_model3.csv"))%>%
	data.table()%>%
	setnames("pred", "yield_hat")%>%
	.[,.(id, rate, yield_hat, sim)]%>%
	.[, var_case:= "aabbyy"]

aabbyytt_source_v1 <- read_csv(here("Shared", "Results", "alldata_model4.csv"))%>%
	data.table()%>%
	setnames("pred", "yield_hat")%>%
	.[,.(id, rate, yield_hat, sim)]%>%
	.[, var_case:= "aabbyytt"]





#=== combine CNN resutls into one dataset ===#
res_CNN_v1 <- rbind(aby_source_v1, abytt_source_v1, aabbyy_source_v1, aabbyytt_source_v1)%>%
  .[, c("subplot_id", "strip_id") := tstrsplit(id, "_", fixed=TRUE)]%>%
  .[,id := paste0(strip_id,"_",subplot_id)]%>%
  .[id %in% nonpadding_id,]%>%
  .[,.(sim, id, rate, yield_hat, var_case)]%>%
  .[, var_case:=factor(var_case, levels = c("aby", "abytt", "aabbyy", "aabbyytt"))]%>%
  .[, model:="CNN"]


# plot_yield_res <- function(res_data, start, case){

# 	ggplot(data=res_data[sim==1 & var_case==case & id%in%id[seq(from=start, length.out=100)]],
#  		aes(x=rate, y=yield_hat, colour=factor(id)))+ 
#   		geom_point(size=0.5)+

#   		geom_smooth(method=lm,se=FALSE, size=0.5)+
#   	 	theme(legend.title = element_blank(),
# 	 		legend.position = "none")

# }

# seq_id <- seq(from=1, by=100, to=res_CNN_v1$id%>%unique()%>%length())

# aby_plot <- list()
# for (i in 1:length(seq_id)){

# 	plot[[i]] <- plot_yield_res(res_CNN_v2, seq_id[i], case="aby")
# }

# grid.arrange(
#   plot[[1]], plot[[2]], plot[[3]], plot[[4]], plot[[5]], plot[[6]], plot[[7]], plot[[8]], plot[[9]], plot[[10]],
#   plot[[11]], plot[[12]], plot[[13]], plot[[14]], plot[[15]], plot[[16]],
#   # plot[[17]],plot[[19]],plot[[20]],
#   # plot[[21]],plot[[22]],plot[[23]],plot[[24]],plot[[25]],plot[[26]],plot[[27]],plot[[28]],plot[[29]],plot[[30]],plot[[31]],
#   ncol = 3
#   )


# /*----------------------------------*/
#' ## Performance of Yield prediction
# /*----------------------------------*/

####==== Version 1 ====####
CNN_y_eval_v1 <- testing_v1[,.(id, sim, yield, rate)]%>%
	res_CNN_v1[., on=c("sim", "id","rate")]%>%
	.[,.(r2_y=summary(lm(yield ~ yield_hat, data =.SD))$r.squared), by=.(sim, var_case, model)]

# ggplot(CNN_y_eval_v1[sim==1&var_case=="aby"])+
# 	geom_point(aes(x=yield_hat, y=yield))+
# 	geom_abline(slope=1, intercept=0, color="red")

CNN_y_eval_v1[,.(r2_y=mean(r2_y)), by=var_case]


# res_CNN_v1[sim==1,id]%>%unique()%>%length()
# testing_v1[sim==1,id]%>%unique()%>%length()

# res_CNN_v1[sim==1, rate]%>%unique()%>%sort()
# testing_v1[sim==1, rate]%>%unique()%>%sort()










CNN_y_eval_v2[var_case=="aby",]%>%
	.[which.max(r2_y),]

CNN_y_eval_v2[var_case=="aby",]%>%
	.[which.min(r2_y),]


ggplot(CNN_y_eval_v2[sim==1&var_case=="aby"])+
	geom_point(aes(x=yield_hat, y=yield))+
	geom_abline(slope=1, intercept=0, color="red")




#'
#' sim615 is pretty good , sim909 is pretty bad
#' Check the differences in the spatial distribution in those case
#' 


training_cell_sim615_sf <- field_cnn%>%
	select(unique_cell_id)%>%
	left_join(., training_cell[sim==615], by="unique_cell_id")%>%
	na.omit()


# ggplot()+geom_sf(data=field_cnn, aes(factor(padding)))

grid.arrange(
	ggplot()+geom_sf(data=training_cell_sim615_sf, aes(fill=alpha),size=0)+ggtitle('alpha')+scale_fill_viridis_c(),
	ggplot()+geom_sf(data=training_cell_sim615_sf, aes(fill=beta),size=0)+ggtitle('beta')+scale_fill_viridis_c(),
	ggplot()+geom_sf(data=training_cell_sim615_sf, aes(fill=ymax),size=0)+ggtitle('ymax')+scale_fill_viridis_c(),
	ggplot()+geom_sf(data=training_cell_sim615_sf, aes(fill=yield),size=0)+ggtitle('yield')+scale_fill_viridis_c(),
	ncol=2
	)


training_cell_sim909_sf <- field_cnn%>%
	select(unique_cell_id)%>%
	left_join(., training_cell[sim==909], by="unique_cell_id")%>%
	na.omit()

grid.arrange(
	ggplot()+geom_sf(data=training_cell_sim909_sf, aes(fill=alpha),size=0)+ggtitle('alpha')+scale_fill_viridis_c(),
	ggplot()+geom_sf(data=training_cell_sim909_sf, aes(fill=beta),size=0)+ggtitle('beta')+scale_fill_viridis_c(),
	ggplot()+geom_sf(data=training_cell_sim909_sf, aes(fill=ymax),size=0)+ggtitle('ymax')+scale_fill_viridis_c(),
	ggplot()+geom_sf(data=training_cell_sim909_sf, aes(fill=yield),size=0)+ggtitle('yield')+scale_fill_viridis_c(),
	ncol=2
	)



###=== density plot of r2 of yield prediction ===###
y_eval_all <- res_forest[model%in%c("BRF", "RF")&var_case%in%c("aby","abytt"),.(sim, var_case, model, r2_y_agg)]%>%
	setnames("r2_y_agg", "r2_y")%>%
	rbind(.,CNN_y_eval_v1)%>%
	.[order(var_case,model),]

y_eval_all[,.(r2_y=mean(r2_y)), by=.(var_case,model)]



ggplot(y_eval_all)+
	geom_density(aes(x=r2_y, fill=model), alpha=0.6)+
	facet_wrap(~var_case, ncol = 1)+
	labs(title="Density plot of R^2 of yield predictions based on CNN, RF and BRF")


test <-y_eval_all%>%
	dcast(sim+var_case~model, value.var="r2_y")%>%
	.[, .(cnn= mean(RF>=CNN)), by=var_case]






	

# /*----------------------------------*/
#' ## Performance of Opt N estimation
# /*----------------------------------*/

####==== Version 1 ====####

###=== get slope of the yield response function ===###
# slope_v1 <- copy(res_CNN_v1)%>%
# 	.[, .(slope = coef(lm(yield_hat~rate))["rate"]), by= .(id, sim, var_case)]


all_var_case <- c("aby", "abytt", "aabbyy", "aabbyytt")

cal_slope <- function(case){
	# case="aby"
	demo <- copy(res_CNN_v1)%>%
	.[var_case==case, ]%>%
	.[, .(slope = coef(lm(yield_hat~rate))["rate"]), by= .(id, sim,var_case)]%>%
	.[order(sim)]

}

slope_all <- mclapply(all_var_case, cal_slope, mc.cores=detectCores()-2)%>%
	rbindlist()

# saveRDS(slope_all, here("CNN_Results", "cnn_response_slope_all.rds"))


slope_all <- readRDS(here("Shared", "Results", "cnn_response_slope_all.rds"))

pN_pC_ratio <- pN/pCorn

CNN_opt_N_eval_v1 <- copy(slope_all)%>%
	res_CNN_v1[.,on=c("sim", "id", "var_case")]%>%
	.[, opt_N_hat:=lapply(.SD, function(x) ifelse(slope<pN_pC_ratio, min(rate), max(rate))), by=.(id, sim, var_case)]%>%
	.[rate==opt_N_hat,]%>%
	.[testing_v1[,.(sim, id, opt_N)], on=c("sim", "id")]


any(CNN_opt_N_eval_v1$slope > pN_pC_ratio) #FALSE


res_CNN_optN <- CNN_opt_N_eval_v1%>%
	.[,`:=`(
		r2_optN = summary(lm(opt_N ~ opt_N_hat))$r.squared,
		# rmse_optN = sqrt(mean((opt_N - opt_N_hat)^2))
		rmse_optN = sqrt(mean((opt_N - opt_N_hat)^2)) #sqrt(mean((actual - preds)^2))
		),by=.(sim, var_case)] %>%
	.[,.(sim, model, var_case, r2_optN, rmse_optN)] %>%
  unique(.,by = c("sim","model","var_case"))

saveRDS(res_CNN_optN, here("Shared", "Results", "res_CNN_r2_optN.rds"))



res_CNN_optN_summary <- res_CNN_optN%>%
	.[,.(r2_N=mean(r2_N)), by=var_case]









# aby_opt_N <- copy(aby)  %>%
# 	.[, pi_hat := pCorn * yield_hat - pN * rate]%>%
# 	.[, .SD[pi_hat == max(pi_hat), ], by = .(id, sim)] %>%
#     .[, .(sim, id, rate)] %>%
#     setnames("rate", "opt_N_hat")%>%
#     .[testing[,.(sim, id, opt_N)], on=c("sim", "id")]










####==== Version 2 ====####
	
###=== get slope of the yield response function ===###
slope_v2 <- copy(res_CNN_v2)%>%
	.[, .(slope = coef(lm(yield_hat~rate))["rate"]), by= .(id, sim, var_case)]

saveRDS(slope_v2, here("CNN_Results", "CNNslope_v2,rds"))


# slope_v2 <- copy(res_CNN_v1)%>%
# 	.[, .SD[which.min(rate)&which.min(rate),]]
# 	.[, .(slope = ), by= .(id, sim, var_case)]



ggplot(slope_v2)+geom_density(aes(x=slope_v2))


# aby_opt_N2 <- aby[,.SD[c(which.min(rate), which.max(rate))],by=.(id, sim)]


pN_pC_ratio <- pN/pCorn

CNN_opt_N_eval <- copy(slope_v2)%>%
	res_CNN_v2[.,on=c("sim", "id", "var_case")]%>%
	.[, opt_N_hat:=lapply(.SD, function(x) ifelse(slope<pN_pC_ratio, min(rate), max(rate))), by=.(id, sim, var_case)]%>%
	.[rate==opt_N_hat,]%>%
	.[testing_v2[,.(sim, id, opt_N)], on=c("sim", "id")]


CNN_opt_N_eval%>%
	.[,.(r2_N=summary(lm(opt_N ~ opt_N_hat))$r.squared), by=.(sim, var_case)]%>%
	.[,.(r2_N=mean(r2_N)), by=var_case]

		
# aby_opt_N <- copy(aby)  %>%
# 	.[, pi_hat := pCorn * yield_hat - pN * rate]%>%
# 	.[, .SD[pi_hat == max(pi_hat), ], by = .(id, sim)] %>%
#     .[, .(sim, id, rate)] %>%
#     setnames("rate", "opt_N_hat")%>%
#     .[testing[,.(sim, id, opt_N)], on=c("sim", "id")]









