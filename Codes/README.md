# **Codes and Data for: Causal Forest Approach for Site-specific Input Management via On-farm Precision Experimentation**

## General Information
+ Principal Investigator Contact Information
	* Name: Shunkei Kakimoto
	* Institution: Department of Agricultural Economics, University of Nebraska-Lincoln
	* Address: 102 Filley Hall 1625 Arbor Drive Lincoln, NE 68583, USA, 
	* E-mail: skakimoto3@hunskers.unl.edu

+ Co-investigator Contact Information
	* Name: Taro Mieno
	* Institution: Department of Agricultural Economics, University of Nebraska-Lincoln
	* Address: Lincoln, NE 68583, USA
	* E-mail: tmieno2@unl.edu
	
	* Name: Takashi S. T. Tanaka
	* Institution: Artificial Intelligence Advanced Research Center, Faculty of Applied Biological Sciences, Gifu University
	* Address: Gifu 501-1193, Japan
	* E-mail: takashit@gifu-u.ac.jp
	
	* Name: David S. Bullock
	* Institution: Department of Agricultural and Consumer Economics, University of Illinois
	* Address: Urbana, IL 61801, USA
	* E-mail: dsbulloc@illinois.edu


## DATA & FILE OVERVIEW

We present here the R and Python codes and data for analysis presented in: "Causal Forest Approach for Site-specific Input Management via On-farm Precision Experimentation". 


### List of Files:
Data folder:
+ field_boundary.rds
	* Description: A polygon boundary data of an actual field, which is necessary in generating a field polygon in 1_1_generate_field.R. 

Code folder:
+ **0\_1\_functions\_gen\_analysis\_data.R**
	* Description: This code file contains functions to be generate field characteristics (e.g., $alpha$, $beta$, $ymax$, etc.) and yield data sets.

+ **0\_2\_functions\_main\_sim.R**
	* Description: This code file contains functions to conduct Random Forest (RF), Boosted Random Forest (BRF), and Causal Forest (CF) analysis to predict yields and EONRs site-specifically.

+ **1\_1\_generate_field.R**
	* Description: This code file creates various size of grids (i.e., "plots", "subplots", and "cells") within a field.
	* Final output: `analysis_field.rds`

+ **1\_2\_generate\_coefficients.R**
	* Description: This code file generates one thousand field characteristic (e.g., $alpah$, $beta$, $ymax$,..., etc.) data sets using unconditional Gaussian geostatistical simulation based on the spherical variogram model.
	* Final output: `coef\_data.rds`

+ **2\_1\_generate\_analysis\_data.R**
	* Description: This code file simulates on-farm experiment data through assigning experimental nitrogen rates across the plots in the field, and calculate cell-level yield with the Mitscherlich-Baule production function. Then, the data sets are aggregated by subplot-level. 
	* Final output: 
		- Cell-levle output: `reg_raw_data.rds`, `test_raw_data.rds`
		- Subplot-level output: `reg_data.rds`, `test_data.rds`

+ **2\_2\_Forest\_main\_sim.R**
	* Description: This code file conducts one thousand simulation of RF, BRF and CF analysis to predict yields (only for RF and BRF) and EONRs site-specifically. 
	* Output Data: `forest_SimRes_alpha_beta_ymax.rds`, `forest_SimRes_alpha_beta_ymax_theta_1_theta_2.rds`, `forest_SimRes_alpha1_alpha2_beta1_beta2_ymax1_ymax2.rds`, `forest_SimRes_alpha1_alpha2_beta1_beta2_ymax1_ymax2_theta_1_theta_2.rds`
		
+ **2\_3\_CNN\_main\_sim\_aabbyytt.py**	
	* Description: This code file conducts one thousand simulation of yield prediction using Convolutional (CNN) by modeling scenarios. Depending on which modeling scenario is used, you need to arrange the input data and architecture.
	* Output Data: `output_..._.csv` 
		- NOTE: `...` contains the simulation number. So, after that you need to merge all the individual simulation results into one data set and save it as `.csv` file with the unique file name. 

+ **3\_1\_SimDataAnalysis.R**
	* Description: This code file calculates RMSE of predicted EONRs and yields by ML methods and Modeling scenario. 
	* Output Data: 

+ **3\_2\_CompTeEstimation.R**
	* Description: This code file estimates treatment effect by treatment types and ML methods under the "aabbyytt" scenario in one of the one thousand simulations rounds. The output data is used to create Figure 5: "True treatment effects vs estimated treatment effects (scenario: aabbyytt)"
	* Output Data: `dt_TEcomparison.rds`

+ **4\_prepare\_results.R**
	* Description: This code file prepares data to be used in showing the field maps. 

+ **PrepareResults.rmd**
	* Description: This code file creates all the figures and tables which will be used in the manuscript. 


### Steps to reproduce
+ Before you start, create a new R project in a folder including "Data" and "Codes" folders. 
+ The simulation series starts from the code **1\_1\_generate_field.R** in a order of the number assigned in front of each file name. By following these file numbers, you can reproduce the simulations.








