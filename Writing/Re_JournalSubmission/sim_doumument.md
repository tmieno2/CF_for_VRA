# Organize what I did for the simulations

# Dataset
## Field Data Generation 
+ create 1000 (n=1,2,...1000) synthetic field data (cell-level) with Variogram model
	* the data name is `coefficients_sprange_400.rds`

## Create Training data and Testing data
For i in (1,2,...,1000): 

+ extract field data of n=i, (i=1,2,...1000) to create a training data set
+ extract field data of n=i+1, (i=1,2,...1000) to create a testing dataset (if i=1000, then n=1 of field data is selected)

	* calculate true optimal N rate (variable `opt_N`)
	* determine five experimental N rates (variable `rate`) based on the distribution of `opt_N`, 
		- If there are application errors, then `aa_n` is calculated 
		- if there are no application errors, `aa_n` is equivalent to `rate`
	* assign five N experiment rates randomly to plots
		- the same five N experiment rates are used to N randomization of a testing field.
	* calculate true yield (MB function): f(alpha, beta, ymax, aa_N)

+ In these ways, 1000 thousand combinations of training and testing dataset are prepared. 

# The differences between Training and Testing dataset
+ looking at each simulation round, training and testing data set are different
+ However, both of the datasets are generated from the same 1000 synthetic field data
	* For example, training data to be used for 1st simulation and testing data to be used for 2nd simulation are the same.
