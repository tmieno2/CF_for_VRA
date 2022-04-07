# /*----------------------------------*/
#' ## Preparation
# /*----------------------------------*/
library(here)
library(mgcv)
library(ggplot2)
library(grf)
library(sf)
library(measurements)
library(data.table)
library(tidyverse)
library(future.apply)
library(parallel)

# === Load Functions === #
source(here("GitControlled/Codes/0_1_functions_gen_analysis_data.R"))

# === Prices ===#
pCorn <- price_table[2, pCorn]
pN <- price_table[2, pN]

# === Load Field data === #
field <-
  here("Shared/Data/for_Simulations/analysis_field.rds") %>%
  readRDS()


# /*===========================================*/
#' = (1) For medium degree of m_error (Main Data)=
# /*===========================================*/

# === Load coef Data sets === #
# coef_data <-
# 	here("Shared/Data/for_Simulations/coefficients_sprange_400.rds") %>%
# 	readRDS()

# /*--------------------------------*/
#' ## (1)-1 Generate Cell-level Data sets
# /*--------------------------------*/
raw_data <- lapply(
  1:1000,
  function(x) {
    prepare_raw_data(
      i = x,
      field = field,
      coef_data_m = coef_data[sim == x, ],
      coef_data_t = coef_data[sim == ifelse(x + 1 >= max(sim), 1, x + 1), ],
      app_error = "no"
    )
  }
)

reg_raw_data <- sapply(raw_data, "[", 1) %>% rbindlist()
test_raw_data <- sapply(raw_data, "[", 2) %>% rbindlist()


# /*--------------------------------*/
#' ## (1)-2 Generate analysis dataset (subplot-level)
# /*--------------------------------*/
reg_raw_data <-
  here("Shared/Data/for_Simulations/reg_raw_data.rds") %>%
  readRDS()

test_raw_data <-
  here("Shared/Data/for_Simulations/test_raw_data.rds") %>%
  readRDS()

## == Aggregate Cell-Level Field Data to Subplot-Level Field Data ==##
sim_data <- lapply(
  1:1000, function(x) {
    # print(paste0("working on ", x, " th iteration."))
    prepare_data_for_sim(
      reg_raw_data = reg_raw_data[sim == x, ],
      test_raw_data = test_raw_data[sim == x, ]
    )
  }
)

reg_data <- sapply(sim_data, "[", 1) %>% rbindlist()
test_data <- sapply(sim_data, "[", 2) %>% rbindlist()

# saveRDS(reg_data, here("Shared/Data/for_Simulations/reg_data.rds"))
# saveRDS(test_data, here("Shared/Data/for_Simulations/test_data.rds"))



# /*===========================================*/
#' =  (2) For low degree of m_error =
# /*===========================================*/

# === Load coef Data sets === #
coef_data_low <-
  here("Shared/Data/for_Simulations/coefficients_sprange_400_low_error.rds") %>%
  readRDS()


# /*--------------------------------*/
#' ## (2)-1 Generate Cell-level Data sets
# /*--------------------------------*/
raw_data <- lapply(
  1:100, function(x) {
    prepare_raw_data(
      i = x,
      field = field,
      coef_data_m = coef_data_low[sim == x, ],
      coef_data_t = coef_data_low[sim == ifelse(x + 1 >= max(sim), 1, x + 1), ],
      app_error = "no"
    )
  }
)


reg_raw_data <- sapply(raw_data, "[", 1) %>% rbindlist()
test_raw_data <- sapply(raw_data, "[", 2) %>% rbindlist()


saveRDS(reg_raw_data, here("Shared/Data/for_Simulations/reg_raw_data_low_error.rds"))
saveRDS(test_raw_data, here("Shared/Data/for_Simulations/test_raw_data_low_error.rds"))



# /*--------------------------------*/
#' ## (2)-2 Generate analysis dataset (subplot-level)
# /*--------------------------------*/
# reg_raw_data <- readRDS(here("Shared/Data/for_Simulations/reg_raw_data_low_error.rds"))
# test_raw_data <- readRDS(here("Shared/Data/for_Simulations/test_raw_data_low_error.rds"))


## == Aggregate Cell-Level Field Data to Subplot-Level Field Data ==##
sim_data <- lapply(
  1:100, function(x) {
    # print(paste0("working on ", x, " th iteration."))
    prepare_data_for_sim(
      reg_raw_data = reg_raw_data[sim == x, ],
      test_raw_data = test_raw_data[sim == x, ]
    )
  }
)

reg_data <- sapply(sim_data, "[", 1) %>% rbindlist()
test_data <- sapply(sim_data, "[", 2) %>% rbindlist()

saveRDS(reg_data, here("Shared/Data/for_Simulations/reg_data_low_error.rds"))
saveRDS(test_data, here("Shared/Data/for_Simulations/test_data_low_error.rds"))







# /*===========================================*/
#' =  (3) For high degree of m_error =
# /*===========================================*/

# === Load coef Data sets === #
coef_data_high <-
  here("Shared/Data/for_Simulations/coefficients_sprange_400_high_error.rds") %>%
  readRDS()


# /*--------------------------------*/
#' ## (3)-1 Generate Cell-level Data sets
# /*--------------------------------*/
raw_data <- lapply(
  1:100, function(x) {
    prepare_raw_data(
      i = x,
      field = field,
      coef_data_m = coef_data_high[sim == x, ],
      coef_data_t = coef_data_high[sim == ifelse(x + 1 >= max(sim), 1, x + 1), ],
      app_error = "no"
    )
  }
)


reg_raw_data <- sapply(raw_data, "[", 1) %>% rbindlist()
test_raw_data <- sapply(raw_data, "[", 2) %>% rbindlist()

saveRDS(reg_raw_data, here("Shared/Data/for_Simulations/reg_raw_data_high_error.rds"))
saveRDS(test_raw_data, here("Shared/Data/for_Simulations/test_raw_data_high_error.rds"))


# /*--------------------------------*/
#' ##  (3)-2 Generate analysis dataset (subplot-level)
# /*--------------------------------*/
# reg_raw_data <- readRDS(here("Shared/Data/for_Simulations/reg_raw_data_high_error.rds"))
# test_raw_data <- readRDS(here("Shared/Data/for_Simulations/test_raw_data_high_error.rds"))

## == Aggregate Cell-Level Field Data to Subplot-Level Field Data ==##
sim_data <- lapply(
  1:100, function(x) {
    # print(paste0("working on ", x, " th iteration."))
    prepare_data_for_sim(
      reg_raw_data = reg_raw_data[sim == x, ],
      test_raw_data = test_raw_data[sim == x, ]
    )
  }
)

reg_data <- sapply(sim_data, "[", 1) %>% rbindlist()
test_data <- sapply(sim_data, "[", 2) %>% rbindlist()

saveRDS(reg_data, here("Shared/Data/for_Simulations/reg_data_high_error.rds"))
saveRDS(test_data, here("Shared/Data/for_Simulations/test_data_high_error.rds"))