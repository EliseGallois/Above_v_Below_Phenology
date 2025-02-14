# Elise Gallois 4th September 2023
# a - load models
# b - create model tables
# c - save in outputs folder

#### 1 - LOAD PACKAGES  ##### 
library(readr)
library(tidyverse)
library(brms)
library(broom)
library(stargazer)

#### 2a - LOAD LTER MODELS  ##### 
load("models/async_temp_comm_quantile_top5.RData") # async_temp_comm
load("models/biomass_temp_comm_quantile_top5.RData") # biomass_temp_comm
load("models/rate_temp_comm_quantile_top5.RData") # rate_temp_comm

#### 2b - MAKE LTER TABLE ####
# using code adapted from Mariana: https://github.com/ShrubHub/ShrubHub/blob/master/scripts/users/mgarciacriado/traits_vs_ranges/scripts/8_model_table.R
# Load a few models, run the table, and then close R and clean the environment
# Rinse and repeat with the rest of models

# Jonathan Chang's function
p_summarize <- function(model) {
  brms::posterior_summary(model) %>% 
    as_tibble(rownames = "parameter")
}

# add model objects to list
models.list <- list(biomass_temp_comm, rate_temp_comm, async_temp_comm)

# compile model titles
model_names <- c(
  "Root Biomass vs Temperature & Community & Phenophase",
  "Root Growth Rate vs Temperature & Community",
  "Root Synchrony Metric vs Temperature & Community")

# number the models 1 through 4
model_number <- 1:3
# bind the model tables together
mod.df <- data.frame(model_number, model_names)

# Extract parameters
mod.table <- lapply(models.list, p_summarize) %>% 
  bind_rows(.id = "model_number") 

# Add model name to table
mod.table$model_number <- as.integer(mod.table$model_number)
mod.table.final <- left_join(mod.table, mod.df, by = "model_number")

# Clean model parameters
mod.table.final <- mod.table.final %>% filter(parameter != "lp__") %>% filter(parameter != "Intercept")
mod.table.final$model_names[duplicated(mod.table.final$model_names)] <- "  "
mod.table.final$model_names <- as.character(mod.table.final$model_names)
mod.table.final$model_number[duplicated(mod.table.final$model_number)] <- "  "

colnames(mod.table.final) <- c("Model number", "Term", "Estimate", "Std. error", "Lower 95% CI", "Upper 95% CI", "Model name")
mod.table.final <- mod.table.final[, c(1, 7, 2, 3, 4, 5, 6)]

#  Round to 3 decimals only because not working on stargazer function
mod.table.final <- mod.table.final %>% mutate_if(is.numeric, round, digits = 2)


# Save in csv
write.csv(mod.table.final, "outputs/top5_roots_model_table.csv")

# remove underscores which are fudging html table production
mod.table.final$Term <- sapply(mod.table.final$Term , function(x) gsub("_", "",  x))

# Convert to table
stargazer(mod.table.final, type = "html",  summary = FALSE, out = "outputs/top5_roots_model_table.html")

# 3 - Continuous models ----
load("models/async_temp_cont_quantile_top5.RData") # async_temp_continuous
load("models/biomass_temp_continuous_top5.RData") # biomass_temp_continuous
load("models/rate_temp_continuous_quantile_top5.RData") # rate_temp_continuous

# using code adapted from Mariana: https://github.com/ShrubHub/ShrubHub/blob/master/scripts/users/mgarciacriado/traits_vs_ranges/scripts/8_model_table.R
# Load a few models, run the table, and then close R and clean the environment
# Rinse and repeat with the rest of models

# Jonathan Chang's function
p_summarize <- function(model) {
  brms::posterior_summary(model) %>% 
    as_tibble(rownames = "parameter")
}

# add model objects to list
models.list <- list(biomass_temp_continuous, rate_temp_continuous, async_temp_continuous)

# compile model titles
model_names <- c(
  "Root Biomass vs Temperature & Community & Phenophase",
  "Root Growth Rate vs Temperature & Community",
  "Root Synchrony Metric vs Temperature & Community")

# number the models 1 through 3
model_number <- 1:3
# bind the model tables together
mod.df <- data.frame(model_number, model_names)

# Extract parameters
mod.table <- lapply(models.list, p_summarize) %>% 
  bind_rows(.id = "model_number") 

# Add model name to table
mod.table$model_number <- as.integer(mod.table$model_number)
mod.table.final <- left_join(mod.table, mod.df, by = "model_number")

# Clean model parameters
mod.table.final <- mod.table.final %>% filter(parameter != "lp__") %>% filter(parameter != "Intercept")
mod.table.final$model_names[duplicated(mod.table.final$model_names)] <- "  "
mod.table.final$model_names <- as.character(mod.table.final$model_names)
mod.table.final$model_number[duplicated(mod.table.final$model_number)] <- "  "

colnames(mod.table.final) <- c("Model number", "Term", "Estimate", "Std. error", "Lower 95% CI", "Upper 95% CI", "Model name")
mod.table.final <- mod.table.final[, c(1, 7, 2, 3, 4, 5, 6)]

#  Round to 3 decimals only because not working on stargazer function
mod.table.final <- mod.table.final %>% mutate_if(is.numeric, round, digits = 2)


# Save in csv
write.csv(mod.table.final, "outputs/roots_model_table_continuous_2024_top5.csv")

# remove underscores which are fudging html table production
mod.table.final$Term <- sapply(mod.table.final$Term , function(x) gsub("_", "",  x))

# Convert to table
stargazer(mod.table.final, type = "html",  summary = FALSE, out = "outputs/roots_model_table.html")


