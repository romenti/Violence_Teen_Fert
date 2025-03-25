source('PAA_R_scripts/upload_libraries.R')
load('PAA_R_files')


data_analysis$area_label <- factor(data_analysis$area,levels=unique(data_analysis$area),
                           labels = c('SSA','NAWA','CSA','EA','SEA','EUNA','LAC','OC')) 


#### Global ####

# Overall Violence
global_model = feols(F15 ~ gpi_overall + gdi_lag_imp + marriage_prev_lag + contraceptive_prev_lag |
                       (Year + iso3), 
                     data = data_analysis, 
                     weights = ~ pop_female_init,
                     vcov="hetero")

# Conflict 

global_conflict = feols(F15 ~ gpi_conflict + gdi_lag_imp + marriage_prev_lag + contraceptive_prev_lag |
                          (iso3+Year), 
                        data = data_analysis, 
                        weights = ~ pop_female_init,
                        vcov="hetero")


# Safety and Security

global_safety = feols(F15 ~ gpi_safety + gdi_lag_imp + marriage_prev_lag + contraceptive_prev_lag |
                        (Year + iso3), 
                      data = data_analysis, 
                      weights = ~ pop_female_init,
                      vcov="hetero")





### Save global models

global_models = list(global_model,global_conflict,global_safety)
names(global_models) = c('Overall_Violence','Conflict','Safety and Security')

# Create a new workbook to save the models
wb <- createWorkbook()



# Loop through each model and add its results as a separate sheet
lapply(names(global_models), function(area) {
  model <- global_models[[area]]
  
  # Extract the tidy results (coefficients, std.errors, etc.)
  model_summary <- tidy(model) %>% 
    select(term, estimate, std.error, statistic, p.value)  # Keep relevant columns
  
  # Sanitize sheet name to make it valid for Excel (replace spaces, special characters, etc.)
  sheet_name <- gsub("[^[:alnum:]_]", "_", area)  # Replace non-alphanumeric characters with underscores
  
  sheet_name <- substr(sheet_name, 1, 16)
  
  # Add a new sheet with the sanitized model results
  addWorksheet(wb, sheet_name)  # Sheet name is sanitized
  writeData(wb, sheet_name, model_summary)  # Write model summary to the sheet
})

# Save the workbook to an Excel file
saveWorkbook(wb, "PAA_regression_results/global_model_results_GPI.xlsx", overwrite = TRUE)



#### Subregions ####

# Overall



subregion_models_overall <- lapply(split(data_analysis, data_analysis$area_label), function(sub_data) {
  feols(F15 ~ gpi_overall + gdi_lag_imp + marriage_prev_lag + contraceptive_prev_lag |
          (Year + iso3), 
        data = sub_data, 
        weights = ~ pop_female_init,
        vcov="hetero")
})




# Conflict

subregion_models_conflict <- lapply(split(data_analysis, data_analysis$area_label), function(sub_data) {
  
  feols(F15 ~ gpi_conflict +gdi_lag_imp+ marriage_prev_lag + contraceptive_prev_lag |
          (Year + iso3), 
        data = sub_data, 
        weights = ~ pop_female_init,
        vcov="hetero")
})


# Safety and Security

subregion_models_safety <- lapply(split(data_analysis, data_analysis$area_label), function(sub_data) {
  feols(F15 ~ gpi_safety + gdi_lag_imp + marriage_prev_lag + contraceptive_prev_lag |
          (Year + iso3), 
        data = sub_data, 
        weights = ~ pop_female_init,
        vcov="hetero")
})


# Run models for each subregion





# Create a new workbook to save the models
wb <- createWorkbook()



# Loop through each model and add its results as a separate sheet
lapply(names(subregion_models_overall), function(area) {
  model <- subregion_models_overall[[area]]
  
  # Extract the tidy results (coefficients, std.errors, etc.)
  model_summary <- tidy(model) %>% 
    select(term, estimate, std.error, statistic, p.value)  # Keep relevant columns
  
  # Sanitize sheet name to make it valid for Excel (replace spaces, special characters, etc.)
  sheet_name <- gsub("[^[:alnum:]_]", "_", area)  # Replace non-alphanumeric characters with underscores
  
  sheet_name <- substr(sheet_name, 1, 16)
  
  # Add a new sheet with the sanitized model results
  addWorksheet(wb, sheet_name)  # Sheet name is sanitized
  writeData(wb, sheet_name, model_summary)  # Write model summary to the sheet
})

# Save the workbook to an Excel file
saveWorkbook(wb, "PAA_regression_results/subregion_model_results_GPI_total.xlsx", overwrite = TRUE)



# Create a new workbook to save the models
wb <- createWorkbook()



# Loop through each model and add its results as a separate sheet
lapply(names(subregion_models_conflict), function(area) {
  model <- subregion_models_conflict[[area]]
  
  # Extract the tidy results (coefficients, std.errors, etc.)
  model_summary <- tidy(model) %>% 
    select(term, estimate, std.error, statistic, p.value)  # Keep relevant columns
  
  # Sanitize sheet name to make it valid for Excel (replace spaces, special characters, etc.)
  sheet_name <- gsub("[^[:alnum:]_]", "_", area)  # Replace non-alphanumeric characters with underscores
  
  sheet_name <- substr(sheet_name, 1, 16)
  
  # Add a new sheet with the sanitized model results
  addWorksheet(wb, sheet_name)  # Sheet name is sanitized
  writeData(wb, sheet_name, model_summary)  # Write model summary to the sheet
})



# Save the workbook to an Excel file
saveWorkbook(wb, "PAA_regression_results/subregion_model_results_GPI_conflict.xlsx", overwrite = TRUE)


# Create a new workbook to save the models
wb <- createWorkbook()



# Loop through each model and add its results as a separate sheet
lapply(names(subregion_models_safety), function(area) {
  model <- subregion_models_safety[[area]]
  
  # Extract the tidy results (coefficients, std.errors, etc.)
  model_summary <- tidy(model) %>% 
    select(term, estimate, std.error, statistic, p.value)  # Keep relevant columns
  
  # Sanitize sheet name to make it valid for Excel (replace spaces, special characters, etc.)
  sheet_name <- gsub("[^[:alnum:]_]", "_", area)  # Replace non-alphanumeric characters with underscores
  
  sheet_name <- substr(sheet_name, 1, 16)
  
  # Add a new sheet with the sanitized model results
  addWorksheet(wb, sheet_name)  # Sheet name is sanitized
  writeData(wb, sheet_name, model_summary)  # Write model summary to the sheet
})



# Save the workbook to an Excel file
saveWorkbook(wb, "PAA_regression_results/subregion_model_results_GPI_safety.xlsx", overwrite = TRUE)



#### Prepare regression results for CI plot ####


# Global models


global_model_overall <- broom::tidy(global_model, conf.int = TRUE) %>%
  filter(term=='gpi_overall') %>%
  mutate(subregion='World')


global_safety_overall <- broom::tidy(global_safety, conf.int = TRUE) %>%
  filter(term=='gpi_safety') %>%
  mutate(subregion='World')


global_conflict_overall <- broom::tidy(global_conflict, conf.int = TRUE) %>%
  filter(term=='gpi_conflict') %>%
  mutate(subregion='World')


# Regional models 

coef_data_overall <- bind_rows(lapply(names(subregion_models_overall), function(region) {
  model <- subregion_models_overall[[region]]
  tidy_model <- broom::tidy(model, conf.int = TRUE) %>%
    filter(term=='gpi_overall')
  
  # Convert model to a tidy format
  tidy_model$subregion <- region # Add subregion identifier
  return(tidy_model)
}))

coef_data_conflict <- bind_rows(lapply(names(subregion_models_conflict), function(region) {
  model <- subregion_models_conflict[[region]]
  tidy_model <- broom::tidy(model, conf.int = TRUE) %>%
    filter(term=='gpi_conflict')
  
  # Convert model to a tidy format
  tidy_model$subregion <- region # Add subregion identifier
  return(tidy_model)
}))

coef_data_safety <- bind_rows(lapply(names(subregion_models_safety), function(region) {
  model <- subregion_models_safety[[region]]
  tidy_model <- broom::tidy(model, conf.int = TRUE) %>%
    filter(term=='gpi_safety')
  
  # Convert model to a tidy format
  tidy_model$subregion <- region # Add subregion identifier
  return(tidy_model)
}))


#### Create data set with model coefficients ####



data_plot_coef = rbind(global_model_overall,global_safety_overall,
                       global_conflict_overall,coef_data_overall,
                       coef_data_conflict,coef_data_safety) %>%
  mutate(term=factor(term,levels=c('gpi_overall','gpi_conflict','gpi_safety'),
                     labels = c('Overall Violence','Conflict','Safety and Security')),
         subregion=ifelse(subregion=='South-eastern Asia','South-Eastern Asia',subregion),
         subregion=factor(subregion,levels=c('World','Europe and North America',
                                             'Eastern Asia',
                                             'Northern Africa and Western Asia',
                                             'South-Eastern Asia',
                                             "Central and Southern Asia",
                                             'Latin America and the Caribbean',
                                             'Sub-Saharan Africa','Oceania'),
                          labels = c('World','Europe and\n North America',
                                     'Eastern Asia',
                                     'Northern Africa and\n Western Asia',
                                     'South-Eastern Asia',
                                     "Central and\n Southern Asia",
                                     'Latin America and\n the Caribbean',
                                     'Sub-Saharan Africa','Oceania'))) %>%
  mutate(macro=ifelse(subregion=='World','Global Association','Subregional Associtions'),
         macro=factor(macro,levels=c('Global Association','Subregional Associtions'))) 


save(data_plot_coef,file='PAA-Data_files/data_coefficients.RData')

save(data_plot_coef,file='data_coefficients.RData')


