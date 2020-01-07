rm(list = ls())
library(survey)
library(data.table)

source("R/functions.R")

data <- readRDS("Data/data_cleaned_pred.Rda")

## Summary table
# Create survey objects
design <- svydesign(id = ~ 1, weights = ~ SW, data = data[`_MULT_` == 1])

# Exclude those flagged in data cleaning step who:
# 1) were missing values needed to find predictions
# 2) were missing body composition or other covariates needed to fit model
# 3) had diabetes diagnosis or used medication to lower blood-sugar
exclusions <- "& excludePred2 == 0 & excludeMain == 0 & excludeSecond == 0"
  
sub_designs <- list(subset(design, eval(parse(text = paste("Gender == \"Male\" & Age < 40", exclusions)))),
                  subset(design, eval(parse(text = paste("Gender == \"Male\" & Age >= 40", exclusions)))),
                  subset(design, eval(parse(text = paste("Gender == \"Female\" & Age < 40", exclusions)))),
                  subset(design, eval(parse(text = paste("Gender == \"Female\" & Age >= 40", exclusions)))))
tab1 <- rbindlist(lapply(sub_designs, getSum))
tab2 <- getSumMI(exclusions)

summary_table <- cbind(tab1, tab2)

## Primary analysis for manuscript
# Right hand formulas for model fits
# Two models: one adjusting only for BMI and one with additional covariates
formulas <- c("BMI", "BMI + Age + Ethnicity + Smoking + PA_Vigorous + PA_Moderate + Alcohol")

# Fit linear models for percent total body fat
fits <- combineFits("TPF", formulas, data, exclusions)
