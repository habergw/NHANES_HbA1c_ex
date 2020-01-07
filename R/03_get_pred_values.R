rm(list = ls())
library(survey)
library(data.table)

source("R/functions.R")

data <- readRDS("Data/data_cleaned.Rda")

## Predictions using sample weights

## Use all data to fit prediction model
data$SW <- getWeights(1:4, data)

## Fit two seperate prediction sets, one using only waist and other using all measurements

## First set

## Fit prediction models seperately for MI values
TPFfitMale <- TPFfitFemale <- TFfitMale <- TFfitFemale <- TPLMfitMale <-
TPLMfitFemale <- TLMfitMale <- TLMfitFemale <- list()

for (i in 1:5) {
  trainTmp <- data[`_MULT_` == i]
  trainDes <- svydesign(id = ~ SEQN, weights = ~SW, data = trainTmp)
  trainMale <- subset(trainDes, Gender == "Male" & excludePred1 == 0)
  trainFemale <- subset(trainDes, Gender == "Female" & excludePred1 == 0)
  
  TPFfitMale[[i]] <- svyglm(TPF ~ Age + Height + Weight + WaistC + Ethnicity, design = trainMale)
  TPFfitFemale[[i]] <- svyglm(TPF ~ Age + Height + Weight + WaistC + Ethnicity, design = trainFemale)
  
  TFfitMale[[i]] <- svyglm(TF ~ Age + Height + Weight + WaistC + Ethnicity, design = trainMale)
  TFfitFemale[[i]] <- svyglm(TF ~ Age + Height + Weight + WaistC + Ethnicity, design = trainFemale)
  
  TPLMfitMale[[i]] <- svyglm(TPLM ~ Age + Height + Weight + WaistC + Ethnicity, design = trainMale)
  TPLMfitFemale[[i]] <- svyglm(TPLM ~ Age + Height + Weight + WaistC + Ethnicity, design = trainFemale)
  
  TLMfitMale[[i]] <- svyglm(TLM ~ Age + Height + Weight + WaistC + Ethnicity, design = trainMale)
  TLMfitFemale[[i]] <- svyglm(TLM ~ Age + Height + Weight + WaistC + Ethnicity, design = trainFemale)
}

## Get predicted values for analysis data

# Subset by gender
datMale <- data[Gender == "Male" & excludePred1 == 0]
datFemale <- data[Gender == "Female" & excludePred1 == 0]

for (i in 1:5) {
  dataMale <- datMale[`_MULT_` == i]
  dataFemale <- datFemale[`_MULT_` == i]
  
  datMale[`_MULT_` == i, c("TPFpred", "TFpred", "TPLMpred", "TLMpred") := list(as.numeric(predict(TPFfitMale[[i]], newdata = dataMale)),
                                                                               as.numeric(predict(TFfitMale[[i]], newData = dataMale)),
                                                                               as.numeric(predict(TPLMfitMale[[i]], newdata = dataMale)),
                                                                               as.numeric(predict(TLMfitMale[[i]], newdata = dataMale)))]
  
  datFemale[`_MULT_` == i, c("TPFpred", "TFpred", "TPLMpred", "TLMpred") := list(as.numeric(predict(TPFfitFemale[[i]], newdata = dataFemale)),
                                                                                 as.numeric(predict(TFfitFemale[[i]], newData = dataFemale)),
                                                                                 as.numeric(predict(TPLMfitFemale[[i]], newdata = dataFemale)),
                                                                                 as.numeric(predict(TLMfitFemale[[i]], newdata = dataFemale)))]
}

## Combine gender data and remove missing values
dat <- rbind(datMale, datFemale)
dat <- dat[, .(`_MULT_`, SEQN, TPFpred, TFpred, TPLMpred, TLMpred)]
datM <- merge(data, dat, by = c("SEQN", "_MULT_"), all.x = T)

## Second set using additional measurements
for (i in 1:5) {
  trainTmp <- data[`_MULT_` == i]
  trainDes <- svydesign(id = ~ SEQN, weights = ~SW, data = trainTmp)
  trainMale <- subset(trainDes, Gender == "Male" & excludePred2 == 0)
  trainFemale <- subset(trainDes, Gender == "Female" & excludePred2 == 0)
  
  TPFfitMale[[i]] <- svyglm(TPF ~ Age + Height + Weight + WaistC + Arm + Thigh + Calf + Triceps + Subscapular + Ethnicity, design = trainMale)
  TPFfitFemale[[i]] <- svyglm(TPF ~ Age + Height + Weight + WaistC + Arm + Thigh + Calf + Triceps + Subscapular +Ethnicity, design = trainFemale)
  
  TFfitMale[[i]] <- svyglm(TF ~ Age + Height + Weight + WaistC + Arm + Thigh + Calf + Triceps + Subscapular +Ethnicity, design = trainMale)
  TFfitFemale[[i]] <- svyglm(TF ~ Age + Height + Weight + WaistC + Arm + Thigh + Calf + Triceps + Subscapular +Ethnicity, design = trainFemale)
  
  TPLMfitMale[[i]] <- svyglm(TPLM ~ Age + Height + Weight + WaistC + Arm + Thigh + Calf + Triceps + Subscapular +Ethnicity, design = trainMale)
  TPLMfitFemale[[i]] <- svyglm(TPLM ~ Age + Height + Weight + WaistC + Arm + Thigh + Calf + Triceps + Subscapular +Ethnicity, design = trainFemale)
  
  TLMfitMale[[i]] <- svyglm(TLM ~ Age + Height + Weight + WaistC + Arm + Thigh + Calf + Triceps + Subscapular +Ethnicity, design = trainMale)
  TLMfitFemale[[i]] <- svyglm(TLM ~ Age + Height + Weight + WaistC + Arm + Thigh + Calf + Triceps + Subscapular +Ethnicity, design = trainFemale)
}

## Get predicted values for analysis data

# Subset by gender
datMale <- data[Gender == "Male" & excludePred2 == 0]
datFemale <- data[Gender == "Female" & excludePred2 == 0]

for (i in 1:5) {
  dataMale <- datMale[`_MULT_` == i]
  dataFemale <- datFemale[`_MULT_` == i]
  
  datMale[`_MULT_` == i, c("TPFpred2", "TFpred2", "TPLMpred2", "TLMpred2") := list(as.numeric(predict(TPFfitMale[[i]], newdata = dataMale)),
                                                                               as.numeric(predict(TFfitMale[[i]], newData = dataMale)),
                                                                               as.numeric(predict(TPLMfitMale[[i]], newdata = dataMale)),
                                                                               as.numeric(predict(TLMfitMale[[i]], newdata = dataMale)))]
  
  datFemale[`_MULT_` == i, c("TPFpred2", "TFpred2", "TPLMpred2", "TLMpred2") := list(as.numeric(predict(TPFfitFemale[[i]], newdata = dataFemale)),
                                                                                 as.numeric(predict(TFfitFemale[[i]], newData = dataFemale)),
                                                                                 as.numeric(predict(TPLMfitFemale[[i]], newdata = dataFemale)),
                                                                                 as.numeric(predict(TLMfitFemale[[i]], newdata = dataFemale)))]
}

## Combine gender data and remove missing values
dat2 <- rbind(datMale, datFemale)
dat2 <- dat2[, .(`_MULT_`, SEQN, TPFpred2, TFpred2, TPLMpred2, TLMpred2)]
datM2 <- merge(datM, dat2, by = c("SEQN", "_MULT_"), all.x = T)

## Save dataset to be used for analysis
saveRDS(datM2, "Data/data_cleaned_pred.Rda")
