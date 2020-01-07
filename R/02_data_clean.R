rm(list = ls())
require(haven)
require(data.table)
require(magrittr)

## Import demographic data
demoFiles <- c("DEMO.XPT", "DEMO_B.XPT", "DEMO_C.XPT", "DEMO_D.XPT")
cohortList <- c("1999-2000", "2001-2002", "2003-2004", "2005-2006")

demoData <- lapply(demoFiles, function(x) data.table(read_xpt(paste0("NHANES_DATA/", x))))

demoData[[3]][, WTMEC4YR := NA]
demoData[[4]][, WTMEC4YR := NA]

demoData <- lapply(1:4, function(x)
  demoData[[x]][, .(SEQN, WTMEC2YR, WTMEC4YR, Gender = RIAGENDR,
                    Age = RIDAGEYR, Ethnicity = RIDRETH1, Cohort = cohortList[x])
                ][, Gender := factor(Gender, labels = c("Male", "Female"))
                  ][, Ethnicity := factor(Ethnicity, labels = c("Mexican American", "Other Hispanic",
                                                                "Caucasian", "African American",
                                                                "Other"))
                    ][, Ethnicity := relevel(Ethnicity, ref = 3)])

datDemo <- Reduce("rbind", demoData)
setkey(datDemo, SEQN)

## Import DXA data
dxaFiles <- c("dxx.xpt", "dxx_b.xpt", "dxx_c.xpt", "dxx_d.xpt")

dxaData <- lapply(dxaFiles, function(x) data.table(read_xpt(paste0("NHANES_DATA/", x))))

dxaData[[4]][, DXITOT := pmax(DXIHE, DXILA, DXILL, DXIRA, DXIRL, DXILR, DXIRR, DXITS, DXILS, DXIPE,
                              DXITR)
             ][DXITOT %in% 1:3, DXITOT := 1
               ][DXITOT == 4, DXITOT := 2]

dxaData <- lapply(dxaData, function(x) x[, .(SEQN, `_MULT_`, DXAEXSTS, DXITOT, TF = DXDTOFAT / 1000,
                                             TPF = DXDTOPF, TLM = DXDTOLE / 1000)])

datDXA <- Reduce("rbind", dxaData)
setkey(datDXA, SEQN)

## Import anthropometric measurements
bmFiles <- c("BMX.xpt", "BMX_B.xpt", "BMX_C.xpt", "BMX_D.xpt")

bmData <- lapply(bmFiles, function(x) data.table(read_xpt(paste0("NHANES_DATA/", x))))

datBM <- Reduce("rbind", lapply(bmData, function(x) x[, .(SEQN, Weight = BMXWT, Height = BMXHT,
                                                          WaistC = BMXWAIST, BMI = BMXBMI, Arm = BMXARMC,
                                                          Thigh = BMXTHICR, Calf = BMXCALF,
                                                          Triceps = BMXTRI, Subscapular = BMXSUB)]))
setkey(datBM, SEQN)

## Import diabetes data
dmFiles <- c("DIQ.xpt", "DIQ_B.xpt", "DIQ_C.xpt", "DIQ_D.xpt")

dmData <- lapply(dmFiles, function(x) data.table(read_xpt(paste0("NHANES_DATA/", x))))

dmData[[4]][, DIQ070 := DID070]

datDM <- Reduce("rbind", lapply(dmData, function(x) x[, .(SEQN, Diabetes = DIQ010, Insulin = DIQ050, Medication = DIQ070)]))
setkey(datDM, SEQN)

datDM[Diabetes == 3, Diabetes := 2][Insulin == 1, Diabetes := 1
                                    ][Medication == 1, Diabetes := 1
                                      ][Diabetes %in% c(7,9) | Insulin %in% c(7, 9) |
                                        Medication %in% c(7,9), Diabetes := NA]

datDM <- datDM[, Diabetes := factor(Diabetes, labels = c("Diabetic", "Non-Diabetic"))][, .(SEQN, Diabetes)]

## Import alcohol consumption data
alcFiles <- c("ALQ.xpt", "ALQ_B.xpt", "ALQ_C.xpt", "ALQ_D.xpt")

alcData <- lapply(alcFiles, function(x) data.table(read_xpt(paste0("NHANES_DATA/", x))))

alcData[[1]][, ALQ101 := ALQ100]
alcData[[2]][, ALQ101 := ALD100]

datALC <- Reduce("rbind", lapply(alcData, function(x) x[, .(SEQN, ALQ101, ALQ120Q, ALQ120U, ALQ130)]))
setkey(datALC, SEQN)

datALC[ALQ101 > 2, ALQ101 := NA][ALQ120Q > 365, ALQ120Q := NA][ALQ130 >= 99, ALQ130 := NA]
datALC[ALQ101 == 2 | ALQ120Q == 0, c("ALQ130", "ALQ120Q", "ALQ120U") := list(0, 0, 0)]
datALC[ALQ120U == 1, ALQ120U := 52][ALQ120U == 2, ALQ120U := 12][ALQ120U == 3, ALQ120U := 1]
datALC <- datALC[, .(SEQN, Alcohol = ALQ120U * ALQ120Q * ALQ130 / 365)]

## Import smoking data
smokeFiles <- c("SMQ.xpt", "SMQ_B.xpt", "SMQ_C.xpt", "SMQ_D.xpt")

smokeData <- lapply(smokeFiles, function(x) data.table(read_xpt(paste0("NHANES_DATA/", x)))
                    [, .(SEQN, Smoking = SMQ040, SMQ020)])

datSmoking <- Reduce("rbind", smokeData)
setkey(datSmoking, SEQN)

datSmoking[SMQ020 == 9, SMQ020 := 2][SMQ020 > 2, Smoking := NA
                                     ][SMQ020 == 2, Smoking := 3
                                       ][Smoking == 9, Smoking := NA]

datSmoking[, Smoking := relevel(factor(Smoking, labels = c("Every Day", "Occasionally", "Never")),
                                ref = 3)][, .(SEQN, Smoking)]

## Import physical activity data
paFiles <- c("PAQ.xpt", "PAQ_B.xpt", "PAQ_C.xpt", "PAQ_D.xpt")
paActFiles <- c("PAQIAF.xpt", "PAQIAF_B.xpt", "PAQIAF_C.xpt", "PAQIAF_D.xpt")

paData <- lapply(paFiles, function(x) data.table(read_xpt(paste0("NHANES_DATA/", x)))
                 [, .(SEQN, PAD200, PAD320)])

paActData <- lapply(paActFiles, function(x) data.table(read_xpt(paste0("NHANES_DATA/", x)))
                    [, .(SEQN, PADMETS, PADTIMES, PADDURAT)])

datPA <- Reduce("rbind", paData)
setkey(datPA, SEQN)
datPAAct <- Reduce("rbind", paActData)
setkey(datPAAct, SEQN)

datPAAct[, Length := PADTIMES * PADDURAT / (60 * 30)
         ][between(PADMETS, 3, 6), Level := "Moderate"
           ][PADMETS > 6, Level := "Vigorous"
             ][PADMETS < 3, Level := NA]

datPAAct <- datPAAct[!is.na(Level), .(sum = sum(Length)), by = .(SEQN, Level)] %>%
  dcast(SEQN ~ Level, value.var = "sum")

datPA <- merge(datPA, datPAAct, by = "SEQN", all.x = T)

datPA[PAD320 %in% 2:3, Moderate := 0
      ][PAD200 %in% 2:3, Vigorous := 0
        ][PAD320 == 1 & is.na(Moderate), Moderate := 0
          ][PAD200 == 1 & is.na(Vigorous), Vigorous := 0
            ]

datPA <- datPA[, .(SEQN, PA_Moderate = Moderate, PA_Vigorous = Vigorous)]

## Import pregnancy data
pregFiles <- c("UC.xpt", "UC_B.xpt", "UC_C.xpt", "UCPREG_D.xpt")

pregData <- lapply(pregFiles, function(x) data.table(read_xpt(paste0("NHANES_DATA/", x)))
                   [, .(SEQN,Pregnant = URXPREG)])

datPreg <- Reduce("rbind", pregData)
setkey(datPreg, SEQN)

## Import HbA1c data
hbFiles <- c("LAB10.xpt", "L10_B.xpt", "L10_C.xpt", "GHB_D.xpt")

hbData <- lapply(hbFiles, function(x) data.table(read_xpt(paste0("NHANES_DATA/", x)))
                 [, .(SEQN, HbA1c = LBXGH)])
datHB <- Reduce("rbind", hbData)
setkey(datHB, SEQN)

## Combine datasets
dat <- merge(datDemo, datDXA, all.x = TRUE) %>%
    merge(datBM, all.x = TRUE) %>%
    merge(datPreg, all.x = TRUE) %>%
    merge(datPA, all.x = TRUE) %>%
    merge(datSmoking, all.x = TRUE) %>%
    merge(datALC, all.x = TRUE) %>%
    merge(datDM, all.x = TRUE) %>%
    merge(datHB, all.x = TRUE)

## Set pregnancy status for men and women over 59
dat[Gender == "Male" | Age > 59, Pregnant := 2]

## Create pregnancy indicator
dat[, PregChar := "No"]
dat[Pregnant != 2 | is.na(Pregnant), PregChar := "Yes or Missing"]

## Define indicators for exclusions from various analyses
dat[, c("excludePred1", "excludePred2", "excludeMain", "excludeSecond") := 
    list(0, 0, 0, 0)]
dat[is.na(Weight) | is.na(Height) | is.na(Age) | is.na(Ethnicity) |
    is.na(TF) | is.na(TLM) | is.na(WaistC) | is.na(TPF) | 
    is.na(WaistC) | is.na(BMI) | Age < 18, excludePred1 := 1]
dat[excludePred1 | is.na(Arm) | is.na(Thigh) | is.na(Calf) |
    is.na(Triceps) | is.na(Subscapular), excludePred2 := 1]
dat[is.na(HbA1c) | Age > 69 | Diabetes != "Non-Diabetic" | 
    is.na(Diabetes) | PregChar != "No", excludeMain := 1]
dat[is.na(Alcohol) | is.na(Smoking) | is.na(PA_Vigorous), excludeSecond := 1]

# Create percent lean mass variable
dat[, TPLM := TLM / Weight * 100]

# Create directory to store data
dir.create("Data")

# Write file
saveRDS(dat, file = "Data/data_cleaned.Rda") 
