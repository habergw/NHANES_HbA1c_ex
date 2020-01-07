require(survey)
require(mitools)
library(magrittr)

## Return appropriate weights adjusted to combine NHANES cohorts
getWeights <- function(ind, dat) {
    n <- length(ind)
    labels <- c("1999-2000", "2001-2002", "2003-2004", "2005-2006")
    weights <- list()
    if (!all(c(1,2) %in% ind)) {
        for (i in 1:n) {
               weights[[i]] <- 1 / n * dat[Cohort == labels[ind[i]], WTMEC2YR]
        }
    }
    else {
        for (i in 1:2) {
               weights[[i]] <- 2 / n * dat[Cohort == labels[ind[i]], WTMEC4YR]
        }
        if (n == 2)
            return(Reduce("c", weights))
        for (i in 3:n) {
                weights[[i]] <- 1 / n * dat[Cohort == labels[ind[i]], WTMEC2YR]                
            }
        }

    return(Reduce("c", weights))
}

# For a given survey design, create summary table
getSum <- function(design) {
    numVars <- svymean(~ Age + Weight + Height + WaistC + BMI + Alcohol + 
                       PA_Moderate + PA_Vigorous + HbA1c, na.rm = T, 
                       design = design)
    numVars <- setDT(as.data.frame(numVars), keep.rownames = TRUE)
    tab1 <-    melt(numVars, id.vars = "rn") %>% dcast(1 ~ rn + variable)

    factVars <- svymean(~ Ethnicity + Smoking, design = design, na.rm = T)
    factVars <- setDT(100 * as.data.frame(factVars)[,1, drop = FALSE], 
                      keep.rownames = TRUE)
    tab2 <- melt(factVars, id.vars = "rn") %>% dcast(1 ~ rn + variable)

    tab <- cbind(tab1, tab2[, -1, with = FALSE])
    tab[,1] <- suppressWarnings(as.numeric(tab[,1]))
    tab[1,1] <- length(design$fpc$sampsize)
    names(tab)[1] <- "n (Unweighted)"

    tab
}

# Calculate mean and variance of total body fat accounting for MIs
getSumMI <- function(exclusions) {
    miMean <- miVar <- rep(list(vector("list", length = 5)), 4)
    for (i in 1:5) {
        dsn <- svydesign(id = ~ 1, weights = ~ SW, data = data[`_MULT_` == i])

        subDesigns <- list(subset(dsn, eval(parse(text = paste("Gender == \"Male\" & Age < 40", exclusions)))),
                      subset(dsn, eval(parse(text = paste("Gender == \"Male\" & Age >= 40", exclusions)))),
                      subset(dsn, eval(parse(text = paste("Gender == \"Female\" & Age < 40", exclusions)))),
                      subset(dsn, eval(parse(text = paste("Gender == \"Female\" & Age >= 40", exclusions)))))

        tmp <- lapply(subDesigns, function(x) as.data.frame(svymean( ~ TPF, design = x)))
        for (j in 1:length(subDesigns)) {
            miMean[[j]][[i]] <- as.numeric(tmp[[j]][1])
            miVar[[j]][[i]] <- as.numeric(tmp[[j]][2]^2)
        }
    }

    tmpList <- lapply(1:4, function(x) MIcombine(miMean[[x]], miVar[[x]]))
    tmpList <- lapply(tmpList, function(x) setDT(list(TPF_mean = x$coefficients, 
                                                      TPF_SE = sqrt(x$variance))))

    rbindlist(tmpList)
}
       

## Fit linear model for each imputed dataset stratified by gender/age
## Return estimated coefficients and CIs
getImpFitsLinear <- function(formula, data, exclusions) {
  resImp <- rep(list(list()), 4)
  formula <- paste("HbA1c ~", formula)
  data <- copy(data)
  data <- data[, c("TPF", "TPFpred") := list(TPF / 5, TPFpred / 5)]

  for (i in 1:5) {
      datTmp <- data[ `_MULT_` == i]
      dsn <- svydesign(id = ~ SEQN, weights = ~ SW, data = datTmp)
      
      subDesigns <- list(subset(dsn, eval(parse(text = paste("Gender == \"Male\" & Age < 40", exclusions)))),
                      subset(dsn, eval(parse(text = paste("Gender == \"Male\" & Age >= 40", exclusions)))),
                      subset(dsn, eval(parse(text = paste("Gender == \"Female\" & Age < 40", exclusions)))),
                      subset(dsn, eval(parse(text = paste("Gender == \"Female\" & Age >= 40", exclusions)))))
      
      for (j in 1:4) {
        resImp[[j]][[i]] <- svyglm(as.formula(formula), 
                                   family = gaussian(link = "identity"), 
                                   design = subDesigns[[j]])
      }
  }

  for (i in 1:4) {
    resImp[[i]] <- summary(MIcombine(resImp[[i]]))[-1, c(1, 3, 4)]
  }
  
  resImp
}

# Return list of fitted model summaries
getFits <- function(formulas, data, exclusions) {
    fitSums <- list() 
   
    for (i in seq_along(formulas)) {
        fitSums[[i]] <- getImpFitsLinear(formulas[[i]], data, exclusions)
    }

    fitSums 
}

# Fit models based on DXA and predicted body composition variables
# and combine results into single table
#
# Input: vars - Body composition variable of interest (one of "TPF", "TLM", "TF")
#        formulas - Character vector of additional variables to include in model fits
#        data - Data in data.table format
#        exclusions - Logical expression determining exclusion criteria
combineFits <- function(vars, formulas, data, exclusions) {
    # Model fits with DXA data
    fitsTrue <- getFits(paste0(vars, " + ", formulas), data, exclusions)
    # Model fits with predicted data
    fitsPred <- getFits(paste0(vars, "pred + ", formulas), data, exclusions)

    fitsCombine <- rep(list(list()), length(formulas))

    for (i in seq_along(formulas)) {
        tmp1 <- rbindlist(lapply(fitsTrue[[i]], function(x) x[1:2,]))
        tmp2 <- rbindlist(lapply(fitsPred[[i]], function(x) x[1:2,]))
        fitsCombine[[i]] <- cbind(tmp1, tmp2)
    }

   Reduce("cbind", fitsCombine) 
}
