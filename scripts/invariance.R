library(corrplot)
library(foreign)
library(lavaan)
library(lavaanPlot)
library(psych)
library(rempsyc)
library(semTools)
library(tidyverse)

## This script is used to investigate measurement invariance in the DASS-21 between 
## the subpopulations of the two cohorts.

backupOptions <- options()

#------------------------------------------------------------------------------#

## Get the DASS data, models and indices. ##

setwd("...")            # YOUR DIRECTORY HERE

load("dass.Rdata")           # Load the DASS-21 data.
load("cfaModels.Rdata")      # Load the factor analytic models.
load("scaleLocs.Rdata")      # Load the scale indices for the DASS-21 questions.

# Get gendered data.

load("chosenData.Rdata")

dass[[1]]$gender     <- data[[1]]$sukupuoli
dass[[2]]$gender     <- data[[2]]$i1

rm(data)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

## Gender invariance (see age invariance below)

fits              <- list(matrix(nrow = 4, 
                                 ncol = 4) %>% 
                            data.frame()   %>% 
                            rbind("cut-offs" = 
                                    c(">0.95", ">0.95", 
                                      "<0.06", "<0.08")))
fits[[2]]         <- fits[[1]]

for (k in 1:2) colnames(fits[[k]]) <- c("CFI", "TLI", "RMSEA", "SRMR")

configRes         <- list()
metricRes         <- list()
scalarRes         <- list()
strictRes         <- list()

configSummary     <- list()
metricSummary     <- list()
comparisons       <- list()

#------------------------------------------------------------------------------#

## 1. Configural invariance ##

# Just a multigroup CFA.

for (k in 1:2) {
  
  configRes[[k]]   <- lavaan::cfa(model = models[[1]],
                                data = dass[[k]],
                                estimator = "WLSMV",
                                group = "gender",
                                orthogonal = TRUE)
  
  configSummary[[k]] <- summary(configRes[[k]],
                                fit.measures = TRUE,
                                standardized = TRUE)
  
  fits[[k]][1, ]    <-  configSummary[[k]]$fit              %>% 
                        rbind(names(configSummary$fit), .)  %>%
                        data.frame()                        %>%
                        dplyr::select("cfi.robust", 
                               "tli.robust",
                               "rmsea.robust",
                               "srmr")
  
}


#------------------------------------------------------------------------------#


## 2. Metric invariance ##

# Factor loadings fixed, intercepts free.

for (k in 1:2) {
  
  metricRes[[k]]   <- lavaan::cfa(model = models[[1]],
                                  data = dass[[k]],
                                  estimator = "WLSMV",
                                  group = "gender", 
                                  group.equal = "loadings",
                                  orthogonal = TRUE)
}


#------------------------------------------------------------------------------#


## 3. Scalar invariance ##

# Factor loadings and intercepts fixed.

for (k in 1:2) {
  scalarRes[[k]] <- lavaan::cfa(model = models[[1]],
                                  data = dass[[k]],
                                  estimator = "WLSMV",
                                  group = "gender", 
                                  group.equal = c("loadings", "intercepts"),
                                  orthogonal = TRUE)
}


#------------------------------------------------------------------------------#

## 4. Strict invariance  ##

# Factor loadings, intercepts and residual variances fixed.


for (k in 1:2) {
  strictRes[[k]] <- lavaan::cfa(model = models[[1]],
                                data = dass[[k]],
                                estimator = "WLSMV",
                                group = "gender", 
                                group.equal = c("loadings", 
                                                "intercepts",
                                                "residuals"),
                                orthogonal = TRUE)
}

#------------------------------------------------------------------------------#

## Format ##

for (k in 1:2) {
  comparisons[[k]]     <- compareFit(configRes[[k]],
                                     metricRes[[k]],
                                     scalarRes[[k]],
                                     strictRes[[k]])
  
  fits[[k]][2:4, ] <- comparisons[[k]]@fit.diff %>%
    dplyr::select("cfi.robust", 
                  "tli.robust",
                  "rmsea.robust",
                  "srmr")
  
  fits[[k]][2, ]   <- (fits[[k]][1, ] %>% as.numeric) + (fits[[k]][2, ] %>% as.numeric)
  fits[[k]][3, ]   <- (fits[[k]][2, ] %>% as.numeric) + (fits[[k]][3, ] %>% as.numeric)
  fits[[k]][4, ]   <- (fits[[k]][3, ] %>% as.numeric) + (fits[[k]][4, ] %>% as.numeric)
}


# Round all numbers, fix names.

for (k in 1:2) for (i in 1:4) for (j in 1:4) fits[[k]][i,j] <- fits[[k]][i,j] %>% 
  as.numeric %>%
  round(digits = 3)

fits           <- data.frame(c("Configural", "Metric",
                               "Scalar", "Strict",
                               "Cut-offs"),
                              fits[[1]], fits[[2]])
colnames(fits) <- c("Model",
                     "Working-age.cfi", "Working-age.tli",
                     "Working-age.rmsea", "Working-age.srmr",
                     "Older.cfi", "Older.tli",
                     "Older.rmsea", "Older.srmr")


#------------------------------------------------------------------------------#

##  Format nicely.  ##


title = c("Table 5", 
          "Inter-gender invariance analyses in the two cohorts between men and women (2022)")
note = "Fits estimated using the robust weighted-least-squares method (WLSMV) estimation method. CFI = Comparative Fit Index; TLI = Tucker-Lewis Index; RMSEA = Root Mean Square Error of Approximation; SRMR = Standardised Root Mean Squared Residual."

niceInvarianceTable <- nice_table(fits, 
                           title = title,
                           note = note,
                           separate.header = TRUE)



## Display ##

print(niceInvarianceTable)


##  Save.  ##

# flextable::save_as_docx(niceInvarianceTable, path = paste0(getwd(), "/genderInvarianceTable.docx"))


#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

## Age invariance (see age invariance below).

# Reshape the data, add a dummy for age.

dass[[1]] <- cbind(dass[[1]][, 1:21], 
                   "age" = 0)
dass[[2]] <- cbind(dass[[2]][, 1:21], 
                   "age" = 1)  

dassAge   <- rbind(dass[[1]],
                   dass[[2]])

rm(k, dass)

# Prepare some objects.

fitsAge   <- matrix(nrow = 4, 
                    ncol = 4)    %>% 
                    data.frame() %>% 
                    rbind( "cut-offs" = c(">0.95", ">0.95", 
                                          "<0.06", "<0.08") )

colnames(fitsAge) <- c("CFI", "TLI", "RMSEA", "SRMR")

configResAge         <- list()
metricResAge         <- list()
scalarResAge         <- list()
strictResAge         <- list()

configSummaryAge     <- list()
metricSummaryAge     <- list()
comparisonsAge       <- list()

#------------------------------------------------------------------------------#

## 1. Configural invariance ##

# Just a multigroup CFA.

configResAge   <- lavaan::cfa(model = models[[1]],
                              data = dassAge,
                              estimator = "WLSMV",
                              group = "age",
                              orthogonal = TRUE)

configSummaryAge <- summary(configResAge,
                            fit.measures = TRUE,
                            standardized = TRUE)

fitsAge[1, ]    <-  configSummaryAge$fit              %>% 
  rbind(names(configSummary$fit), .)  %>%
  data.frame()                        %>%
  dplyr::select("cfi.robust", 
                "tli.robust",
                "rmsea.robust",
                "srmr")


#------------------------------------------------------------------------------#


## 2. Metric invariance ##

# Factor loadings fixed, intercepts free.

metricResAge   <- lavaan::cfa(model = models[[1]],
                              data = dassAge,
                              estimator = "WLSMV",
                              group = "age", 
                              group.equal = "loadings",
                              orthogonal = TRUE)


#------------------------------------------------------------------------------#


## 3. Scalar invariance ##

# Factor loadings and intercepts fixed.

scalarResAge <- lavaan::cfa(model = models[[1]],
                            data = dassAge,
                            estimator = "WLSMV",
                            group = "age", 
                            group.equal = c("loadings", "intercepts"),
                            orthogonal = TRUE)


#------------------------------------------------------------------------------#

## 4. Strict invariance  ##

# Factor loadings, intercepts and residual variances fixed.



strictResAge <- lavaan::cfa(model = models[[1]],
                            data = dassAge,
                            estimator = "WLSMV",
                            group = "age", 
                            group.equal = c("loadings", 
                                            "intercepts",
                                            "residuals"),
                            orthogonal = TRUE)

#------------------------------------------------------------------------------#

## Format ##

comparisonsAge     <- compareFit(configResAge,
                                 metricResAge,
                                 scalarResAge,
                                 strictResAge)
  
fitsAge[2:4, ] <- comparisonsAge@fit.diff %>%
  dplyr::select("cfi.robust", 
                "tli.robust",
                "rmsea.robust",
                "srmr")

fitsAge[2, ]   <- (fitsAge[1, ] %>% as.numeric) + (fitsAge[2, ] %>% as.numeric)
fitsAge[3, ]   <- (fitsAge[2, ] %>% as.numeric) + (fitsAge[3, ] %>% as.numeric)
fitsAge[4, ]   <- (fitsAge[3, ] %>% as.numeric) + (fitsAge[4, ] %>% as.numeric)


# Round all numbers, fix names.

for (i in 1:4) for (j in 1:4) fitsAge[i,j] <- fitsAge[i,j] %>% 
                                              as.numeric   %>%
                                              round(digits = 3)

fitsAge <- data.frame("Model" = c("Configural", "Metric",
                                  "Scalar", "Strict",
                                  "Cut-offs"),
                      fitsAge)

colnames(fitsAge) <- paste0(colnames(fitsAge), " ") # else nice_table breaks


#------------------------------------------------------------------------------#

##  Format nicely.  ##


titleAge <- c("Table 5", 
              "Inter-age cohort invariance analyses in the two cohorts between men and women (2022)")
note     <- "Fits estimated using the robust weighted-least-squares method (WLSMV) estimation method. CFI = Comparative Fit Index; TLI = Tucker-Lewis Index; RMSEA = Root Mean Square Error of Approximation; SRMR = Standardised Root Mean Squared Residual."

niceInvarianceTableAge <- nice_table(fitsAge, 
                                     title = titleAge,
                                     note = note)



## Display ##

print(niceInvarianceTableAge)


##  Save.  ##

# flextable::save_as_docx(niceInvarianceTable, path = paste0(getwd(), "/invarianceTableAge.docx"))


#------------------------------------------------------------------------------#

options(backupOptions)
