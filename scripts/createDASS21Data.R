## This script takes as its input two sets of chosen data: one among the younger
## cohort, the other among the older one. The input data have been sorted. This
## script simply further condenses them down, as certain script (e.g. cfa.R)
## require no demographic variables. The output includes three objects:
## 1. < dass >   [list of 2 DFs]     DASS-21 data for each cohort.
## 2. < scales > [list of 2 DFs]     DASS-21 sum scores (subscales) for each cohort.
## 3. < suomi >  [list of 2 vectors] TRUE = answered in Finnish, FALSE = not.

library(tidyverse)

backupOptions <- options()
setwd(".../data")              #  YOUR DIRECTORY HERE"

#------------------------------------------------------------------------------#

dass   <- list()
scales <- list()
suomi  <- list()
id     <- list()

load("chosenData.Rdata")    

# Younger cohort

dass[[1]]           <- data[[1]][, 151:171]
colnames(dass[[1]]) <- paste(rep("Q", 21), 
                             1:21, sep = "")
suomi[[1]]          <- data[[1]]$suomi
id[[1]]             <- data[[1]]$ID

# Older cohort

dass[[2]]           <- data[[2]][, 49:69]
colnames(dass[[2]]) <- paste(rep("Q", 21), 
                             1:21, sep = "")
suomi[[2]]          <- data[[2]]$suomi
id[[2]]             <- data[[2]]$tunnus1

# Fix NAs for language.

suomi[[2]] <- replace_na(suomi[[2]], 1)

load("scaleLocs.Rdata")

#------------------------------------------------------------------------------#

## Number of NAs per DASS scale. ##

youngNaRatio <- lapply(dass[[1]], FUN = function(x) sum(is.na(x))) %>% 
                unlist() / 3206

youngNaRatioByScale <- c( "D" = youngNaRatio[scaleLocs$d] %>% sum(),
                        "A" = youngNaRatio[scaleLocs$a] %>% sum(),
                        "S" = youngNaRatio[scaleLocs$s] %>% sum() )

oldNaRatio <- lapply(dass[[2]], FUN = function(x) sum(is.na(x))) %>% 
                unlist() / 5736

oldNaRatioByScale <- c( "D" = oldNaRatio[scaleLocs$d] %>% sum(),
                        "A" = oldNaRatio[scaleLocs$a] %>% sum(),
                        "S" = oldNaRatio[scaleLocs$s] %>% sum() )

#------------------------------------------------------------------------------#

for (k in 1:2) { # 2 cohorts
  
  dass[[k]] <- dass[[k]] - 1
  
  ## Impute the missing values (max 1 per scale) with the average over the scale.
  
  # Find missing values.
  
  naInd <- which(is.na(dass[[k]]), arr.ind=TRUE)
  
  for (i in 1:3) {
    
    scaleInd    <- scaleLocs[i] %>% unlist  #  The locations of the Qs for the scale.
    
    qsToBeReplaced <- naInd[naInd[, 2] %in% scaleInd, ]
    
    for (j in 1:dim(qsToBeReplaced)[1]) {
      
      ind         <- qsToBeReplaced[j, ] %>% unlist %>% c
      chosenRow   <- dass[[k]][ind[1], scaleInd]
      dass[[k]][ind[1], ind[2]] <- sum(chosenRow, na.rm = TRUE) / 6
      
    }
  }
  
  ## Table the sum variables. ##
  
  scales[[k]]        <- data.frame(
    dass[[k]] %>% dplyr::select(scaleLocs$d) %>% rowSums(),
    dass[[k]] %>% dplyr::select(scaleLocs$a) %>% rowSums(),
    dass[[k]] %>% dplyr::select(scaleLocs$s) %>% rowSums(),
    dass[[k]]                         %>% rowSums())
  
  names(scales[[k]]) <- c("D", "A", "S", "GD")
}

save(dass, scales, suomi, id, file = "dass.Rdata")
