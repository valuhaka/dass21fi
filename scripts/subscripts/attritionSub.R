## This folder contains subscripts for the file < attritionAnalyses.R >.
## See there for more.

# Prepare the table.
# Note: < contBool > is a column of booleans naming the continuous
# variables (e.g. BMI) as opposed to factor variables (e.g. SES).
# Note: < attrition > is a column of booleans for attrition per participant.


#------------------------------------------------------------------------------#


## Function to write the relative portion of cases after a value. ##

addRelativePortion    <- function(x) {
  x <- x %>% as.numeric() %>%
    {. / sum(.)} %>% 
    round(digits = 2) %>% 
    paste0(x, " (", ., ")")
}

#------------------------------------------------------------------------------#

makeAttrTable      <- function(data, contBool, attrition) {
  
  nRows                    <- 21               # CHANGE
  
  attritionTable           <- matrix(NA, nRows, 10) %>% data.frame
  
  colnames(attritionTable) <- c("variable",  "men.2017 sample", "men.2022 sample", "men.p",
                                "women.2017 sample", "women.2022 sample", "women.p",
                                "all.2017 sample", "all.2022 sample", "all.p")
  
  #----------------------------------------------------------------------------#
  
  ## Label the variables. ##
  
  #----------------------------------------------------------------------------#
  
  dataNames <-  colnames(data)
  
  # Create an index to locate the variables in the final data frame (see below).
  
  nCoordsList <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 14, 17, 20, 22)
  
  varNames                <- c("Age", "BMI", "RAND-PF", "RAND-RP", "RAND-EMOT", 
                               "RAND-ENER", "RAND-EW", "RAND-SF", "RAND-P", 
                               "RAND-GH", "Lower education", "Middling education",
                               "Higher education", "Single", "Married or cohabiting", 
                               "Divorced or widowed", "Professionals and managers",
                               "Semi-professionals", "Routine workers", 
                               "Other employment", "Retired or unemployed"
  )
  
  attritionTable$variable <- varNames
  
  
  # Create the table and add continuous data.
  
  # This loops through both genders (s=0 and s=1) and creates subdata ("sexData")
  # from them. It lists discrepancies in all variables between elimination groups. 
  # Finally (s=2), it does the same for the whole group.
  
  for (s in 0:2) {
    
    if (s == 0 || s == 1) {
      sexData     <- data[data$gender == s + 1, ]  
      sexAttr     <- attrition[data$gender == s + 1]
    } else {
      sexData     <- data
      sexAttr     <- attrition
    }
    
    sampleN   <- dim(sexData)[1]
    
    for (i in 2:length(data)) {
      
      if (s == 2) browser()
      
      chosenCol <- sexData[, i]
      
      ## Continuous variables.
      
      if (contBool[i] == 1) {
        
        # Produce the values describing the continuous values. That is,
        # means, confidence intervals and p-values for the differences.
        
        means     <- tapply(chosenCol, INDEX = sexAttr, 
                            FUN = mean, na.rm = TRUE) %>% array() %>% round(1)
        stds      <- tapply(chosenCol, INDEX = sexAttr, 
                            FUN = sd, na.rm = TRUE) %>% array()
        se        <- stds / sqrt(sampleN)
        t         <- qt(p = 0.025, df = sampleN - 1, lower.tail = FALSE)
        err       <- t * se
        cis       <- paste("[", 
                           round(means - err, 1), 
                           "-", 
                           round(means + err, 1), 
                           "]", 
                           sep = "")
        
        # Format the table.
        
        attritionTable[i - 1, 2:3 + 3*s] <- paste(means, cis)
        bounds           <- data.frame(means - err, means + err)
        
        attritionTable[i - 1, 4 + 3*s]   <- t.test(chosenCol, chosenCol[sexAttr == TRUE])$p.value %>%
          round(digits = 2)
        
      } else if (i == 14) {
        
        ## Treat SES separately. ##
        
        ## Below is an overtly complicated solution to a problem. ##
        ## Namely that both cohorts haven't got all the classes of workers. ##
        
        # Create an empty table.
        
        sesTable           <- matrix(NA, 5, 3) %>% data.frame
        sesRows            <- c(2, 3, 4, 23, 92)    # These are the factor names.
        
        # Crosstabulate by gender.
        
        crossTable         <- table(chosenCol, sexAttr) %>% data.frame
        
        # Join the tables.
        
        for (m in 1:5){
          vars            <- crossTable[crossTable$chosenCol == sesRows[m],]
          
          # If missing the factor, jump to the next iteration.
          if (dim(vars)[1] > 0) {
            sesTable[m, 1]  <- vars[vars$sexAttr == FALSE,]$Freq
            sesTable[m, 2]  <- vars[vars$sexAttr == TRUE, ]$Freq
          } else {
            sesTable[m, 1]  <- 0
            sesTable[m, 2]  <- 0 
          }
        }
        
        sesTable[, 1] <- addRelativePortion(sesTable[, 1])
        sesTable[, 2] <- addRelativePortion(sesTable[, 2])
        
        
        # Find the correct location to place the data frame fragment ("df").
        
        mCoords  <- (2:4) + 3*s
        
        # Finally bind to the full attrition table.
        
        attritionTable[17:21, mCoords] <- sesTable
        
      } else {
        
        # Then format the remaining factor-type variables.
        
        # Crosstabulate...
        
        crossTable   <- table(chosenCol, sexAttr) %>% matrix(ncol = 2)
        
        # ... and format (first half: 2017 measurement, second half: 2022 measurement).
        
        df <- data.frame(addRelativePortion(crossTable[, 1]), 
                         addRelativePortion(crossTable[, 2]),
                         rep(NA, dim(crossTable)[1]))
        
        # Find the correct location to place the data frame fragment ("df").
        
        nCoords  <- nCoordsList[i]:(nCoordsList[i + 1] - 1)
        mCoords  <- (2:4) + 3*s
        
        # Finally bind to the full attrition table.
        
        attritionTable[nCoords, mCoords] <- df
      }
    }
  }
  
  return(attritionTable)
}