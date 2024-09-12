## This script takes the full data set for both cohorts and attrition analyses.
## Note: this only analyses attrition between current state and the earliest 
## measurement (2000 for the older cohort, 2017 for the younger). Previous 
## nonparticipation analyses are covered in the Method section of the article.

library(tidyverse)
library(rempsyc)

baseDir    <- "..."            # YOUR BASE DIRECTORY HERE

#------------------------------------------------------------------------------#

# Create paths.

subSFolder <- paste0(baseDir, "scripts/subscripts")
dataFolder <- paste0(baseDir, "data")

# Load the subscript. 

setwd(subSFolder)
source("attritionSub.R")

# Load the data.

setwd(dataFolder)                      
load("fullData.Rdata")

# Create empty lists. 

attritionTable      <- list()
niceAttritionTable  <- list()

#------------------------------------------------------------------------------#

## Transform data. General fixes. ##

#------------------------------------------------------------------------------#

## Define attrition. ##

attrition <- list()
attrition[[1]] <- is.na(data[[1]]$b_palautuspvm)  # 2022 completion date == NA
attrition[[2]] <- is.na(data[[2]]$i_palautuspvm)  # 2022 completion data == NA


## Transform education and marital status. ##

# Younger cohort.

data[[1]]$education3      <- {data[[1]]$koulutus / 2} %>% unlist() %>% ceiling()
data[[1]]$marital_status3 <- {data[[1]]$siviilisaaty / 2} %>% unlist() %>% ceiling()

# Older cohort.

data[[2]]$education3      <- {data[[2]]$k4 / 2} %>% unlist() %>% ceiling()
data[[2]]$marital_status3 <- {data[[2]]$h3 / 2} %>% unlist() %>% ceiling()


## Transform SEP. ##

# Younger cohort.

ses           <- data[[1]]$sep4l_aalto1_nuoret
ses           <- replace(ses, ses == 11, 4)   # merge routine and manual labour
ses           <- replace(ses, ses == 22, 23)  # merge entrepreneur and other
ses           <- replace(ses, ses == 91, 92)  # merge retired and outside employment

data[[1]]$ses <- ses %>% factor(levels = c(2, 3, 4, 23, 92))

# Older cohort.

ses           <- data[[2]]$sep4l_ks
ses           <- replace(ses, ses == 11, 4)   # merge routine and manual labour
ses           <- replace(ses, ses == 22, 23)  # merge entrepreneur and other
ses           <- replace(ses, ses == 91, 92)  # merge retired and outside employment

data[[2]]$ses <- ses %>% factor(levels = c(2, 3, 4, 23, 92))

rm(ses)

#------------------------------------------------------------------------------#

## Choose the variables ##

chosenData      <- list()

chosenData[[1]] <- data[[1]] %>% dplyr::select(
  gender = sukupuoli,                 # Gender in 2017.
  age    = a_ika,                     # Age in 2017.
  BMI    = aBMI,                      # BMI in 2017.
  84:91,                              # RAND in 2017.
  education3,                         # Education in 2017.
  marital_status3,                    # Marital status in 2017.
  ses                                 # SES in 2017. 
)                            %>%
                                  dplyr::mutate(
    gender          = gender          %>% factor,
    education3      = education3      %>% factor,
    marital_status3 = marital_status3 %>% factor,
    ses             = ses             %>% factor
)

chosenData[[2]] <- data[[2]] %>% dplyr::select(
  gender = h1,                        # Gender in 2017.
  age    = h_ika,                     # Age in 2017.
  hBMI,                               # BMI in 2017.
  10:17,                              # RAND in 2017.
  education3,                         # Education in 2017.
  marital_status3 = h3,               # Marital status in 2017.
  ses                                 # SES in 2017.
)                            %>%
  dplyr::mutate(
    gender          = gender          %>% factor,
    education3      = education3      %>% factor,
    marital_status3 = marital_status3 %>% factor,
    ses             = ses             %>% factor
  )

rm(data)

# Create an index column of variables that are continuous:

contBool <- c(0,1,1,                  # Gender, age, BMI,
             rep(1, 8),               # RAND,
             0, 0, 0)                 # education, marital status, SES.

#------------------------------------------------------------------------------#

## Run the analyses ##

#------------------------------------------------------------------------------#

attritionTable[[1]] <- makeAttrTable(data = chosenData[[1]], 
                                     contBool = contBool,
                                     attrition = attrition[[1]])

attritionTable[[2]] <- makeAttrTable(data = chosenData[[2]], 
                                     contBool = contBool,
                                     attrition = attrition[[2]])

##  Format nicely.  ##

niceAttritionTable[[1]] <- nice_table(attritionTable[[1]], title = c("Table 1", 
                                                                     "Attrition analysis in the working-age cohort. All values for variables measured in 2017."),
                                      separate.header = TRUE,
                                      note = "N = 5898,  4630 women; 95 % confidence interval given.
                        p-values from Welch's unequal variances T-test.")

niceAttritionTable[[2]] <- nice_table(attritionTable[[2]], title = c("Table 1", 
                                                                     "Attrition analysis in the older cohort. All values for variables measured in 2017."),
                                      separate.header = TRUE,
                                      note = "N = 5898,  4630 women; 95 % confidence interval given.
                        p-values from Welch's unequal variances T-test.")

#------------------------------------------------------------------------------#

## Display ##

# niceAttritionTable[[1]]
# niceAttritionTable[[2]]


##  Save.  ##

# flextable::save_as_docx(niceAttritionTable[[1]], path = paste0(getwd(), "/attritionWorking-age.docx"))
# flextable::save_as_docx(niceAttritionTable[[2]], path = paste0(getwd(), "/attritionOlder.docx"))
