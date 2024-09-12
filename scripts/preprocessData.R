## This script preprocesses the data. It takes as its input an SAS file with 
## DASS-21, RAND-36 and demographic variables. It returned a trimmed .Rdata file.
## First we do the younger cohorts, then the older one.

library(tidyverse)

setwd("...")                    # YOUR DIRECTORY HERE
backupOptions <- options()


chosenData             <- list(data.frame(),
                               data.frame())
fullData               <- list(data.frame(),
                               data.frame())
fullfillsAllConditions <- list()


#------------------------------------------------------------------------------#

## YOUNGER COHORT ##

#------------------------------------------------------------------------------#

# Get the younger data. 

data <- haven::read_sas("vworkingage.sas7bdat") %>%
  data.frame() %>%
  haven::zap_formats()


# Find how many questions in each subscale each participants missed (1 allowed).

d_missing <- data %>%
  dplyr::select(c(b66_3,  b66_5,  b66_10, 
           b66_13, b66_16, 
           b66_17, b66_21)) %>%
  is.na() %>%
  rowSums()

a_missing <- data %>%
  dplyr::select(c(b66_2,  b66_4,  b66_7, 
           b66_9,  b66_15, 
           b66_19, b66_20)) %>%
  is.na() %>%
  rowSums()

s_missing <- data %>%
  dplyr::select(c(b66_1,  b66_6,  b66_8, 
         b66_11, b66_12, 
         b66_14, b66_18)) %>%
  is.na() %>%
  rowSums()

## Language (Finnish or not, boolean.) ##

# Those that answered through letter are missing the variable < b_kieli > 
# ("language in 2022"). All of them answered in Finnish, so they can be added.

data$suomi <- data$b_kieli == "Suomi" | data$b_vastaustapa == "Paperinen"


## Column names ##

colnames(data)[4:13] <- c("sukupuoli", 
                          "syntymavuosi", "syntymamaa",
                          "aidinmaa", "isanmaa", 
                          "koulutus", "aidinkoulutus",
                          "isankoulutus", "tyonantaja",
                          "siviilisaaty")
data$sukupuoli <- factor(data$sukupuoli)


## Choosing the data ##

# Conditions. #

condition1 <- is.na(data$b_palautuspvm) == 0      # Answered in 2022.
condition2 <- d_missing < 2                       # Missing at most 1 depression item.
condition3 <- a_missing < 2                       # Missing at most 1 anxiety item.
condition4 <- s_missing < 2                       # Missing at most 1 stress item.
condition5 <- is.na(data$brand_emot_well) == 0    # Not missing RAND-EW in 2022.

fullfillsAllConditions[[1]] <- condition1 & condition2 & condition3 & condition4 & condition5


# Save the data.

fullData[[1]]   <- data
chosenData[[1]] <- data[fullfillsAllConditions[[1]], ] 



#------------------------------------------------------------------------------#

## OLDER COHORT ##

#------------------------------------------------------------------------------#


# Get the older data.

data <- haven::read_sas('older.sas7bdat') %>%
  data.frame() %>%
  haven::zap_formats()


# Find how many questions in each subscale each participants missed (1 allowed).


d_missing <- data %>%
  dplyr::select(c(i27_3,  i27_5,  i27_10, 
                  i27_13, i27_16, 
                  i27_17, i27_21)) %>%
  is.na() %>%
  rowSums()

a_missing <- data %>%
  dplyr::select(c(i27_2,  i27_4,  i27_7, 
                  i27_9,  i27_15, 
                  i27_19, i27_20)) %>%
  is.na() %>%
  rowSums()

s_missing <- data %>%
  dplyr::select(c(i27_1,  i27_6,  i27_8, 
                  i27_11, i27_12, 
                  i27_14, i27_18)) %>%
  is.na() %>%
  rowSums()


## Language (Finnish or not, boolean.) ##

data$suomi <- data$k3 == 1
data$suomi <- replace_na(data$suomi, 1)


## Choosing the data ##


# Conditions. #

condition1 <- d_missing < 2                       # Missing at most 1 depression item.
condition2 <- a_missing < 2                       # Missing at most 1 anxiety item.
condition3 <- s_missing < 2                       # Missing at most 1 stress item.
condition4 <- is.na(data$irand_emot_well) == 0    # Not missing the RAND-EW.

fullfillsAllConditions[[2]] <- condition1 & condition2 & condition3 & condition4
fullData[[2]]   <- data
chosenData[[2]] <- data[fullfillsAllConditions[[2]], ]


#------------------------------------------------------------------------------#

## Save the data sets. ##

#------------------------------------------------------------------------------#

# This is the data used for the main analyses. It only contains those that
# fullfil all the conditions.

data <- chosenData
save(data, file = "chosenData.Rdata")

# This file contains the full data and logical vectors of who fullfils all
# the given conditions.

data <- fullData
save(data, fullfillsAllConditions, file = "fullData.Rdata")


# Return to your own options.
options(backupOptions)