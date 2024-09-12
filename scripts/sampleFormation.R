library(DiagrammeR)
library(visR)
library(tidyverse)

## Load the data. ##

backupOptions <- options()

setwd(".../data")              #  YOUR DIRECTORY HERE"

load("fullData.Rdata")

#------------------------------------------------------------------------------#

# Miscellaneous transformations.

data[[1]]$fullfillsAllConditions <- fullfillsAllConditions[[1]]
data[[2]]$fullfillsAllConditions <- fullfillsAllConditions[[2]]

data[[1]]$missing_2017          <- data[[1]]$palautuspvm %>% is.na()
data[[1]]$missing_2022          <- data[[1]]$b_palautuspvm %>% is.na()

data[[2]]$missing_2000          <- FALSE # Account for nonparticipation (see below).
data[[2]]$missing_2020          <- data[[2]]$i_palautuspvm %>% is.na()


# Account for nonparticipation.

df         <- matrix(NA, 5561, 206) %>% data.frame
names(df)  <- names(data[[1]])
data[[1]]  <- rbind(data[[1]], df)

data[[1]]$missing_2017          <- data[[1]]$palautuspvm %>% is.na()
data[[1]]$missing_2022          <- data[[1]]$b_palautuspvm %>% is.na()

data[[1]]$ID[5899:11459]        <- 105899:111459

df         <- matrix(NA, 4384, 86) %>% data.frame()

names(df)  <- names(data[[2]])
data[[2]] <- rbind(data[[2]], df)

data[[2]]$tunnus1[8961:13344]        <- 1:4384

#------------------------------------------------------------------------------#

## The young first. ##

# Create the diagram. Some definitions:

attrition      <- list()
diagram        <- list()

attrition[[1]] <- visR::get_attrition(data[[1]],
             criteria_descriptions = c("1. Participated in 2017.",
                                       "2. Participated in 2022.",
                                       "3. Answered to DASS and RAND-EW",
                                       "4. Answered in Finnish."),
             criteria_conditions   = c("missing_2017 == FALSE",
                                       "missing_2022 == FALSE",
                                       "fullfillsAllConditions == TRUE",
                                       "suomi == TRUE"),
             subject_column_name   = "ID")


# Fix the first text and add commas for thousands.

attrition[[1]]$Criteria[1] <- "Target cohort."

#attrition[[1]]$'Remaining N' <- sapply(attrition[[1]]$'Remaining N', 
#                                       FUN = function(x) format(round(as.numeric(x), 1), nsmall=1, big.mark=","))


# Add complement info.

attrition[[1]]$Complement <- c(
  "NA",
  "Nonparticipation.",
  "Attrition.",
  "Skipped RAND-EW or ≥ 2 questions in any of the DAS subscales.",
  "Answered in Swedish (n = 75), English (n = 21) or Russian (n = 12)."
)


# Plot the diagram.

diagram[[1]]             <- visR::visr(attrition[[1]], 
                       description_column_name = "Criteria", 
                       value_column_name = "Remaining N",
                       complement_column_name = "Complement")

#------------------------------------------------------------------------------#

## Then the old ##

# Create the diagram. Some definitions:

attrition[[2]] <- visR::get_attrition(data[[2]],
              criteria_descriptions = c("1. Participated in 2000-2002.",
                                        "2. Participated in 2020.",
                                        "3. Answered to DASS and RAND-EW",
                                        "4. Answered in Finnish."),
              criteria_conditions   = c("missing_2000 == FALSE",
                                        "missing_2020 == FALSE",
                                        "fullfillsAllConditions == TRUE",
                                        "suomi == TRUE"),
              subject_column_name   = "tunnus1")


# Fix the first text and add commas for thousands.

attrition[[2]]$Criteria[1]      <- "Target cohort."

#attrition[[2]]$'Remaining N' <- sapply(attrition[[2]]$'Remaining N', 
#                                       FUN = function(x) format(round(as.numeric(x), 1), nsmall=1, big.mark=","))


# Add complement info.

attrition[[2]]$Complement <- c(
  "NA",
  "Nonparticipation",
  "Attrition.",
  "Skipped RAND-EW or ≥ 2 questions in any of the DAS subscales.",
  "Answered in Swedish (n = 245) or English (n = 29)."
)


# Plot the diagram.

diagram[[2]]                  <- visR::visr(attrition[[2]], 
                       description_column_name = "Criteria", 
                       value_column_name = "Remaining N",
                       complement_column_name = "Complement")

#------------------------------------------------------------------------------#

# Reload options.

options(backupOptions)