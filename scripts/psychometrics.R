# Creates a table of statistics on the DASS questions and subscales (D, A, S, GD),
# including range, n, mean, median, SD, skew, kurtosis, alpha, omegas and SEm.

require(tidyverse)
require(rempsyc)

## SETTINGS ##

dataFolder       <- ".../data"                # YOUR DIRECTORY HERE
subScriptsFolder <- ".../scripts/subscripts"  # YOUR DIRECTORY HERE

## Load the data. ##

setwd(dataFolder)          
load("scaleLocs.Rdata")    # Load the locations of the scales among the questions.
load("dass.Rdata")         # Load the data.


## Load the subscript. ##

setwd(subScriptsFolder)
source("psychometricsSub.R")
setwd(dataFolder)  


## Produce Table 5. ##

tableFive  <- psychometricsTable(dass, scaleLocs, TRUE)
print(tableFive[[2]])

## Compare Finnish answers with non-Finnish answers. ##

nonFinnish <- psychometricsTable(dass, scaleLocs, FALSE)
comparison <- tableFive$raw[, 3:20] - nonFinnish$raw[, 3:20]
comparison <- cbind(tableFive$raw[, 1:2], comparison)

niceComparison  <- nice_table(comparison, 
                            title = c("", 
                                      "The raw differences in sample distribution and reliability statistics between all participants and Finnish responses only."),
                            separate.header = TRUE,
                            note = "SEm = SD × √(1 - reliability). n1 = Finnish answers, n2 = non-Finnish answers. Skew estimated as G1, kurtosis as G2. For details, see Joanes & Gill (1998).",
                            width = 1)

print(niceComparison)
